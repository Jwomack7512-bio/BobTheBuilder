mathml2R <-function(node)  {UseMethod("mathml2R", node)}

mathml2R.XMLDocument <-function(doc) {return(mathml2R(doc$doc$children))}

mathml2R.default<-function(children) {  
  expr <- expression()  # this gets used when a "list" of children nodes are sent in
  n=length(children)
  #    cat("into default length n is ",n,"\n")
  #    for(i in children)  expr=c(expr, mathml2R(i)) 
  for(i in 1:n)  expr=c(expr, mathml2R(children[[i]])) 
  if (n>3) {#print("n>3")  # this fixes libsbml problem that times is not binary
    if (expr[[1]]=="*") expr[[1]]=as.name("prod") # in R, prod takes arb # of args
    if (expr[[1]]=="+") expr[[1]]=as.name("sum")  # similary for sum
  }
  #    print(children)
  #    print(expr)
  #    print("leaving default")
  return(expr)
}

mathml2R.XMLNode <-function(node){
  nm <- xmlName(node) 
  # cat("XMLNode: node name is ",nm," and the node class is",class(node),"\n")
  if(nm=="power"||nm == "divide"||nm =="times"||nm=="plus"||nm=="minus") {
    op <- switch(nm, power="^", divide="/",times="*",plus="+",minus="-")
    val <- as.name(op)
  } else if((nm == "ci")|(nm == "cn")) {
    if(nm == "ci") val <- as.name(node$children[[1]]$value)
    if(nm == "cn") val <- as.numeric(node$children[[1]]$value)
  }  else if(nm == "apply") {
    val <- mathml2R(node$children)
    mode(val) <- "call"
  } else  {cat("error: nm =",nm," not in set!\n")}
  return(as.expression(val))
}

Attributes2Tibble <- function(xmlAttributeStruct) {
  # When parsing sbml things get weird. Convert these structures to df
  out.list <- list()
  for (i in seq_along(xmlAttributeStruct)) {
    out.list[[i]] <- unlist(attributes(xmlAttributeStruct[[i]]))
  }
  
  return(bind_rows(out.list))
}

listToXml <- function(item, tag){
  if(typeof(item)!='list')
    return(xmlNode(tag, item))
  xml <- xmlNode(tag)
  for(name in names(item)){
    xml <- append.xmlNode(xml, listToXml(item[[name]], name))
  }
  return(xml)
}

LoadSBML <- function(sbmlFile) {
  
  
  # Check if certain structures exist:
  # Search For
  #     Compartments
  #     Species
  #     Parameters
  #     Equations
  #     Reactions
  #     Rules
  
  # Set initializers and bools
  out <- list()
  exists.listOfCompartments <- FALSE
  exists.listOfSpecies <- FALSE
  exists.listOfParameters <- FALSE
  exists.listOfRules <- FALSE
  exists.listOfReactions <- FALSE
  exists.parInReactions <- FALSE
  
  # Keep xml doc to remove eqn maths
  doc <- xmlTreeParse(sbmlFile, ignoreBlanks = TRUE)
  
  # Extract model from sbml file
  sbmlList <- read_xml(sbmlFile) %>% as_list()
  modelList <- sbmlList$sbml$model
  out[["model"]] <- modelList
  print(names(modelList))
  
  # Extract Compartments
  if (!is.null(modelList$listOfCompartments)) {
    out[["compartments"]] <- Attributes2Tibble(modelList$listOfCompartments)
    exists.listOfCompartments <- TRUE
  }
  
  # Extract Species
  if (!is.null(modelList$listOfSpecies)) {
    out[["species"]] <- Attributes2Tibble(modelList$listOfSpecies)
    exists.listOfSpecies <- TRUE
  }
  
  # Extract Parameters
  if (!is.null(modelList$listOfParameters)) {
    listOfParameters <- Attributes2Tibble(modelList$listOfParameters)
    exists.listOfParameters <- TRUE
  }
  
  # Extract Rules
  if (!is.null(modelList$listOfRules)) {
    rules.header <- Attributes2Tibble(modelList$listOfRules)
    rules.assignment.vars <- rules.header %>% pull(variable)
    rules.list <- ExtractRulesMathFromSBML(doc, rules.assignment.vars)
    
    out[["rules"]] <- rules.list
    exists.listOfRules <- TRUE
  }
  
  # Extract Reactions
  if (!is.null(modelList$listOfReactions)) {
    # Use this still to extract id and reversible and then use other parsers to 
    # grab other info
    reaction.tags <- Attributes2Tibble(modelList$listOfReactions)
    exists.listOfReactions <- TRUE
    
    # Loop through reactions grabbing relevant information
    reaction.list <- vector("list", length(modelList$listOfReactions))
    reaction.par.df <- tibble()
    for (i in seq_along(modelList$listOfReactions)) {
      # Separate current reaction node
      current.reaction <- modelList$listOfReactions[[i]]
      
      # Cycle through node finding the elements we want
      for (j in seq_along(current.reaction)) {
        cur.node <- current.reaction[j]
        node.name <- names(cur.node)
        
        if (node.name == "listOfReactants") {
          
          node.reactants <- Attributes2Tibble(cur.node$listOfReactants)
          # Grab the species from tibble
          spec.grab <- node.reactants %>% pull(species)
          # Condense multiple values to be comma separated
          collapsed.grab <- paste(spec.grab, collapse = ",");
          reaction.list[[i]]$reactants <- collapsed.grab
          
        } else if (node.name == "listOfProducts") {
          node.products <- Attributes2Tibble(cur.node$listOfProducts)
          reaction.list[[i]]$products <- paste(node.products %>% pull(species),
                                               collapse = ",")
        } else if (node.name == "kineticLaw") {
          # We want to extract the parameters here
          node.par <- Attributes2Tibble(cur.node$kineticLaw$listOfParameters)
          # Build Parameter df to join with parameters
          reaction.par.df <- rbind(reaction.par.df, node.par)

          if (!is.null(node.par)) {exists.parInReactions <- TRUE}
          
          # Condense parameter data to build with equations table
          reaction.list[[i]]$parameters <- paste(node.par %>% pull(id),
                                                 collapse = ",")
          reaction.list[[i]]$parameters.val <- paste(node.par %>% pull(value),
                                                     collapse = ",")
        } else {
          #print(paste0("Not Accounted For: ", node.name))
        }
      }
    }
    # Add math to reactions list
    reaction.list <- ExtractReactionMathFromSBML(doc, reaction.list)
    
    # Create df with all equation information
    reaction.list <- cbind(bind_rows(reaction.list), reaction.tags)
    out[["reactions"]] <- reaction.list
    
    # Clean up parameter df to match format (need names, constant)
    n.pars <- nrow(reaction.par.df)
    name <- reaction.par.df$id
    constant <- rep("true", n.pars)
    
    reaction.par.df <- cbind(reaction.par.df, name, constant)
    
  }
  
  # Bind Parameter lists if they both exist
  # Many times this will pull the same parameter if it is used in multiple 
  # places. I will not remove them here for completeness. 
  if (exists.parInReactions & exists.listOfParameters) {
    # join data
    final.par.df <- bind_rows(listOfParameters, reaction.par.df)
  } else if(exists.parInReactions & !exists.listOfParameters) {
    final.par.df <- reaction.par.df
  } else if (!exists.parInReactions & exists.listOfParameters) {
    final.par.df <- listOfParameters
  }
  
  # Convert nas to true in constant
  if (!is.null(final.par.df$constant)) {
    final.par.df$constant[is.na(final.par.df$constant)] <- "true"
  }

  out[["parameters"]] <- final.par.df
  return(out)
}


# The next two functions are used by rules and were taken straight from read.SBML
# The idea is that SBML doesn't provide a list of atoms/leaves with rules, so we have to create them
# to place them in their model slots, and to use them to create the R function definition for the rule
# using makeLaw with a null for parameters, since they are passed global for rules.
# map MathML operator symbols into R symbols
ML2R <- function(type) {
  switch(type,
         "times" = "*",
         "divide" = "/",
         "plus" = "+",
         "minus" = "-",
         "power" = "^",
         "exp" = "exp",
         "ln" = "log",
         "not found") 
}   

getRuleLeaves <- function(math) { 
  n=length(math)
  S=c(NULL)
  op=ML2R(xmlName(math[[1]]))
  for (j in 2:n ) {
    if ((xmlName(math[[j]])=="ci")|(xmlName(math[[j]])=="cn")) {
      S=c(S,as.character(xmlValue(math[[j]])))
    } else {
      S=c(S,Recall(math[[j]])  )
    } 
  }
  
  
  return(S)
} 


ExtractRulesMathFromSBML <- function(doc, assignmentVars) {
  # Extracts mathmatical rules from sbml document that use assignment
  # An instance of this is a parameter that is not constant: V1 = 5*V1i
  #
  # Inputs: 
  #   doc - parsed xml doc from xmltreeparse
  #   assignmentVars - vars on left hand side of rules (V1)
  
  # Parse to rules section
  rules <- doc$doc$children$sbml[["model"]][["listOfRules"]]
  n.rules <- length(rules)
  
  rulesList <- vector("list", n.rules)
  # Extract mathml for each rule and store info to list
  for (i in seq_along(rules)) {
    
    mathml    <- rules[[i]][["math"]][[1]]
    e         <- mathml2R(mathml)
    e.exp.law <- e[[1]]
    e.str.law <- gsub(" ","",toString(e[1]))
    
    rulesList[[i]]$LHS.var <- assignmentVars[i]
    rulesList[[i]]$mathml <- toString(mathml)
    rulesList[[i]]$str.law <- e.str.law
  }
  
  return(rulesList)
}

ExtractReactionMathFromSBML <- function(doc, reactionList) {
  # xmlDoc - parsed xml doc from xmltreeparse
  # reactionList - list of reactions to update
  
  # print(reactionList)
  # doc <- xmlTreeParse(sbmlFile, ignoreBlanks = TRUE)
  # Extract reactions from xml tree
  reactions=doc$doc$children$sbml[["model"]][["listOfReactions"]]
  n.reactions <- length(reactions)
  
  # print(length(reactionList))
  for (i in seq_along(reactions)) {
    # Extract mathematical expression
    mathml.exp <- toString(reactions[[i]][["kineticLaw"]][["math"]])
    # print(mathml.exp)
    exp.r <- reactions[[i]][["kineticLaw"]][["math"]][[1]]
    # Convert mathml to r
    e <- mathml2R(exp.r)
    # Remove from expression tag
    e.exp.law <- e[[1]]
    # Convert to full string law
    e.str.law <- gsub(" ", "", toString(e[1]))
    
    # Append information to list
    reactionList[[i]]$mathml  <- mathml.exp
    # reactionList[[i]]$exp.law <- e.exp.law
    reactionList[[i]]$str.law <- e.str.law
    
    # model.reactions[[i]]$mathml <- 
    
  }
  
  return(reactionList)
}

convertReactionVarsFromSBML <- function(var2Convert) {
  
  out <- c()
  for (i in seq_along(var2Convert)) {
    if (!is.na(var2Convert[i])) {
      # Split Var on Comma
      split.var <- strsplit(var2Convert[i], ",")[[1]]
      # Remove Excess white space from var names if they exist
      subbed.var <- gsub(" ", "", split.var, fixed = TRUE)
      # Recondense with space delmiter
      condensed.var <- paste0(subbed.var, collapse = " ")
      out <- c(out, condensed.var)
    } else {
      out <- c(out, NA)
    }
    
  }
  
  return(out)
}

FindIdSplit <- function(string2Search) {
  
  out.ids <- c()
  split <- strsplit(string2Search, " ")[[1]]
  
  for (i in seq_along(split)) {
    out.ids <- c(out.ids, FindId(split[i]))
  }
  
  return(out.ids)
} 

FindIDReactionStructure <- function(structure2Search) {
  browser()
  out.ids <- c()
  for (i in seq_along(structure2Search)) {
    if ( !is.na(structure2Search[i])) {
      # split it 
      split.struc <- strsplit(structure2Search[i], " ")[[1]]
      # Convert each component
      row.ids <- c()
      for (j in seq_along(split.struc)) {
        print(j)
        print(split.struc)
        print(length(split.struc))
        print(split.struc[j])
        row.ids <- c(row.ids, FindId(split.struc[j]))
      }
      out.ids <- c(out.ids, paste0(row.ids, " "))
    } else {
      out.ids <- c(out.ids, NA)
    }
    
  }
}
