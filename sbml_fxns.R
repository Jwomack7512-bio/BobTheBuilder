mathml2R <-function(node)  {UseMethod("mathml2R", node)}

mathml2R.XMLDocument <-function(doc) {return(mathml2R(doc$doc$children))}

mathml2R.default<-function(children) 
{  expr <- expression()  # this gets used when a "list" of children nodes are sent in
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

ParseSBML <- function(sbmlFile) {
  
  
  # Check if certain structures exist:
  # Search For
  #     Compartments
  #     Species
  #     Parameters
  #     Equations
  #     Reactions
  #     Rules
  
  
  out <- list()
  
  # Keep xml doc to remove eqn maths
  doc <- xmlTreeParse(a, ignoreBlanks = TRUE)
  
  # Extract model from sbml file
  sbmlList <- read_xml(sbmlFile) %>% as_list()
  modelList <- sbmlList$sbml$model
  out[["model"]] <- modelList
  print(names(modelList))
  
  # Extract Compartments
  if (!is.null(modelList$listOfCompartments)) {
    out[["compartments"]] <- Attributes2Tibble(modelList$listOfCompartments)
  }
  
  # Extract Species
  if (!is.null(modelList$listOfSpecies)) {
    out[["species"]] <- Attributes2Tibble(modelList$listOfSpecies)
  }
  
  # Extract Parameters
  if (!is.null(modelList$listOfParameters)) {
    out[["parameters"]] <- Attributes2Tibble(modelList$listOfParameters)
  }
  
  # Extract Rules
  if (!is.null(modelList$listOfRules)) {
    out[["rules"]] <- Attributes2Tibble(modelList$listOfRules)
    print("rules")
  }
  
  # Extract Reactions
  if (!is.null(modelList$listOfReactions)) {
    # Use this still to extract id and reversible and then use other parsers to 
    # grab other info
    for.titles <- Attributes2Tibble(modelList$listOfReactions)
    reactions.ids <- for.titles %>% pull(id)
    print(reactions.ids)
    # Use for loop below looping through reactions grabbing relevant information
    reaction.list <- vector("list", length(modelList$listOfReactions))
    for (i in seq_along(modelList$listOfReactions)) {
      # Separate current reaction node
      current.reaction <- modelList$listOfReactions[[i]]
      
      # Cycle through node finding the elements we want
      for (j in seq_along(current.reaction)) {
        cur.node <- current.reaction[j]
        node.name <- names(cur.node)
        
        if (node.name == "listOfReactants") {
          
          node.reactants <- Attributes2Tibble(cur.node$listOfReactants)
          reaction.list[[i]]$reactants <- node.reactants %>% pull(species)
          
        } else if (node.name == "listOfProducts") {
          node.products <- Attributes2Tibble(cur.node$listOfProducts)
          reaction.list[[i]]$products <- node.products %>% pull(products)
        } else {
          #print(paste0("Not Accounted For: ", node.name))
        }
        
      }
      
    }
    
    # Add reaction ids to list
    for (i in seq_along(reactions.ids)) {
      reaction.list[[i]]$id <- reactions.ids[i]
    }
    # Give ids as list titles
    names(reaction.list) <- reactions.ids
    
    out[["reactions"]] <- reaction.list
    
    # Parameters (maybe can grab with math. Will have to check) 
    
  }
  
  return(out)
}

ExtractMathFromSBML <- function(xmlDoc, reactionList) {
  # xmlDoc - parsed xml doc from xmltreeparse
  # reactionList - list of reactions to update
  
  
  # Extract reactions from xml tree
  reactions=doc$doc$children$sbml[["model"]][["listOfReactions"]]
  n.reactions <- length(reactions)
  
  
  for (i in seq_along(reactions)) {
    
    # Find Products
    
    # Extract mathematical expression
    mathml.exp <- toString(reactions[[i]][["kineticLaw"]][["math"]])
    exp.r <- reactions[[i]][["kineticLaw"]][["math"]][[1]]
    # Convert mathml to r
    e <- mathml2R(exp.r)
    # Remove from expression tag
    e.exp.law <- e[[1]]
    # Convert to full string law
    e.str.law <- gsub(" ", "", toString(e[1]))
    
    # Append information to list
    reactionList[[i]]$mathml  <- mathml.exp
    reactionList[[i]]$exp.law <- e.exp.law
    reactionList[[i]]$str.law <- e.str.law
    
    # model.reactions[[i]]$mathml <- 
    
  }
  
  return(reactionList)
}