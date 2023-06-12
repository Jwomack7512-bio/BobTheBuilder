

load.model <- LoadSBML("curto.xml")
mod$listOfCompartments


mod <- LoadSBML("cellcycle.xml")
mod$compartments
mod$reactions
mod$rules[[1]]

# Procedure for sbml file load of rules from sbml--------------------------------

# Load sbml
sbmlFile <- "cellcycle.xml"

# Create xml Tree Parse function
doc <- xmlTreeParse(sbmlFile, ignoreBlanks = TRUE)
# Pull rules
rules <- doc$doc$children$sbml[["model"]][["listOfRules"]]
# Extract first rule
mathml    <- rules[[1]][["math"]][[1]]
# Convert to string
e         <- convertML2R(mathml)


print(rules)
print(mathml)
print(e)

# Procedure for function definition load----------------------------------------
# Load sbml
sbmlFile <- "C:\\Users\\ju61191\\Downloads\\untitled.xml"

# Create xml Tree Parse function
doc <- xmlTreeParse(sbmlFile, ignoreBlanks = TRUE)
sbmlList <- read_xml(sbmlFile) %>% as_list()
modelList <- sbmlList$sbml$model

# Find function information
func <- doc$doc$children$sbml[["model"]][["listOfFunctionDefinitions"]]
func.info <- Attributes2Tibble(sbmlList$sbml$model$listOfFunctionDefinitions)
function.definitions <- ExtractFunctionDefFromSBML(doc, func.info)
function.definitions <- FindFunctionDefInformation(function.definitions,
                                                   sbmlList)
print(function.definitions)


reaction.list <- vector("list", length(modelList$listOfReactions))

# Build reactions math from sbml
reactions <- doc$doc$children$sbml[["model"]][["listOfReactions"]]
n.reactions <- length(reactions)
reaction.parameters.df <- tibble()


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
      collapsed.grab <- paste0(spec.grab, collapse = ", ");
      reaction.list[[i]]$reactants <- collapsed.grab
      
    } else if (node.name == "listOfProducts") {
      node.products <- Attributes2Tibble(cur.node$listOfProducts)
      reaction.list[[i]]$products <- paste0(node.products %>% pull(species),
                                           collapse = ", ")
    } else if (node.name == "kineticLaw") {
      # We want to extract the parameters here
      node.par <- Attributes2Tibble(cur.node$kineticLaw$listOfParameters)
      # Build Parameter df to join with parameters
      reaction.parameters.df <- rbind(reaction.parameters.df, node.par)
      
      if (!is.null(node.par)) {exists.parInReactions <- TRUE}
      
      # Condense parameter data to build with equations table
      reaction.list[[i]]$parameters <- paste0(node.par %>% pull(id),
                                             collapse = ", ")
      reaction.list[[i]]$parameters.val <- paste0(node.par %>% pull(value),
                                                 collapse = ", ")
    } else {
      #print(paste0("Not Accounted For: ", node.name))
    }
  }
}
reaction.list

# WHERE I START CURRENT TEST ---------------------------------------------------
sbmlFile <- "C:\\Users\\ju61191\\Downloads\\untitled.xml"

# Create xml Tree Parse function
doc <- xmlTreeParse(sbmlFile, ignoreBlanks = TRUE)
sbmlList <- read_xml(sbmlFile) %>% as_list()
modelList <- sbmlList$sbml$model

starting.tags <- ExtractionReactionTagLineFromSBML(modelList$listOfReactions)
print(starting.tags)
if(!is.null(starting.tags)) {
  reaction.ids <- starting.tags %>% pull(id)
}

# Get Base Reaction Information
reaction.list <- vector("list", length(modelList$listOfReactions))
for (i in seq_along(modelList$listOfReactions)) {
  current.reaction <- modelList$listOfReactions[[i]]
  reaction.list[[i]] <- ExtractReactionBaseFromSBML(current.reaction)
  names(reaction.list)[i] <- reaction.ids[i]
}
print(reaction.list)

# TODO: Figure out parameter stuff if not in mathml kinetic law

# Find function information
func <- doc$doc$children$sbml[["model"]][["listOfFunctionDefinitions"]]
func.info <- Attributes2Tibble(sbmlList$sbml$model$listOfFunctionDefinitions)
function.definitions <- ExtractFunctionDefFromSBML(doc, func.info)
function.definitions <- FindFunctionDefInformation(function.definitions,
                                                   sbmlList)
print(function.definitions)
func.def.names <- unname(sapply(function.definitions, get, x = "id"))

reaction.list <- vector("list", length(modelList$listOfReactions))

# Lets look at a single reaction


# Build reactions math from sbml
reactions <- doc$doc$children$sbml[["model"]][["listOfReactions"]]
print(reactions)
# pull single law
for (i in seq_along(reactions)) {
  
  # Want to start by pulling base reaction information from tibble for entry
  
  #Placeholder for reversible variable
  is.reversible <- FALSE
  
  # String of mathml.exp for function check
  mathml.string <- toString(reactions[[i]][["kineticLaw"]][["math"]])
  # mathml expression for processing to rate law
  mathml.exp <- reactions[[i]][["kineticLaw"]][["math"]][[1]]
  
  # Check the mathml string to see if it contains a function definition
  is.func <- FALSE
  for (j in seq_along(func.def.names)) {
    fxn.check <- CheckForTermInMathml(mathml.string, func.def.names[j])
    if (fxn.check$term.found) {
      is.func <- TRUE
      def.terms <- fxn.check$function.terms
      func.id <- func.def.names[j]
      print("Is function")
      print(mathml.string)
      print(func.def.names[j])
      print(def.terms)
      break
    }
  }
  
  if (is.func) {
    # Solve for term if its a function definition
    
    # Find the function definition in function list.
    function.entry <- function.definitions[[func.id]]
    print(function.entry)
    print(function.entry$law)
    
    # Setup Variables
    rate.law   <- function.entry$law
    reactants  <- SplitEntry(function.entry$Reactants)
    products   <- SplitEntry(function.entry$Products)
    modifiers  <- SplitEntry(function.entry$Modifiers)
    parameters <- SplitEntry(function.entry$Parameters)
    new.reactants  <- c()
    new.products   <- c()
    new.modifiers  <- c()
    new.parameters <- c()
    
    
    # Cycle through unknown terms assigning them to proper reaction terms
    for (var.idx in seq_along(def.terms)) {
      
      replace.value <- def.terms[var.idx]
      funct.value   <- function.entry$variables[var.idx]
      # Check if funct.value is react/prod/mod/parameter and assing to replace
      if (funct.value %in% reactants) {
        new.reactants <- c(new.reactants, replace.value)
      } else if (funct.value %in% products) {
        new.products <- c(new.products, replace.value)
      } else if (funct.value %in% modifiers) {
        new.modifiers <- c(new.modifiers, replace.value)
      } else if (funct.value %in% parameters) {
        new.parameters <- c(new.parameters, replace.value)
      }
    }
    
    # Create new rate law
    new.rate.law <- SubstituteRateLawTerms(rate.law,
                                           reactants,
                                           products,
                                           modifiers,
                                           parameters,
                                           new.reactants,
                                           new.products,
                                           new.modifiers,
                                           new.parameters)
    
    # Create Other Versions of Law
    convert.rate.law <- ConvertRateLaw(new.rate.law)
    latex.law   <- convert.rate.law$latex
    mathjax.law <- convert.rate.law$mathjax
    mathml.law  <- katex::katex_mathml(latex.law)
    
    # Build Reaction Schemes
    eqn.builds <- BuildCustomEquationText(new.reactants,
                                          new.products,
                                          new.modifiers,
                                          new.parameters)
    
    text.eqn    <- eqn.builds$text
    latex.eqn   <- eqn.builds$latex
    mathjax.eqn <- eqn.builds$mathjax
    
    par.collapsed          <- collapseVector(new.parameters, 
                                             convertBlank = TRUE)
    reactants.collapsed    <- collapseVector(new.reactants, 
                                             convertBlank = TRUE)
    products.collapsed     <- collapseVector(new.products, 
                                             convertBlank = TRUE)
    species.collapsed      <- collapseVector(c(new.reactants, new.products), 
                                             convertBlank = TRUE)
    modifiers.collapsed    <- collapseVector(new.modifiers, 
                                             convertBlank = TRUE)

    # Add overall reaction information
    reaction.entry <- list(
      "ID"               = "CREATE ID/GRAB ID",
      "Eqn.Display.Type" = function.entry$name,
      "Reaction.Law"     = function.entry$id,
      "Species"          = species.collapsed,
      "Reactants"        = reactants.collapsed,
      "Products"         = products.collapsed, 
      "Modifiers"        = modifiers.collapsed,
      "Parameters"       = par.collapsed,
      "Compartment"      = "PH",
      "Description"      = "Custom Load",
      "Species.id"       = NA,
      "Reactants.id"     = NA,
      "Products.id"      = NA,
      "Modifiers.id"     = NA, 
      "Parameters.id"    = NA,
      "Compartment.id"   = NA,
      "Equation.Text"    = text.eqn,
      "Equation.Latex"   = latex.eqn,
      "Equation.MathJax" = mathjax.eqn,
      "String.Rate.Law"  = new.rate.law,
      "Pretty.Rate.Law"  = "",
      "Latex.Rate.Law"   = latex.law,
      "MathJax.Rate.Law" = mathjax.law,
      "MathMl.Rate.Law"  = mathml.exp,
      "Reversible"       = is.reversible
    )
    
    reaction.list[[i]] <- reaction.entry
    
  } else {
    # Solve for term if it just has a rate law
    
    # Convert mathml to string rate law for r
    string.exp <- gsub(" ", "", convertML2R(math.ml.exp))
    
    # Append information to list
    reaction.list[[i]]$Rate.MathML  <- mathml.exp
    # reactionList[[i]]$exp.law <- e.exp.law
    reaction.list[[i]]$String.Rate.Law <- string.exp
  }
}
print(reaction.list)
# Check if mathml law has function definition in it.
typeof(mathml.string)
print(mathml.string)

ExtractionReactionTagLineFromSBML <- function(reactionXML) {
  # Extract the tagline on Reactions that contains information that can 
  # includes id, reversible, name, fast
  # Inputs:
  # @ reactionXML: modelList$listOfReactions
  
  # Create Tags Tibble
  tags <- Attributes2Tibble(reactionXML)
  print(head(tags))
  # Check which terms exist
  to.pull <- c()
  if (!is.null(tags$id)) {to.pull <- c(to.pull, "id")}
  if (!is.null(tags$reversible)) {to.pull <- c(to.pull, "reversible")}
  if (!is.null(tags$name)) {to.pull <- c(to.pull, "name")}
  if (!is.null(tags$fast)) {to.pull <- c(to.pull, "fast")}
  PrintVar(to.pull)
  out <- tags %>% select(to.pull)
  return(out)
}
tags <- Attributes2Tibble(modelList$listOfReactions)
is.null(tags$fast)


ExtractReactionBaseFromSBML <- function(reactionEntry) {
  # Inputs: 
  #   @reaction.entry: current.reaction <- modelList$listOfReactions[[i]]
  # Cycle through reaction entry tags pull reaction information
  
  # Some SBML files have parameter information below the kinetic law in 
  # reaction entries but some don't and instead list that information in a 
  # XML node "listOfParameters" on the base level with all parameters. So,
  # we need to check for that.
  
  out.list <- list("Reactants"  = NA,
                   "Products"   = NA,
                   "Modifers"  = NA,
                   "Parameters" = NA,
                   "Parameter.Values" = NA)
  
  for (i in seq_along(reactionEntry)) {
    current.node <- reactionEntry[i]
    node.name <- names(current.node)
    
    if (node.name == "listOfReactants") {
      # Convert node to Tibble
      node.reactants <- Attributes2Tibble(current.node$listOfReactants)
      
      # Grab the species from tibble, collapse, add to output
      out.list$Reactants <- collapseVector(node.reactants %>% pull(species),
                                           convertBlank = TRUE)
    } else if (node.name == "listOfProducts") {
      # Convert node to Tibble
      node.products <- Attributes2Tibble(current.node$listOfProducts)
      
      # Grab the species from tibble, collapse, add to output
      out.list$Products <- collapseVector(node.products %>% pull(species),
                                          convertBlank = TRUE)
    } else if (node.name == "listOfModifiers") {
      # Convert node to Tibble
      node.modifiers <- Attributes2Tibble(current.node$listOfModifiers)
      
      # Grab the species from tibble, collapse, add to output
      out.list$Modifers <- collapseVector(node.modifiers %>% pull(species),
                                          convertBlank = TRUE)
    } else if (node.name == "kineticLaw") {
      # Check if parameter node exists
      node.par <- Attributes2Tibble(current.node$kineticLaw$listOfParameters)
      if (!is.null(node.par)) {
        # IF PARAMETER INFORMATION IN REACTION XML INFO
        out.list$Parameters <- collapseVector(node.par %>% pull(id), 
                                              convertBlank = TRUE)
        out.list$Parameter.Values <- collapseVector(node.par %>% pull(value), 
                                                    convertBlank = TRUE)
      } 
    } 
  }
  return(out.list)
}

n.reactions <- length(reactions)
reaction.parameters.df <- tibble()
# Read in reaction and determine if it is a fxn definition of not
for (i in seq_along(reactions)) {
  # Extract mathematical expression
  mathml.exp <- toString(reactions[[i]][["kineticLaw"]][["math"]])
  # print(mathml.exp)
  exp.r <- reactions[[i]][["kineticLaw"]][["math"]][[1]]
  # Convert mathml to r
  e <- convertML2R(exp.r)
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

# check if function def in terms

# if so remove that terms.  if not continue

# Compare these terms to vector of stored terms to determine which are equation
# parameters.

CheckForTermInMathml <- function(mathml.exp,
                                 search.term) {
  # Search for string term in mathml expression. 
  # Inputs: 
  # @mathml.exp - (str) mathml terms to search for keyword in
  # @search.term- (str) term to search for in mathml.exp
  # Output:
  # @ (bool) TRUE if search term exists, false if it doesn't
  # @ (vec)  vector of string terms that occur after function defintion
  in.expression <- FALSE
  terms.in.function <- c()
  
  # Regex pattern to remove tags
  pattern <- "<[^>]+>"
  # Replace tags with empty space
  result <- gsub(pattern, "", mathml.exp)
  # Remove newlines
  result <- gsub("\n", "", result)
  # Split on spaces and clear all empty strings from vector
  result <- strsplit(result, " ")[[1]]
  result <- result[nzchar(result)]
  
  if (search.term %in% result) {
    in.expression <- TRUE
    idx.for.search <- which(result %in% search.term)
    terms.in.function  <- result[(idx.for.search+1):length(result)]
  } 
  
  out <- list(term.found = in.expression,
              function.terms = terms.in.function)
  return(out)
}
