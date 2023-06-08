# Load in function
doc <- xmlTreeParse(sbmlFile, ignoreBlanks = TRUE)
# Pull rules
func <- doc$doc$children$sbml[["model"]][["listOfFunctionDefinitions"]]
sbmlList <- read_xml(sbmlFile) %>% as_list()
print(sbmlList)
func.info <- Attributes2Tibble(sbmlList$sbml$model$listOfFunctionDefinitions)
print(func.info)
func.info$name
func.info$id
out <- ExtractFunctionDefFromSBML(doc, func.info)
out

# Read in reactions and find match
modelList <- sbmlList$sbml$model
func.id <- func.info$id[1]
func.id
for (i in seq_along(modelList$listOfReactions)) {
  # Separate current reaction node
  current.reaction <- modelList$listOfReactions[[i]]
  # Pull math law and check if it contains the current search fxn
  reactions <- doc$doc$children$sbml[["model"]][["listOfReactions"]]
  # Extract mathml string
  mathml.exp <- toString(reactions[[i]][["kineticLaw"]][["math"]])
  # mathml.exp <- toString(current.reaction[["kineticLaw"]][["math"]])
  print(func.id)
  print(mathml.exp)
  if (grepl(func.id, mathml.exp, fixed = TRUE)) {
    print("function term found, extracting being performed")
    
    for (j in seq_along(current.reaction)) {
      cur.node <- current.reaction[j]
      node.name <- names(cur.node)
      reactants.exists <- FALSE
      products.exists   <- FALSE
      modifiers.exists  <- FALSE
      parameters.exists <- FALSE
      reaction.list <- vector("list", length(modelList$listOfReactions))
      if (node.name == "listOfReactants") {
        reacts.exists <- TRUE
        node.reactants <- Attributes2Tibble(cur.node$listOfReactants)
        # Grab the species from tibble
        spec.grab <- node.reactants %>% pull(species)
        # Condense multiple values to be comma separated
        collapsed.grab <- paste(spec.grab, collapse = ",");
        reaction.list[[1]]$reactants <- collapsed.grab
      } else if (node.name == "listOfModifiers") {
        modifiers.exsts <- TRUE
        node.modifiers <- Attributes2Tibble(cur.node$listOfmodifiers)
        reaction.list[[1]]$products <- paste(node.modifiers %>% pull(species),
                                             collapse = ",")
      } else if (node.name == "listOfProducts") {
        products.exists <- TRUE
        node.products <- Attributes2Tibble(cur.node$listOfProducts)
        reaction.list[[1]]$products <- paste(node.products %>% pull(species),
                                             collapse = ",")
      } else if (node.name == "kineticLaw") {
        node.par <- Attributes2Tibble(cur.node$kineticLaw$listOfParameters)
        # Build Parameter df to join with parameters

        if (!is.null(node.par)) {parameters.exists <- TRUE}
        
        # Condense parameter data to build with equations table
        reaction.list[[1]]$parameters <- paste(node.par %>% pull(id),
                                               collapse = ",")
        reaction.list[[1]]$parameters.val <- paste(node.par %>% pull(value),
                                                   collapse = ",")
      }
    }
    # Check for null cases 
    if (!reactants.exists)  {reaction.list[[1]]$reactants <- NA}
    if (!products.exists)   {reaction.list[[1]]$products <- NA}
    if (!modifiers.exists)  {reaction.list[[1]]$modifiers <- NA}
    if (!parameters.exists) {reaction.list[[1]]$parameters <- NA}
    print(reaction.list)
    break
  }
}
# Determine what parts of function definitions are substrate, prod, mod, etc

# Read in function definintion.

# Read in reactions and find one that has matching definition

# Grab that structures tree for sub, prod, mod

# Assign those to proper list variable