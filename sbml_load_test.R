

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


# Removes all tag terms from mathml leaving only variables
expression <- "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n <apply>\n  <times/>\n  <ci>compartment</ci>\n  <apply>\n   <ci>Henri_Michaelis_Menten__irreversible</ci>\n   <ci>A</ci>\n   <ci>Km</ci>\n   <ci>V</ci>\n  </apply>\n </apply>\n</math>"

pattern <- "<[^>]+>"
result <- gsub(pattern, "", expression)
result <- gsub("\n", "", result)
print(result)
t <- strsplit(result, " ")[[1]]
vec.of.terms <- t[nzchar(t)]
vec.of.terms
# check if function def in terms

# if so remove that terms.  if not continue

# Compare these terms to vector of stored terms to determine which are equation
# parameters.