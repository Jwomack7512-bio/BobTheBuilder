

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
# Pull rules
func <- doc$doc$children$sbml[["model"]][["listOfFunctionDefinitions"]]
sbmlList <- read_xml(sbmlFile) %>% as_list()
func.info <- Attributes2Tibble(sbmlList$sbml$model$listOfFunctionDefinitions)
func.info$name

out <- ExtractFunctionDefFromSBML(doc, func.info)
out
# Extract first rule (first 1 grabs the functionDefinition would be i in loop)

funcdef    <- func[[2]][["math"]][["lambda"]]
funcdef
e          <- convertML2R(mathml)
mathml
typeof(funcdef)

# TODO: Read reactions that use functiondefs


# TODO: Pull info from reactions to determine what in the functiondef which 
# variables are reactants, products, modifiers, or parameters