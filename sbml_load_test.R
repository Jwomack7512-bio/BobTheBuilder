

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
func
# Extract first rule (first 1 grabs the functionDefinition would be i in loop)

funcdef    <- func[[1]][["math"]][[1]]
e          <- convertML2R(mathml)
mathml


# Whats the difference with function Definitions
# Things are stored in a lambda tag
# all varibales that go into the function are stored in <bvar> before the eqn

mathml


