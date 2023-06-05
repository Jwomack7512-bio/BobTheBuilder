

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
react <- sbmlList$sbml$model$listOfReactions[[1]]
reactants <- Attributes2Tibble(react[2]$listOfReactants)
reactants
# law
law <- react[4]$kineticLaw
reaction.list <- vector("list", length(sbmlList$sbml$model$listOfReactions))
reaction.list <- ExtractReactionMathFromSBML(doc, reaction.list)
reaction.list
law
print("OH")

reactions <- doc$doc$children$sbml[["model"]][["listOfReactions"]]
# Extract mathml string
mathml.exp <- toString(reactions[[3]][["kineticLaw"]][["math"]])
mathml.exp
# Search mathml string for custom function (<ci>customfunc</ci>)
# Step: grab function definition names
func.names <- unname(sapply(out,
                            get,
                            x = "name"))
func.names
# If exist, extract and perform different sort of mathml extraction
to.search <- RemoveWS(mathml.exp)
to.search
for (i in seq_along(func.names)) {
  term <- paste0("<ci>", func.names[i], "</ci>")
  if (grepl(term, to.search, fixed = TRUE)) {
    print("exists in string")
    # Extract function information (pull apply tag)
    str1 <- paste0("<apply>\n<ci>", func.names[i], "</ci>")
    print(str1)
    str2 <- paste0("</apply>")
    expr <- paste0(str1, "\\s*(.*?)\\s*", str2)
    print(expr)
    res <- str_match(to.search, expr)
    print(res[,2])
    result <- regmatches(to.search, regexec(expr, to.search))
    print(result[[1]][2])
  } else {
    print("NOPE")
  }
}
mathml.exp

# TODO: Pull info from reactions to determine what in the functiondef which 
# variables are reactants, products, modifiers, or parameters
#grepl(value, chars, fixed = TRUE)