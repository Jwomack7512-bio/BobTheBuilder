

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
sbmlFile <- "C:\\Users\\justi\\Downloads\\untitled.xml"

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
                            x = "id"))
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



# Read in funciton and determine what the output is
sbmlFile <- "C:\\Users\\justi\\Downloads\\untitled.xml"

# Create xml Tree Parse function
doc <- xmlTreeParse(sbmlFile, ignoreBlanks = TRUE)
# Pull rules
func <- doc$doc$children$sbml[["model"]][["listOfFunctionDefinitions"]]
sbmlList <- read_xml(sbmlFile) %>% as_list()
func.info <- Attributes2Tibble(sbmlList$sbml$model$listOfFunctionDefinitions)
func.info$name
func.info$id
out <- ExtractFunctionDefFromSBML(doc, func.info)
out

#Grab ids from outputs 
func.ids <- func.info$id
 
# Grab reaction to search 
reactions <- doc$doc$children$sbml[["model"]][["listOfReactions"]]
# Extract mathml string
mathml.exp <- toString(reactions[[3]][["kineticLaw"]][["math"]])
to.search <- mathml.exp
exp.r <- reactions[[3]][["kineticLaw"]][["math"]][[1]]

for (id in func.ids) {
  # Search if reaction has id in it
  print(id)
  print(to.search)
  has.func <- grepl(id, to.search, fixed = TRUE)
  print(has.func)
  if (has.func) {
    # Pull the  reaction information from old mathml structure.
    # Convert mathml to r
    e <- mathml2R(exp.r)
    # Remove from expression tag
    e.exp.law <- e[[1]]
    # Convert to full string law
    e.str.law <- gsub(" ", "", toString(e[1]))
    print(e.str.law)
    
    # Remove function name and replace it with rate law from function
    # Split term using math split.  Search each term and if its the function,
    # take the variables and replace them with the variables from teh rate law.
    split.terms <- SplitTerm(e.str.law)
    new.law <- c()
    # for (i in seq_along(split.terms)) {
    #   if (grepl(id, split.terms[i]))
    # }
  } else {
    # Perform previous extraction of reaction information used
  }
  
}
a <- func.names[1]
 a <- "Fake Name"
to.search
regmatches(to.search, regexec(a, to.search))
grepl(a, to.search, fixed = TRUE)
# Check if parse string has <apply> functionname
# if so send to a different parser.

# TODO: Pull info from reactions to determine what in the functiondef which 
# variables are reactants, products, modifiers, or parameters
#grepl(value, chars, fixed = TRUE)

paste0(func.name, "<- function(", 
       paste0(func.varibles, collapse = ", "),
       ") {out <- ",
       func.rate.law,
       "return(out)}")

# Test evaluation of express
Henri_Michaelis_Menten__irreversible <- function(A, Km, V) {
  out <- paste0(A, "*CDL", "/", Km, "+", V)
  return(out)
}
expr <- expression()
expr <- c(expr, "V + func1(a,b)")
expr
e
e.exp.law
eval(e.exp.law)
