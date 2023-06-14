# 
# 
# load.model <- LoadSBML("curto.xml")
# mod$listOfCompartments
# 
# 
# mod <- LoadSBML("cellcycle.xml")
# mod$compartments
# mod$reactions
# mod$rules[[1]]
# 
# # Procedure for sbml file load of rules from sbml--------------------------------
# 
# # Load sbml
# sbmlFile <- "cellcycle.xml"
# 
# # Create xml Tree Parse function
# doc <- xmlTreeParse(sbmlFile, ignoreBlanks = TRUE)
# # Pull rules
# rules <- doc$doc$children$sbml[["model"]][["listOfRules"]]
# # Extract first rule
# mathml    <- rules[[1]][["math"]][[1]]
# # Convert to string
# e         <- convertML2R(mathml)
# 
# 
# print(rules)
# print(mathml)
# print(e)
# 
# # Procedure for function definition load----------------------------------------
# # Load sbml
# sbmlFile <- "C:\\Users\\ju61191\\Downloads\\untitled.xml"
# 
# # Create xml Tree Parse function
# doc <- xmlTreeParse(sbmlFile, ignoreBlanks = TRUE)
# sbmlList <- read_xml(sbmlFile) %>% as_list()
# modelList <- sbmlList$sbml$model
# 
# # Find function information
# func <- doc$doc$children$sbml[["model"]][["listOfFunctionDefinitions"]]
# func.info <- Attributes2Tibble(sbmlList$sbml$model$listOfFunctionDefinitions)
# function.definitions <- ExtractFunctionDefFromSBML(doc, func.info)
# function.definitions <- FindFunctionDefInformation(function.definitions,
#                                                    sbmlList)
# print(function.definitions)
# 
# 
# reaction.list <- vector("list", length(modelList$listOfReactions))
# 
# # Build reactions math from sbml
# reactions <- doc$doc$children$sbml[["model"]][["listOfReactions"]]
# n.reactions <- length(reactions)
# reaction.parameters.df <- tibble()
# 
# 
# for (i in seq_along(modelList$listOfReactions)) {
#   # Separate current reaction node
#   current.reaction <- modelList$listOfReactions[[i]]
#   
#   # Cycle through node finding the elements we want
#   for (j in seq_along(current.reaction)) {
#     cur.node <- current.reaction[j]
#     node.name <- names(cur.node)
#     
#     if (node.name == "listOfReactants") {
#       
#       node.reactants <- Attributes2Tibble(cur.node$listOfReactants)
#       # Grab the species from tibble
#       spec.grab <- node.reactants %>% pull(species)
#       # Condense multiple values to be comma separated
#       collapsed.grab <- paste0(spec.grab, collapse = ", ");
#       reaction.list[[i]]$reactants <- collapsed.grab
#       
#     } else if (node.name == "listOfProducts") {
#       node.products <- Attributes2Tibble(cur.node$listOfProducts)
#       reaction.list[[i]]$products <- paste0(node.products %>% pull(species),
#                                            collapse = ", ")
#     } else if (node.name == "kineticLaw") {
#       # We want to extract the parameters here
#       node.par <- Attributes2Tibble(cur.node$kineticLaw$listOfParameters)
#       # Build Parameter df to join with parameters
#       reaction.parameters.df <- rbind(reaction.parameters.df, node.par)
#       
#       if (!is.null(node.par)) {exists.parInReactions <- TRUE}
#       
#       # Condense parameter data to build with equations table
#       reaction.list[[i]]$parameters <- paste0(node.par %>% pull(id),
#                                              collapse = ", ")
#       reaction.list[[i]]$parameters.val <- paste0(node.par %>% pull(value),
#                                                  collapse = ", ")
#     } else {
#       #print(paste0("Not Accounted For: ", node.name))
#     }
#   }
# }
# reaction.list

# comps <- test$compartments
# comps.df <- comps %>% select(id, name)
# comps.df
# 
# spec <- test$species
# spec.comp <- spec %>% pull(compartment)
# spec.comp
# 
# idx <- which(spec.comp[i] %in% comps.df$id)
# comps.df$name[idx]
# WHERE I START CURRENT TEST ---------------------------------------------------
sbmlFile <- "C:\\Users\\ju61191\\Downloads\\untitled.xml"
sbmlFile <- "C:\\Users\\ju61191\\Downloads\\cellcycle.xml"
sbmlFile <- "C:\\Users\\ju61191\\Downloads\\curto.xml"
sbmlFile <- "C:\\Users\\ju61191\\Downloads\\Abroudi2017.xml"

test <- LoadSBML(sbmlFile)

test$compartments
test$species
test$rules
bind_rows(test$reactions)
test$functions
bind_rows(test$functions)
# print(test$parameters, n = 149)
test$parameters

# Create xml Tree Parse function
doc <- xmlTreeParse(sbmlFile, ignoreBlanks = TRUE)
sbmlList <- read_xml(sbmlFile) %>% as_list()
modelList <- sbmlList$sbml$model


# Grab Parameters
if (!is.null(modelList$listOfParameters)) {
  listOfParameters <- Attributes2Tibble(modelList$listOfParameters)
  listOfParameters <- listOfParameters %>% select(-metaid)
  print(listOfParameters)
  exists.listOfParameters <- TRUE
}


starting.tags <- ExtractionReactionTagFromSBML(modelList$listOfReactions)
print(starting.tags)
if(!is.null(starting.tags)) {
  reaction.ids <- starting.tags %>% pull(id)
}
print(reaction.ids)

# Get Base Reaction Information
reaction.list <- vector("list", length(modelList$listOfReactions))
for (i in seq_along(modelList$listOfReactions)) {
  current.reaction <- modelList$listOfReactions[[i]]
  reaction.list[[i]] <- ExtractReactionBaseFromSBML(current.reaction)
  names(reaction.list)[i] <- reaction.ids[i]
}

ent <- modelList$listOfReactions[[2]]
parsss <- ent$kineticLaw$listOfParameters
parsss
node.par <- Attributes2Tibble(ent$kineticLaw$listOfParameters)
node.par
ncol(node.par)
ncol(node.par) != 0
# Check to see if reactions have a separate list of parameters
print(reaction.list)
print(reaction.list$R8_p53_synthesis)

# TODO: Figure out parameter stuff if not in mathml kinetic law. Figure out
#       these strange parameter paths and how best to create the parameter df.


# Find function information
func <- doc$doc$children$sbml[["model"]][["listOfFunctionDefinitions"]]
func.info <- Attributes2Tibble(sbmlList$sbml$model$listOfFunctionDefinitions)
function.definitions <- ExtractFunctionDefFromSBML(doc, func.info)
function.definitions <- FindFunctionDefInformation(doc,
                                                   function.definitions,
                                                   sbmlList)
func.def.names <- unname(sapply(function.definitions, get, x = "id"))
print(function.definitions)
bind_rows(function.definitions)
print(reaction.ids)
# Build reactions math from sbml
reactions <- doc$doc$children$sbml[["model"]][["listOfReactions"]]

new.list <- ExtractReactionMathFromSBML(doc,
                                        reaction.list,
                                        function.definitions)
print(new.list)

newest <- CombineReactionTagsWReactions(reaction.tags, new.list)
newest
# 
# # pull single law
# for (i in seq_along(reactions)) {
# 
#   # String of mathml.exp for function check
#   mathml.string <- toString(reactions[[i]][["kineticLaw"]][["math"]])
#   # mathml expression for processing to rate law
#   mathml.exp <- reactions[[i]][["kineticLaw"]][["math"]][[1]]
#   
#   # Check the mathml string to see if it contains a function definition
#   is.func <- FALSE
#   for (j in seq_along(func.def.names)) {
#     fxn.check <- CheckForTermInMathml(mathml.string, func.def.names[j])
#     if (fxn.check$term.found) {
#       is.func <- TRUE
#       def.terms <- fxn.check$function.terms
#       func.id <- func.def.names[j]
#       break
#     }
#   }
#   
#   if (is.func) {
#     # Solve for term if its a function definition
#     
#     # Find the function definition in function list.
#     function.entry <- function.definitions[[func.id]]
#     func.vars <- function.entry$variables
#     
#     # Setup Variables
#     rate.law   <- function.entry$law
#     reactants  <- SplitEntry(function.entry$Reactants)
#     products   <- SplitEntry(function.entry$Products)
#     modifiers  <- SplitEntry(function.entry$Modifiers)
#     parameters <- SplitEntry(function.entry$Parameters)
# 
#     # Grab Values from reactionlist
#     new.reactants <- SplitEntry(reaction.list[[i]]$Reactants)
#     new.products  <- SplitEntry(reaction.list[[i]]$Products)
#     new.modifiers <- SplitEntry(reaction.list[[i]]$Modifers)
#     if (!is.na(reaction.list[[i]]$Parameters)) {
#       new.parameters <- SplitEntry(reaction.list[[i]]$Parameters)
#     } else {
#       species <- c(new.reactants, new.products, new.modifiers)
#       species <- RemoveNA(species)
#       new.parameters <- def.terms[-(which(def.terms %in% species))]
#     }
# 
#     
#     # Create new rate law
#     new.rate.law <- SubstituteRateLawTerms(rate.law,
#                                            reactants,
#                                            products,
#                                            modifiers,
#                                            parameters,
#                                            new.reactants,
#                                            new.products,
#                                            new.modifiers,
#                                            new.parameters)
#     
#     # Create Other Versions of Law
#     convert.rate.law <- ConvertRateLaw(new.rate.law)
#     latex.law   <- convert.rate.law$latex
#     mathjax.law <- convert.rate.law$mathjax
#     mathml.law  <- katex::katex_mathml(latex.law)
#     
#     # Build Reaction Schemes
#     eqn.builds <- BuildCustomEquationText(new.reactants,
#                                           new.products,
#                                           new.modifiers,
#                                           new.parameters)
#     
#     text.eqn    <- eqn.builds$text
#     latex.eqn   <- eqn.builds$latex
#     mathjax.eqn <- eqn.builds$mathjax
#     
#     par.collapsed          <- collapseVector(new.parameters, 
#                                              convertBlank = TRUE)
#     reactants.collapsed    <- collapseVector(new.reactants, 
#                                              convertBlank = TRUE)
#     products.collapsed     <- collapseVector(new.products, 
#                                              convertBlank = TRUE)
#     species.collapsed      <- collapseVector(c(new.reactants, new.products), 
#                                              convertBlank = TRUE)
#     modifiers.collapsed    <- collapseVector(new.modifiers, 
#                                              convertBlank = TRUE)
# 
#     # Add overall reaction information
#     reaction.entry <- list(
#       "ID"               = "CREATE ID/GRAB ID",
#       "Eqn.Display.Type" = function.entry$name,
#       "Reaction.Law"     = function.entry$id,
#       "Species"          = species.collapsed,
#       "Reactants"        = reactants.collapsed,
#       "Products"         = products.collapsed, 
#       "Modifiers"        = modifiers.collapsed,
#       "Parameters"       = par.collapsed,
#       "Compartment"      = "PH",
#       "Description"      = "Custom Load",
#       "Species.id"       = NA,
#       "Reactants.id"     = NA,
#       "Products.id"      = NA,
#       "Modifiers.id"     = NA, 
#       "Parameters.id"    = NA,
#       "Compartment.id"   = NA,
#       "Equation.Text"    = text.eqn,
#       "Equation.Latex"   = latex.eqn,
#       "Equation.MathJax" = mathjax.eqn,
#       "String.Rate.Law"  = new.rate.law,
#       "Pretty.Rate.Law"  = "",
#       "Latex.Rate.Law"   = latex.law,
#       "MathJax.Rate.Law" = mathjax.law,
#       "MathMl.Rate.Law"  = mathml.exp,
#       "Reversible"       = is.reversible
#     )
#     
#     reaction.list[[i]] <- reaction.entry
#     
#   } else {
#     # Solve for term if it just has a rate law
#     # Convert mathml to string rate law for r
#     string.exp <- gsub(" ", "", convertML2R(mathml.exp))
#     
#     # Extract all variables in string
#     def.terms <- extract_variables(string.exp)
#     
#     # Set up reactants, products, modifiers
#     reactants <- SplitEntry(reaction.list[[i]]$Reactants)
#     products  <- SplitEntry(reaction.list[[i]]$Products)
#     modifiers <- SplitEntry(reaction.list[[i]]$Modifers)
#     
#     if (!is.na(reaction.list[[i]]$Parameters)) {
#       parameters <- SplitEntry(reaction.list[[i]]$Parameters)
#     } else {
#       species <- c(reactants, products, modifiers)
#       species <- RemoveNA(species)
#       parameters <- def.terms[-(which(def.terms %in% species))]
#     }
#     
#     # Create Other Versions of Law
#     convert.rate.law <- ConvertRateLaw(string.exp)
#     latex.law   <- convert.rate.law$latex
#     mathjax.law <- convert.rate.law$mathjax
#     mathml.law  <- katex::katex_mathml(latex.law)
#     
#     # Build Reaction Schemes
#     eqn.builds <- BuildCustomEquationText(reactants,
#                                           products,
#                                           modifiers,
#                                           parameters)
#     
#     text.eqn    <- eqn.builds$text
#     latex.eqn   <- eqn.builds$latex
#     mathjax.eqn <- eqn.builds$mathjax
#     
#     par.collapsed          <- collapseVector(parameters, 
#                                              convertBlank = TRUE)
#     reactants.collapsed    <- collapseVector(reactants, 
#                                              convertBlank = TRUE)
#     products.collapsed     <- collapseVector(products, 
#                                              convertBlank = TRUE)
#     species.collapsed      <- collapseVector(c(reactants, products), 
#                                              convertBlank = TRUE)
#     modifiers.collapsed    <- collapseVector(modifiers, 
#                                              convertBlank = TRUE)
#     
#     # Add overall reaction information
#     reaction.entry <- list(
#       "ID"               = "CREATE ID/GRAB ID",
#       "Eqn.Display.Type" = reaction.ids[i],
#       "Reaction.Law"     = "CUSTOM",
#       "Species"          = species.collapsed,
#       "Reactants"        = reactants.collapsed,
#       "Products"         = products.collapsed, 
#       "Modifiers"        = modifiers.collapsed,
#       "Parameters"       = par.collapsed,
#       "Compartment"      = "PH",
#       "Description"      = "Custom Load",
#       "Species.id"       = NA,
#       "Reactants.id"     = NA,
#       "Products.id"      = NA,
#       "Modifiers.id"     = NA, 
#       "Parameters.id"    = NA,
#       "Compartment.id"   = NA,
#       "Equation.Text"    = text.eqn,
#       "Equation.Latex"   = latex.eqn,
#       "Equation.MathJax" = mathjax.eqn,
#       "String.Rate.Law"  = string.exp,
#       "Pretty.Rate.Law"  = "",
#       "Latex.Rate.Law"   = latex.law,
#       "MathJax.Rate.Law" = mathjax.law,
#       "MathMl.Rate.Law"  = mathml.exp,
#       "Reversible"       = is.reversible
#     )
#     
#     reaction.list[[i]] <- reaction.entry
#   }
# }
# 
# 
# # check if function def in terms
# 
# # if so remove that terms.  if not continue
# 
# # Compare these terms to vector of stored terms to determine which are equation
# # parameters.
# 
# 
# 
# 
# 
# n.reactions <- length(reactions)
# reaction.parameters.df <- tibble()
# # Read in reaction and determine if it is a fxn definition of not
# for (i in seq_along(reactions)) {
#   # Extract mathematical expression
#   mathml.exp <- toString(reactions[[i]][["kineticLaw"]][["math"]])
#   # print(mathml.exp)
#   exp.r <- reactions[[i]][["kineticLaw"]][["math"]][[1]]
#   # Convert mathml to r
#   e <- convertML2R(exp.r)
#   # Remove from expression tag
#   e.exp.law <- e[[1]]
#   # Convert to full string law
#   e.str.law <- gsub(" ", "", toString(e[1]))
#   
#   # Append information to list
#   reactionList[[i]]$mathml  <- mathml.exp
#   # reactionList[[i]]$exp.law <- e.exp.law
#   reactionList[[i]]$str.law <- e.str.law
#   
#   # model.reactions[[i]]$mathml <- 
#   
# }