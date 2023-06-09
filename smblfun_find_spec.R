# Load in function
sbmlFile <- "C:\\Users\\ju61191\\Downloads\\untitled.xml"
sbmlFile <- "C:\\Users\\ju61191\\Downloads\\Abroudi2017.xml"
doc <- xmlTreeParse(sbmlFile, ignoreBlanks = TRUE)
# Pull rules
func <- doc$doc$children$sbml[["model"]][["listOfFunctionDefinitions"]]
sbmlList <- read_xml(sbmlFile) %>% as_list()
print(sbmlList)
func.info <- Attributes2Tibble(sbmlList$sbml$model$listOfFunctionDefinitions)
print(func.info)
func.info$name
func.info$id
function.definitions <- ExtractFunctionDefFromSBML(doc, func.info)
function.definitions

# Read in reactions and find match
modelList <- sbmlList$sbml$model
# func.id <- func.info$id[1]
# func.id
# Iterating function definitions, the iterating reactions to find matching 
# function id in the reaction. From there we will extract reaction info to 
# build up the proper function definition.
for (i in seq_along(function.definitions)) {
  function.id <- function.definitions[[i]]$id
  if (i == length(function.definitions)) {
    print(paste0("Searching Function ID: ", function.id))
    for (j in seq_along(modelList$listOfReactions)) {
      # Separate current reaction node
      current.reaction <- modelList$listOfReactions[[j]]
      # Pull math law and check if it contains the current search fxn
      reactions <- doc$doc$children$sbml[["model"]][["listOfReactions"]]
      
      # Extract mathml expression and make string
      mathml.exp <- reactions[[j]][["kineticLaw"]][["math"]][[1]]
      mathml.exp.string <- toString(reactions[[j]][["kineticLaw"]][["math"]])
      # mathml.exp <- toString(current.reaction[["kineticLaw"]][["math"]])
      print(function.id)
      print(mathml.exp.string)
      
      # Search if the function id exists in the mathml string
      if (grepl(function.id, mathml.exp.string, fixed = TRUE)) {
        # Extract from mathml string block
        # There is probably a much better way to do this but I'm straped for time
        # We will push the mathml string through an expression solver getting a 
        # results like "V1*funcDef(var1,var2)" and will extract var1/2 from funcDef
        solved.expr <- toString(mathml2R(mathml.exp))
        # Extract terms between parentheses
        terms <- str_extract_all(solved.expr, "\\((.*?)\\)")[[1]]
        # Remove the parentheses from the extracted terms
        terms <- gsub("\\(|\\)", "", terms)
        # Split the terms by commas and trim white space
        terms <- trimws(strsplit(terms, ",")[[1]])
        
        # Pull reaction information
        print("function term found, extracting being performed")
        reactants.exists <- FALSE
        products.exists   <- FALSE
        modifiers.exists  <- FALSE
        parameters.exists <- FALSE
        reaction.list <- vector("list", 1)
        found.terms <- c()
        
        for (k in seq_along(current.reaction)) {
          cur.node <- current.reaction[k]
          node.name <- names(cur.node)
          if (node.name == "listOfReactants") {
            print("list of reactants")
            reactants.exists <- TRUE
            node.reactants <- Attributes2Tibble(cur.node$listOfReactants)
            # Grab the species from tibble
            spec.grab <- node.reactants %>% pull(species)
            found.terms <- c(found.terms, spec.grab)
            # Condense multiple values to be comma separated
            collapsed.grab <- paste(spec.grab, collapse = ", ");
            reaction.list[[1]]$reactants <- collapsed.grab
          } else if (node.name == "listOfModifiers") {
            print("list of modidfiers")
            modifiers.exists <- TRUE
            node.modifiers <- Attributes2Tibble(cur.node$listOfModifiers)
            print(node.modifiers)
            modifier.grab <- node.modifiers %>% pull(species)
            found.terms <- c(found.terms, modifier.grab)
            
            reaction.list[[1]]$modifiers <- paste(modifier.grab,
                                                 collapse = ", ")
          } else if (node.name == "listOfProducts") {
            print("list of products")
            products.exists <- TRUE
            node.products <- Attributes2Tibble(cur.node$listOfProducts)
            product.grab <- node.products %>% pull(species)
            found.terms <- c(found.terms, product.grab)
            reaction.list[[1]]$products <- paste(product.grab,
                                                 collapse = ", ")
          } else if (node.name == "kineticLaw") {
            print("list of kineic law")
            # Check if parameter node exists
            node.par <- Attributes2Tibble(cur.node$kineticLaw$listOfParameters)
            # Build Parameter df to join with parameters
            print("NODE PAR")
            print(node.par)
            if (nrow(node.par)> 0) {
              parameters.exists <- TRUE
              # Condense parameter data to build with equations table
              reaction.list[[1]]$parameters <- paste(node.par %>% pull(id),
                                                     collapse = ", ")
              reaction.list[[1]]$parameters.val <- paste(node.par %>% pull(value),
                                                         collapse = ", ")
            } else {
              # assign all remaining variables to parameters
              parameters.exists <- TRUE
              
              pars.grab <- terms[which(!(terms %in% found.terms))]
              reaction.list[[1]]$parameters <- paste0(pars.grab, collapse = ", ")
            }
            
            
          }
        }
        # Check for null cases 
        if (!reactants.exists)  {reaction.list[[1]]$reactants  <- NA}
        if (!products.exists)   {reaction.list[[1]]$products   <- NA}
        if (!modifiers.exists)  {reaction.list[[1]]$modifiers  <- NA}
        if (!parameters.exists) {reaction.list[[1]]$parameters <- NA}
        print(reaction.list)
        
        # Perform model extraction for fxn definitions
        # Here we know the mathml code looks like 
        # <apply> <ci>lawname</ci><ci>var1</ci><ci>var2</ci></apply>
        # We want to extract the var names (var1, var2)
        
        
        
        # So now we have terms <- c("var1", "var2") We need to pull our original 
        # fxn variables in and compare them to these to see which are what kind of 
        # variable.  
        # For example, fdef$var <- c("sub", "v"), fdef$law <- "v*sub" 
        # Reaction dat: rdat$reactions <- var1, rdat$par <- v2
        # results fdef$reactants <- sub, fdef$par <- v
        # Notes: Need to account for when reactions have reactants/products that 
        #        exist but are not found in the law.
        print(terms)
        print("FUNCTION INFORMATION")
        # Pull function information
        # if (reactants.exists)  {fxn.reactants  <- c()}  else{fxn.reactants <- NA}
        # if (products.exists)   {fxn.products  <- c()}   else{fxn.products <- NA}
        # if (modifiers.exists)  {fxn.modifiers  <- c()}  else{fxn.modifiers <- NA}
        # if (parameters.exists) {fxn.parameters  <- c()} else{fxn.parameters <- NA}
        
        fxn.reactants  <- NA
        fxn.products   <- NA
        fxn.modifiers  <- NA
        fxn.parameters <- NA
        
        n.reactants  <- 0
        n.products   <- 0
        n.modifiers  <- 0
        n.parameters <- 0
        
        fxn.vars <- function.definitions[[i]]$variables
        print(fxn.vars)
        print(reaction.list[[1]]$reactants)
        print(reaction.list[[1]]$products)
        print(reaction.list[[1]]$modifiers)
        print(reaction.list[[1]]$parameters)
        for (ii in seq_along(terms)) {
          print(terms[ii])
          # check if the var is in elements
          if (terms[ii] %in% SplitEntry(reaction.list[[1]]$reactants)) {
            if (anyNA(fxn.reactants)) {fxn.reactants <- c()}
            fxn.reactants <- c(fxn.reactants, fxn.vars[ii])
            n.reactants <- n.reactants + 1
          } else if (terms[ii] %in% SplitEntry(reaction.list[[1]]$products)) {
            if (anyNA(fxn.products)) {fxn.products <- c()}
            fxn.products <- c(fxn.products, fxn.vars[ii])
            n.products <- n.products + 1
          } else if (terms[ii] %in% SplitEntry(reaction.list[[1]]$modifiers)) {
            if (anyNA(fxn.modifiers)) {fxn.modifiers <- c()}
            fxn.modifiers <- c(fxn.modifiers, fxn.vars[ii])
            n.modifiers <- n.modifiers + 1
          } else if (terms[ii] %in% SplitEntry(reaction.list[[1]]$parameters)) {
            if (anyNA(fxn.parameters)) {fxn.parameters <- c()}
            fxn.parameters <- c(fxn.parameters, fxn.vars[ii])
            n.parameters <- n.parameters + 1
          }
        }
        
        # Take into account possible variables that aren't in law (react/prod)
        if (!is.na(reaction.list[[1]]$reactants)) {
          react.i <- 1
          while (n.reactants < length(reaction.list[[1]]$reactants)) {
            if (anyNA(fxn.reactants)) {fxn.reactants <- c()}
            n.reactants <- n.reactants + 1
            to.add <- paste0("reactant_", react.i)
            fxn.reactants <- c(fxn.reactants, to.add)
            react.i <- react.i + 1
          }
        }
        
        if (!is.na(reaction.list[[1]]$products)) {
          prod.i <- 1
          while (n.products < length(reaction.list[[1]]$products)) {
            if (anyNA(fxn.products)) {fxn.products <- c()}
            n.products <- n.products + 1
            to.add <- paste0("product_", prod.i)
            fxn.products <- c(fxn.products, to.add)
            prod.i <- prod.i + 1
          }
        }
        
        
        function.definitions[[i]]$Reactants  <- collapseVector(fxn.reactants)
        function.definitions[[i]]$Products   <- collapseVector(fxn.products)
        function.definitions[[i]]$Modifiers  <- collapseVector(fxn.modifiers)
        function.definitions[[i]]$Parameters <- collapseVector(fxn.parameters)
        
        break
      }
      # Build up the function definition with found information
    }
  }
  
}
function.definitions
# Determine what parts of function definitions are substrate, prod, mod, etc

# Read in function definintion.

# Read in reactions and find one that has matching definition

# Grab that structures tree for sub, prod, mod

# Assign those to proper list variable