waiter_fxn <- function(msg, spinner, bar_value) {
  # hostess <- Hostess$new()
  out <- tagList(eval(parse(text = spinner)),
                 br(),
                 br(),
                 shinyWidgets::progressBar(
                   id = "sbml_load_bar",
                   value = bar_value,
                   total = 100
                 ),
                 h4(msg)
  )
  return(out)
}

LoadSBML_show_progress <- function(sbmlFile, w_sbml, spinner) {
  # This function is the same as LoadSBML but it is designed to show the 
  # progress bar screesn
  sleep.time <- 0.5
  out <- list()
  # Set initializers and bools
  
  exists.listOfCompartments        <- FALSE
  exists.listOfSpecies             <- FALSE
  exists.listOfParameters          <- FALSE
  exists.listOfRules               <- FALSE
  exists.listOfReactions           <- FALSE
  exists.listOfFunctionDefinitions <- FALSE
  exists.listOfUnitDefinitions     <- FALSE
  exists.parInReactions            <- FALSE
  
  function.definitions <- NA
  listOfParameters <- NA
  reaction.parameters.df <- NA
  compartment.df <- NA
  species.df <- NA
  rules.list <- NA
  
  message.log <- c()
  w_sbml$update(html = waiter_fxn("Reading In SBML File",
                                  spinner, 10))
  # Keep xml doc to remove eqn maths
  doc <- xmlTreeParse(sbmlFile, ignoreBlanks = TRUE)
  
  # Extract model from sbml file
  sbmlList <- read_xml(sbmlFile) %>% as_list()
  modelList <- sbmlList$sbml$model
  out[["model"]] <- modelList
  
  # Extract Compartments
  if (!is.null(modelList$listOfCompartments)) {
    mes <- "Extracting Compartments"
    compartment.df <- Attributes2Tibble(modelList$listOfCompartments)
    compartment.df <- FinalizeCompartmentData(compartment.df)
    out[["compartments"]] <- compartment.df
    exists.listOfCompartments <- TRUE
  } else {mes <- "No Compartments to Extract"}
  message.log <- c(message.log, mes)
  w_sbml$update(html = waiter_fxn(paste0(message.log, collapse = "/n"),
                                  spinner, 
                                  10))
  Sys.sleep(sleep.time)
  
  w_sbml$update(html = waiter_fxn("Extracting Species", spinner, 20))
  # Extract Species
  if (!is.null(modelList$listOfSpecies)) {
    species.df <- Attributes2Tibble(modelList$listOfSpecies)
    species.df <- FinalizeSpeciesData(species.df)
    out[["species"]] <- species.df
    exists.listOfSpecies <- TRUE
  }
  Sys.sleep(sleep.time)
  
  w_sbml$update(html = waiter_fxn("Extracting Parameters", spinner, 30))
  # Extract Parameters
  if (!is.null(modelList$listOfParameters)) {
    listOfParameters <- Attributes2Tibble(modelList$listOfParameters)
    exists.listOfParameters <- TRUE
  }
  Sys.sleep(sleep.time)
  
  w_sbml$update(html = waiter_fxn("Extracting Rules", spinner, 35))
  # Extract Rules
  if (!is.null(modelList$listOfRules)) {
    rules.header <- Attributes2Tibble(modelList$listOfRules)
    rules.assignment.vars <- rules.header %>% pull(variable)
    rules.list <- ExtractRulesMathFromSBML(doc, rules.assignment.vars)
    
    out[["rules"]] <- rules.list
    exists.listOfRules <- TRUE
  }
  Sys.sleep(sleep.time)
  
  w_sbml$update(html = waiter_fxn("Extracting Function Definitions", 
                                  spinner, 40))
  # Extract Function Definitions
  if (!is.null(modelList$listOfFunctionDefinitions)) {
    func.info <- Attributes2Tibble(modelList$listOfFunctionDefinitions)
    function.definitions <- ExtractFunctionDefFromSBML(doc, func.info)
    function.definitions <- FindFunctionDefInformation(doc,
                                                       function.definitions,
                                                       sbmlList)
    out[["functions"]] <- function.definitions
  }
  Sys.sleep(sleep.time)
  
  w_sbml$update(html = waiter_fxn("Extracting Reactions", 
                                  spinner, 50))
  # Extract Reactions
  if (!is.null(modelList$listOfReactions)) {
    exists.listOfReactions <- TRUE
    
    # Pull Reaction Tags
    reaction.tags <- ExtractionReactionTagFromSBML(modelList$listOfReactions)
    reaction.ids  <- reaction.tags %>% pull(id)
    
    # Loop through reactions grabbing relevant information
    reaction.list <- vector("list", length(modelList$listOfReactions))
    for (i in seq_along(modelList$listOfReactions)) {
      current.reaction <- modelList$listOfReactions[[i]]
      reaction.list[[i]] <- ExtractReactionBaseFromSBML(current.reaction)
      names(reaction.list)[i] <- reaction.ids[i]
    }
    
    # Check if Reaction Parameters Exist
    if (!is.na(reaction.list[[1]]$Parameter.Values)) {
      exists.parInReactions <- TRUE
      reaction.pars <- c()
      reaction.pars.vals <- c()
      for (ii in seq_along(reaction.list)) {
        reaction.pars <- c(reaction.pars,
                           SplitEntry(reaction.list[[ii]]$Parameters))
        reaction.pars.vals <- c(reaction.pars.vals,
                                SplitEntry(reaction.list[[ii]]$Parameter.Values))
      }
      reaction.parameters.df <- data.frame(reaction.pars, reaction.pars.vals)
      colnames(reaction.parameters.df) <- c("Parameters", "Values")
    }
    
    # Add math to reactions list
    reaction.list <- ExtractReactionMathFromSBML(doc, 
                                                 reaction.list,
                                                 function.definitions)
    
    # Combine Tags With Reaction Math
    reaction.list <- CombineReactionTagsWReactions(reaction.tags,
                                                   reaction.list)
    
    out[["reactions"]] <- reaction.list
    
  }
  Sys.sleep(sleep.time)
  w_sbml$update(html = waiter_fxn("Combining Parameter Information", 
                                  spinner, 60))  # Bind Parameter lists if they both exist
  
  # Finalize Data Outputs to Normalize Output
  final.parameters.df <- FinalizeParameterData(listOfParameters,
                                               reaction.parameters.df,
                                               rules.list)
  out[["parameters"]] <- final.parameters.df
  
  Sys.sleep(sleep.time)
  
  return(out)
}

# Load from sbml file (xml)
observeEvent(input$file_input_load_sbml, {

  spinner <- RandomHTMLSpinner()
  w_sbml <- Waiter$new(html = waiter_fxn("Loading SBML Model",
                                         spinner, 
                                         0))
  w_sbml$show()
  sbml.model <- LoadSBML_show_progress(input$file_input_load_sbml$datapath,
                                       w_sbml, 
                                       spinner)
  # browser()
  # Load SMBL
  # sbml.model <- LoadSBML(input$file_input_load_sbml$datapath)
  # print(sbml.model)

  ## Unpack SBML Compartments --------------------------------------------------
  # Current compartment values used by this program
  # Values:
  #   Name
  #   ID
  #   Value
  #   Volume (volume variable: V1, V2 etc)
  #   par.Id (id associated with volume)
  #   Unit
  #   UnitDescription
  #   BaseUnit
  #   BaseValue
  #   Description

  # Have to also add to parameters for volumn
  # Values:
  #   Name
  #   ID
  #   Value
  #   Unit
  #   UnitDescription
  #   BaseUnit
  #   BaseValue
  #   Description
  #   Type
  #   Type.note

  # SBML stores the compartment volume as the V_{compartment_name}
  # units come out as the type: which would be "volume".
  # So the only things we really look for here are the name
  # Overwrite ids
  # Assign base units as base volumn
  # browser()
  sleep.time <- 0.5
  mes <- "Converting Compartment information to BioModME..."
  w_sbml$update(html = waiter_fxn(mes, 
                                  spinner, 65))
  Sys.sleep(sleep.time)
  
  # Bool that is used in reactions. SBML stores compartment id and we want to 
  # store the name.  If TRUE, we will need to perform a conversion. 
  need.to.convert.comp.names <- FALSE
  
  compartments <- sbml.model$compartments
  n.compartments <- nrow(compartments)
  print(compartments)
  
  # Compartments have the following columns
  #   id, name, size, constant, spatialDimensions
 
  comp.ids   <- compartments %>% pull(id)
  comp.names <- compartments %>% pull(name)
  comp.df.conv <- data.frame(comp.ids, comp.names)
  colnames(comp.df.conv) <- c("id", "name")
  if (!identical(comp.ids, comp.names)) {
    need.to.convert.comp.names <- TRUE
  }
  
  comp.values <- compartments %>% pull(size)


  # Compartment Volume Names
  # SBML used the comp name for the volume var, which is fine but we want to 
  # create a separate variable for that
  comp.vol.names <- paste0("V_", comp.names)

  # Generate Compartment IDs (overwrite sbml ids and create vol ids)
  comp.ids <- c()
  vol.ids  <- c()
  for (i in seq_len(nrow(compartments))) {
    # Generate Compartment IDs
    new.id <- GenerateId(rv.ID$id.comp.seed, "compartment")
    comp.ids <- c(comp.ids, new.id$id)
    rv.ID$id.comp.seed <- new.id$seed
    # Store to id db
    idx.to.add <- nrow(rv.ID$id.df) + 1
    rv.ID$id.df[idx.to.add, ] <- c(new.id$id, comp.names[i])

    # Generate Volume IDs
    new.id <- GenerateId(rv.ID$id.param.seed, "parameter")
    vol.ids <- c(vol.ids, new.id$id)
    rv.ID$id.param.seed <- new.id$seed
    # Store id to db
    idx.to.add <- nrow(rv.ID$id.df) + 1
    rv.ID$id.df[idx.to.add, ] <- c(new.id$id, comp.vol.names[i])

  }
  
  comp.list     <- vector("list", n.compartments)
  comp.vol.list <- vector("list", n.compartments)
  # Add additional list tags for our problem
  for (i in seq_along(comp.list)) {
    # Build Compartment Entry
    comp.list[[i]]$ID              <- comp.ids[i]
    comp.list[[i]]$Name            <- comp.names[i]
    comp.list[[i]]$Value           <- comp.values[i]
    comp.list[[i]]$Volume          <- comp.vol.names[i]
    comp.list[[i]]$par.id          <- vol.ids[i]
    comp.list[[i]]$Unit            <- rv.UNITS$units.base$Volume
    comp.list[[i]]$UnitDescription <- "volume"
    comp.list[[i]]$BaseUnit        <- rv.UNITS$units.base$Volume
    comp.list[[i]]$BaseValue       <- comp.values[i]
    comp.list[[i]]$Description     <- ""

    comp.vol.list[[i]]$Name            <- comp.vol.names[i]
    comp.vol.list[[i]]$ID              <- vol.ids[i]
    comp.vol.list[[i]]$Value           <- comp.values[i]
    comp.vol.list[[i]]$Unit            <- rv.UNITS$units.base$Volume
    comp.vol.list[[i]]$UnitDescription <- "volume"
    comp.vol.list[[i]]$BaseUnit        <- rv.UNITS$units.base$Volume
    comp.vol.list[[i]]$BaseValue       <- comp.values[i]
    comp.vol.list[[i]]$Description     <- ""
    comp.vol.list[[i]]$Type            <- "Compartment"
    comp.vol.list[[i]]$Type.note       <- "Volume"
  }

  names(comp.list) <- comp.ids

  # Assign to RV
  rv.COMPARTMENTS$compartments <- comp.list
  
  ## Unpack SBML Species --------------------------------------------------
  # Current compartment values used by this program
  # Values:
  #   Name
  #   ID
  #   Value
  #   Unit
  #   UnitDescription
  #   BaseUnit
  #   BaseValue
  #   Description
  #   Compartment
  #   Compartment ID
  #   boundaryCondition (if true, differential eqn gen is ignored)

  mes <- "Converting Species information to BioModME..."
  w_sbml$update(html = waiter_fxn(mes, 
                                  spinner, 70))
  
  # browser()
  species <- sbml.model$species
  n.species <- nrow(species)
  # print(species)

  # Species from SBML have the following columns
  #   id, name, initialConcentration, substanceUnits, compartment, constant,
  #   boundaryCondition
  
  species.id     <- species %>% pull(id)
  species.names  <- species %>% pull(name)
  species.values <- as.numeric(species %>% pull(initialConcentration))
  species.comp   <- species %>% pull(compartment)
  
  # Conversion df
  species.df.conv <- data.frame(species.id, species.names)
  colnames(species.df.conv) <- c("id", "name")
  
  # Convert compartments names 
  if (need.to.convert.comp.names) {
    new.spec <- vector(mode = "character", length = length(species.comp))
    if (need.to.convert.comp.names) {
      for (i in seq_along(species.comp)) {
        idx <- which(species.comp[i] %in% comp.df.conv$id)
        new.spec[i] <- comp.df.conv$name[idx]
      }
      species.comp <- new.spec
    }
  }

  # Extract Boundary Condition
  species.bounds <- species %>% pull(boundaryCondition)


  # Generate Species IDs
  species.ids <- c()
  for (i in seq_len(nrow(species))) {
    # Generate Compartment IDs
    new.id <- GenerateId(rv.ID$id.var.seed, "var")
    species.ids <- c(species.ids, new.id$id)
    rv.ID$id.var.seed <- new.id$seed
    idx.to.add <- nrow(rv.ID$id.df) + 1
    rv.ID$id.df[idx.to.add, ] <- c(new.id$id, species.names[i])
  }

  species.list     <- vector("list", n.species)
  # Add additional list tags for our problem
  for (i in seq_along(species.list)) {
    # Build Compartment Entry
    species.list[[i]]$ID                <- species.ids[i]
    species.list[[i]]$Name              <- species.names[i]
    species.list[[i]]$Value             <- species.values[i]
    species.list[[i]]$Unit              <- rv.UNITS$units.base$For.Var
    species.list[[i]]$UnitDescription   <- "conc (mol)"
    species.list[[i]]$BaseUnit          <- rv.UNITS$units.base$For.Var
    species.list[[i]]$BaseValue         <- species.values[i]
    species.list[[i]]$Description       <- ""
    species.list[[i]]$Compartment       <- species.comp[i]
    species.list[[i]]$Compartment.id    <- ""
    species.list[[i]]$boundaryCondition <- species.bounds[i]
  }


  names(species.list) <- species.ids

  # Assign to RV
  rv.SPECIES$species <- species.list
  rv.SPECIES$species.df <- bind_rows(rv.SPECIES$species)
  var.names <- rv.SPECIES$species.df %>% dplyr::select(Name)
  rv.SPECIES$species.names <- as.vector(unlist(var.names))
  rv.REFRESH$refresh.species.table <- rv.REFRESH$refresh.species.table + 1

  # print(species.list)
  # print(rv.SPECIES$species)
  # print(rv.SPECIES$species.df)

  ## Unpack SBML Params --------------------------------------------------
  # Current Parmaeter values used by this program
  # Values:
  #   Name
  #   ID
  #   Value
  #   Unit
  #   UnitDescription
  #   BaseUnit
  #   BaseValue
  #   Description
  #   Type
  #   Type.Note

  # SMBL load passes a list with two different parameter dfs.
  # pars$parameters is constant parameters
  # pars$non.constant.parameters are non constant parameters which need to be 
  #     added to the appropriate RV
  
  mes <- "Converting Parameter information to BioModME..."
  w_sbml$update(html = waiter_fxn(mes, 
                                  spinner, 80))
  
  pars    <- sbml.model$parameters$Parameters
  nc.pars <- sbml.model$parameters$Variable.Parameters
  n.pars  <- nrow(pars)
  
  # Parameter Load has the following df columns:
  #   id, name, value, constant
  
  parameters.names <- pars %>% dplyr::pull(name)
  par.vals         <- pars %>% dplyr::pull(value)
  par.constant     <- pars %>% dplyr::pull(constant)

  # Overwrite Ids
  par.ids  <- vector("character", n.pars)
  for (i in seq_len(nrow(pars))) {
    # Generate Parameter IDs
    new.id <- GenerateId(rv.ID$id.param.seed, "parameter")
    par.ids[i] <- new.id$id
    rv.ID$id.param.seed <- new.id$seed

    idx.to.add <- nrow(rv.ID$id.df) + 1
    rv.ID$id.df[idx.to.add, ] <- c(new.id$id, parameters.names[i])
  }

  par.list <- vector("list", n.pars)
  # TODO add custom to pars (change constant to custom and flip bool propbably)
  # Add additional list tags for our problem
  for (i in seq(n.pars)) {
    par.list[[i]]$Name            <- parameters.names[i]
    par.list[[i]]$ID              <- par.ids[i]
    par.list[[i]]$Value           <- as.numeric(par.vals[i])
    par.list[[i]]$Unit            <- NA
    par.list[[i]]$UnitDescription <- NA
    par.list[[i]]$BaseUnit        <- NA
    par.list[[i]]$BaseValue       <- as.numeric(par.vals[i])
    par.list[[i]]$Description     <- ""
    par.list[[i]]$Type            <- "Loaded From SBML File"
    par.list[[i]]$Type.note       <- ""
    par.list[[i]]$Used.In         <- NA
    par.list[[i]]$Custom          <- FALSE
    par.list[[i]]$ConstantValue   <- par.constant[i]
  }

  names(par.list) <- par.ids

  # Store information to our parameter tables
  rv.PARAMETERS$parameters <- par.list

  ## Unpack SBML Reaction ____--------------------------------------------------
  # Current Reaction values used by this program
  # ID                || Specific equation ID
  # Eqn.Display.Type  || Display name shown on tables
  # Reaction.Law      || Law that the equation uses
  # Species           || Species in equations
  # Reactants         || Reactants in reactions
  # Products          || Products in reactions
  # Modifiers          || Species in equations that arent involved in diff eqns
  # Rate.Constants    || Parameters in equation
  # Compartment       || Compartment reaction occurs in
  # Description       || Equation Description
  # Species.id        || IDs of species in reaction
  # Reactants.id      || IDs of reactants in reaction
  # Products.id       || IDs of products in reaction
  # Modifiers.id       || IDs of modifiers in model
  # Parameters.id     || IDs of parameters in model
  # Compartment.id    || ID of compartment eqn is in
  # Equation.Text     || Text version of equation
  # Equation.Latex    || Latex text version of equation
  # Equation.MathJax  || Mathjax text version of equation
  # String.Rate.Law   || String text for rate law
  # Latex.Rate.Law    || Latex version of rate law
  # MathJax.Rate.Law  || MathJax version of rate law
  # Rate.MathML       || MathMl for rate law
  # Reversible        || Bool if the equation is reversible or not

  
  # mes <- "Converting Reactions to BioModME..."
  # w_sbml$update(html = waiter_fxn(mes, 
  #                                 spinner, 85))
  # 
  # reactions <- sbml.model$reactions
  # 
  # for (i in seq_along(reactions)) {
  #   entry <- reactions[i,]
  # 
  #   # Equation info
  #   if (!is.null(entry$name)) {
  #     eqn.display <- entry %>% pull(name)
  #   } else {
  #     eqn.display <- "Custom Load Law"
  #   }
  # 
  #   #TODO: set laws for custom eqn, make law user_custom_law_..
  # 
  #   # Extract Reactants
  #   reactants  <- SplitEntry(entry %>% pull(reactants))
  #   products   <- SplitEntry(entry %>% pull(products))
  #   parameters <- SplitEntry(entry %>% pull(parameters))
  #   par.vals   <- SplitEntry(entry %>% pull(parameters.val))
  #   string.law <- entry %>% pull(str.law)
  #   mathml.law <- entry %>% pull(mathml)
  #   reversible <- entry %>% pull(reversible)
  #   modifiers   <- NA
  # 
  #   # Get/Set ID for reaction
  #   if (!is.null(entry$id)) {
  #     ID.to.add  <- entry %>% pull(id)
  #     idx.to.add <- nrow(rv.ID$id.df) + 1
  #     rv.ID$id.df[idx.to.add, ] <- c(ID.to.add, string.law)
  #   } else {
  #     # Create new id
  #     ID.gen <- GenerateId(rv.ID$id.eqn.seed, "eqn")
  #     rv.ID$id.eqn.seed <- rv.ID$id.eqn.seed + 1
  #     ID.to.add <- ID.gen[["id"]]
  #     idx.to.add <- nrow(rv.ID$id.df) + 1
  #     rv.ID$id.df[idx.to.add, ] <- c(ID.to.add, text.eqn)
  #   }
  # 
  #   # Find IDs of species, reactants, products, and modifiers in reaction
  #   reactants.id <- c()
  #   for (i in seq_along(reactants)) {
  #     reactants.id[i] <- FindId(reactants[i])
  #   }
  # 
  #   products.id <- c()
  #   for (i in seq_along(products)) {
  #     products.id[i] <- FindId(products[i])
  #   }
  # 
  #   modifiers.id <- c()
  #   for (i in seq_along(modifiers)) {
  #     modifiers.id[i] <- FindId(modifiers[i])
  #   }
  # 
  #   parameters.id <- c()
  #   for (i in seq_along(parameters)) {
  #     parameters.id[i] <- FindId(parameters[i])
  #   }
  # 
  #   # TODO: Find the compartment the reaction takes place in
  #   compartment    <- "TODO FIND"
  #   compartment.id <- "COMP ID"
  # 
  #   # TODO: eqn desctipoint
  #   eqn.description <- "TODO FIND DESCRIPT"
  # 
  #   # Build equation text, latex, and mathjax
  #   eqn.builds <- BuildCustomEquationText(reactants,
  #                                         products,
  #                                         modifiers,
  #                                         parameters)
  # 
  #   text.eqn    <- eqn.builds$text
  #   latex.eqn   <- eqn.builds$latex
  #   mathjax.eqn <- eqn.builds$mathjax
  # 
  #   # Build rate laws from string
  #   # TODO: these don't seem to be converting things
  #   convert.rate.law <- ConvertRateLaw(string.law)
  #   p.rate.law       <- NA
  #   latex.law        <- convert.rate.law$latex
  #   mathjax.law      <- convert.rate.law$mathjax
  #   mathml.law       <- katex::katex_mathml(latex.law)
  # 
  #   species    <- RemoveNA(c(reactants, products))
  #   species.id <- RemoveNA(c(reactants.id, products.id))
  # 
  #   par.collapsed          <- collapseVector(parameters)
  #   par.id.collapsed       <- collapseVector(parameters.id)
  #   reactants.collapsed    <- collapseVector(reactants)
  #   reactants.id.collapsed <- collapseVector(reactants.id)
  #   products.collapsed     <- collapseVector(products)
  #   products.id.collapsed  <- collapseVector(products.id)
  #   species.collapsed      <- collapseVector(species)
  #   species.id.collapsed   <- collapseVector(species.id)
  #   modifiers.collapsed     <- collapseVector(modifiers)
  #   modifiers.id.collapsed  <- collapseVector(modifiers.id)
  # 
  #   # Add overall reaction information
  #   reaction.entry <- list(
  #     "ID"               = ID.to.add,
  #     "Eqn.Display.Type" = eqn.display,
  #     "Reaction.Law"     = "CUSTOM LAW",
  #     "Species"          = species.collapsed,
  #     "Reactants"        = reactants.collapsed,
  #     "Products"         = products.collapsed,
  #     "Modifiers"         = modifiers.collapsed,
  #     "Parameters"       = par.collapsed,
  #     "Compartment"      = compartment,
  #     "Description"      = eqn.description,
  #     "Species.id"       = species.id.collapsed,
  #     "Reactants.id"     = reactants.id.collapsed,
  #     "Products.id"      = products.id.collapsed,
  #     "Modifiers.id"      = modifiers.id.collapsed,
  #     "Parameters.id"    = par.id.collapsed,
  #     "Compartment.id"   = compartment.id,
  #     "Equation.Text"    = text.eqn,
  #     "Equation.Latex"   = latex.eqn,
  #     "Equation.MathJax" = mathjax.eqn,
  #     "String.Rate.Law"  = string.law,
  #     "Pretty.Rate.Law"  = p.rate.law,
  #     "Latex.Rate.Law"   = latex.law,
  #     "MathJax.Rate.Law" = mathjax.law,
  #     "MathMl.Rate.Law"  = mathml.law,
  #     "Reversible"       = reversible
  #   )
  # 
  #   rv.REACTIONS$reactions[[ID.to.add]] <- reaction.entry
  # }


  # So this is where things get tricky. SBML level 2 doesn't really store
  # information on what the equation is. We just a string law and mathml law to
  # relate them. SO. Lets thing about this.  We information can we extract from
  # the pulled sbml data.
  # products
  # parameters
  # parameters.val
  # math.ml
  # str.law
  # reactants
  # id
  # metaid
  # name
  # reversible
  # fast (have to look up what this one means)







  # TODO:
  # Current found problem:
  # Reactions are using ids not names.  So I take the names from up above
  # and try to use those in id lookup but the names don't exist because they
  # are different variables found in reactions.
  # So...
  # We will have to create a conversion dictionary when we extract the species
  # if nescessary, and then use that to do conversions in reactions for
  # reactants, products, and math equations.
  # It seems level to version 1 just useds ids, but version 4 has names
  # We set up the workaround for this and then continue with next steps.


  # Other values extracted are useless and not needed and should be deleted from
  # the extraction function in the future.
  # reactions <- sbml.model$reactions
  # 
  # # Grab Reactants and Products, split on ",", and re-collapse on space.
  # # Remove spaces from split word if they exist
  # # Search for ids related to reactants and products, vectorize, and store
  # 
  # pull.reactants <- reactions %>% dplyr::pull(reactants)
  # # print(pull.reactants)
  # reactants <- convertReactionVarsFromSBML(pull.reactants)
  # 
  # pull.products <- reactions %>% dplyr::pull(products)
  # products <- convertReactionVarsFromSBML(pull.products)
  # 
  # pull.params <- reactions %>% dplyr::pull(parameters)
  # parameters <- convertReactionVarsFromSBML(pull.params)



  # for (i in seq_along(reactants)) {
  #   reactant.ids <- c(reactant.ids)
  # }

  # Find compartment and lookup compartment id

  # Find parameters and lookup corresponding id

  # Try to copy equation string from load to rvs

  # Try to convert to latex and Mathjax

  # Use name from sbml load as description

  # Make Eqn.Type be custom and Null out Law.


  # Load Variables ---------------------------------------------------------------
  # rv.COMPARTMENTS$compartments  <- model$compartments.info
  # rv.COMPARTMENTS$compartments.df    <- model$compartments.df
  # vars$compartment.table  <- model$compartment.table
  # rv.COMPARTMENTS$compartments.names <- model$compartments.names
  # rv.SPECIES$df.by.compartment  <- model$df.by.compartment
  # rv.SPECIES$species.names          <- model$var.names
  # rv.SPECIES$species.names            <- model$species
  # vars$descriptions       <- model$descriptions
  # vars$table              <- model$table
  # rv.SPECIES$species           <- model$var.info
  #
  # End UI Trigger Events
  w_sbml$hide()
  
})
