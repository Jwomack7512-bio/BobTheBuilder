
# Functions --------------------------------------------------------------------
LoadCheck <- function(loadedValue, initValue) {
  #this function is meant to perform checks on if loaded value is null
  #between versions there are often things that get added that save as null.
  #these checks are meant to stop breakdown between old models and new app versions
  # Inputs:
  #  @loadedValue - the value loaded from the model
  #  @initi value - what the initialzied value should be if it is null
  if (!isTruthy(loadedValue)) {
    out <- initValue
  } else {
    out <- loadedValue
  }
  return(out)
}

# Waiters ----------------------------------------------------------------------
w_load <- Waiter$new(html =  spin_pong(),
                     color = transparent(0.9))

#when load model button is pressed, the .rds file is loaded in and its components are broken apart and added to the model
#some of these loads for reactive variables use a "is.null" check to make sure they exist.  These variables were added
# after specific models were made and this adds functionality to those models that would otherwise have issues.
# Event: Load model ############################################################
observeEvent(input$load_model, {
  
  waiter_show(html = waiting_screen)
  Sys.sleep(1)
  model <- readRDS(input$load_model$datapath)
  
  # Load Compartments ----------------------------------------------------------
  rv.COMPARTMENTS$compartments       <- model$compartments.info
  rv.COMPARTMENTS$compartments.df    <- model$compartments.df
  rv.COMPARTMENTS$compartments.names <- model$compartments.names
  
  # Load Species ---------------------------------------------------------------
  rv.SPECIES$df.by.compartment  <- model$df.by.compartment
  rv.SPECIES$species.names      <- model$species.names
  rv.SPECIES$species            <- model$species

  # Load Equations--------------------------------------------------------------
  rv.REACTIONS$reaction.id.counter   <- model$reaction.id.counter
  
  rv.REACTIONS$eqn.info        <- model$eqn.info
  rv.REACTIONS$massAction      <- model$massAction
  rv.REACTIONS$michaelisMenten <- model$michaelisMenten
  rv.REACTIONS$synthesis       <- model$synthesis
  rv.REACTIONS$degradation     <- model$degradation
  
  # Load Parameters ------------------------------------------------------------
  params$par.info <- model$par.info
  params$par.df   <- model$par.df
  params$par.names <- model$par.names
  params$non.constant.pars <- model$non.constant.pars
  
  # Load Differential Equations ------------------------------------------------
  DE$eqns               <- model$eqns
  DE$de.eqns.for.solver <- model$de.eqns.for.solver
  DE$eqn.in.latex       <- model$eqn.in.latex
  DE$custom.diffeq.var  <- model$custom.diffeq.var
  DE$custom.diffeq      <- model$custom.diffeq
  DE$custom.diffeq.df   <- model$custom.diffeq.df
  
  # Load Input/Output ----------------------------------------------------------
  rv.IO$IO.df              <- model$IO.df
  rv.IO$IO.logs            <- model$IO.logs
  rv.IO$IO.info        <- model$IO.info
  
  # Load Options ---------------------------------------------------------------
  options$time.start <- model$time.start
  options$time.end <- model$time.end
  options$time.step <- model$time.step
  options$time.scale.bool <- model$time.scale.bool
  options$time.scale.value <- model$time.scale.value
  options$ode.solver.type <- model$ode.solver.type
  
  # Load Results ---------------------------------------------------------------
  results$model       <- model$model
  results$is.pp       <- model$is.pp
  results$pp.eqns     <- model$pp.eqns
  results$pp.eqns.col <- model$pp.eqns.col
  results$pp.vars     <- model$pp.vars
  results$pp.model    <- model$pp.model
  results$model.final <- model$model.final
  results$model.has.been.solved <- model$model.has.been.solved
  results$model.units.view <- model$model.units.view
  results$time.units <- model$time.units
  results$concentration.units <- model$concentration.units
  
  # Load Logs ------------------------------------------------------------------
  logs$variable.debug.button <- model$variable.debug.button
  logs$variable.debug.table <- model$variable.debug.table
  
  # Load IDs -------------------------------------------------------------------
  id$id.df <- model$id.df
  id$id.var.seed    <- model$id.var.seed
  id$id.param.seed  <- model$id.param.seed
  id$id.eqn.seed    <- model$id.eqn.seed
  id$id.io.seed     <- model$id.io.seed
  id$id.comp.seed   <- model$id.comp.seed

  # Parameter Estimation -------------------------------------------------------
  pe$loaded.species    <- model$loaded.species
  pe$pars              <- model$pars
  pe$initial.guess     <- model$initial.guess
  pe$lb                <- model$lb
  pe$ub                <- model$ub
  pe$calculated.values <- model$calculated.values
  pe$solved.model      <- model$solved.model
  pe$successful.run    <- model$successful.run
  pe$previous.values   <- model$previous.values
  pe$log.of.run        <- model$log.of.run
  
  # Load Units -----------------------------------------------------------------
  # Dont need to load types, base.units, or possible.units
  units$selected.units <- model$selected.units

  # Load Loop Mode RVs ---------------------------------------------------------
  loop$ICs <- ICs$ICs.table
  loop$model.results <- results$model.final
  loop$time.start <- options$time.start
  loop$time.end <- options$time.end
  loop$time.step <- options$time.step

  
  counts$loading.model <- counts$loading.model + 1
  # Plot - Compare Mode --------------------------------------------------------
  # compareModel$model.1 <- results$model.final
  # compareModel$model.2 <- results$model.final
  # compareModel$model.3 <- results$model.final
  # compareModel$model.4 <- results$model.final
  
  # Update UI w/ Loaded Values -------------------------------------------------
  # The next two reset the parameter table
  updatePickerInput(session = session,
                    inputId = "parameters_filter_type",
                    selected = "Eqns")
  updatePickerInput(session = session,
                    inputId = "parameters_filter_type",
                    selected = "All")
  
  updatePickerInput(
    session = session,
    "createVar_deleteVarPicker",
    choices = sort(names(rv.SPECIES$species))
  )
  
  updatePickerInput(session,
                    "eqnCreate_rate_firstvar",
                    choices = names(params$par.info))
  
  updatePickerInput(session,
                    "InOut_selectVar",
                    choices = sort(names(rv.SPECIES$species)))
  
  updatePickerInput(session,
                    "Inout_delete_IO_eqn",
                    choices = seq(rv.IO$n.IO))

  updatePickerInput(session,
                    'eqnCreate_edit_select_equation',
                    choices = seq(length(rv.REACTIONS$eqn.info)))
  
  #updates output enzyme choices for enzyme degradation
  updatePickerInput(session,
                    "enzyme_deg_enzyme",
                    choices = sort(names(rv.SPECIES$species)))
  
  updatePickerInput(session,
                    "MA_species",
                    choices = sort(names(rv.SPECIES$species)))
  
  #updates output substrate choices for enzyme degradation
  updatePickerInput(session, 
                    "enzyme_deg_substrate",
                    choices = sort(names(rv.SPECIES$species)))
  
  # Update Model Options -------------------------------------------------------
  updateTextInput(session,
                  "execute_time_start",
                  value = options$time.start)
  updateTextInput(session,
                  "execute_time_end",
                  value = options$time.end)
  updateTextInput(session,
                  "execute_time_step",
                  value = options$time.step)
  updateCheckboxInput(session,
                      "execute_turnOn_time_scale_var",
                      value = options$time.scale.bool)
  updateTextInput(session,
                  "execute_time_scale_var",
                  value = options$time.scale.value)
  updatePickerInput(session,
                    "execute_ode_solver_type",
                    selected = options$ode.solver.type)
  
  if (ncol(results$model.final) != 0) {
    updatePickerInput(session
                      , "lineplot_xvar"
                      , choices = colnames(results$model.final[1]))
  }
  
  updateSelectizeInput(
    session,
    "lineplot_yvar",
    choices  = colnames(results$model.final)[2:ncol(results$model.final)],
    selected = colnames(results$model.final)[2:ncol(results$model.final)]
  )
  updateTextInput(session, "loop_start_time", value = input$execute_time_start)
  updateTextInput(session, "loop_end_time", value = input$execute_time_end)
  updateTextInput(session, "loop_time_step", value = input$execute_time_step)
  
  # w_load$hide()
  waiter_hide()
})

# Load from sbml file (xml)
observeEvent(input$file_input_load_sbml, {
  
  # UI Trigger Events
  waiter_show(html = waiting_screen)
  Sys.sleep(1)
  
  # Load SMBL
  sbml.model <- LoadSBML(input$file_input_load_sbml$datapath)
  print(sbml.model)
  
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
  compartments <- sbml.model$compartments
  n.compartments <- nrow(compartments)
  print(compartments)
  
  # Check to see if smbl uses "name" tag.  If so assign these as names.
  # If not take id tag and use as name and rewrite id.
  if (!is.null(compartments$name)) {
    comp.names <- compartments %>% pull(name)
  } else {
    comp.names <- compartments %>% pull(id)
    
  }
  
  print(compartments$initialAmount)
  
  # Extract value (size in old sbml, initialAmount in newer)
  if (!is.null(compartments$initialAmount)) {
    comp.values <- compartments %>% pull(initialAmount)
  } else if (!is.null(compartments$size)) {
    comp.values <- compartments %>% pull(size)
  } else {
    sbml.load.bug <- TRUE
  }
  
  
  # Compartment Volume Names
  # These arent stored but seem to take the value of V_{compartment.name}
  comp.vol.names <- paste0("V_", comp.names)
  
  # Generate Compartment IDs
  comp.ids <- c()
  vol.ids  <- c()
  for (i in seq(n.compartments)) {
    # Generate Compartment IDs
    new.id <- GenerateId(id$id.comp.seed, "compartment")
    comp.ids <- c(comp.ids, new.id$id)
    id$id.comp.seed <- new.id$seed
    # Store to id db
    idx.to.add <- nrow(id$id.df) + 1
    id$id.df[idx.to.add, ] <- c(new.id$id, comp.names[i])
    
    # Generate Volume IDs
    new.id <- GenerateId(id$id.param.seed, "parameter")
    vol.ids <- c(vol.ids, new.id$id)
    id$id.param.seed <- new.id$seed
    # Store id to db
    idx.to.add <- nrow(id$id.df) + 1
    id$id.df[idx.to.add, ] <- c(new.id$id, comp.vol.names[i])
    
  }

  comp.list     <- vector("list", n.compartments)
  comp.vol.list <- vector("list", n.compartments)
  # Add additional list tags for our problem
  for (i in seq(n.compartments)) {
    # Build Compartment Entry
    comp.list[[i]]$ID              <- comp.ids[i]
    comp.list[[i]]$Name            <- comp.names[i]
    comp.list[[i]]$Value           <- comp.values[i]
    comp.list[[i]]$Volume          <- comp.vol.names[i]
    comp.list[[i]]$par.id          <- vol.ids[i]
    comp.list[[i]]$Unit            <- units$base.units$Volume
    comp.list[[i]]$UnitDescription <- "volume"
    comp.list[[i]]$BaseUnit        <- units$base.units$Volume
    comp.list[[i]]$BaseValue       <- comp.values[i]
    comp.list[[i]]$Description     <- ""
    
    comp.vol.list[[i]]$Name            <- comp.vol.names[i]
    comp.vol.list[[i]]$ID              <- vol.ids[i]
    comp.vol.list[[i]]$Value           <- comp.values[i]
    comp.vol.list[[i]]$Unit            <- units$base.units$Volume
    comp.vol.list[[i]]$UnitDescription <- "volume"
    comp.vol.list[[i]]$BaseUnit        <- units$base.units$Volume
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
  
  
  browser()
  
  species <- sbml.model$species
  n.species <- nrow(species)
  print(species)
  
  # Check to see if smbl uses "name" tag.  If so assign these as names.
  # If not take id tag and use as name and rewrite id.
  if (!is.null(species$name)) {
    species.names <- species %>% pull(name)
  } else {
    species.names <- species %>% pull(id)
  }
  
  # Extract value (initialConcentration in old sbml, initialAmount in newer)
  if (!is.null(species$initialAmount)) {
    species.values <- species %>% pull(initialAmount)
  } else if (!is.null(species$initialConcentration)) {
    species.values <- species %>% pull(initialConcentration)
  } else {
    sbml.load.bug <- TRUE
  }
  
  # Extract Compartment 
  if (!is.null(species$compartment)) {
    species.comp <- species %>% pull(compartment)
  } else {
    sbml.load.bug <- TRUE
  }
  
  # Extract Boundary Condition
  if (!is.null(species$boundaryCondition)) {
    species.bounds <- species %>% pull(boundaryCondition)
  } else {
    species.bounds <- rep(FALSE, n.species)
  }
  
  # Generate Species IDs
  species.ids <- c()
  for (i in seq(n.species)) {
    # Generate Compartment IDs
    new.id <- GenerateId(id$id.var.seed, "var")
    species.ids <- c(species.ids, new.id$id)
    id$id.var.seed <- new.id$seed
    idx.to.add <- nrow(id$id.df) + 1
    id$id.df[idx.to.add, ] <- c(new.id$id, species.names[i])
  }
  
  # browser()
  species.list     <- vector("list", n.species)
  # Add additional list tags for our problem
  for (i in seq(n.species)) {
    # Build Compartment Entry
    species.list[[i]]$ID                <- species.ids[i]
    species.list[[i]]$Name              <- species.names[i]
    species.list[[i]]$Value             <- species.values[i]
    species.list[[i]]$Unit              <- units$base.units$For.Var
    species.list[[i]]$UnitDescription   <- "conc (mol)"
    species.list[[i]]$BaseUnit          <- units$base.units$For.Var
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
  TableOverrides$var.table <- TableOverrides$var.table + 1
  
  print(species.list)
  print(rv.SPECIES$species)
  print(rv.SPECIES$species.df)
  
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
  
  # Parameters are pulled from two different areas of the smbl
  # (1) <listOfParameters> as subset of <model> (not in v2 from what I can see)
  # (2) <listOfParameters> as subset of <reaction><kineticLaw>
  # I have changed the LoadSMBL function to account for this and merge them to 
  # one df: `parameters`
  
  pars <- sbml.model$parameters
  # browser()
  
  # We also want to remove all non constant parameters and store them elsewhere
  non.constant.pars <- pars %>% dplyr::filter(constant == "false")
  
  # Remove duplicates of the same parameter in the table (they come from 
  # different areas of the model but have same values)
  pars <- pars %>% dplyr::distinct(id, .keep_all = TRUE)
  pars <- pars %>% dplyr::filter(constant == "true")
  n.pars <- nrow(pars)
  
  # if parameters have name tag give them that name else use id as name
  # This check should be obselete now as loadsmbl creates a name tag if it
  # doesn't exist but we will keep it in for now. 
  if (!is.null(pars$name)) {
    par.names <- pars %>% dplyr::pull(name)
  } else {
    par.names <- pars %>% dplyr::pull(id)
  }
  
  par.vals <- pars %>% dplyr::pull(value)
  par.constant <- pars %>% dplyr::pull(constant)
  
  par.ids  <- vector("character", n.pars)
  for (i in seq(n.pars)) {
    # Generate Parameter IDs
    new.id <- GenerateId(id$id.param.seed, "parameter")
    par.ids[i] <- new.id$id
    id$id.param.seed <- new.id$seed
    
    idx.to.add <- nrow(id$id.df) + 1
    id$id.df[idx.to.add, ] <- c(new.id$id, par.names[i])
  }
  
  par.list <- vector("list", n.pars)
  # Add additional list tags for our problem
  for (i in seq(n.pars)) {
    par.list[[i]]$Name            <- par.names[i]
    par.list[[i]]$ID              <- par.ids[i]
    par.list[[i]]$Value           <- as.numeric(par.vals[i])
    par.list[[i]]$Unit            <- NA
    par.list[[i]]$UnitDescription <- NA
    par.list[[i]]$BaseUnit        <- NA
    par.list[[i]]$BaseValue       <- as.numeric(par.vals[i])
    par.list[[i]]$Description     <- ""
    par.list[[i]]$Type            <- "Loaded From SBML File"
    par.list[[i]]$Type.note       <- ""
    par.list[[i]]$ConstantValue   <- as.logical(par.constant[i])
  }
  
  names(par.list) <- par.ids
  
  # Store information to our parameter tables
  params$par.info <- par.list
  
  ## Unpack SBML Reaction ____--------------------------------------------------
  # Current Equation values used by this program
  # Values: 
  # "ID",             (1)  Specific equation ID
  # "Eqn.Type",       (2)  Type of equation (chem, enz)
  # "Law",            (3)  Law that the equation uses
  # "Species",        (4)  Species in equations
  # "Rate.Constants", (5)  Parameters in equation
  # "Compartment",    (6)  Compartment reaction occurs in
  # "Description",    (7)  Equation Description
  # "Species.Id",     (8)  IDs of species in model
  # "Parameters.Id",  (9)  IDs of parameters in model
  # "Compartment.Id"  (10) ID of compartment eqn is in
  # "Equation.Text"   (11) Text version of equation
  # "Equation.Latex"  (12) Latex text version of equation
  # "Equation.MathJax (13) Mathjax text version of equation
  
  
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
  reactions <- sbml.model$reactions
  
  # Grab Reactants and Products, split on ",", and re-collapse on space.
  # Remove spaces from split word if they exist
  # Search for ids related to reactants and products, vectorize, and store
  
  pull.reactants <- reactions %>% dplyr::pull(reactants)
  print(pull.reactants)
  reactants <- convertReactionVarsFromSBML(pull.reactants)
  
  pull.products <- reactions %>% dplyr::pull(products)
  products <- convertReactionVarsFromSBML(pull.products)
  
  pull.params <- reactions %>% dplyr::pull(parameters)
  parameters <- convertReactionVarsFromSBML(pull.params)
  
  # Grab IDs
  print(id$id.df)
  
  # for (i in seq_along(reactants)) {
  #   reactant.ids <- c(reactant.ids)
  # }
  
  print(reactants)
  reactant.ids <- FindIDReactionStructure(reactants)
  print(reactant.ids)
  
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
  waiter_hide()
})
