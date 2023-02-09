
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
  model.load <- readRDS(input$load_model$datapath)
  
  # Load Variables ---------------------------------------------------------------
  vars$compartments.info  <- model.load$compartments.info
  vars$compartments.df    <- model.load$compartments.df
  vars$compartment.table  <- model.load$compartment.table
  vars$compartments.names <- model.load$compartments.names
  vars$df.by.compartment  <- model.load$df.by.compartment
  vars$var.names          <- model.load$var.names
  vars$species            <- model.load$species
  vars$descriptions       <- model.load$descriptions
  vars$table              <- model.load$table
  vars$var.info           <- model.load$var.info

  # Load Equations----------------------------------------------------------------
  eqns$main             <- model.load$main
  eqns$eqn.main.latex   <- model.load$eqn.main.latex
  eqns$eqn.descriptions <- model.load$eqn.descriptions
  

  eqns$n.eqns.no.del   <- model.load$n.eqns.no.del
  eqns$n.eqns          <- model.load$n.eqns
  eqns$n.eqns.chem     <- model.load$n.eqns.chem
  eqns$n.eqns.enz      <- model.load$n.eqns.enz
  eqns$n.eqns.syn      <- model.load$n.eqns.syn
  eqns$n.eqns.deg      <- model.load$n.eqns.deg
  eqns$rate.eqns       <- model.load$rate.eqns 
  eqns$time.dep.eqns   <- model.load$time.dep.eqns 
  eqns$additional.eqns <- model.load$additional.eqns
  eqns$first.run       <- model.load$first.run
  eqns$eqn.info        <- model.load$eqn.info
  eqns$eqn.chem        <- model.load$eqn.chem
  eqns$eqn.enzyme      <- model.load$eqn.enzyme
  eqns$eqn.syn         <- model.load$eqn.syn
  eqns$eqn.deg         <- model.load$eqn.deg
  

  # Load Parameters ------------------------------------------------------------
  ic.unit <- input$GO_base_energy
  n.val <- length(model.load$vars.all)
  print(model.load$params)
  params$params <- model.load$params
  
  #load total parameters from eqns, inputs, outputs (sum of vectors)
  params$par.units.all <- model.load$par.units.all

  params$first.param.eqn.stored <- model.load$first.param.eqn.stored
  
  #load parameters from rate variables
  params$rate.eqn.vars <- model.load$rate.eqn.vars
  params$rate.eqn.vals <- model.load$rate.eqn.vals
  params$rate.eqn.comments <- model.load$rate.eqn.comments
  params$first.rate.eqn.stored <- model.load$first.rate.eqn.stored
  params$rate.params <- model.load$rate.params
  #load parameterts from time dependent equations
  params$time.dep.vars <- model.load$time.dep.vars
  params$time.dep.values <- model.load$time.dep.values
  params$time.dep.comments <- model.load$time.dep.comments
  params$first.time.dep.stored <- model.load$first.time.dep.stored
  params$parameters.based.on.other.values <-
    model.load$parameters.based.on.other.values #list of parameters used in rate equations on LHS
  

  # Load Initial Conditions ----------------------------------------------------
  #Determine if mol or mass being used
  if (input$GO_species_unit_choice == "Mol") {
    ic.unit <- units$base.units$Count
  } else {
    ic.unit <- units$base.units$Mass
  }
  n.val <- length(model.load$vals)
  #load initial condition variables
  ICs$vals      <- model.load$vals
  ICs$comments  <- model.load$comments
  ICs$units     <- model.load$units
  ICs$ICs.table <- model.load$ICs.table
  ICs$first.IC.stored <- model.load$first.IC.stored
  
  # Load Differential Equations ------------------------------------------------
  DE$eqns               <- model.load$eqns
  DE$de.eqns.for.solver <- model.load$de.eqns.for.solver
  DE$eqn.in.latex       <- model.load$eqn.in.latex
  DE$custom.diffeq.var  <- model.load$custom.diffeq.var
  DE$custom.diffeq      <- model.load$custom.diffeq
  DE$custom.diffeq.df   <- model.load$custom.diffeq.df
  
  # Load Input/Output ----------------------------------------------------------
  IO$n.IO           <- model.load$n.IO
  IO$bool.IO.exists <- model.load$bool.IO.exists
  IO$bool.IO.added  <- model.load$bool.IO.added 
  IO$IO.info        <- model.load$IO.info
  IO$n.inputs       <- model.load$n.inputs
  IO$n.outputs      <- model.load$n.outputs
  IO$bool.input.exists  <- model.load$bool.input.exists
  IO$bool.output.exists <- model.load$bool.output.exist
  IO$bool.input.added   <- model.load$bool.input.added
  IO$bool.output.added  <- model.load$bool.output.added
  IO$input.info         <- model.load$input.info
  IO$output.info        <- model.load$output.info
  IO$IO.df              <- model.load$IO.df
  IO$IO.logs            <- model.load$IO.logs

  # Load Counts ----------------------------------------------------------------
  counts$loading.model <- counts$loading.model + 1
  
  # Load Options ---------------------------------------------------------------
  options$time.start <- model.load$time.start
  options$time.end <- model.load$time.end
  options$time.step <- model.load$time.step
  options$time.scale.bool <- model.load$time.scale.bool
  options$time.scale.value <- model.load$time.scale.value
  options$ode.solver.type <- model.load$ode.solver.type
  
  # Load Results ---------------------------------------------------------------
  results$model       <- model.load$model
  results$is.pp       <- model.load$is.pp
  results$pp.eqns     <- model.load$pp.eqns
  results$pp.eqns.col <- model.load$pp.eqns.col
  results$pp.vars     <- model.load$pp.vars
  results$pp.model    <- model.load$pp.model
  results$model.final <- model.load$model.final
  results$model.has.been.solved <- model.load$model.has.been.solved
  
  # Load Logs ------------------------------------------------------------------
  logs$IO.logs     <- model.load$IO.logs
  logs$input.logs  <- model.load$input.logs
  logs$output.logs <- model.load$output.logs
  
  # Load IDs -------------------------------------------------------------------
  id$id.df <- model.load$id.df
  id$id.var.seed    <- model.load$id.var.seed
  id$id.param.seed  <- model.load$id.param.seed
  id$id.eqn.seed    <- model.load$id.eqn.seed

  # Parameter Estimation -------------------------------------------------------
  pe$loaded.species    <- model.load$loaded.species
  pe$pars              <- model.load$pars
  pe$initial.guess     <- model.load$initial.guess
  pe$lb                <- model.load$lb
  pe$ub                <- model.load$ub
  pe$calculated.values <- model.load$calculated.values
  pe$solved.model      <- model.load$solved.model
  pe$successful.run    <- model.load$successful.run
  pe$previous.values   <- model.load$previous.values
  pe$log.of.run        <- model.load$log.of.run
  
  # Load Units -----------------------------------------------------------------
  # Dont need to load types, base.units, or possible.units
  units$selected.units <- model.load$selected.units

  # Load Loop Mode RVs ---------------------------------------------------------
  loop$ICs <- ICs$ICs.table
  loop$model.results <- results$model.final
  loop$time.start <- options$time.start
  loop$time.end <- options$time.end
  loop$time.step <- options$time.step

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
  
  my.choices <- paste0(seq(eqns$n.eqns), ") ", eqns$main)
  
  updatePickerInput(session,
                    "eqnCreate_selectEqnForDescription",
                    choices = my.choices)
  
  updatePickerInput(
    session = session,
    "createVar_deleteVarPicker",
    choices = sort(names(vars$var.info))
  )
  
  updatePickerInput(session,
                    "eqnCreate_rate_firstvar",
                    choices = names(params$params))
  
  updatePickerInput(session,
                    "InOut_selectVar",
                    choices = sort(names(vars$var.info)))
  
  updatePickerInput(session,
                    "Inout_delete_IO_eqn",
                    choices = seq(IO$n.IO))

  updatePickerInput(session,
                    'eqnCreate_edit_select_equation',
                    choices = seq(length(eqns$main)))
  
  #updates output enzyme choices for enzyme degradation
  updatePickerInput(session,
                    "enzyme_deg_enzyme",
                    choices = sort(names(vars$var.info)))
  
  updatePickerInput(session,
                    "MA_species",
                    choices = sort(names(vars$var.info)))
  
  #updates output substrate choices for enzyme degradation
  updatePickerInput(session, 
                    "enzyme_deg_substrate",
                    choices = sort(names(vars$var.info)))
  
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

