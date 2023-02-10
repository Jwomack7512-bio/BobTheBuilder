############################# Run Model Server ###############################

# Waiters ----------------------------------------------------------------------
# create our watier
w_execute <- Waiter$new(id = "box3")

# Functions --------------------------------------------------------------------
ModelFxn <- function(t, 
                     state, 
                     parameters,
                     extraEqns,
                     differentialEqns,
                     vars){
  with(as.list(c(state, parameters)), {
    eval(parse(text = extraEqns))
    eval(parse(text = differentialEqns))
    list(eval(parse(text = vars)))
  })
}

# Update UI, Renders, Animations, etc... ---------------------------------------

observeEvent(input$execute_time_unit, {
  # Store Time Unit RV
  units$selected.units$Duration <- input$execute_time_unit
  
})

observeEvent(units$selected.units$Duration, {
  
  if (units$selected.units$Duration != input$execute_time_unit) {
    updatePickerInput(
      session = session,
      "execute_time_unit",
      selected = units$selected.units$Duration
    )
  }
  
  if (units$selected.units$Duration != input$GO_base_duration) {
    updatePickerInput(
      session = session,
      "GO_base_duration",
      selected = units$selected.units$Duration
    )
  }

})

# Store Model Options ----------------------------------------------------------
observeEvent(input$execute_run_model, {
  options$time.start       <- input$execute_time_start
  options$time.end         <- input$execute_time_end
  options$time.step        <- input$execute_time_step
  options$time.scale.bool  <- input$execute_turnOn_time_scale_var
  options$time.scale.value <- input$execute_time_scale_var
  options$ode.solver.type  <- input$execute_ode_solver_type
})


# Event: Solve Model -----------------------------------------------------------
model_output <- eventReactive(input$execute_run_model, {
  
  # Error Checks for button
  w_execute$show()
  # browser()
  #set up time for solver
  error.found   <- FALSE
  error.message <- "Model failed to solve. "
  error.time    <- FALSE

  time_in   <- as.numeric(input$execute_time_start)
  time_out  <- as.numeric(input$execute_time_end)
  time_step <- as.numeric(input$execute_time_step)
  
  # Error Checking Time Values
  if (is.na(time_in) | is.na(time_out) | is.na(time_step)) {
    error.found <- TRUE
    error.time <- TRUE
    error.message <- paste0(error.message, 
                            "Time values are not numerical values. ")
  } else if (time_out < time_in) {
    error.found <- TRUE
    error.time <- TRUE
    error.message <- paste0(error.message, 
                            "Time out is a lower value than time in. ")
  }
  if (!error.time) {
    converted.time <- FALSE
    times <- seq(time_in, time_out, by = time_step)
    print(units$selected.units)
    selected.time.unit <- units$selected.units$Duration
    print(units$selected.units$Duration)
    print(selected.time.unit)
    results$time.units <- selected.time.unit
    base.time.unit <- units$base.units$Duration
    if (selected.time.unit != base.time.unit) {
      converted.time <- TRUE
      # Convert it with same number of steps
      conv.time.in <- UnitConversion("time",
                                     selected.time.unit,
                                     base.time.unit,
                                     time_in)
      conv.time.out <- UnitConversion("time",
                                      selected.time.unit,
                                      base.time.unit,
                                      time_out)
      time.breaks <- length(times)
      times <- seq(conv.time.in, conv.time.out, length.out = time.breaks)
    }
    if (length(times) < 10) {
      error.found <- TRUE
      error.message <- paste0(error.message, 
                              "Step size not small enough. 
                              Must have at least 10 units of time. ")
    }
  }
  
  # Preping Terms for ODE Solver
  #initialize parameters
  parameters <- output_param_for_ode_solver(params$params)
  
  #initialize initial conditions
  state <- output_ICs_for_ode_solver(vars$var.info)

  #set up differential equations input string form
  diff_eqns <- diffeq_to_text(DE$de.eqns.for.solver, names(vars$var.info))

  d_of_var <- output_var_for_ode_solver(names(vars$var.info))
  
  rate_eqns <- rateEqns_to_text(eqns$additional.eqns)

  if (input$execute_turnOn_time_scale_var) {
    d_of_var = paste0(input$execute_time_scale_var, "*", d_of_var)
  }

  print("Before Solver")
  print(DE$de.eqns.for.solver)
  print(DE$eqns)
  print(DE$eqns.in.latex)
  print("into solver")
  print(parameters)
  print(state)
  print(diff_eqns)
  print(d_of_var)
  
  # Solve ODEs
  jPrint("Before ode solver")

  out <- ode(y = state, 
             times = times, 
             func =   ModelFxn, 
             parms = parameters,
             extraEqns = rate_eqns,
             differentialEqns = diff_eqns,
             vars = d_of_var
             #,method = input$execute_ode_solver_type
  )
  
  jPrint("After ode solver")
  if (converted.time) {
    result.time <- out[,1]
    conv.time.in <- UnitConversion("time",
                                   base.time.unit,
                                   selected.time.unit,
                                   result.time)
    out[,1] <- conv.time.in
  }

  
  # Save Results to Appropriate Places
  results$model <- out #store model to reactive var
  results$model.has.been.solved <- TRUE
  # Initialize other plotting modes with this model
  # loop$model.results <- out
  # compareModel$model.1 <- out
  # compareModel$model.2 <- out
  # compareModel$model.3 <- out
  # compareModel$model.4 <- out
  #this is meant to prepare a previous version of save file that didn't have
  #these properly done
  if (is.null(results$is.pp)) results$is.pp = FALSE
  if (is.null(results$pp.eqns)) results$pp.eqns = vector()
  if (is.null(results$pp.vars)) results$pp.vars = vector()
  if (is.null(results$pp.model)) results$pp.model = data.frame()
  if (is.null(results$pp.eqns.col)) results$pp.eqns.col = vector()
  
  w_execute$hide()
  
  return(out)
})

# Download Table of Model Results ----------------------------------------------
output$download_model_results <- downloadHandler(
  filename = function(){"model_results.csv"},
  content = function(con){
    write.csv(results$model.final, con, row.names = FALSE)
  }
)

# Results Table Render ---------------------------------------------------------
output$execute_table_for_model <- DT::renderDataTable({
  req(results$model.has.been.solved)
  m <- results$model.final
  rounded.model <- round(m[1:nrow(m), 1:ncol(m)], digits = 3)
  time.w.units <- paste0("time (", results$time.units, ")")
  # time.w.units <- "time (min)"
  colnames(rounded.model)[1] <- time.w.units
  DT::datatable(rounded.model,
                options = list(autoWidth = TRUE,
                               ordering = FALSE,
                               dom = "ltipr",
                               lengthMenu = list(c(5, 15, -1),
                                                 c('5', '15', 'All')
                                                 ),
                               pageLength = -1)
                )
})
