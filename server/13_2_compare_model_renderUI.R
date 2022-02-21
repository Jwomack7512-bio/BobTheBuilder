##############################################################################

# Compare Model

##############################################################################

#This would be the server to rendering varying UI parts for the plotting module.
#Controls if normal plot Ui is shown or multiple plots.  Changes plots around, etc. 

# 
observeEvent(params$vars.all, {
  updatePickerInput(session,
                    "compare_models_select_vars",
                    choices = params$vars.all, 
                    selected = NULL)
})
#storage for compare model values to be put into a datatable


  
compareModel <- reactiveValues(
  df = data.frame(matrix(ncol = 3,
                         nrow = 0,
                         dimnames = list(NULL, c("Variable",
                                                 "Model 1 Value",
                                                 "Model 2 Value")))),
  no.values = TRUE,
  model.1 = data.frame(),
  model.2 = data.frame()
)

observeEvent(input$compare_models_select_vars, {
  req(length(input$compare_models_select_vars) > 0)
  #check to add vars to table if they aren't in
  for (var in input$compare_models_select_vars) {
    df.2.vec <- pull(compareModel$df, "Variable")
    if (!(var %in% df.2.vec)) {
      #find vars parameter value
      idx = match(var, params$vars.all)
      value = params$vals.all[idx]
      row.to.df <- c(var, value, value)
      if (compareModel$no.values) {
        compareModel$no.values = FALSE
        compareModel$df[1,] <- row.to.df
      } else {
        compareModel$df <- rbind(compareModel$df, row.to.df)
      }
    }
  }
  #check to see if current variables are no longer in pickerinput
  df.2.vec <- pull(compareModel$df, "Variable")
  for (var in df.2.vec) {
    if (!(var %in% input$compare_models_select_vars)) {
      idx = match(var, df.2.vec)
      #remove row from RV
      compareModel$df <- compareModel$df[-idx, 1:ncol(compareModel$df)]
    }
  }
})

output$compare_models_DT <- renderDT({
  DT::datatable(compareModel$df,
                editable = list(target = "column", disable = list(columns = 0)),
                class = "cell-border stripe",
                options = list(autoWidth = TRUE,
                               pageLength = -1,
                               ordering = FALSE,
                               dom = 't',
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#4e5c68', 'color': 'white'});",
                                 "}"))
  )
})

proxy_compare_models_DT = dataTableProxy("compare_models_DT")

observeEvent(input$compare_models_DT_cell_edit, {
  info = input$compare_models_DT_cell_edit
  compareModel$df <- editData(compareModel$df, info)
  replaceData(proxy_compare_models_DT, compareModel$df, resetPaging = FALSE)
})

# ------------------------------------------------------------------------------

# Solving compared models on button press based on changes in parameters

# ------------------------------------------------------------------------------
observeEvent(input$run_compared_model, {
  # ------------------------------------------------------------------------------
  # Variables that are shared between the compared models
  # ------------------------------------------------------------------------------
  time.in <- as.numeric(input$execute_time_start)
  time.out <- as.numeric(input$execute_time_end)
  time.step <- as.numeric(input$execute_time_step)
  times <- seq(time.in, time.out, by = time.step)
  diff_eqns <- diffeq_to_text(DE$eqns, vars$species)
  rate_eqns <- rateEqns_to_text(eqns$additional.eqns)
  state <- output_ICs_for_ode_solver(vars$species ,ICs$vals)
  d_of_var <- output_var_for_ode_solver(vars$species)
  if (input$execute_turnOn_time_scale_var) {
    d_of_var = paste0(input$execute_time_scale_var, "*", d_of_var)
  }
  
  # ------------------------------------------------------------------------------
  # Variables that are changed based on table values
  # Run Models and Store them to respective RVs
  # ------------------------------------------------------------------------------
  
  # Model 1
  # Find and change parameter values
  params.to.change <- pull(compareModel$df, "Variable")
  new.values <- pull(compareModel$df, "Model.1.Value")
  #copy original param tables
  param.vars <- params$vars.all
  param.vals <- params$vals.all
  count = 1
  for (var in params.to.change) {
    # find idx matching parameter to change
    idx <- match(var, param.vars) 
    # use above index to change param value for the model
    param.vals[idx] <- new.values[count]
    count = count + 1
  }
  
  parameters <- output_param_for_ode_solver(param.vars, param.vals)
  solver <- function(t, state, parameters){
    with(as.list(c(state, parameters)), {
      eval(parse(text = rate_eqns))
      eval(parse(text = diff_eqns))
      list(eval(parse(text = d_of_var)))
    })
  }
  
  compareModel$model.1 <- ode(y = state, 
                              times = times, 
                              func = solver, 
                              parms = parameters
  )
  
  # Model 2
  # Find and change parameter values
  params.to.change <- pull(compareModel$df, "Variable")
  new.values <- pull(compareModel$df, "Model.2.Value")
  #copy original param tables
  param.vars <- params$vars.all
  param.vals <- params$vals.all
  count = 1
  for (var in params.to.change) {
    # find idx matching parameter to change
    idx <- match(var, param.vars) 
    # use above index to change param value for the model
    param.vals[idx] <- new.values[count]
    count = count + 1
  }
  
  parameters <- output_param_for_ode_solver(param.vars, param.vals)
  
  solver <- function(t, state, parameters){
    with(as.list(c(state, parameters)), {
      eval(parse(text = rate_eqns))
      eval(parse(text = diff_eqns))
      list(eval(parse(text = d_of_var)))
    })
  }
  
  compareModel$model.2 <- ode(y = state, 
                              times = times, 
                              func = solver, 
                              parms = parameters
  )
})

# ------------------------------------------------------------------------------

# Set up and execute plots for the compared models

# ------------------------------------------------------------------------------

output$LinePlot_compare1 <- renderPlot({
  print(plotLineplotInput(gatherData(compareModel$model.1)))
})

output$LinePlot_compare2 <- renderPlot({
  print(plotLineplotInput(gatherData(compareModel$model.2)))
})
