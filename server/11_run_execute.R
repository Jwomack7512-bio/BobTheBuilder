############################# Run Model Server ###############################
#add options for ode server

#add table views for equations, parameters, and ICs
output$execute_equations_show <- renderText({
  paste(DE$eqns, collapse = "<br>")
})

output$execute_parameters_show <- renderText({
  paste(params$vals.all, collapse = "<br>")
})

output$execute_ICS_show <- renderText({
  paste(ICs$vals, collapse = "<br>")
})

# Store relevant model information to be saved for future runs
observeEvent(input$execute_run_model, {
  options$time.start <- input$execute_time_start
  options$time.end <- input$execute_time_end
  options$time.step <- input$execute_time_step
  options$time.scale.bool <- input$execute_turnOn_time_scale_var
  options$time.scale.value <- input$execute_time_scale_var
  options$ode.solver.type <- input$execute_ode_solver_type
})


#need to create an event reactive here that stores the model that is being run
model_output <- eventReactive(input$execute_run_model, {
  #set up time for solver
  time_in <- as.numeric(input$execute_time_start)
  time_out <- as.numeric(input$execute_time_end)
  time_step <- as.numeric(input$execute_time_step)

  times <- seq(time_in, time_out, by = time_step)

  #initialize parameters
  parameters <- output_param_for_ode_solver(params$vars.all,
                                            params$vals.all)
  #initialize initial conditions
  state <- output_ICs_for_ode_solver(vars$species ,ICs$vals)

  #set up differential equations input string form
  diff_eqns <- diffeq_to_text(DE$eqns, vars$species)

  d_of_var <- output_var_for_ode_solver(vars$species)
  
  rate_eqns <- rateEqns_to_text(eqns$additional.eqns)

  if (input$execute_turnOn_time_scale_var) {
    d_of_var = paste0(input$execute_time_scale_var, "*", d_of_var)
  }
  # jPrint("Printing execution parameters")
  # jPrint(state)
  # jPrint(parameters)
  # jPrint(diff_eqns)
  # jPrint(d_of_var)
  # jPrint(eqns$additional.eqns)
  # jPrint(rate_eqns)

  Lorenz <- function(t, state, parameters){
    with(as.list(c(state, parameters)), {
      eval(parse(text = rate_eqns))
      eval(parse(text = diff_eqns))
      list(eval(parse(text = d_of_var)))
    })
  }
  jPrint("Before ode solver")
  #out <- ode(y=state, times=times, func=model, parms=parameters)
  out <- ode(y = state, 
             times = times, 
             func = Lorenz, 
             parms = parameters
             #,method = input$execute_ode_solver_type
             )
  
  jPrint("After ode solver")

  
  results$model <- out #store model to reactive var
  results$model.has.been.solved <- TRUE
  #this is meant to prepare a previous version of save file that didn't have
  #these properly done
  if (is.null(results$is.pp)) results$is.pp = FALSE
  if (is.null(results$pp.eqns)) results$pp.eqns = vector()
  if (is.null(results$pp.vars)) results$pp.vars = vector()
  if (is.null(results$pp.model)) results$pp.model = data.frame()
  if (is.null(results$pp.eqns.col)) results$pp.eqns.col = vector()
  jPrint("All this if statements")
  jPrint(head(out))
  return(out)
})

#hook up table to result of event reactive above
# output$execute_table_for_model <- renderDT({
#   
#   rhandsontable(model_output(),
#                 readOnly = TRUE, 
#                 contextMenu = FALSE,
#                 maxRoxs = 10)
# })

output$download_model_results <- downloadHandler(
  filename = function(){"model_results.csv"},
  content = function(con){
    write.csv(results$model.final, con, row.names = FALSE)
  }
)

output$execute_table_for_model <- DT::renderDataTable({
  req(results$model.has.been.solved)
  m <- results$model.final
  rounded.model <- round(m[1:nrow(m), 1:ncol(m)], digits = 3)
  DT::datatable(rounded.model,
                options = list(autoWidth = TRUE,
                               ordering = FALSE,
                               dom = "ltipr",
                               lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                               pageLength = -1) 
                )
  
})

