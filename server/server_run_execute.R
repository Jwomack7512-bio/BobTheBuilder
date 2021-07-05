############################# Run Model Server ###############################
#add options for ode server

source("./server/helper_diffeqs_to_text.R") #load functions to solve differential equations


#add table views for equations, parameters, and ICs
output$execute_equations_show <- renderText({
  paste(rv$diffEQs, collapse="<br>")
})

output$execute_parameters_show <- renderText({
  paste(rv$parameter_values, collapse="<br>")
})

output$execute_ICS_show <- renderText({
  paste(rv$IC_values, collapse="<br>")
})



#need to create an event reactive here that stores the model that is being run
model_output <- eventReactive(input$execute_run_model, {
  
  #set up time for solver
  time_in <- as.numeric(input$execute_time_start)
  time_out <- as.numeric(input$execute_time_end)
  time_step <- as.numeric(input$execute_time_step)
  times <- seq(time_in, time_out, by=time_step)
  
  #initialize parameters
  parameters <- output_param_for_ode_solver(rv$parameters_in_model, rv$parameter_values)
  
  #initialize initial conditions
  state <- output_ICs_for_ode_solver(rv$vars_in_model ,rv$IC_values)
  
  #set up differential equations input string form
  diff_eqns <- diffeq_to_text(rv$diffEQs, rv$vars_in_model)
  d_of_var <- output_var_for_ode_solver(rv$vars_in_model)
  rate_eqns <- rateEqns_to_text(rv$rate_eqns)
  
  if(input$execute_turnOn_time_scale_var)
  {
    d_of_var = paste0(input$execute_time_scale_var, "*", d_of_var)
  }
  observe({
    print(state)
    print(length(state))
    print(typeof(state))
    print(parameters)
    print(length(parameters))
    print(typeof(parameters))
    print(diff_eqns)
    print(length(diff_eqns))
    print(typeof(diff_eqns))
    print(d_of_var)
    print(length(d_of_var))
    print(typeof(d_of_var))
    print(rv$rate_eqns)
    print(rate_eqns)
  })
  
  Lorenz <- function(t, state, parameters){
    with(as.list(c(state, parameters)), {
      eval(parse(text=rate_eqns))
      eval(parse(text=diff_eqns))
      list(eval(parse(text=d_of_var)))
    })
  }
  observe({})
  #out <- ode(y=state, times=times, func=model, parms=parameters)
  out <- ode(y=state, times=times, func = Lorenz, parms = parameters, method = input$execute_ode_solver_type)
  
  return(out)
})

#hook up table to result of event reactive above
output$execute_table_for_model <- renderRHandsontable({
  
  rhandsontable(model_output(),
                readOnly=TRUE, 
                contextMenu = FALSE)
})

