############################# Run Model Server ###############################
#add options for ode server

source("./server/helper_diffeqs_to_text.R") #load functions to solve differential equations


#add table views for equations, parameters, and ICs
output$execute_equations_show <- renderText({
  paste(DE$eqns, collapse = "<br>")
})

output$execute_parameters_show <- renderText({
  paste(parameters$vals.all, collapse = "<br>")
})

output$execute_ICS_show <- renderText({
  paste(ICs$vals, collapse = "<br>")
})



#need to create an event reactive here that stores the model that is being run
model_output <- eventReactive(input$execute_run_model, {
  observe({print("begin")})
  
  #set up time for solver
  time_in <- as.numeric(input$execute_time_start)
  time_out <- as.numeric(input$execute_time_end)
  time_step <- as.numeric(input$execute_time_step)
  observe({print("times")})
  
  times <- seq(time_in, time_out, by = time_step)
  
  #initialize parameters
  observe({print("0")})
  
  parameters <- output_param_for_ode_solver(parameters$vars.all,
                                            parameters$vals.all)
  observe({print("1")})
  #initialize initial conditions
  state <- output_ICs_for_ode_solver(vars$species ,ICs$vals)
  observe({print("2")})
  
  #set up differential equations input string form
  diff_eqns <- diffeq_to_text(DE$eqns, vars$species)
  observe({print("3")})
  
  d_of_var <- output_var_for_ode_solver(vars$species)
  observe({print("4")})
  
  rate_eqns <- rateEqns_to_text(eqns$additional.eqns)
  observe({print("5")})
  
  
  if (input$execute_turnOn_time_scale_var) {
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
    print(eqns$additional.eqns)
    print(rate_eqns)
  })
  
  Lorenz <- function(t, state, parameters){
    with(as.list(c(state, parameters)), {
      eval(parse(text = rate_eqns))
      eval(parse(text = diff_eqns))
      list(eval(parse(text = d_of_var)))
    })
  }
  #out <- ode(y=state, times=times, func=model, parms=parameters)
  out <- ode(y = state, times = times, func = Lorenz, parms = parameters, 
             method = input$execute_ode_solver_type)
  
  observe({print("out")})
  results$model <- out #store model to reactive var
  observe({print("results$model")})
  #this is meant to prepare a previous version of save file that didn't have
  #these properly done
  if (is.null(results$is.pp)) results$is.pp = FALSE
  if (is.null(results$pp.eqns)) results$pp.eqns = vector()
  if (is.null(results$pp.vars)) results$pp.vars = vector()
  if (is.null(results$pp.model)) results$pp.model = data.frame()
  if (is.null(results$pp.eqns.col)) results$pp.eqns.col = vector()
  observe({print("All this if statements")})
  #observe({head(out)})
  return(out)
})

#hook up table to result of event reactive above
output$execute_table_for_model <- renderRHandsontable({
  
  rhandsontable(model_output(),
                readOnly = TRUE, 
                contextMenu = FALSE)
})

