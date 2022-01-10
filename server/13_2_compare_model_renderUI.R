##############################################################################

#Compare Model

##############################################################################


#-------------------------------------------------------------------------------

#Parameters Rendered from Equations

#-------------------------------------------------------------------------------
output$compare_parameters_eqns_header <- renderUI({
  #req(input$eqnCreate_addEqnToVector)
  h4("Parameters From Equations")
})

output$compare_parameters_eqns <- renderUI({
  #req(input$eqnCreate_addEqnToVector)
  number_parameters = length(params$eqns.vars)
  
  fluidRow(column(width = 2
                  ,lapply(seq(number_parameters), function(i){
                    textInput(inputId = paste0("compare_parameter_", as.character(i))
                              ,label = params$eqns.vars[i]
                              ,value = ifelse(params$first.param.eqn.stored, params$eqns.vals[i], "0"))
                  }))
           ,column(width = 8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId = paste0("compare_parameter_description_", as.character(i))
                               ,label = "Parameter Description"
                               ,value = ifelse(params$first.param.eqn.stored, params$eqns.comments[i], ""))
                   }))
  ) #end fluidRow
})

#-------------------------------------------------------------------------------

#Parameters Rendered from Input values

#-------------------------------------------------------------------------------
output$compare_parameters_inputs_header <- renderUI({
  #req(input$Inout_addInVarToDf)
  h4("Parameters From Inputs")
})

output$compare_parameters_inputs <- renderUI({
  #req(input$Inout_addInVarToDf)
  number_parameters = length(params$inputs.vars) #find number of parameters in inputs
  
  #generate labels with paramters name to put value into
  #generate text input next to it to put comment for variable into
  #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
  fluidRow(column(width = 2
                  ,lapply(seq(number_parameters), function(i){
                    textInput(inputId = paste0("compare_parameter_input_", as.character(i))
                              ,label = params$inputs.vars[i]
                              ,value = ifelse(params$first.inputs.stored, params$inputs.vals[i], "0"))
                  }))
           ,column(width = 8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId = paste0("compare_parameter_description_input_", as.character(i))
                               ,label = "Parameter Description"
                               ,value = ifelse(params$first.inputs.stored, params$inputs.comments[i], ""))
                   }))
  ) #end fluidRow
})

#-------------------------------------------------------------------------------

#Parameters Rendered from Output values

#-------------------------------------------------------------------------------

output$compare_parameters_outputs_header <- renderUI({
  #req(input$Inout_addOutVarToDf)
  h4("Parameters From Output")
})

output$compare_parameters_outputs <- renderUI({
  #req(input$Inout_addOutVarToDf)
  number_parameters = length(params$outputs.vars) #find number of parameters in inputs
  
  #generate labels with paramters name to put value into
  #generate text input next to it to put comment for variable into
  #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
  fluidRow(column(width = 2
                  ,lapply(seq(number_parameters), function(i){
                    textInput(inputId = paste0("compare_parameter_output_", as.character(i))
                              ,label = params$outputs.vars[i]
                              ,value = ifelse(params$first.outputs.stored, params$outputs.vals[i], "0"))
                  }))
           ,column(width = 8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId = paste0("compare_parameter_description_output_", as.character(i))
                               ,label = "Parameter Description"
                               ,value = ifelse(params$first.outputs.stored, params$outputs.comments[i], ""))
                   }))
  ) #end fluidRow
})

#-------------------------------------------------------------------------------

#Parameters Rendered from Rate Equations

#-------------------------------------------------------------------------------

output$compare_parameters_rate_header <- renderUI({
  #req(input$Inout_addOutVarToDf)
  h4("Parameters From Rate")
})

output$compare_parameters_rates <- renderUI({
  #req(input$Inout_addOutVarToDf)
  number_parameters = length(params$rate.eqn.vars) #find number of parameters in inputs
  
  #generate labels with paramters name to put value into
  #generate text input next to it to put comment for variable into
  #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
  fluidRow(column(width = 4
                  ,lapply(seq(number_parameters), function(i){
                    textInput(inputId = paste0("compare_parameter_rate_", as.character(i))
                              ,label = params$rate.eqn.vars[i]
                              ,value = ifelse(params$first.rate.eqn.stored, params$rate.eqn.vals[i], "0"))
                  }))
           ,column(width = 8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId = paste0("compare_rate_description_output_", as.character(i))
                               ,label = "Parameter Description"
                               ,value = ifelse(params$first.rate.eqn.stored, params$rate.eqn.comments[i], ""))
                   }))
  ) #end fluidRow
})


#-------------------------------------------------------------------------------

#Load initial Conditions in

#-------------------------------------------------------------------------------
output$compare_ICs_UI <- renderUI({
  #req(input$createVar_addVarToList)
  number_var = length(vars$species)
  
  fluidRow(column(width = 6
                  ,lapply(seq(number_var), function(i){
                    textInput(inputId = paste0("compare_IC_", as.character(i))
                              ,label = paste(vars$species[i], "initial value:")
                              ,value = ifelse(ICs$first.IC.stored, ICs$vals[i], "0"))
                  }))
  ) #end fluidRown
})

#-------------------------------------------------------------------------------

#Run Comparison Model

#-------------------------------------------------------------------------------

compare_model_output <- eventReactive(input$compare_execute_run_model, {
  num_ICs <- length(vars$species)
  
  IC_values <- vector()
  for (i in seq(num_ICs)) {
    single_value <- eval(parse(text = paste0("input$compare_IC_", as.character(i))))
    IC_values <- append(IC_values, single_value)
  }
  observe({print("IC values:")})
  observe({print(IC_values)})
  IC_values <- paste(IC_values, sep = " ")
  #observe({print(paste0("IC values ", IC_values))})
  observe({print("IC values:")})
  observe({print(IC_values)})
  IC_values_for_model <- as.numeric(IC_values)
  observe({print("IC values:")})
  observe({print(IC_values_for_model)})

  #-------------------------------store params
  #store equation parameters
  if (length(params$eqns.vars != 0)) {
    params$first.param.eqn.stored <- TRUE #set boolean so parameter values refil when UI is rendered
    num_params <- length(params$eqns.vars)
    param_values <- vector() #create vector to store parameter values

    for (i in seq(num_params)) {
      single_value <- eval(parse(text = paste0("input$compare_parameter_", as.character(i)))) #evaluate value in textinput
      param_values <- append(param_values, single_value) #add value from textinput to vector
      
      param_values <- paste(param_values, sep = " ") #drop vector to a single string separated by spaces
      param_eqns_values_for_model <- as.numeric(param_values) #store parameter values to reactive value
    }
  }
  
  #store input parameters
  if (length(params$inputs.vars != 0)) {
    params$first.inputs.stored <- TRUE #set boolean so parameter values refil when UI is rendered
    num_params <- length(params$inputs.vars)
    param_values <- vector() #create vector to store parameter values

    for (i in seq(num_params)) {
      single_value <- eval(parse(text = paste0("input$compare_parameter_input_", as.character(i)))) #evaluate value in textinput
      param_values <- append(param_values, single_value) #add value from textinput to vector
      param_values <- paste(param_values, sep = " ") #drop vector to a single string separated by spaces
      param_inputs_values_for_model <- as.numeric(param_values) #store parameter values to reactive value
    }
  }
  
  #store output parameters
  if (length(params$outputs.vars != 0)) {
    params$first.outputs.stored <- TRUE #set boolean so parameter values refil when UI is rendered
    num_params <- length(params$outputs.vars)
    param_values <- vector() #create vector to store parameter values
    
    for (i in seq(num_params)) {
      single_value <- eval(parse(text = paste0("input$compare_parameter_output_", as.character(i)))) #evaluate value in textinput
      param_values <- append(param_values, single_value) #add value from textinput to vector
      
      param_values <- paste(param_values, sep = " ") #drop vector to a single string separated by spaces
      param_outputs_values_for_model <- as.numeric(param_values) #store parameter values to reactive value
      
    }
  }
  
  #store rate parameters
  if (length(params$rate.eqn.vars != 0)) {
    params$first.rate.eqn.stored <- TRUE #set boolean so parameter values refil when UI is rendered
    num_params <- length(params$rate.eqn.vars)
    param_values <- vector() #create vector to store parameter values
    
    for (i in seq(num_params)) {
      single_value <- eval(parse(text = paste0("input$compare_parameter_rate_", as.character(i)))) #evaluate value in textinput
      param_values <- append(param_values, single_value) #add value from textinput to vector
      
      param_values <- paste(param_values, sep = " ") #drop vector to a single string separated by spaces
      param_rates_values_for_model <- as.numeric(param_values) #store parameter values to reactive value
      
    }
  }
  

  #Store all Paramters to overall vector
  parameters_in_model = c(params$eqns.vars, params$inputs.vars, params$outputs.vars, params$rate.eqn.vars)
 
  parameter_values_for_model = c(param_eqns_values_for_model, param_inputs_values_for_model, param_outputs_values_for_model, param_rates_values_for_model)
  #run the model 
  #set up time for solver
  time_in <- as.numeric(input$execute_time_start)
  time_out <- as.numeric(input$execute_time_end)
  time_step <- as.numeric(input$execute_time_step)
  times <- seq(time_in, time_out, by = time_step)
  #initialize parameters
  parameters <- output_param_for_ode_solver(parameters_in_model, parameter_values_for_model)
  state <- output_ICs_for_ode_solver(vars$species ,IC_values_for_model)
  

  #initialize initial conditions
 
  
  #set up differential equations input string form
  diff_eqns <- diffeq_to_text(DE$eqns, vars$species)
  d_of_var <- output_var_for_ode_solver(vars$species)
  rate_eqns <- rateEqns_to_text(eqns$rate.eqns)
  
  if (input$execute_turnOn_time_scale_var) {
    d_of_var = paste0(input$execute_time_scale_var, "*", d_of_var)
  }
  
  Lorenz <- function(t, state, parameters){
    with(as.list(c(state, parameters)), {
      eval(parse(text = rate_eqns))
      eval(parse(text = diff_eqns))
      list(eval(parse(text = d_of_var)))
    })
  }
  
  #out <- ode(y=state, times=times, func=model, parms=parameters)
  out <- ode(y = state, times = times, func = Lorenz, parms = parameters)
  
  return(out)
})
