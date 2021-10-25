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
  number_parameters = length(rv$param_eqns)
  
  fluidRow(column(width=2
                  ,lapply(seq(number_parameters), function(i){
                    textInput(inputId=paste0("compare_parameter_", as.character(i))
                              ,label=rv$param_eqns[i]
                              ,value = ifelse(rv$first_param_eqn_stored, rv$param_eqns_values[i], "0"))
                  }))
           ,column(width=8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId=paste0("compare_parameter_description_", as.character(i))
                               ,label="Parameter Description"
                               ,value =ifelse(rv$first_param_eqn_stored, rv$param_eqns_comments[i], ""))
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
  number_parameters = length(rv$param_inputs) #find number of parameters in inputs
  
  #generate labels with paramters name to put value into
  #generate text input next to it to put comment for variable into
  #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
  fluidRow(column(width=2
                  ,lapply(seq(number_parameters), function(i){
                    textInput(inputId=paste0("compare_parameter_input_", as.character(i))
                              ,label=rv$param_inputs[i]
                              ,value = ifelse(rv$first_param_inputs_stored, rv$param_inputs_values[i], "0"))
                  }))
           ,column(width=8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId=paste0("compare_parameter_description_input_", as.character(i))
                               ,label="Parameter Description"
                               ,value =ifelse(rv$first_param_inputs_stored, rv$param_inputs_comments[i], ""))
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
  number_parameters = length(rv$param_outputs) #find number of parameters in inputs
  
  #generate labels with paramters name to put value into
  #generate text input next to it to put comment for variable into
  #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
  fluidRow(column(width=2
                  ,lapply(seq(number_parameters), function(i){
                    textInput(inputId=paste0("compare_parameter_output_", as.character(i))
                              ,label=rv$param_outputs[i]
                              ,value = ifelse(rv$first_param_outputs_stored, rv$param_outputs_values[i], "0"))
                  }))
           ,column(width=8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId=paste0("compare_parameter_description_output_", as.character(i))
                               ,label="Parameter Description"
                               ,value =ifelse(rv$first_param_outputs_stored, rv$param_outputs_comments[i], ""))
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
  number_parameters = length(rv$param_rateEqn) #find number of parameters in inputs
  
  #generate labels with paramters name to put value into
  #generate text input next to it to put comment for variable into
  #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
  fluidRow(column(width=4
                  ,lapply(seq(number_parameters), function(i){
                    textInput(inputId=paste0("compare_parameter_rate_", as.character(i))
                              ,label=rv$param_rateEqn[i]
                              ,value = ifelse(rv$first_param_rateEqn_stored, rv$param_rateEqn_values[i], "0"))
                  }))
           ,column(width=8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId=paste0("compare_rate_description_output_", as.character(i))
                               ,label="Parameter Description"
                               ,value =ifelse(rv$first_param_rateEqn_stored, rv$param_rateEqn_comments[i], ""))
                   }))
  ) #end fluidRow
})


#-------------------------------------------------------------------------------

#Load initial Conditions in

#-------------------------------------------------------------------------------
output$compare_ICs_UI <- renderUI({
  #req(input$createVar_addVarToList)
  number_var = length(rv$vars_in_model)
  
  fluidRow(column(width=6
                  ,lapply(seq(number_var), function(i){
                    textInput(inputId=paste0("compare_IC_", as.character(i))
                              ,label=paste(rv$vars_in_model[i], "initial value:")
                              ,value = ifelse(rv$first_IC_stored, rv$IC_values[i], "0"))
                  }))
  ) #end fluidRown
})

#-------------------------------------------------------------------------------

#Run Comparison Model

#-------------------------------------------------------------------------------

compare_model_output <- eventReactive(input$compare_execute_run_model, {
  num_ICs<- length(rv$vars_in_model)
  
  IC_values <- vector()
  for(i in seq(num_ICs)){
    single_value <- eval(parse(text=paste0("input$compare_IC_", as.character(i))))
    IC_values <- append(IC_values, single_value)
  }
  observe({print("IC values:")})
  observe({print(IC_values)})
  IC_values <- paste(IC_values, sep=" ")
  #observe({print(paste0("IC values ", IC_values))})
  observe({print("IC values:")})
  observe({print(IC_values)})
  IC_values_for_model <- as.numeric(IC_values)
  observe({print("IC values:")})
  observe({print(IC_values_for_model)})

  #-------------------------------store params
  #store equation parameters
  if(length(rv$param_eqns != 0)){
    rv$first_param_eqn_stored <- TRUE #set boolean so parameter values refil when UI is rendered
    num_params <- length(rv$param_eqns)
    param_values <- vector() #create vector to store parameter values

    for(i in seq(num_params)){
      single_value <- eval(parse(text=paste0("input$compare_parameter_", as.character(i)))) #evaluate value in textinput
      param_values <- append(param_values, single_value) #add value from textinput to vector
      
      param_values <- paste(param_values, sep = " ") #drop vector to a single string separated by spaces
      param_eqns_values_for_model <- as.numeric(param_values) #store parameter values to reactive value
    }
  }
  
  #store input parameters
  if(length(rv$param_inputs != 0)){
    rv$first_param_inputs_stored <- TRUE #set boolean so parameter values refil when UI is rendered
    num_params <- length(rv$param_inputs)
    param_values <- vector() #create vector to store parameter values

    for(i in seq(num_params)){
      single_value <- eval(parse(text=paste0("input$compare_parameter_input_", as.character(i)))) #evaluate value in textinput
      param_values <- append(param_values, single_value) #add value from textinput to vector
      param_values <- paste(param_values, sep = " ") #drop vector to a single string separated by spaces
      param_inputs_values_for_model <- as.numeric(param_values) #store parameter values to reactive value
    }
  }
  
  #store output parameters
  if(length(rv$param_outputs != 0)){
    rv$first_param_outputs_stored <- TRUE #set boolean so parameter values refil when UI is rendered
    num_params <- length(rv$param_outputs)
    param_values <- vector() #create vector to store parameter values
    
    for(i in seq(num_params)){
      single_value <- eval(parse(text=paste0("input$compare_parameter_output_", as.character(i)))) #evaluate value in textinput
      param_values <- append(param_values, single_value) #add value from textinput to vector
      
      param_values <- paste(param_values, sep = " ") #drop vector to a single string separated by spaces
      param_outputs_values_for_model <- as.numeric(param_values) #store parameter values to reactive value
      
    }
  }
  
  #store rate parameters
  if(length(rv$param_rateEqn != 0)){
    rv$first_param_rateEqn_stored <- TRUE #set boolean so parameter values refil when UI is rendered
    num_params <- length(rv$param_rateEqn)
    param_values <- vector() #create vector to store parameter values
    
    for(i in seq(num_params)){
      single_value <- eval(parse(text=paste0("input$compare_parameter_rate_", as.character(i)))) #evaluate value in textinput
      param_values <- append(param_values, single_value) #add value from textinput to vector
      
      param_values <- paste(param_values, sep = " ") #drop vector to a single string separated by spaces
      param_rates_values_for_model <- as.numeric(param_values) #store parameter values to reactive value
      
    }
  }
  

  #Store all Paramters to overall vector
  parameters_in_model = c(rv$param_eqns, rv$param_inputs, rv$param_outputs, rv$param_rateEqn)
 
  parameter_values_for_model = c(param_eqns_values_for_model, param_inputs_values_for_model, param_outputs_values_for_model, param_rates_values_for_model)
  #run the model 
  #set up time for solver
  time_in <- as.numeric(input$execute_time_start)
  time_out <- as.numeric(input$execute_time_end)
  time_step <- as.numeric(input$execute_time_step)
  times <- seq(time_in, time_out, by=time_step)
  #initialize parameters
  parameters <- output_param_for_ode_solver(parameters_in_model, parameter_values_for_model)
  state <- output_ICs_for_ode_solver(rv$vars_in_model ,IC_values_for_model)
  

  #initialize initial conditions
 
  
  #set up differential equations input string form
  diff_eqns <- diffeq_to_text(rv$diffEQs, rv$vars_in_model)
  d_of_var <- output_var_for_ode_solver(rv$vars_in_model)
  rate_eqns <- rateEqns_to_text(rv$rate_eqns)
  
  if(input$execute_turnOn_time_scale_var)
  {
    d_of_var = paste0(input$execute_time_scale_var, "*", d_of_var)
  }
  
  Lorenz <- function(t, state, parameters){
    with(as.list(c(state, parameters)), {
      eval(parse(text=rate_eqns))
      eval(parse(text=diff_eqns))
      list(eval(parse(text=d_of_var)))
    })
  }
  
  #out <- ode(y=state, times=times, func=model, parms=parameters)
  out <- ode(y=state, times=times, func = Lorenz, parms = parameters)
  
  return(out)
})
