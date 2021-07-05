##############################################################################

#LooP

##############################################################################

#---------------------------- P A R A M E T E R S ------------------------------


#------------------------------------------------
#Parameters Rendered from Equations
#------------------------------------------------
output$loop_parameters_eqns_header <- renderUI({
  req(input$eqnCreate_addEqnToVector)
  h4("Parameters From Equations")
})

output$loop_parameters_eqns <- renderUI({
  req(input$eqnCreate_addEqnToVector)
  number_parameters = length(rv$param_eqns)
  
  fluidRow(column(width=2
                  ,lapply(seq(number_parameters), function(i){
                    textInput(inputId=paste0("loop_parameter_", as.character(i))
                              ,label=rv$param_eqns[i]
                              ,value = ifelse(rv$first_param_eqn_stored, rv$param_eqns_values[i], "0"))
                  }))
           ,column(width=8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId=paste0("loop_parameter_description_", as.character(i))
                               ,label="Parameter Description"
                               ,value =ifelse(rv$first_param_eqn_stored, rv$param_eqns_comments[i], ""))
                   }))
  ) #end fluidRow
})

#------------------------------------------------
#Parameters Rendered from Input values
#------------------------------------------------
output$loop_parameters_inputs_header <- renderUI({
  req(input$Inout_addInVarToDf)
  h4("Parameters From Inputs")
})

output$loop_parameters_inputs <- renderUI({
  req(input$Inout_addInVarToDf)
  number_parameters = length(rv$param_inputs) #find number of parameters in inputs
  
  #generate labels with paramters name to put value into
  #generate text input next to it to put comment for variable into
  #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
  fluidRow(column(width=2
                  ,lapply(seq(number_parameters), function(i){
                    textInput(inputId=paste0("loop_parameter_input_", as.character(i))
                              ,label=rv$param_inputs[i]
                              ,value = ifelse(rv$first_param_inputs_stored, rv$param_inputs_values[i], "0"))
                  }))
           ,column(width=8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId=paste0("loop_parameter_description_input_", as.character(i))
                               ,label="Parameter Description"
                               ,value =ifelse(rv$first_param_inputs_stored, rv$param_inputs_comments[i], ""))
                   }))
  ) #end fluidRow
})

#------------------------------------------------
#Parameters Rendered from Output values
#------------------------------------------------
output$loop_parameters_outputs_header <- renderUI({
  req(input$Inout_addOutVarToDf)
  h4("Parameters From Output")
})

output$loop_parameters_outputs <- renderUI({
  req(input$Inout_addOutVarToDf)
  number_parameters = length(rv$param_outputs) #find number of parameters in inputs
  
  #generate labels with paramters name to put value into
  #generate text input next to it to put comment for variable into
  #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
  fluidRow(column(width=2
                  ,lapply(seq(number_parameters), function(i){
                    textInput(inputId=paste0("loop_parameter_output_", as.character(i))
                              ,label=rv$param_outputs[i]
                              ,value = ifelse(rv$first_param_outputs_stored, rv$param_outputs_values[i], "0"))
                  }))
           ,column(width=8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId=paste0("loop_parameter_description_output_", as.character(i))
                               ,label="Parameter Description"
                               ,value =ifelse(rv$first_param_outputs_stored, rv$param_outputs_comments[i], ""))
                   }))
  ) #end fluidRow
})

################################################################################

#Load initial Conditions in

################################################################################
output$loop_ICs_UI <- renderUI({
  req(input$createVar_addVarToList)
  number_var = length(rv$vars_in_model)
  
  
  fluidRow(column(width=2
                  ,lapply(seq(number_var), function(i){
                    textInput(inputId=paste0("loop_IC_", as.character(i))
                              ,label=paste(rv$vars_in_model[i], "initial value:")
                              ,value = ifelse(rv$first_IC_stored, rv$IC_values[i], "0"))
                  }))
           # ,column(width=1
           #         ,lapply(seq(number_parameters), function(i){
           #           checkboxInput(inputId=paste0("parameter_check_unknown_", as.character(i))
           #                     ,label="Value Unknown"
           #                     ,value = FALSE)
           #         }))
           # ,column(width=8
           #         ,lapply(seq(number_var), function(i){
           #           textInput(inputId=paste0("loop_ICs_description_", as.character(i))
           #                     ,label="Comment"
           #                     ,value = ifelse(rv$first_IC_stored, rv$IC_descriptions[i], ""))
           #         })
           # )
  ) #end fluidRown
  
})

loop_model_output <- eventReactive(input$loop_execute_run_model, {
  num_ICs<- length(rv$vars_in_model)
  
  IC_values <- vector()
  IC_comments <- vector()
  for(i in seq(num_ICs)){
    single_value <- eval(parse(text=paste0("input$loop_IC_", as.character(i))))
    IC_values <- append(IC_values, single_value)
    
    single_comment <- eval(parse(text=paste0("input$loop_ICs_description_", as.character(i)))) #evaluate value in textinput
    IC_comments <- append(IC_comments, single_comment) #append comments to vector
  }
  IC_values <- paste(IC_values, sep=" ")
  
  rv$IC_values <- as.numeric(IC_values)
  rv$IC_descriptions <- IC_comments
  
  #-------------------------------store params
  #store equation parameters
  if(length(rv$param_eqns != 0)){
    rv$first_param_eqn_stored <- TRUE #set boolean so parameter values refil when UI is rendered
    num_params <- length(rv$param_eqns)
    param_values <- vector() #create vector to store parameter values
    param_comments <- vector() #create vector to store parameter commemts
    
    for(i in seq(num_params)){
      single_value <- eval(parse(text=paste0("input$loop_parameter_", as.character(i)))) #evaluate value in textinput
      param_values <- append(param_values, single_value) #add value from textinput to vector
      
      single_comment <- eval(parse(text=paste0("input$loop_parameter_description_", as.character(i)))) #evaluate value in textinput
      param_comments <- append(param_comments, single_comment) #append comments to vector
      param_values <- paste(param_values, sep = " ") #drop vector to a single string separated by spaces
      rv$param_eqns_values <- as.numeric(param_values) #store parameter values to reactive value
      rv$param_eqns_comments <- param_comments # store paramter comments to reactive value
    }
  }
  
  #store input parameters
  if(length(rv$param_inputs != 0)){
    rv$first_param_inputs_stored <- TRUE #set boolean so parameter values refil when UI is rendered
    num_params <- length(rv$param_inputs)
    param_values <- vector() #create vector to store parameter values
    param_comments <- vector() #create vector to store parameter commemts
    
    for(i in seq(num_params)){
      single_value <- eval(parse(text=paste0("input$loop_parameter_input_", as.character(i)))) #evaluate value in textinput
      param_values <- append(param_values, single_value) #add value from textinput to vector
      
      single_comment <- eval(parse(text=paste0("input$loop_parameter_description_input_", as.character(i)))) #evaluate value in textinput
      param_comments <- append(param_comments, single_comment) #append comments to vector
      param_values <- paste(param_values, sep = " ") #drop vector to a single string separated by spaces
      rv$param_inputs_values <- as.numeric(param_values) #store parameter values to reactive value
      rv$param_inputs_comments <- param_comments # store paramter comments to reactive value
    }
  }
  
  #store output parameters
  if(length(rv$param_outputs != 0)){
    rv$first_param_outputs_stored <- TRUE #set boolean so parameter values refil when UI is rendered
    num_params <- length(rv$param_outputs)
    param_values <- vector() #create vector to store parameter values
    param_comments <- vector() #create vector to store parameter commemts
    
    for(i in seq(num_params)){
      single_value <- eval(parse(text=paste0("input$loop_parameter_output_", as.character(i)))) #evaluate value in textinput
      param_values <- append(param_values, single_value) #add value from textinput to vector
      
      single_comment <- eval(parse(text=paste0("input$loop_parameter_description_output_", as.character(i)))) #evaluate value in textinput
      param_comments <- append(param_comments, single_comment) #append comments to vector
      param_values <- paste(param_values, sep = " ") #drop vector to a single string separated by spaces
      rv$param_outputs_values <- as.numeric(param_values) #store parameter values to reactive value
      rv$param_outputs_comments <- param_comments # store paramter comments to reactive value
    }
  }
  
  #Store all Paramters to overall vector
  rv$parameters_in_model = c(rv$param_eqns, rv$param_inputs, rv$param_outputs)
  rv$parameter_values = c(rv$param_eqns_values, rv$param_inputs_values, rv$param_outputs_values)
  rv$parameter_descriptions = c(rv$param_eqns_comments, rv$param_inputs_comments, rv$param_outputs_comments)
  
  #run the model 
  #set up time for solver
  time_in <- as.numeric(input$loop_execute_time_start)
  time_out <- as.numeric(input$loop_execute_time_end)
  time_step <- as.numeric(input$loop_execute_time_step)
  times <- seq(time_in, time_out, by=time_step)
  
  #initialize parameters
  parameters <- output_param_for_ode_solver(rv$parameters_in_model, rv$parameter_values)
  
  #initialize initial conditions
  state <- output_ICs_for_ode_solver(rv$vars_in_model ,rv$IC_values)
  
  #set up differential equations input string form
  diff_eqns <- diffeq_to_text(rv$diffEQs, rv$vars_in_model)
  d_of_var <- output_var_for_ode_solver(rv$vars_in_model)
  
  Lorenz <- function(t, state, parameters){
    with(as.list(c(state, parameters)), {
      eval(parse(text=diff_eqns))
      list(eval(parse(text=d_of_var)))
    })
  }
  
  #out <- ode(y=state, times=times, func=model, parms=parameters)
  out <- ode(y=state, times=times, func = Lorenz, parms = parameters)
  
  return(out)
})

#updates filter_2 variable choices based on items selected in checkbox selct boxes
observeEvent(input$loop_execute_run_model, {
  updateSelectInput(session
                    ,"lineplot_xvar"
                    ,choices = colnames(loop_model_output())[1])
})

#updates filter_2 variable choices based on items selected in checkbox selct boxes
observeEvent(input$loop_execute_run_model, {
  updatePickerInput(session,
                    "lineplot_yvar"
                    ,choices  = colnames(loop_model_output())[2:ncol(loop_model_output())]
                    ,selected = colnames(loop_model_output())[2:ncol(loop_model_output())]
  )
})