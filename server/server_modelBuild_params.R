########################### Paramaeter Server ################################
#add title line to parameters from equations (since I don't seem to be able to do it with the renderUi below)

#------------------------------------------------
#Parameters Rendered from Equations
#------------------------------------------------
output$parameters_eqns_header <- renderUI({
  req(input$eqnCreate_addEqnToVector)
  h4("Parameters From Equations")
})

output$parameters_eqns <- renderUI({
  req(input$eqnCreate_addEqnToVector)
  number_parameters = length(rv$param_eqns)
  
  fluidRow(column(width=2
                  ,lapply(seq(number_parameters), function(i){
                    textInput(inputId=paste0("parameter_", as.character(i))
                              ,label=rv$param_eqns[i]
                              ,value = ifelse(rv$first_param_eqn_stored, rv$param_eqns_values[i], "0"))
                  }))
           ,column(width=8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId=paste0("parameter_description_", as.character(i))
                               ,label="Parameter Description"
                               ,value =ifelse(rv$first_param_eqn_stored, rv$param_eqns_comments[i], ""))
                   }))
  ) #end fluidRow
})

#------------------------------------------------
#Parameters Rendered from Input values
#------------------------------------------------
output$parameters_inputs_header <- renderUI({
  req(input$Inout_addInVarToDf)
  h4("Parameters From Inputs")
})

output$parameters_inputs <- renderUI({
  req(input$Inout_addInVarToDf)
  number_parameters = length(rv$param_inputs) #find number of parameters in inputs
  
  #generate labels with paramters name to put value into
  #generate text input next to it to put comment for variable into
  #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
  fluidRow(column(width=2
                  ,lapply(seq(number_parameters), function(i){
                    textInput(inputId=paste0("parameter_input_", as.character(i))
                              ,label=rv$param_inputs[i]
                              ,value = ifelse(rv$first_param_inputs_stored, rv$param_inputs_values[i], "0"))
                  }))
           ,column(width=8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId=paste0("parameter_description_input_", as.character(i))
                               ,label="Parameter Description"
                               ,value =ifelse(rv$first_param_inputs_stored, rv$param_inputs_comments[i], ""))
                   }))
  ) #end fluidRow
})

#------------------------------------------------
#Parameters Rendered from Output values
#------------------------------------------------
output$parameters_outputs_header <- renderUI({
  req(input$Inout_addOutVarToDf)
  h4("Parameters From Output")
})

output$parameters_outputs <- renderUI({
  req(input$Inout_addOutVarToDf)
  number_parameters = length(rv$param_outputs) #find number of parameters in inputs
  
  #generate labels with paramters name to put value into
  #generate text input next to it to put comment for variable into
  #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
  fluidRow(column(width=2
                  ,lapply(seq(number_parameters), function(i){
                    textInput(inputId=paste0("parameter_output_", as.character(i))
                              ,label=rv$param_outputs[i]
                              ,value = ifelse(rv$first_param_outputs_stored, rv$param_outputs_values[i], "0"))
                  }))
           ,column(width=8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId=paste0("parameter_description_output_", as.character(i))
                               ,label="Parameter Description"
                               ,value =ifelse(rv$first_param_outputs_stored, rv$param_outputs_comments[i], ""))
                   }))
  ) #end fluidRow
})

#------------------------------------------------
#Parameters Rendered from RateEqn Values
#------------------------------------------------
output$parameters_rateEqns_header <- renderUI({
  req(input$eqnCreate_rate_store_new_parameter)
  h4("Parameters From Rate Equation")
})

output$parameters_rateEqns <- renderUI({
  req(input$eqnCreate_rate_store_new_parameter)
  number_parameters = length(rv$param_rateEqn) #find number of parameters in inputs
  
  #generate labels with paramters name to put value into
  #generate text input next to it to put comment for variable into
  #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
  fluidRow(column(width=2
                  ,lapply(seq(number_parameters), function(i){
                    textInput(inputId=paste0("parameter_rateEqn_", as.character(i))
                              ,label=rv$param_rateEqn[i]
                              ,value = ifelse(rv$first_param_rateEqn_stored, rv$param_rateEqn_values[i], "0"))
                  }))
           ,column(width=8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId=paste0("parameter_description_rateEqn_", as.character(i))
                               ,label="Parameter Description"
                               ,value =ifelse(rv$first_param_rateEqn_stored, rv$param_rateEqn_comments[i], ""))
                   }))
  ) #end fluidRow
})

#-------------------------------------------------------------------------------

#Storing Parameter info to proper vectors for data analysis

#-------------------------------------------------------------------------------
observeEvent(input$param_store_parameters, {
  #store equation parameters
  if(length(rv$param_eqns != 0)){
    rv$first_param_eqn_stored <- TRUE #set boolean so parameter values refil when UI is rendered
    num_params <- length(rv$param_eqns)
    param_values <- vector() #create vector to store parameter values
    param_comments <- vector() #create vector to store parameter commemts
    
    for(i in seq(num_params)){
      single_value <- eval(parse(text=paste0("input$parameter_", as.character(i)))) #evaluate value in textinput
      param_values <- append(param_values, single_value) #add value from textinput to vector
      
      single_comment <- eval(parse(text=paste0("input$parameter_description_", as.character(i)))) #evaluate value in textinput
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
      single_value <- eval(parse(text=paste0("input$parameter_input_", as.character(i)))) #evaluate value in textinput
      param_values <- append(param_values, single_value) #add value from textinput to vector
      
      single_comment <- eval(parse(text=paste0("input$parameter_description_input_", as.character(i)))) #evaluate value in textinput
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
      single_value <- eval(parse(text=paste0("input$parameter_output_", as.character(i)))) #evaluate value in textinput
      param_values <- append(param_values, single_value) #add value from textinput to vector
      
      single_comment <- eval(parse(text=paste0("input$parameter_description_output_", as.character(i)))) #evaluate value in textinput
      param_comments <- append(param_comments, single_comment) #append comments to vector
      param_values <- paste(param_values, sep = " ") #drop vector to a single string separated by spaces
      rv$param_outputs_values <- as.numeric(param_values) #store parameter values to reactive value
      rv$param_outputs_comments <- param_comments # store paramter comments to reactive value
    }
  }
  
  #store rate equation parameters
  if(length(rv$param_rateEqn != 0)){
    rv$first_param_rateEqn_stored <- TRUE #set boolean so parameter values refil when UI is rendered
    num_params <- length(rv$param_rateEqn)
    param_values <- vector() #create vector to store parameter values
    param_comments <- vector() #create vector to store parameter commemts
    
    for(i in seq(num_params)){
      single_value <- eval(parse(text=paste0("input$parameter_rateEqn_", as.character(i)))) #evaluate value in textinput
      param_values <- append(param_values, single_value) #add value from textinput to vector
      
      single_comment <- eval(parse(text=paste0("input$parameter_description_rateEqn_", as.character(i)))) #evaluate value in textinput
      param_comments <- append(param_comments, single_comment) #append comments to vector
      param_values <- paste(param_values, sep = " ") #drop vector to a single string separated by spaces
      rv$param_rateEqn_values <- as.numeric(param_values) #store parameter values to reactive value
      rv$param_rateEqn_comments <- param_comments # store paramter comments to reactive value
    }
  }
  
  #Store all Paramters to overall vector
  rv$parameters_in_model = c(rv$param_eqns, rv$param_inputs, rv$param_outputs, rv$param_rateEqn)
  rv$parameter_values = c(rv$param_eqns_values, rv$param_inputs_values, rv$param_outputs_values, rv$param_rateEqn_values)
  rv$parameter_descriptions = c(rv$param_eqns_comments, rv$param_inputs_comments, rv$param_outputs_comments, rv$param_rateEqn_comments)
  
  # observe({
  #   print(rv$param_inputs_values)
  #   print(rv$param_inputs_comments)
  #   print(rv$parameters_in_model)
  #   print(rv$parameter_values)
  #   print(rv$parameter_descriptions)
  # })
  
  session$sendCustomMessage(type = 'testmessage',
                            message = 'Parameters Stored')
})



observeEvent(input$param_view_parameters, {
  observe({print(rv$parameters_in_model)})
  observe({print(paste(length(rv$parameters_in_model), length(rv$parameter_values), length(rv$parameter_descriptions)))})
  observe({print(paste(length(rv$param_eqns), length(rv$param_eqns_values), length(rv$param_eqns_comments)))})
  observe({print(paste(length(rv$param_inputs), length(rv$param_inputs_values), length(rv$param_inputs_comments)))})
  observe({print(paste(length(rv$param_outputs), length(rv$param_outputs_values), length(rv$param_outputs_comments)))})
  observe({print(paste(length(rv$param_rateEqn), length(rv$param_rateEqn_values), length(rv$param_rateEqn_comments)))})
})

observeEvent(input$param_remove_duplicate_parameters, {
  rv$parameters_in_model <- unique(rv$parameters_in_model)
  rv$param_eqns <- unique(rv$param_eqns)
  rv$param_inputs <- unique(rv$param_inputs)
  rv$param_outputs <- unique(rv$param_outputs)
  rv$param_rateEqn <- unique(rv$param_rateEqn)
  observe({print(paste(length(rv$parameters_in_model), length(rv$parameter_values), length(rv$parameter_descriptions)))})
  observe({print(paste(length(rv$param_eqns), length(rv$param_eqns_values), length(rv$param_eqns_comments)))})
  observe({print(paste(length(rv$param_inputs), length(rv$param_inputs_values), length(rv$param_inputs_comments)))})
  observe({print(paste(length(rv$param_outputs), length(rv$param_outputs_values), length(rv$param_outputs_comments)))})
  observe({print(paste(length(rv$param_rateEqn), length(rv$param_rateEqn_values), length(rv$param_rateEqn_comments)))})
  
})




