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
  number_parameters = length(params$eqns.vars)
  
  fluidRow(column(width = 2
                  ,lapply(seq(number_parameters), function(i){
                    textInput(inputId = paste0("parameter_", as.character(i))
                              ,label = params$eqns.vars[i]
                              ,value = ifelse(params$first.param.eqn.stored, params$eqns.vars_values[i], "0"))
                  }))
           ,column(width = 8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId = paste0("parameter_description_", as.character(i))
                               ,label = "Parameter Description"
                               ,value = ifelse(params$first.param.eqn.stored, params$eqns.vars_comments[i], ""))
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
  number_parameters = length(params$inputs.vars) #find number of parameters in inputs
  
  #generate labels with paramters name to put value into
  #generate text input next to it to put comment for variable into
  #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
  fluidRow(column(width = 2
                  ,lapply(seq(number_parameters), function(i){
                    textInput(inputId = paste0("parameter_input_", as.character(i))
                              ,label = params$inputs.vars[i]
                              ,value = ifelse(params$first.inputs.stored, params$inputs.vars_values[i], "0"))
                  }))
           ,column(width = 8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId = paste0("parameter_description_input_", as.character(i))
                               ,label = "Parameter Description"
                               ,value = ifelse(params$first.inputs.stored, params$inputs.vars_comments[i], ""))
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
  number_parameters = length(params$outputs.vars) #find number of parameters in inputs
  
  #generate labels with paramters name to put value into
  #generate text input next to it to put comment for variable into
  #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
  fluidRow(column(width = 2
                  ,lapply(seq(number_parameters), function(i){
                    textInput(inputId = paste0("parameter_output_", as.character(i))
                              ,label = params$outputs.vars[i]
                              ,value = ifelse(params$first.outputs.stored, params$outputs.vars_values[i], "0"))
                  }))
           ,column(width = 8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId = paste0("parameter_description_output_", as.character(i))
                               ,label = "Parameter Description"
                               ,value = ifelse(params$first.outputs.stored, params$outputs.vars_comments[i], ""))
                   }))
  ) #end fluidRow
})

#------------------------------------------------
#Parameters Rendered from RateEqn Values
#------------------------------------------------
output$parameters_rateEqns_header <- renderUI({
  req(params$first.rate.eqn.stored)
  h4("Parameters From Rate Equation")
})

output$parameters_rateEqns <- renderUI({
  req(params$first.rate.eqn.stored)
  number_parameters = length(params$rate.eqn.vars) #find number of parameters in inputs
  
  #generate labels with paramters name to put value into
  #generate text input next to it to put comment for variable into
  #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
  fluidRow(column(width = 2
                  ,lapply(seq(number_parameters), function(i){
                    textInput(inputId = paste0("parameter_rateEqn_", as.character(i))
                              ,label = params$rate.eqn.vars[i]
                              ,value = ifelse(params$first.rate.eqn.stored, params$rate.eqn.vars_values[i], "0"))
                  }))
           ,column(width = 8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId = paste0("parameter_description_rateEqn_", as.character(i))
                               ,label = "Parameter Description"
                               ,value = ifelse(params$first.rate.eqn.stored, params$rate.eqn.vars_comments[i], ""))
                   }))
  ) #end fluidRow
})

#------------------------------------------------
#Parameters Rendered from Time Dependent Values
#------------------------------------------------
output$parameters_TD_eqns_header <- renderUI({
  req(input$eqnCreate_time_dependent_store_new_parameter)
  h4("Parameters From Time Dependent Equations")
})

output$parameters_TD_eqns <- renderUI({
  req(input$eqnCreate_time_dependent_store_new_parameter)
  number_parameters = length(params$time.dep.vars) #find number of parameters in inputs
  
  #generate labels with paramters name to put value into
  #generate text input next to it to put comment for variable into
  #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
  fluidRow(column(width = 2
                  ,lapply(seq(number_parameters), function(i){
                    textInput(inputId = paste0("parameter_TD_", as.character(i))
                              ,label = params$time.dep.vars[i]
                              ,value = ifelse(params$first.time.dep.stored, params$time.dep.vars_values[i], "0"))
                  }))
           ,column(width = 8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId = paste0("parameter_description_TD_", as.character(i))
                               ,label = "Parameter Description"
                               ,value = ifelse(params$first.time.dep.stored, params$time.dep.vars_comments[i], ""))
                   }))
  ) #end fluidRow
})

#-------------------------------------------------------------------------------

#Storing Parameter info to proper vectors for data analysis

#-------------------------------------------------------------------------------
observeEvent(input$param_store_parameters, {
  #store equation parameters
  if (length(params$eqns.vars != 0)) {
    params$first.param.eqn.stored <- TRUE #set boolean so parameter values refil when UI is rendered
    num_params <- length(params$param.eqns)
    param_values <- vector() #create vector to store parameter values
    param_comments <- vector() #create vector to store parameter commemts
    
    for (i in seq(num_params)) {
      single_value <- eval(parse(text = paste0("input$parameter_", as.character(i)))) #evaluate value in textinput
      param_values <- append(param_values, single_value) #add value from textinput to vector
      
      single_comment <- eval(parse(text = paste0("input$parameter_description_", as.character(i)))) #evaluate value in textinput
      param_comments <- append(param_comments, single_comment) #append comments to vector
      param_values <- paste(param_values, sep = " ") #drop vector to a single string separated by spaces
      params$param.eqns_values <- as.numeric(param_values) #store parameter values to reactive value
      params$param.eqns_comments <- param_comments # store paramter comments to reactive value
    }
  }
  
  #store input parameters
  if (length(params$inputs.vars != 0)) {
    params$first.inputs.stored <- TRUE #set boolean so parameter values refil when UI is rendered
    num_params <- length(params$inputs.vars)
    param_values <- vector() #create vector to store parameter values
    param_comments <- vector() #create vector to store parameter commemts
    
    for (i in seq(num_params)) {
      single_value <- eval(parse(text = paste0("input$parameter_input_", as.character(i)))) #evaluate value in textinput
      param_values <- append(param_values, single_value) #add value from textinput to vector
      
      single_comment <- eval(parse(text = paste0("input$parameter_description_input_", as.character(i)))) #evaluate value in textinput
      param_comments <- append(param_comments, single_comment) #append comments to vector
      param_values <- paste(param_values, sep = " ") #drop vector to a single string separated by spaces
      params$inputs.vars_values <- as.numeric(param_values) #store parameter values to reactive value
      params$inputs.vars_comments <- param_comments # store paramter comments to reactive value
    }
  }
  
  #store output parameters
  if (length(params$outputs.vars != 0)) {
    params$first.param.outputs.stored <- TRUE #set boolean so parameter values refil when UI is rendered
    num_params <- length(params$param.outputs)
    param_values <- vector() #create vector to store parameter values
    param_comments <- vector() #create vector to store parameter commemts
    
    for (i in seq(num_params)) {
      single_value <- eval(parse(text = paste0("input$parameter_output_", as.character(i)))) #evaluate value in textinput
      param_values <- append(param_values, single_value) #add value from textinput to vector
      
      single_comment <- eval(parse(text = paste0("input$parameter_description_output_", as.character(i)))) #evaluate value in textinput
      param_comments <- append(param_comments, single_comment) #append comments to vector
      param_values <- paste(param_values, sep = " ") #drop vector to a single string separated by spaces
      params$param.outputs_values <- as.numeric(param_values) #store parameter values to reactive value
      params$param.outputs_comments <- param_comments # store paramter comments to reactive value
    }
  }
  
  #store rate equation parameters
  if (length(params$rate.eqn.vars != 0)) {
    params$first.rate.eqn.stored <- TRUE #set boolean so parameter values refil when UI is rendered
    num_params <- length(params$rate.eqn.vars)
    param_values <- vector() #create vector to store parameter values
    param_comments <- vector() #create vector to store parameter commemts
    
    for (i in seq(num_params)) {
      single_value <- eval(parse(text = paste0("input$parameter_rateEqn_", as.character(i)))) #evaluate value in textinput
      param_values <- append(param_values, single_value) #add value from textinput to vector
      
      single_comment <- eval(parse(text = paste0("input$parameter_description_rateEqn_", as.character(i)))) #evaluate value in textinput
      param_comments <- append(param_comments, single_comment) #append comments to vector
      param_values <- paste(param_values, sep = " ") #drop vector to a single string separated by spaces
      params$rate.eqn.vars_values <- as.numeric(param_values) #store parameter values to reactive value
      params$rate.eqn.vars_comments <- param_comments # store paramter comments to reactive value
    }
  }
  
  if (length(params$time.dep.vars != 0)) {
    params$first.time.dep.stored <- TRUE #set boolean so parameter values refil when UI is rendered
    num_params <- length(params$time.dep.vars)
    param_values <- vector() #create vector to store parameter values
    param_comments <- vector() #create vector to store parameter commemts
    
    for (i in seq(num_params)) {
      single_value <- eval(parse(text = paste0("input$parameter_TD_", as.character(i)))) #evaluate value in textinput
      param_values <- append(param_values, single_value) #add value from textinput to vector
      
      single_comment <- eval(parse(text = paste0("input$parameter_description_TD_", as.character(i)))) #evaluate value in textinput
      param_comments <- append(param_comments, single_comment) #append comments to vector
      param_values <- paste(param_values, sep = " ") #drop vector to a single string separated by spaces
      params$time.dep.vars_values <- as.numeric(param_values) #store parameter values to reactive value
      params$time.dep.vars_comments <- param_comments # store paramter comments to reactive value
    }
  }
  
  #Store all Paramters to overall vector
  params$vars.all = c(params$param.eqns, params$inputs.vars, params$param.outputs, params$rate.eqn.vars, params$time.dep.vars)
  params$vals.all = c(params$eqns.vals, params$inputs.vals, params$outputs.vals, params$rate.eqn.vals, params$time.dep.values)
  params$commments.all = c(params$eqns.comments, params$inputs.comments, params$outputs.comments, params$rate.eqn.comments, params$time.dep.comments)
  
  # observe({
  #   print(params$inputs.vars_values)
  #   print(params$param.inputs_comments)
  #   print(params$vars.all)
  #   print(params$vals.all)
  #   print(params$commments.all)
  # })
  
  session$sendCustomMessage(type = 'testmessage',
                            message = 'Parameters Stored')
})



observeEvent(input$param_view_parameters, {
  observe({print(params$vars.all)})
  observe({print(paste(length(params$vars.all), length(params$vals.all), length(params$commments.all)))})
  observe({print(paste(length(params$param.eqns), length(params$eqns.vals), length(params$eqns.comments)))})
  observe({print(paste(length(params$param.inputs), length(params$inputs.vals), length(params$inputs.comments)))})
  observe({print(paste(length(params$param.outputs), length(params$outputs.vals), length(params$outputs.comments)))})
  observe({print(paste(length(params$rate.eqn.vars), length(params$rate.eqn.vals), length(params$rate.eqn.comments)))})
})

observeEvent(input$param_remove_duplicate_parameters, {
  params$vars.all <- unique(params$vars.all)
  params$param.eqns <- unique(params$param.eqns)
  params$param.inputs <- unique(params$param.inputs)
  params$param.outputs <- unique(params$param.outputs)
  params$rate.eqn.vars <- unique(params$rate.eqn.vars)
  observe({print(paste(length(params$vars.all), length(params$vals.all), length(params$commments.all)))})
  observe({print(paste(length(params$param.eqns), length(params$param.eqnsparams$commments.allvalues), length(params$param.eqnsparams$commments.allcomments)))})
  observe({print(paste(length(params$param.inputs), length(params$param.inputsparams$commments.allvalues), length(params$param.inputsparams$commments.allcomments)))})
  observe({print(paste(length(params$param.outputs), length(params$param.outputsparams$commments.allvalues), length(params$param.outputsparams$commments.allcomments)))})
  observe({print(paste(length(parameters$rate.eqn.vars), length(parameters$rate.eqn.varsparameters$commments.allvalues), length(parameters$rate.eqn.varsparameters$commments.allcomments)))})
  
})




