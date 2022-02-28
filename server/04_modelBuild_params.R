########################### Parameter Server ################################
#add title line to parameters from equations (since I don't seem to be able to do it with the renderUi below)



################################################################################
#Server Section that controls editable table of variables
# needs to create table that is editable and changes the respectable RVs.
# should control the parameters: 

output$parameters_DT <- renderDT({
  DT::datatable(params$param.table
                ,editable = list(target = "column", disable = list(columns = 0))
                ,class = "cell-border stripe"
                ,options = list(autoWidth = TRUE
                                ,pageLength = -1
                                ,ordering = FALSE
                                ,columnDefs = list(list(width = "60%", targets = 3),
                                                   list(width = "20%", targets = 1),
                                                   list(className = 'dt-center', targets = c(1,2)),
                                                   list(className = 'dt-left', targets = 3)
                                                   )
                                ,dom = 'ft'
                )
   )
})

proxy_param_table = dataTableProxy("parameters_DT")

observeEvent(input$parameters_DT_cell_edit, {
  info = input$parameters_DT_cell_edit
  #str(info)
  params$param.table <- editData(params$param.table, info)
  replaceData(proxy_param_table, params$param.table, resetPaging = FALSE)

  #Reset the parameter data to match the table values by pulling table values
  # to match parameter vectors

  # Check if/which variables changed -- store idx values
  original.param.values <- params$vars.all
  idx.to.change = vector()
  for (i in seq(length(params$vars.all))) {
    if (params$vars.all[i] != params$param.table[, 1][i]) {
      idx.to.change <- c(idx.to.change, i)
    }
  }
  jPrint(idx.to.change)
  
  #change all RV based on table
  params$vars.all <- params$param.table[, 1] #will need to add a check here in teh future to change this value in all equations.
  params$vals.all <- params$param.table[, 2]
  params$comments.all <- params$param.table[, 3]
  
  #TODO: editing function to change those variables everywhere

  for (idx in idx.to.change) {
    #jPrint(idx)
    old.value <- original.param.values[idx]
    new.value <- params$vars.all[idx]
    eqns$main <- RenameParameterVector(old.value, new.value, eqns$main)
    eqns$additional.eqns <- RenameParameterVector(old.value, new.value, eqns$additional.eqns)
    eqns$rate.eqns <- RenameParameterVector(old.value, new.value, eqns$rate.eqns)
    eqns$time.dep.eqns <- RenameParameterVector(old.value, new.value, eqns$time.dep.eqns)
    #jPrint(params$comments.all)
    params$comments.all <- RenameParameterVector(old.value, new.value, params$comments.all)
    #jPrint(params$comments.all)
    logs$IO.logs <- RenameParameterVector(old.value, new.value, logs$IO.logs)
    params$param.table[, 3] <- params$comments.all

    #Change dataframes
    eqns$eqn.info <- RenameParameterDF(old.value, new.value, eqns$eqn.info)
    IO$IO.info <- RenameParameterDF(old.value, new.value, IO$IO.info)
    
  }
  
  
})

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
                              ,value = ifelse(params$first.param.eqn.stored, params$eqns.vals[i], "0"))
                  }))
           ,column(width = 8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId = paste0("parameter_description_", as.character(i))
                               ,label = "Parameter Description"
                               ,value = ifelse(params$first.param.eqn.stored, params$eqns.comments[i], ""))
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
                              ,value = ifelse(params$first.inputs.stored, params$inputs.vals[i], "0"))
                  }))
           ,column(width = 8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId = paste0("parameter_description_input_", as.character(i))
                               ,label = "Parameter Description"
                               ,value = ifelse(params$first.inputs.stored, params$inputs.comments[i], ""))
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
                              ,value = ifelse(params$first.outputs.stored, params$outputs.vals[i], "0"))
                  }))
           ,column(width = 8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId = paste0("parameter_description_output_", as.character(i))
                               ,label = "Parameter Description"
                               ,value = ifelse(params$first.outputs.stored, params$outputs.comments[i], ""))
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
                              ,value = ifelse(params$first.rate.eqn.stored, params$rate.eqn.vals[i], "0"))
                  }))
           ,column(width = 8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId = paste0("parameter_description_rateEqn_", as.character(i))
                               ,label = "Parameter Description"
                               ,value = ifelse(params$first.rate.eqn.stored, params$rate.eqn.comments[i], ""))
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
                              ,value = ifelse(params$first.time.dep.stored, params$time.dep.vals[i], "0"))
                  }))
           ,column(width = 8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId = paste0("parameter_description_TD_", as.character(i))
                               ,label = "Parameter Description"
                               ,value = ifelse(params$first.time.dep.stored, params$time.dep.comments[i], ""))
                   }))
  ) #end fluidRow
})

#-------------------------------------------------------------------------------

#Storing Parameter info to proper vectors for data analysis

#-------------------------------------------------------------------------------
observeEvent(input$param_store_parameters, {
  #store equation parameters
  if (length(params$eqns.vars != 0)) {
    params$first.param.eqn.stored <- TRUE #set boolean so parameter values refill when UI is rendered
    num_params <- length(params$eqns.vars)
    observe({print("Number of params")})
    observe({print(num_params)})
    param_values <- vector() #create vector to store parameter values
    param_comments <- vector() #create vector to store parameter commemts
    
    for (i in seq(num_params)) {
      observe({seq(num_params)})
      # single_value <- eval(parse(text = paste0("input$parameter_", as.character(i)))) #evaluate value in textinput
      # param_values <- append(param_values, single_value) #add value from textinput to vector
      # observe(print({single_value}))
      # single_comment <- eval(parse(text = paste0("input$parameter_description_", as.character(i)))) #evaluate value in textinput
      # param_comments <- append(param_comments, single_comment) #append comments to vector
      # param_values <- paste(param_values, sep = " ") #drop vector to a single string separated by spaces
      # params$eqns.vals <- as.numeric(param_values) #store parameter values to reactive value
      # params$eqns.comments <- param_comments # store paramter comments to reactive value
      line.val <- eval(parse(text = paste0("input$parameter_", as.character(i))))
      line.comment <- eval(parse(text = paste0("input$parameter_description_", as.character(i))))
      params$eqns.vals[i] <- as.numeric(line.val)
      params$eqns.comments[i] <- line.comment
      #find position of parameters in all parameters, change value and comment in all vectors at this location
      idx <- match(params$eqns.vars[i], params$vars.all)
      params$vals.all[idx] <- params$eqns.vals[i]
      params$comments.all[idx] <- params$eqns.comments[i]
    }
  }
  
  #store input parameters
  if (length(params$inputs.vars != 0)) {
    params$first.inputs.stored <- TRUE #set boolean so parameter values refil when UI is rendered
    num_params <- length(params$inputs.vars)

    
    for (i in seq(num_params)) {
      line.val <- eval(parse(text = paste0("input$parameter_input_", as.character(i))))
      line.comment <- eval(parse(text = paste0("input$parameter_description_input_", as.character(i))))
      params$inputs.vals[i] <- as.numeric(line.val)
      params$inputs.comments[i] <- line.comment
      #find position of parameters in all parameters, change value and comment in all vectors at this location
      idx <- match(params$inputs.vars[i], params$vars.all)
      params$vals.all[idx] <- params$inputs.vals[i]
      params$comments.all[idx] <- params$inputs.comments[i]
    }
  }
  
  #store output parameters
  if (length(params$outputs.vars != 0)) {
    params$first.param.outputs.stored <- TRUE #set boolean so parameter values refil when UI is rendered
    num_params <- length(params$outputs.vars)
    
    for (i in seq(num_params)) {
      line.val <- eval(parse(text = paste0("input$parameter_output_", as.character(i))))
      line.comment <- eval(parse(text = paste0("input$parameter_description_output_", as.character(i))))
      params$outputs.vals[i] <- as.numeric(line.val)
      params$outputs.comments[i] <- line.comment
      #find position of parameters in all parameters, change value and comment in all vectors at this location
      idx <- match(params$outputs.vars[i], params$vars.all)
      params$vals.all[idx] <- params$outputs.vals[i]
      params$comments.all[idx] <- params$outputs.comments[i]
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
      params$rate.eqn.vals <- as.numeric(param_values) #store parameter values to reactive value
      params$rate.eqn.comments <- param_comments # store paramter comments to reactive value
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
      params$time.dep.vals <- as.numeric(param_values) #store parameter values to reactive value
      params$time.dep.comments <- param_comments # store paramter comments to reactive value
    }
  }
  
  #Store all Paramters to overall vector
  # params$vars.all = c(params$param.eqns, params$inputs.vars, params$param.outputs, params$rate.eqn.vars, params$time.dep.vars)
  # params$vals.all = c(params$eqns.vals, params$inputs.vals, params$outputs.vals, params$rate.eqn.vals, params$time.dep.values)
  # params$comments.all = c(params$eqns.comments, params$inputs.comments, params$outputs.comments, params$rate.eqn.comments, params$time.dep.comments)
  
  # observe({
  #   print(params$inputs.vals)
  #   print(params$param.inputs_comments)
  #   print(params$vars.all)
  #   print(params$vals.all)
  #   print(params$comments.all)
  # })
  
  session$sendCustomMessage(type = 'testmessage',
                            message = 'Parameters Stored')
})



observeEvent(input$param_view_parameters, {
  observe({print(params$vars.all)})
  observe({print(params$vals.all)})
  observe({print(params$eqns.vars)})
  observe({print(params$eqns.vals)})
  observe({print(params$inputs.vars)})
  observe({print(params$inputs.vals)})
  observe({print(params$outputs.vars)})
  observe({print(params$outputs.vals)})
  observe({print(params$comments.all)})
  observe({print(params$param.table)})
  observe({print(ICs$vals)})
  observe({print(vars$species)})
  # observe({print(paste(length(params$vars.all), length(params$vals.all), length(params$comments.all)))})
  # observe({print(paste(length(params$param.eqns), length(params$eqns.vals), length(params$eqns.comments)))})
  # observe({print(paste(length(params$param.inputs), length(params$inputs.vals), length(params$inputs.comments)))})
  # observe({print(paste(length(params$param.outputs), length(params$outputs.vals), length(params$outputs.comments)))})
  # observe({print(paste(length(params$rate.eqn.vars), length(params$rate.eqn.vals), length(params$rate.eqn.comments)))})
})

observeEvent(input$param_remove_duplicate_parameters, {
  params$vars.all <- unique(params$vars.all)
  params$param.eqns <- unique(params$param.eqns)
  params$param.inputs <- unique(params$param.inputs)
  params$param.outputs <- unique(params$param.outputs)
  params$rate.eqn.vars <- unique(params$rate.eqn.vars)
  # observe({print(paste(length(params$vars.all), length(params$vals.all), length(params$comments.all)))})
  # observe({print(paste(length(params$param.eqns), length(params$param.eqnsparams$comments.allvalues), length(params$param.eqnsparams$comments.allcomments)))})
  # observe({print(paste(length(params$param.inputs), length(params$param.inputsparams$comments.allvalues), length(params$param.inputsparams$comments.allcomments)))})
  # observe({print(paste(length(params$param.outputs), length(params$param.outputsparams$comments.allvalues), length(params$param.outputsparams$comments.allcomments)))})
  # observe({print(paste(length(params$rate.eqn.vars), length(params$rate.eqn.varsparams$comments.allvalues), length(params$rate.eqn.varsparams$comments.allcomments)))})
  # 
})




