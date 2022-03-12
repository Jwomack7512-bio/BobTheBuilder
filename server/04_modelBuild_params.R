########################### Parameter Server ################################
#add title line to parameters from equations (since I don't seem to be able to do it with the renderUi below)



################################################################################
#start with box removed on load
updateBox("parameter_info_box", action = "remove")

observeEvent(input$parameter_info_button, {
  #if odd box appears, if even box disappears
  if (input$parameter_info_button %% 2 == 0) {
    updateBox("parameter_info_box", action = "remove")
  } else {
    updateBox("parameter_info_box", action = "restore")
  }
})

parameter_table_values <- reactiveValues(table = data.frame(),
                                         table.copy = data.frame()
                                         )

observeEvent(input$parameters_filter_type, {
  if (input$parameters_filter_type == "All") {
    my.table <- params$param.table
  } else if (input$parameters_filter_type == "Eqns") {
    #subset table based on param eqn vars
    my.table <- params$param.table[params$param.table[,1] %in% params$eqns.vars,]
  } else if (input$parameters_filter_type == "Inputs") {
    my.table <- params$param.table[params$param.table[,1] %in% params$inputs.vars,]
  } else if (input$parameters_filter_type == "Outputs") {
    my.table <- params$param.table[params$param.table[,1] %in% params$outputs.vars,]
  }
  parameter_table_values$table <- my.table
  parameter_table_values$table.copy <- my.table
}) 

output$parameters_DT <- renderDT({
  DT::datatable(parameter_table_values$table
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
  parameter_table_values$table <- editData(parameter_table_values$table, info)
  replaceData(proxy_param_table, parameter_table_values$table, resetPaging = FALSE)

  

  # Check if/which variables changed -- store idx values --RENAMEing VARS
  original.param.values <- params$vars.all
  idx.to.change = vector()
  for (i in seq(length(params$vars.all))) {
    if (params$vars.all[i] != params$param.table[, 1][i]) {
      idx.to.change <- c(idx.to.change, i)
    }
  }
  jPrint(idx.to.change)
  
  #change all RV based on table
  if (input$parameters_filter_type == "All") {
    params$vars.all <- parameter_table_values$table[, 1] #will need to add a check here in the future to change this value in all equations.
    params$vals.all <- parameter_table_values$table[, 2]
    params$comments.all <- parameter_table_values$table[, 3]
  } else {
    #find the location of variable that is changed in original 
    
    for (row in seq(nrow(parameter_table_values$table))) {
      #go row by row find name. get value in pair
      param.var <- parameter_table_values$table[row, 1]
      param.val <- parameter_table_values$table[row, 2]
      param.com <- parameter_table_values$table[row, 3]
      
      #find that value in param table original and value.
      idx <- match(param.var, params$param.table[, 1])
      #check if the value has changed.  If so change it in param$param.table
      if (params$param.table[idx, 2] != param.val) {
        params$param.table[idx, 2] = param.val
      }
      if (params$param.table[idx, 3] != param.com) {
        params$param.table[idx, 3] = param.com
      }
    }
    #store it to its appropriate reactive variable
    params$vars.all <- params$param.table[, 1] 
    params$vals.all <- params$param.table[, 2]
    params$comments.all <- params$param.table[, 3]

  }
  
  #compare current table to table copy to find variable names that changed
  
  #If changed add to vector
  
  #search through vector of changes and search for their ids in id dataframe
  
  #go change these values in all places
  
  
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
  jPrint("params$vars.all")
  jPrint(params$vars.all)
  jPrint("Params$vals.all")
  jPrint(params$vals.all)
  jPrint("Params$eqns.vars")
  jPrint(params$eqns.vars)
  # jPrint("Params$eqns.vals")
  # jPrint(params$eqns.vals)
  jPrint("Params$inputs.vars")
  jPrint(params$inputs.vars)
  # jPrint("Params$inputs.vals")
  # jPrint(params$inputs.vals)
  jPrint("Params$outputs.vars")
  jPrint(params$outputs.vars)
  # jPrint("Params$outputs.vals")
  # jPrint(params$outputs.vals)
  # jPrint("Params$comments.all")
  # jPrint(params$comments.all)
  jPrint("Params$param.table")
  jPrint(params$param.table)
  # observe({print(ICs$vals)})
  # observe({print(vars$species)})
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




