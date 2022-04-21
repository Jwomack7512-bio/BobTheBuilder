########################### Parameter Server ################################
#add title line to parameters from equations (since I don't seem to be able to do it with the renderUi below)



################################################################################
DeleteParameters <- function(paramToDelete) {
  idx <- match(paramToDelete, params$vars.all)
  params$vars.all <- params$vars.all[-idx]
  params$vals.all <- params$vals.all[-idx]
  params$comments.all <- params$comments.all[-idx]
  params$param.table <- data.frame(params$vars.all, params$vals.all, params$comments.all)
  colnames(params$param.table) <- c("Parameter", "Value", "Description")
  
  
  #check if it exists in other parameter instances
  idx <- match(paramToDelete, params$eqns.vars)
  if (!is.null(idx)) {
    params$eqns.vars <- params$eqns.vars[-idx]
    params$eqns.vals <- params$eqns.vals[-idx]
    params$eqns.comments <- params$eqns.comments[-idx]
  }
  idx <- match(paramToDelete, params$inputs.vars)
  if (!is.null(idx)) {
    params$inputs.vars <- params$inputs.vars[-idx]
    params$inputs.vals <- params$inputs.vals[-idx]
    params$inputs.comments <- params$inputs.comments[-idx]
  }
  idx <- match(paramToDelete, params$outputs.vars)
  if (!is.null(idx)) {
    params$outputs.vars <- params$outputs.vars[-idx]
    params$outputs.vals <- params$outputs.vals[-idx]
    params$outputs.comments <- params$outputs.comments[-idx]
  }
  idx <- match(paramToDelete, params$rate.eqn.vars)
  if (!is.null(idx)) {
    params$rate.eqn.vars <- params$rate.eqn.vars[-idx]
    params$rate.eqn.vals <- params$rate.eqn.vals[-idx]
    params$rate.eqn.comments <- params$rate.eqn.comments[-idx]
  }
  idx <- match(paramToDelete, params$time.dep.vars)
  if (!is.null(idx)) {
    params$time.dep.vars <- params$time.dep.vars[-idx]
    params$time.dep.values <- params$time.dep.values[-idx]
    params$time.dep.comments <- params$time.dep.comments[-idx]
  }
  updatePickerInput(session, "parameters_filter_type", selected = "Eqns")
  updatePickerInput(session, "parameters_filter_type", selected = "All")
}

#start with box removed on load
updateBox("parameter_info_box", action = "remove")

# button that displays info box on parameter page
observeEvent(input$parameter_info_button, {
  #if odd box appears, if even box disappears
  if (input$parameter_info_button %% 2 == 0) {
    updateBox("parameter_info_box", action = "remove")
  } else {
    updateBox("parameter_info_box", action = "restore")
  }
})

# Reactive variable that keeps track of parameters - used when editing table values to keep track of whats changed
parameter_table_values <- reactiveValues(table = data.frame(),
                                         table.copy = data.frame()
                                         )

param.reset.event <- reactive({
  list(input$eqnCreate_addEqnToVector,
       input$Inout_edit_addInVarToDf,
       input$Inout_addOutVarToDf_edit,
       input$Inout_addInVarToDf,
       input$Inout_addOutVarToDf,
       input$Inout_button_delete_IO_eqn)
})

observeEvent(param.reset.event(), {
  
  updatePickerInput(
    session = session,
    inputId = "parameters_filter_type",
    selected = "Eqns"
  )
  updatePickerInput(
    session = session,
    inputId = "parameters_filter_type",
    selected = "All"
  )
  #input$parameters_filter_type <- "All"
})
# Filters parameter table based on what pickerinputs are requesting
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

# Set up DT to display parameters
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

# Proxy table used for table editing
proxy_param_table = dataTableProxy("parameters_DT")



# Changes parameter information in all the right places on cell edit
observeEvent(input$parameters_DT_cell_edit, {
  info = input$parameters_DT_cell_edit
  parameter_table_values$table <- editData(parameter_table_values$table, info)
  replaceData(proxy_param_table, parameter_table_values$table, resetPaging = FALSE)
  
  #change all RV based on table
  if (input$parameters_filter_type == "All") {
    params$vars.all <- parameter_table_values$table[, 1] #will need to add a check here in the future to change this value in all equations.
    params$vals.all <- parameter_table_values$table[, 2]
    params$comments.all <- parameter_table_values$table[, 3]
    
    params$param.table[, 1] <- params$vars.all
    params$param.table[, 2] <- params$vals.all
    params$param.table[, 3] <- params$comments.all
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
  new.params <- c()
  original.params <- c()
  #compare current table to table copy to find variable names that changed
  for (row in seq(nrow(parameter_table_values$table))) {
    #go row by row find name. get value in pair
    new.var <- parameter_table_values$table[row, 1]
    original.var <- parameter_table_values$table.copy[row,1]
    #If parameter changed add to vector to search later
    if (new.var != original.var) {
      new.params <- c(new.params, new.var)
      original.params <- c(original.params, original.var)
    }
  }
  #search through vector of changes and search for their ids in id dataframe
  if (length(new.params > 0)) {
    for (i in seq(length(new.params))) {
      old.value <- original.params[i]
      new.value <- new.params[i]
      
      # idx <- match(param, id$id.parameters$idName)
      # param.id <-  id$id.parameters$id[idx]
      # param.name <- id$id.parameters$idName[idx]
      params$vars.all <- RenameParameterVector(old.value, new.value, params$vars.all)
      params$comments.all <- RenameParameterVector(old.value, new.value, params$comments.all)
      params$eqns.vars <- RenameParameterVector(old.value, new.value, params$eqns.vars)
      params$eqns.comments <- RenameParameterVector(old.value, new.value, params$eqns.comments)
      params$inputs.vars <- RenameParameterVector(old.value, new.value, params$inputs.vars)
      params$inputs.comments <- RenameParameterVector(old.value, new.value, params$inputs.comments)
      params$outputs.vars <- RenameParameterVector(old.value, new.value, params$outputs.vars)
      params$outputs.comments <- RenameParameterVector(old.value, new.value, params$outputs.comments) 
      params$rate.eqn.vars <- RenameParameterVector(old.value, new.value, params$rate.eqn.vars)
      params$rate.eqn.comments <- RenameParameterVector(old.value, new.value, params$rate.eqn.comments) 
      params$time.dep.vars <- RenameParameterVector(old.value, new.value, params$time.dep.vars)
      params$time.dep.comments <- RenameParameterVector(old.value, new.value, params$time.dep.comments)
      eqns$main <- RenameParameterVector(old.value, new.value, eqns$main)
      eqns$additional.eqns <- RenameParameterVector(old.value, new.value, eqns$additional.eqns)
      eqns$rate.eqns <- RenameParameterVector(old.value, new.value, eqns$rate.eqns)
      eqns$time.dep.eqns <- RenameParameterVector(old.value, new.value, eqns$time.dep.eqns)
      logs$IO.logs <- RenameParameterVector(old.value, new.value, logs$IO.logs)
      
      params$param.table <- RenameParameterDF(old.value, new.value, params$param.table)
      eqns$eqn.info <- RenameParameterDF(old.value, new.value, eqns$eqn.info)
      IO$IO.info <- RenameParameterDF(old.value, new.value, IO$IO.info)
    }
  }
  #go change these values in all places
    
  #copy data to loop mode
  loop$parameters <- params$param.table
  #TODO: editing function to change those variables everywhere

  # for (idx in idx.to.change) {
  #   #jPrint(idx)
  #   old.value <- original.param.values[idx]
  #   new.value <- params$vars.all[idx]
  #   eqns$main <- RenameParameterVector(old.value, new.value, eqns$main)
  #   eqns$additional.eqns <- RenameParameterVector(old.value, new.value, eqns$additional.eqns)
  #   eqns$rate.eqns <- RenameParameterVector(old.value, new.value, eqns$rate.eqns)
  #   eqns$time.dep.eqns <- RenameParameterVector(old.value, new.value, eqns$time.dep.eqns)
  #   #jPrint(params$comments.all)
  #   params$comments.all <- RenameParameterVector(old.value, new.value, params$comments.all)
  #   #jPrint(params$comments.all)
  #   logs$IO.logs <- RenameParameterVector(old.value, new.value, logs$IO.logs)
  #   params$param.table[, 3] <- params$comments.all
  # 
  #   #Change dataframes
  #   eqns$eqn.info <- RenameParameterDF(old.value, new.value, eqns$eqn.info)
  #   IO$IO.info <- RenameParameterDF(old.value, new.value, IO$IO.info)
  # }
})


observeEvent(params$vars.all, {
  updatePickerInput(session, "modal_params_to_delete", choices = params$vars.all)
})

# Modal for creating parameter
observeEvent(input$modal_create_param_button, {
  #create row for parameter df
  var <- input$modal_param_param_name
  check.vars <- variableCheck(var, vars$species, params$vars.all)
  passed.check <- check.vars[[1]]
  error.message <- check.vars[[2]]
  error.code <- check.vars[[3]]
  
  if (passed.check) {
    row.to.add <- c(input$modal_param_param_name,
                    input$modal_param_value,
                    input$modal_param_description)
    
    params$param.table[nrow(params$param.table)+1,] <- row.to.add
    updatePickerInput(session, "parameters_filter_type", selected = "Eqns")
    updatePickerInput(session, "parameters_filter_type", selected = "All")
    toggleModal(session, "modal_create_parameter", toggle =  "close")
  } else {
    session$sendCustomMessage(type = 'testmessage',
                              message = error.message)
  }
})

# Modal for deleting parameter
observeEvent(input$modal_delete_param_button, {
  var.to.delete <- input$modal_params_to_delete
  DeleteParameters(var.to.delete)
  toggleModal(session, "modal_delete_param", toggle =  "close")
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




