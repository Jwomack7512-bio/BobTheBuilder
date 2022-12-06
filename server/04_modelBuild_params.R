########################### Parameter Server ################################
#add title line to parameters from equations (since I don't seem to be able to do it with the renderUi below)



################################################################################
DeleteParameters <- function(paramToDelete) {
  # Delete Parameter From Storage List
  params$params[[paramToDelete]] <- NULL
  print(params$params)
  
  # Delete Parameter From Param Vector
  params$vars.all <- RemoveFromVector(paramToDelete, params$vars.all)
  
  # Delete Parameter From Param Dataframe
  idx <- match(paramToDelete, params$param.table[,1])
  params$param.table <- params$param.table[-idx, ]
  
  # idx <- match(paramToDelete, params$vars.all)
  # params$vars.all <- params$vars.all[-idx]
  # params$vals.all <- params$vals.all[-idx]
  # params$comments.all <- params$comments.all[-idx]
  # params$param.table <- data.frame(params$vars.all, params$vals.all, params$comments.all)
  # colnames(params$param.table) <- c("Parameter", "Value", "Description")
  
  
  #check if it exists in other parameter instances
  # idx <- match(paramToDelete, params$eqns.vars)
  # if (!is.null(idx)) {
  #   params$eqns.vars <- params$eqns.vars[-idx]
  #   params$eqns.vals <- params$eqns.vals[-idx]
  #   params$eqns.comments <- params$eqns.comments[-idx]
  # }
  # idx <- match(paramToDelete, params$inputs.vars)
  # if (!is.null(idx)) {
  #   params$inputs.vars <- params$inputs.vars[-idx]
  #   params$inputs.vals <- params$inputs.vals[-idx]
  #   params$inputs.comments <- params$inputs.comments[-idx]
  # }
  # idx <- match(paramToDelete, params$outputs.vars)
  # if (!is.null(idx)) {
  #   params$outputs.vars <- params$outputs.vars[-idx]
  #   params$outputs.vals <- params$outputs.vals[-idx]
  #   params$outputs.comments <- params$outputs.comments[-idx]
  # }
  # idx <- match(paramToDelete, params$rate.eqn.vars)
  # if (!is.null(idx)) {
  #   params$rate.eqn.vars <- params$rate.eqn.vars[-idx]
  #   params$rate.eqn.vals <- params$rate.eqn.vals[-idx]
  #   params$rate.eqn.comments <- params$rate.eqn.comments[-idx]
  # }
  # idx <- match(paramToDelete, params$time.dep.vars)
  # if (!is.null(idx)) {
  #   params$time.dep.vars <- params$time.dep.vars[-idx]
  #   params$time.dep.values <- params$time.dep.values[-idx]
  #   params$time.dep.comments <- params$time.dep.comments[-idx]
  # }
  updatePickerInput(session, "parameters_filter_type", selected = "Eqns")
  updatePickerInput(session, "parameters_filter_type", selected = "All")
}


# Modal for creating parameter
observeEvent(input$modal_create_param_button, {
  #create row for parameter df
  var <- input$modal_param_param_name
  check.vars <- variableCheck(var, vars$species, params$vars.all)
  passed.check <- check.vars[[1]]
  error.message <- check.vars[[2]]
  error.code <- check.vars[[3]]
  
  if (passed.check) {
    # Generate Param Id
    ids <- GenerateId(id$id.var.seed, "parameter")
    id <- ids$id
    
    # Create Parameter Entry For List Entry
    p.list.entry <- list(Name = input$modal_param_param_name,
                         ID = id,
                         Value = input$modal_param_value,
                         Unit = input$model_param_unit,
                         Description = input$modal_param_description,
                         Type = "Custom Added",
                         TypeNote = "")
    nPars <- length(params$params)
    params$params[[nPars+1]] <- p.list.entry
    names(params$params)[[nPars+1]] <- input$modal_param_param_name
    # Add Param to Param Table
    row.to.add <- c(input$modal_param_param_name,
                    input$modal_param_value,
                    input$model_param_unit,
                    input$modal_param_description)
    
    params$param.table[nrow(params$param.table)+1,] <- row.to.add
    updatePickerInput(session, "parameters_filter_type", selected = "Eqns")
    updatePickerInput(session, "parameters_filter_type", selected = "All")
    
    params$vars.all <- c(params$vars.all, input$modal_param_param_name) 
    # params$vals.all <- c(params$vals.all, input$modal_param_value)
    # params$comments.all <- c(params$comments.all, input$modal_param_description)
    
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

# param.reset.event <- reactive({
#   list(input$eqnCreate_addEqnToVector,
#        input$Inout_edit_addInVarToDf,
#        input$Inout_addOutVarToDf_edit,
#        input$Inout_addInVarToDf,
#        input$Inout_addOutVarToDf,
#        input$Inout_button_delete_IO_eqn,
#        input$parameters_DT$changes$changes,
#        parameter_table_values$table,
#        params$param.table)
# })

# observeEvent(param.reset.event(), {
#   
#   updatePickerInput(
#     session = session,
#     inputId = "parameters_filter_type",
#     selected = "Eqns"
#   )
#   updatePickerInput(
#     session = session,
#     inputId = "parameters_filter_type",
#     selected = "All"
#   )
#   #input$parameters_filter_type <- "All"
# })
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

output$parameters_DT <- renderRHandsontable({
  rhandsontable(params$param.table,
                #parameter_table_values$table,
                #rowHeaders = NULL,
                colHeaderWidth = 100,
                stretchH = "all",
                overflow = "visible"
  ) %>%
    hot_cols(colWidth = c(30, 15, 15, 90),
      manualColumnMove = FALSE,
      manualColumnResize = TRUE,
      halign = "htCenter",
      valign = "htMiddle",
      renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (row % 2 == 0) {
              td.style.background = '#f9f9f9';
             } else {
              td.style.background = 'white';
             };
           }") %>%
    #hot_col("Parameter", readOnly = TRUE) %>%
    #hot_col("Description", halign = "htLeft", valign = "htMiddle") %>%
    hot_rows(rowHeights = 40) %>%
    hot_context_menu(allowRowEdit = FALSE,
                     allowColEdit = FALSE
    ) %>%
    hot_validate_numeric(col = 2, min = 0)
})

observeEvent(input$parameters_DT$changes$changes, {
  xi  = input$parameters_DT$changes$changes[[1]][[1]]
  yi  = input$parameters_DT$changes$changes[[1]][[2]]
  old = input$parameters_DT$changes$changes[[1]][[3]]
  new = input$parameters_DT$changes$changes[[1]][[4]]
  
  # Converts number to proper form (.69 -> 0.69)
  if (yi == 1) {
    new <- as.character(new)
    if (str_split(new, "")[[1]][1] == ".") {
      new <- as.numeric(paste0("0", new))
    }
  }
  
  if (input$parameters_filter_type == "All") {
    
    # Change in parameter list
    changed.par <- params$param.table[xi+1, 1]
    par.names <- names(params$params)
    par.idx <- match(changed.par, par.names)

    if (yi == 0) {
      # Parameter name was changed
      params$params[[par.idx]]$Name <- new
      names(params$params)[par.idx] <- new
      
    } else if (yi == 1) {
      # Parameter value was changed
      params$params[[par.idx]]$Value <- new
      
    } else if (yi == 2) {
      # Parameter unit was changed
      comparison <- UnitCompare(params$params[[par.idx]]$Unit.Description,
                                new,
                                units$possible.units$For.Var,
                                units$possible.units$Duration)
      print(comparison)
      if (comparison$is.match) {
        params$params[[par.idx]]$Unit <- new
        # Perform Unit Conversion
        PrintVar(old)
        PrintVar(new)
        print(params$params[[par.idx]])
        print(params$params[[par.idx]]$Value)
        new.value <- UnitConversion(params$params[[par.idx]]$Unit.Description,
                                    old,
                                    new,
                                    as.numeric(params$params[[par.idx]]$Value))
        params$params[[par.idx]]$Value <- new.value
        params$param.table[xi+1, 2] <- new.value
        PrintVar(new.value)
      } else {
        # Change back to original
        new <- old
      }
      
      # Perform Unit Conversion
      
    } else if (yi == 3) {
      # Parameter description was changed
      params$params[[par.idx]]$Description <- new
    }

    params$param.table[xi+1, yi+1] <- new
    params$vars.all[xi+1]          <- params$param.table[xi+1, 1]
    params$vals.all[xi+1]          <- params$param.table[xi+1, 2]
    params$par.units.all[xi+1]     <- params$param.table[xi+1, 3]
    params$comments.all[xi+1]      <- params$param.table[xi+1, 4]
    
  } else {
    row.changed <- xi+1
    col.changed <- yi+1
    
    #find name of variable that had a value changed
    name.changed <- parameter_table_values$table[xi+1, 1]
    #find the idx of that variable in main parameter table
    idx <- match(name.changed, params$param.table[,1])
    #change the changed value in main parameter table
    params$param.table[idx, col.changed] <- new
    
    #store it to its appropriate reactive variable
    params$vars.all     <- params$param.table[, 1] 
    params$vals.all     <- params$param.table[, 2]
    params$comments.all <- params$param.table[, 3]
    }
  
  loop$parameters <- params$param.table

  # If change of parameter name
  if (yi+1 == 1) {
    jPrint("Changing Parameters")
    params$vars.all          <- RenameParameterVector(old, new, params$vars.all)
    params$comments.all      <- RenameParameterVector(old, new, params$comments.all)
    params$eqns.vars         <- RenameParameterVector(old, new, params$eqns.vars)
    params$eqns.comments     <- RenameParameterVector(old, new, params$eqns.comments)
    params$inputs.vars       <- RenameParameterVector(old, new, params$inputs.vars)
    params$inputs.comments   <- RenameParameterVector(old, new, params$inputs.comments)
    params$outputs.vars      <- RenameParameterVector(old, new, params$outputs.vars)
    params$outputs.comments  <- RenameParameterVector(old, new, params$outputs.comments) 
    params$rate.eqn.vars     <- RenameParameterVector(old, new, params$rate.eqn.vars)
    params$rate.eqn.comments <- RenameParameterVector(old, new, params$rate.eqn.comments) 
    params$time.dep.vars     <- RenameParameterVector(old, new, params$time.dep.vars)
    params$time.dep.comments <- RenameParameterVector(old, new, params$time.dep.comments)
    eqns$main                <- RenameParameterVector(old, new, eqns$main)
    eqns$additional.eqns     <- RenameParameterVector(old, new, eqns$additional.eqns)
    eqns$rate.eqns           <- RenameParameterVector(old, new, eqns$rate.eqns)
    eqns$time.dep.eqns       <- RenameParameterVector(old, new, eqns$time.dep.eqns)
    logs$IO.logs             <- RenameParameterVector(old, new, logs$IO.logs)
    
    params$param.table       <- RenameParameterDF(old, new, params$param.table)
    eqns$eqn.info            <- RenameParameterDF(old, new, eqns$eqn.info)
    IO$IO.info               <- RenameParameterDF(old, new, IO$IO.info)
  }
  
  # Rerender Parameter Ttable
  output$parameters_DT <- renderRHandsontable({
    rhandsontable(params$param.table,
                  #parameter_table_values$table,
                  #rowHeaders = NULL,
                  colHeaderWidth = 100,
                  stretchH = "all",
                  overflow = "visible"
    ) %>%
      hot_cols(colWidth = c(30, 15, 15, 90),
               manualColumnMove = FALSE,
               manualColumnResize = TRUE,
               halign = "htCenter",
               valign = "htMiddle",
               renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (row % 2 == 0) {
              td.style.background = '#f9f9f9';
             } else {
              td.style.background = 'white';
             };
           }") %>%
      #hot_col("Parameter", readOnly = TRUE) %>%
      #hot_col("Description", halign = "htLeft", valign = "htMiddle") %>%
      hot_rows(rowHeights = 40) %>%
      hot_context_menu(allowRowEdit = FALSE,
                       allowColEdit = FALSE
      ) %>%
      hot_validate_numeric(col = 2, min = 0)
  })
})

observeEvent(params$vars.all, {
  updatePickerInput(session, "modal_params_to_delete", choices = params$vars.all)
})

#-------------------------------------------------------------------------------

# Parameter Debug  

#-------------------------------------------------------------------------------
  
observeEvent(input$param_view_parameters, {
  jPrint("Parameter Variables")
  jPrint(params$vars.all)

  jPrint("Parameter Table")
  jPrint(params$param.table)
  
  jPrint("Parameter List")
  jPrint(params$params)

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




