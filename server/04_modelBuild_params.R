########################### Parameter Server ################################
#add title line to parameters from equations (since I don't seem to be able to do it with the renderUi below)



# Functions --------------------------------------------------------------------
DeleteParameters <- function(paramToDelete) {
  # Delete Parameter From Storage List
  params$params[[paramToDelete]] <- NULL
  print(params$params)
  
  # Delete Parameter From Param Vector
  params$vars.all <- RemoveFromVector(paramToDelete, params$vars.all)
  
  # Delete Parameter From Param Dataframe
  idx <- match(paramToDelete, params$param.table[,1])
  params$param.table <- params$param.table[-idx, ]
  
  updatePickerInput(session, "parameters_filter_type", selected = "Eqns")
  updatePickerInput(session, "parameters_filter_type", selected = "All")
}

# On Application Load ----------------------------------------------------------
# Start with box removed on load
updateBox("parameter_info_box", action = "remove")

# Event Updates ----------------------------------------------------------------
observeEvent(params$vars.all, {
  updatePickerInput(session, "modal_params_to_delete", choices = params$vars.all)
})

# Modal for creating parameter -------------------------------------------------
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

    
    toggleModal(session, "modal_create_parameter", toggle =  "close")
  } else {
    session$sendCustomMessage(type = 'testmessage',
                              message = error.message)
  }
})

# Modal for deleting parameter--------------------------------------------------
observeEvent(input$modal_delete_param_button, {
  var.to.delete <- input$modal_params_to_delete
  DeleteParameters(var.to.delete)
  toggleModal(session, "modal_delete_param", toggle =  "close")
})

# New Table Reactive Variables -------------------------------------------------
# Reactive variable that keeps track of parameters 
# Used when editing table values to keep track of whats changed
parameter_table_values <- reactiveValues(table = data.frame(),
                                         table.copy = data.frame()
                                         )

# Parameter Filters ------------------------------------------------------------
observeEvent(input$parameters_filter_type, {
  if (input$parameters_filter_type == "All") {
    my.table <- params$param.table
  } else if (input$parameters_filter_type == "Eqns") {
    #subset table based on param eqn vars
    my.table <- 
      params$param.table[params$param.table[,1] %in% params$eqns.vars,]
  } else if (input$parameters_filter_type == "Inputs") {
    my.table <- 
      params$param.table[params$param.table[,1] %in% params$inputs.vars,]
  } else if (input$parameters_filter_type == "Outputs") {
    my.table <- 
      params$param.table[params$param.table[,1] %in% params$outputs.vars,]
  }
  parameter_table_values$table <- my.table
  parameter_table_values$table.copy <- my.table
}) 

# Parameter Table RHandsontable ------------------------------------------------
output$parameters_DT <- renderRHandsontable({
  req(length(params$params) > 0)
  
  for.table <- params$params.df %>%
    select("Name", "Value", "Unit", "Description")
  # rhandsontable(for.table)
  rhandsontable(for.table,
                #rowHeaders = NULL,
                colHeaderWidth = 100,
                stretchH = "all"
                #overflow = "visible"
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
    hot_rows(rowHeights = 30) %>%
    hot_context_menu(allowRowEdit = FALSE,
                     allowColEdit = FALSE
    ) %>%
    hot_validate_numeric(col = 2, min = 0)
})

# Event: Parameter table value changes -----------------------------------------
observeEvent(input$parameters_DT$changes$changes, {
  xi  = input$parameters_DT$changes$changes[[1]][[1]]
  yi  = input$parameters_DT$changes$changes[[1]][[2]]
  old = input$parameters_DT$changes$changes[[1]][[3]]
  new = input$parameters_DT$changes$changes[[1]][[4]]

  # Find parameter name that was changed
  plotted.table <- params$params.df %>%
    select("Name", "Value", "Unit", "Description")
  par.name <- unname(unlist(plotted.table[xi+1, 1]))
  
  if (yi == 0) {
    # Parameter name change 
    params$params[[par.name]]$Name <- new
    params$vars.all          <- RenameParameterVector(old,
                                                      new,
                                                      params$vars.all)
    params$rate.eqn.vars     <- RenameParameterVector(old,
                                                      new,
                                                      params$rate.eqn.vars)
    params$rate.eqn.comments <- RenameParameterVector(old,
                                                      new,
                                                      params$rate.eqn.comments)
    params$time.dep.vars     <- RenameParameterVector(old,
                                                      new,
                                                      params$time.dep.vars)
    params$time.dep.comments <- RenameParameterVector(old,
                                                      new,
                                                      params$time.dep.comments)
    eqns$main                <- RenameParameterVector(old,
                                                      new,
                                                      eqns$main)
    eqns$additional.eqns     <- RenameParameterVector(old,
                                                      new,
                                                      eqns$additional.eqns)
    eqns$rate.eqns           <- RenameParameterVector(old,
                                                      new,
                                                      eqns$rate.eqns)
    eqns$time.dep.eqns       <- RenameParameterVector(old,
                                                      new,
                                                      eqns$time.dep.eqns)
    logs$IO.logs             <- RenameParameterVector(old,
                                                      new,
                                                      logs$IO.logs)

    params$param.table       <- RenameParameterDF(old, new, params$param.table)
    eqns$eqn.info            <- RenameParameterDF(old, new, eqns$eqn.info)
    IO$IO.info               <- RenameParameterDF(old, new, IO$IO.info)
    
  } else if (yi == 1) {
    # Parameter value change 
    params$params[[par.name]]$Value <- new
  } else if (yi == 2) {
    # Parameter unit change
    params$params[[par.name]]$Unit <- new
  } else if (yi == 3) {
    # Parameter description change
    params$params[[par.name]]$Description <- new
  }
  
  
})

observeEvent(params$params, {
  params$params.df <- bind_rows(params$params)
  print(params$params.df)
})

# Parameter Debug -------------------------------------------------------------- 

observeEvent(input$param_view_parameters, {
  jPrint("Parameter Variables")
  jPrint(params$vars.all)
  
  jPrint("Parameter List")
  jPrint(params$params)
  
})



