########################### Parameter Server ################################
#add title line to parameters from equations (since I don't seem to be able to do it with the renderUi below)



# Functions --------------------------------------------------------------------


# On Application Load ----------------------------------------------------------
# Start with box removed on load
updateBox("parameter_info_box", action = "remove")

# Event Updates ----------------------------------------------------------------






# New Table Reactive Variables -------------------------------------------------
# Reactive variable that keeps track of parameters 
# Used when editing table values to keep track of whats changed
# parameter_table_values <- reactiveValues(table = data.frame(),
#                                          table.copy = data.frame()
#                                          )

# Parameter Filters ------------------------------------------------------------
observeEvent(input$parameters_filter_type, {
  print("TODO update filters")
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

    eqns$eqn.info            <- RenameVarInDF(old, new, eqns$eqn.info)
    eqns$eqn.chem            <- RenameVarInDF(old, new, eqns$eqn.chem)
    eqns$eqn.enzyme          <- RenameVarInDF(old, new, eqns$eqn.enzyme)
    eqns$eqn.syn             <- RenameVarInDF(old, new, eqns$eqn.syn)
    eqns$eqn.deg             <- RenameVarInDF(old, new, eqns$eqn.deg)
    
    IO$IO.info               <- RenameVarInDF(old, new, IO$IO.info)
    
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
  jPrint("Parameter List")
  jPrint(params$params)
})



