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
  req(length(params$par.info) > 0)
  
  # Override storage used to rerender table when table edits are rejected.
  override <- TableOverrides$param.table
  
  for.table <- params$par.info.df
  
  # Apply Filters
  if (input$parameters_filter_type != "All") {
    for.table <- for.table %>%
      filter(Type == input$parameters_filter_type)
  }
  
  for.table <- for.table %>%
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
  # browser()
  # Find parameter name that was changed
  
  plotted.table <- params$par.info.df
  
  # Apply Filters
  if (input$parameters_filter_type != "All") {
    plotted.table <- plotted.table %>%
      filter(Type == input$parameters_filter_type)
  }
  
  plotted.table <- plotted.table %>%
    select("Name", "Value", "Unit", "Description")
  
  par.name <- unname(unlist(plotted.table[xi+1, 1]))
  par.id   <- FindId(par.name)
  
  if (yi == 0) {
    # Parameter name change 
    params$par.info[[par.id]]$Name <- new

    params$rate.eqn.vars     <- RenameVarInVector(old,
                                                      new,
                                                      params$rate.eqn.vars)
    params$rate.eqn.comments <- RenameVarInVector(old,
                                                      new,
                                                      params$rate.eqn.comments)
    params$time.dep.vars     <- RenameVarInVector(old,
                                                      new,
                                                      params$time.dep.vars)
    params$time.dep.comments <- RenameVarInVector(old,
                                                      new,
                                                      params$time.dep.comments)
    eqns$main                <- RenameVarInVector(old,
                                                      new,
                                                      eqns$main)
    eqns$additional.eqns     <- RenameVarInVector(old,
                                                      new,
                                                      eqns$additional.eqns)
    eqns$rate.eqns           <- RenameVarInVector(old,
                                                      new,
                                                      eqns$rate.eqns)
    eqns$time.dep.eqns       <- RenameVarInVector(old,
                                                      new,
                                                      eqns$time.dep.eqns)
    logs$IO.logs             <- RenameVarInVector(old,
                                                      new,
                                                      logs$IO.logs)

    eqns$eqn.info            <- RenameVarInDF(old, new, eqns$eqn.info)
    eqns$eqn.chem            <- RenameVarInDF(old, new, eqns$eqn.chem)
    eqns$eqn.enzyme          <- RenameVarInDF(old, new, eqns$eqn.enzyme)
    eqns$eqn.syn             <- RenameVarInDF(old, new, eqns$eqn.syn)
    eqns$eqn.deg             <- RenameVarInDF(old, new, eqns$eqn.deg)
    
    IO$IO.info               <- RenameVarInDF(old, new, IO$IO.info)
    
    # If volume change in compartment data structure
    if (params$par.info[[par.id]]$Type == "Compartment") {
      # Find which compartment has this volume
      for (i in seq(length(vars$compartments.info))) {
        # If the volume name == old volume name 
        if (vars$compartments.info[[i]]$Volume == old) {
          vars$compartments.info[[i]]$Volume = new
          break
        }
      }
    }
    
  } else if (yi == 1) {
    # Parameter Value Change
    
    # Set booleans
    conversion.needed <- FALSE
    
    # Parameter value change 
    params$par.info[[par.id]]$Value <- new

    # Change base value of parameter if needed
    selected.unit <- params$par.info[[par.id]]$Unit
    base.unit     <- params$par.info[[par.id]]$BaseUnit
    if (selected.unit != base.unit) {
      # Perform unit conversion
      conversion.needed <- TRUE
      descriptor <- params$par.info[[par.id]]$UnitDescription
      converted.value <- UnitConversion(descriptor,
                                        selected.unit,
                                        base.unit,
                                        as.numeric(new))
      params$par.info[[par.id]]$BaseValue <- converted.value
    } else {
      params$par.info[[par.id]]$BaseValue <- new
    }
    
    # If volume change in compartment data structure
    if (params$par.info[[par.id]]$Type == "Compartment") {
      # Find which compartment has this volume
      vol.name <- params$par.info[[par.id]]$Name
      for (i in seq(length(vars$compartments.info))) {
        if (vars$compartments.info[[i]]$Volume == vol.name) {
          if (conversion.needed) {
            vars$compartments.info[[i]]$BaseValue <- converted.value
          } else {
            vars$compartments.info[[i]]$BaseValue <- new
          }
          vars$compartments.info[[i]]$Value <- new
          break
        }
      }
    }
  } else if (yi == 2) {
    
    # check if units are acceptable
    descriptor <- params$par.info[[par.id]]$UnitDescription
    
    # Check to make sure units entered are the right ones
    comparison <- UnitCompare(descriptor,
                              new,
                              units$possible.units)
    
    if (comparison$is.match) {
      # Parameter unit change
      params$par.info[[par.id]]$Unit <- new
      
      
      # We take current value on table as unitvalue
      # We take current unit as the previous units
      # We take base unit as new Units
      # The converted value will be the new base unit value
      
      # Perform Conversion for base value if needed
      from.unit <- params$par.info[[par.id]]$Unit
      to.unit   <- params$par.info[[par.id]]$BaseUnit
      from.val  <- params$par.info[[par.id]]$Value
      
      if (from.unit != to.unit) {
        # Perform unit conversion for base
        descriptor <- params$par.info[[par.id]]$UnitDescription
        converted.value <- UnitConversion(descriptor,
                                          from.unit,
                                          to.unit,
                                          as.numeric(from.val))
        params$par.info[[par.id]]$BaseValue <- converted.value
      } else {
        params$par.info[[par.id]]$BaseValue <- from.val
      }
      
      # If volume change in compartment data structure change unit there
      if (params$par.info[[par.id]]$Type == "Compartment") {
        # Find which compartment has this volume and change unit/basevalue
        vol.name <- params$par.info[[par.id]]$Name
        for (i in seq(length(vars$compartments.info))) {
          if (vars$compartments.info[[i]]$Volume == vol.name) {
            vars$compartments.info[[i]]$Unit <- params$par.info[[par.id]]$Unit
            
            vars$compartments.info[[i]]$BaseValue <- 
                                            params$par.info[[par.id]]$BaseValue
            break
          }
        }
      }
    } else {
      # if unit conversion isn't allowed
      TableOverrides$param.table <- TableOverrides$param.table + 1
      params$par.info[[par.id]]$Unit <- old
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = comparison$message,
        type = "error"
      )
      print(comparison$message)
    }
  } else if (yi == 3) {
    # Parameter description change
    params$par.info[[par.id]]$Description <- new
  }
})

observeEvent(params$par.info, {
  params$par.info.df <- bind_rows(params$par.info)
})

# Parameter Debug -------------------------------------------------------------- 

observeEvent(input$param_view_parameters, {
  jPrint("Parameter List")
  jPrint(params$par.info)
})


observeEvent(params$par.info, {
  params$par.df <- bind_rows(params$par.info)
  if (nrow(params$par.df) > 0) {
    par.names <- params$par.df %>% dplyr::select(Name)
    params$par.names <- as.vector(unlist(par.names))
  } else {
    params$par.names <- vector()
  }
  
})
