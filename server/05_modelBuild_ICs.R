############################### ICs Server ###################################


# Table Render RHandsontable for ICs -------------------------------------------
output$ICs_RHT <- renderRHandsontable({
  
  rhandsontable(ICs$ICs.table,
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
    hot_col("Variable", readOnly = TRUE) %>%
    #hot_col("Description", halign = "htLeft", valign = "htMiddle") %>%
    hot_rows(rowHeights = 40) %>%
    hot_context_menu(allowRowEdit = FALSE,
                     allowColEdit = FALSE
    ) %>%
    hot_validate_numeric(col = 2, min = 0) %>%
    hot_validate_character(col = 3, choices = units$possible.units$For.Var)
})

# Event: IC Table Value Changes ------------------------------------------------
observeEvent(input$ICs_RHT$changes$changes, {
  xi  <- input$ICs_RHT$changes$changes[[1]][[1]]
  yi  <- input$ICs_RHT$changes$changes[[1]][[2]]
  old <- input$ICs_RHT$changes$changes[[1]][[3]]
  new <- input$ICs_RHT$changes$changes[[1]][[4]]
  
  
  var.name <- ICs$ICs.table[xi+1, 1]
  # Value column
  if (yi == 1) {
    new <- as.character(new)
    # Adds 0 before decimal points
    if (str_split(new, "")[[1]][1] == ".") {
      new <- as.numeric(paste0("0", new))
    }
    
    vars$var.info[[var.name]]$Value <- new
    select.unit <- vars$var.info[[var.name]]$Unit
    base.unit   <- vars$var.info[[var.name]]$BaseUnit
    if (select.unit != base.unit) {
      descriptor <- vars$var.info[[var.name]]$UnitDescription
      converted.value <- UnitConversion(descriptor,
                                        select.unit,
                                        base.unit,
                                        as.numeric(new))
      vars$var.info[[var.name]]$BaseValue <- converted.value
    } else {
      vars$var.info[[var.name]]$BaseValue <- as.numeric(new)
    }
  }
  
  # Check if unit was converted
  if (yi == 2) {
    descriptor <- vars$var.info[[var.name]]$UnitDescription

    comparison <- UnitCompare(descriptor,
                              new,
                              units$possible.units$For.Var,
                              units$possible.units$Duration)
    if (comparison$is.match) {
      ICs$units[xi+1] <- new
      # Perform Unit Conversion
      new.value <- UnitConversion(descriptor,
                                  old, 
                                  new,
                                  as.numeric(ICs$ICs.table[xi+1, 2]))
      ICs$ICs.table[xi+1, 2] <- new.value
      ICs$vals[xi+1] <- new.value
      vars$var.info[[var.name]]$Value <- new.value
      vars$var.info[[var.name]]$Unit <- new
      
    } else {
      new <- old
    }
  }
  
  #copying table to dataframe
  ICs$ICs.table[xi+1, yi+1] <- new
  
  # Store value to appropriate RV
  ICs$vals[xi+1] <- ICs$ICs.table[xi+1, 2]
  ICs$units[xi+1] <- ICs$ICs.table[xi+1, 3]
  ICs$comments[xi+1] <- ICs$ICs.table[xi+1, 4]
  
  # Pass to other variables
  loop$ICs <- ICs$ICs.table
  
  
  # Reset Table
  output$ICs_RHT <- renderRHandsontable({
    
    rhandsontable(ICs$ICs.table,
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
      hot_col("Variable", readOnly = TRUE) %>%
      #hot_col("Description", halign = "htLeft", valign = "htMiddle") %>%
      hot_rows(rowHeights = 40) %>%
      hot_context_menu(allowRowEdit = FALSE,
                       allowColEdit = FALSE
      ) %>%
      hot_validate_numeric(col = 2, min = 0) %>%
      hot_validate_character(col = 3, choices = units$possible.units$For.Var)
  })
})

# Debug ------------------------------------------------------------------------
observeEvent(input$IC_print, {
  print(vars$species)
  print(ICs$vals)
  print(ICs$units)
  print(ICs$comments)
})
