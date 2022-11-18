############################### ICs Server ###################################

################################################################################
#Server Section that controls editable table of variables
# needs to create table that is editable and changes the respectable RVs.
# should control the parameters: 

output$ICs_RHT <- renderRHandsontable({
  ifelse(input$GO_species_unit_choice == "Mol",
         unit <- units$possible.units$Count,
         unit <- units$possible.units$Mass)
  
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
    hot_validate_character(col = 3, choices = unit)
})


observeEvent(input$ICs_RHT$changes$changes, {
  xi = input$ICs_RHT$changes$changes[[1]][[1]]
  yi = input$ICs_RHT$changes$changes[[1]][[2]]
  old = input$ICs_RHT$changes$changes[[1]][[3]]
  new = input$ICs_RHT$changes$changes[[1]][[4]]
  
  # Value column
  if (yi == 1) {
    new <- as.character(new)
    # Adds 0 before decimal points
    if (str_split(new, "")[[1]][1] == ".") {
      new <- as.numeric(paste0("0", new))
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
})
