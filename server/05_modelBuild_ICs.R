############################### ICs Server ###################################

################################################################################
#Server Section that controls editable table of variables
# needs to create table that is editable and changes the respectable RVs.
# should control the parameters: 

output$ICs_DT <- renderDT({
  DT::datatable(ICs$ICs.table
                ,editable = list(target = "column", disable = list(columns = c(0,1)))
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

proxy_ICs_table = dataTableProxy("ICs_DT")

observeEvent(input$ICs_DT_cell_edit, {
  info = input$ICs_DT_cell_edit
  #str(info)
  ICs$ICs.table <- editData(ICs$ICs.table, info)
  replaceData(proxy_ICs_table, ICs$ICs.table, resetPaging = FALSE)

  ICs$vals <- ICs$ICs.table[, 2]
  ICs$comments <- ICs$ICs.table[, 3]
  
  loop$ICs <- ICs$ICs.table
  
})

output$ICs_RHT <- renderRHandsontable({
  rhandsontable(ICs$ICs.table,
                rowHeaders = NULL,
                colHeaderWidth = 100,
                stretchH = "all"
                ) %>%
    hot_cols(colWidth = c(30, 30, 90),
             manualColumnMove = FALSE,
             manualColumnResize = TRUE,
             halign = "htCenter",
             valign = "htMiddle",
             renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (row % 2 == 0) {
              td.style.background = 'white';
             } else {
              td.style.background = '#f9f9f9';
             } 
           }") %>%
    hot_col("Variable", readOnly = TRUE) %>%
    hot_col("Description", halign = "htLeft", valign = "htMiddle") %>%
    hot_rows(rowHeights = 40) %>%
    hot_context_menu(allowRowEdit = FALSE,
                     allowColEdit = FALSE
    ) %>%
    hot_validate_numeric(col = 2, min = 0)
})

observeEvent(input$ICs_RHT$changes$changes, {
  xi = input$ICs_RHT$changes$changes[[1]][[1]]
  yi = input$ICs_RHT$changes$changes[[1]][[2]]
  old = input$ICs_RHT$changes$changes[[1]][[3]]
  new = input$ICs_RHT$changes$changes[[1]][[4]]
  
  #copying table to dataframe
  ICs$ICs.table[xi+1, yi+1] <- new
  ICs$vals[xi+1] <- ICs$ICs.table[xi+1, 2]
  ICs$comments[xi+1] <- ICs$ICs.table[xi+1, 3]
  jPrint(ICs$vals)
  jPrint(ICs$comments)
  jPrint(ICs$ICs.table)
  loop$ICs <- ICs$ICs.table
})


################################################################################




output$ICs_UI <- renderUI({
  req(input$createVar_addVarToList)
  number_var = length(vars$species)
  

  fluidRow(column(width = 4
                  ,lapply(seq(number_var), function(i){
                    textInput(inputId = paste0("IC_", as.character(i))
                              ,label = paste(vars$species[i], "initial value:")
                              ,value = ifelse(ICs$first.IC.stored, ICs$vals[i], "0"))
                  }))
           # ,column(width=1
           #         ,lapply(seq(number_parameters), function(i){
           #           checkboxInput(inputId=paste0("parameter_check_unknown_", as.character(i))
           #                     ,label="Value Unknown"
           #                     ,value = FALSE)
           #         }))
           ,column(width = 6
                   ,lapply(seq(number_var), function(i){
                     textInput(inputId = paste0("ICs_description_", as.character(i))
                               ,label = "Comment"
                               ,value = ifelse(ICs$first.IC.stored, ICs$comments[i], ""))
                   })
           )
  ) #end fluidRow
})

observeEvent(input$ICs_store_ICs, {
  ICs$first.IC.stored = TRUE #set boolean so program knows button to store was pressed so render ui will load data instead of blank descriptions
  num_ICs <- length(vars$species)
  
  IC_values <- vector()
  IC_comments <- vector()
  for (i in seq(num_ICs)) {
    single_value <- eval(parse(text = paste0("input$IC_", as.character(i))))
    IC_values <- append(IC_values, single_value)
    
    single_comment <- eval(parse(text = paste0("input$ICs_description_", as.character(i)))) #evaluate value in textinput
    IC_comments <- append(IC_comments, single_comment) #append comments to vector
  }
  IC_values <- paste(IC_values, sep = " ")
  
  ICs$vals <- as.numeric(IC_values)
  ICs$comments <- IC_comments
  
  session$sendCustomMessage(type = 'testmessage',
                            message = 'ICs Stored')
})
