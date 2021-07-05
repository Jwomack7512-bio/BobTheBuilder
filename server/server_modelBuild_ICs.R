############################### ICs Server ###################################
output$ICs_UI <- renderUI({
  req(input$createVar_addVarToList)
  number_var = length(rv$vars_in_model)
  

  fluidRow(column(width=4
                  ,lapply(seq(number_var), function(i){
                    textInput(inputId=paste0("IC_", as.character(i))
                              ,label=paste(rv$vars_in_model[i], "initial value:")
                              ,value = ifelse(rv$first_IC_stored, rv$IC_values[i], "0"))
                  }))
           # ,column(width=1
           #         ,lapply(seq(number_parameters), function(i){
           #           checkboxInput(inputId=paste0("parameter_check_unknown_", as.character(i))
           #                     ,label="Value Unknown"
           #                     ,value = FALSE)
           #         }))
           ,column(width=6
                   ,lapply(seq(number_var), function(i){
                     textInput(inputId=paste0("ICs_description_", as.character(i))
                               ,label="Comment"
                               ,value = ifelse(rv$first_IC_stored, rv$IC_descriptions[i], ""))
                   })
           )
  ) #end fluidRow


})

observeEvent(input$ICs_store_ICs, {
  rv$first_IC_stored = TRUE #set boolean so program knows button to store was pressed so render ui will load data instead of blank descriptions
  num_ICs<- length(rv$vars_in_model)
  
  IC_values <- vector()
  IC_comments <- vector()
  for(i in seq(num_ICs)){
    single_value <- eval(parse(text=paste0("input$IC_", as.character(i))))
    IC_values <- append(IC_values, single_value)
    
    single_comment <- eval(parse(text=paste0("input$ICs_description_", as.character(i)))) #evaluate value in textinput
    IC_comments <- append(IC_comments, single_comment) #append comments to vector
  }
  IC_values <- paste(IC_values, sep=" ")
  
  rv$IC_values <- as.numeric(IC_values)
  rv$IC_descriptions <- IC_comments
  
  session$sendCustomMessage(type = 'testmessage',
                            message = 'ICs Stored')
})