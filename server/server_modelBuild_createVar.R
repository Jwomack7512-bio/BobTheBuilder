
observeEvent(input$createVar_addVarToList, {
  if (input$createVar_varInput == "")
  {
    #nothing happens if a blank space is added
  }
  else if (input$createVar_varInput %in% vars$species) #var already exists in model, let user know
  {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'This variable is already used')
    updateTextInput(session = session
                    ,'createVar_varInput'
                    ,value = "")
  }
  else
  {
    #store selected variable to list of variables
    vars$species <- append(vars$species, 
                                      input$createVar_varInput)
    #reset text input to blank when variable entered
    updateTextInput(session = session
                    ,'createVar_varInput'
                    ,value = "")
  }
})

observeEvent(input$createVar_removeVarFromList, {
  vars$species <- vars$species[-length(vars$species)]
  
  updatePickerInput(session
                    ,"InOut_selectVar"
                    ,choices = vars$species)
})

output$createVar_displayVars <- renderText({
  paste(vars$species, collapse = "<br>")
})

