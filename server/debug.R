
# View Variables ---------------------------------------------------------------
observeEvent(input$debug_view_variables, {

  updatePickerInput(
    session = session,
    inputId = "debug_filter_searchType",
    choices = c("Overall (Species)")
  )
  rv.DEBUG$button_pressed_last <- "Species"
  
})

observeEvent(input$debug_view_compartments, {
  updatePickerInput(
    session = session,
    inputId = "debug_filter_searchType",
    choices = c("Overall (Compartments)")
  )
  rv.DEBUG$button_pressed_last <- "Compartments"
})

observeEvent(input$debug_view_equations, {
  
  updatePickerInput(
    session = session,
    inputId = "debug_filter_searchType",
    choices = c("Overall", 
                "Mass Action", 
                "Synthesis"),
    selected = "Overall"
  )
  rv.DEBUG$button_pressed_last <- "Equations"
})

observeEvent(input$debug_view_ids, {
  updatePickerInput(
    session = session,
    inputId = "debug_filter_searchType",
    choices = c("Overall (IDs)")
  )
  rv.DEBUG$button_pressed_last <- "Ids"
})

# Debug Input Output
observeEvent(input$debug_view_IO, {
  updatePickerInput(
    session = session,
    inputId = "debug_filter_searchType",
    choices = c("Overall (IO)",
                "Flow.In",
                "Flow.Out",
                "Flow.Between",
                "Clearance",
                "Simple.Diffusion",
                "Facilitated.Diffusion")
  )
  rv.DEBUG$button_pressed_last <- "IO"
})

observeEvent(input$debug_view_parameters, {
  updatePickerInput(
    session = session,
    inputId = "debug_filter_searchType",
    choices = c("Overall (Parameters)")
  )
  rv.DEBUG$button_pressed_last <- "Parameters"
  
  print(rv.PARAMETERS$parameters)

})

observeEvent(input$debug_filter_searchType, {
  print(input$debug_filter_searchType)
  if (rv.DEBUG$button_pressed_last == "Equations") {
    if (input$debug_filter_searchType == "Overall") {
      rv.LOGS$variable.debug.button <- print(rv.REACTIONS$reactions)
      rv.LOGS$variable.debug.table  <- rv.REACTIONS$reactions.df
    } else if (input$debug_filter_searchType == "Mass Action") {
      rv.LOGS$variable.debug.button <- print(rv.REACTIONS$massAction)
      rv.LOGS$variable.debug.table  <- rv.REACTIONS$massAction.df
    } else if (input$debug_filter_searchType == "Synthesis") {
      rv.LOGS$variable.debug.button <- print(rv.REACTIONS$synthesis)
      rv.LOGS$variable.debug.table  <- rv.REACTIONS$synthesis.df
    }
  } else if (rv.DEBUG$button_pressed_last == "Compartments") {
    rv.LOGS$variable.debug.button <- print(rv.COMPARTMENTS$compartments)
    rv.LOGS$variable.debug.table  <- rv.COMPARTMENTS$compartments.df
  } else if (rv.DEBUG$button_pressed_last == "Species") {
    rv.LOGS$variable.debug.button <- print(rv.SPECIES$species)
    rv.LOGS$variable.debug.table  <- rv.SPECIES$species.df
  } else if (rv.DEBUG$button_pressed_last == "Ids") {
    rv.LOGS$variable.debug.button <- print(rv.ID$id.df)
    rv.LOGS$variable.debug.table  <- rv.ID$id.df
  } else if (rv.DEBUG$button_pressed_last == "IO") {
    if (input$debug_filter_searchType == "Overall (IO)") {
      rv.LOGS$variable.debug.button <- print(rv.IO$InputOutput)
      rv.LOGS$variable.debug.table  <- rv.IO$IO.df
    } else if (input$debug_filter_searchType == "Flow.In") {
      rv.LOGS$variable.debug.button <- print(rv.IO$Flow.In)
      rv.LOGS$variable.debug.table  <- rv.IO$IO.df
    } else if (input$debug_filter_searchType == "Flow.Out") {
      rv.LOGS$variable.debug.button <- print(rv.IO$Flow.Out)
      rv.LOGS$variable.debug.table  <- rv.IO$IO.df
    } else if (input$debug_filter_searchType == "Flow.Between") {
      rv.LOGS$variable.debug.button <- print(rv.IO$Flow.Between)
      rv.LOGS$variable.debug.table  <- rv.IO$IO.df
    } else if (input$debug_filter_searchType == "Clearance") {
      rv.LOGS$variable.debug.button <- print(rv.IO$Clearance)
      rv.LOGS$variable.debug.table  <- rv.IO$IO.df
    } else if (input$debug_filter_searchType == "Simple.Diffusion") {
      rv.LOGS$variable.debug.button <- print(rv.IO$Simple.Diffusion)
      rv.LOGS$variable.debug.table  <- rv.IO$IO.df
    } else if (input$debug_filter_searchType == "Facilitated.Diffusion") {
      rv.LOGS$variable.debug.button <- print(rv.IO$Facilitated.Diffusion)
      rv.LOGS$variable.debug.table  <- rv.IO$IO.df
    }
  } else if (rv.DEBUG$button_pressed_last == "Parameters") {
    rv.LOGS$variable.debug.button <- print(rv.PARAMETERS$parameters)
    rv.LOGS$variable.debug.table  <- rv.PARAMETERS$parameters.df
  }
  
})


output$debug_text_view <- renderPrint(
  print(rv.LOGS$variable.debug.button)
)

output$debug_table_view <- renderRHandsontable(
  rhandsontable(rv.LOGS$variable.debug.table,
                width = "100%",
                readOnly = TRUE)
)
