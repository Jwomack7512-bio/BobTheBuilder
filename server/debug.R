
# View Variables ---------------------------------------------------------------
observeEvent(input$debug_view_variables, {
  print(rv.SPECIES$species)
  logs$variable.debug.button <- print(rv.SPECIES$species)
  logs$variable.debug.table  <- rv.SPECIES$species.df
})

observeEvent(input$debug_view_compartments, {
  print(rv.COMPARTMENTS$compartments)
  logs$variable.debug.button <- print(rv.COMPARTMENTS$compartments)
  logs$variable.debug.table  <- rv.COMPARTMENTS$compartments.df
})

observeEvent(input$debug_view_equations, {
  print(rv.REACTIONS$reactions.df)
  print("Equations: Chemical Dataframe")
  print(rv.REACTIONS$massAction.df)
  print("Equations: Enzyme Dataframe")
  print(rv.REACTIONS$michaelisMenten.df)
  print("Equations: Synthesis Dataframe")
  print(rv.REACTIONS$synthesis.df)
  print("Equations: Degradation Dataframe")
  print(rv.REACTIONS$degradation.df)
  print("Equations: Main Latex Dataframe")
  print(rv.REACTIONS$eqn.main.latex)
  print("Equations: Additional Equations Dataframe")
  print(rv.REACTIONS$additional.eqns)
  
  logs$variable.debug.button <- print(rv.REACTIONS$eqn.info)
  logs$variable.debug.table  <- rv.REACTIONS$reactions.df
})

observeEvent(input$debug_view_ids, {
  print(id$id.df)
  logs$variable.debug.button <- print(id$id.df)
  logs$variable.debug.table  <- id$id.df
})

# Debug Input Output
observeEvent(input$debug_view_IO, {
  logs$variable.debug.button <- print(rv.IO$rv.IO$InputOutput)
  logs$variable.debug.table  <- rv.IO$IO.df
})

observeEvent(input$debug_view_parameters, {
  print(rv.PARAMETERS$parameters)
  logs$variable.debug.button <- print(rv.PARAMETERS$parameters)
  logs$variable.debug.table  <- rv.PARAMETERS$parameters.df
})

output$debug_text_view <- renderPrint(
  print(logs$variable.debug.button)
)

output$debug_table_view <- renderRHandsontable(
  rhandsontable(logs$variable.debug.table,
                width = "100%",
                readOnly = TRUE)
)
