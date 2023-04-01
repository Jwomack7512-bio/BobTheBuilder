
# View Variables ---------------------------------------------------------------
observeEvent(input$debug_view_variables, {
  print(vars$var.info)
  logs$variable.debug.button <- print(vars$var.info)
  logs$variable.debug.table  <- vars$var.df
})

observeEvent(input$debug_view_compartments, {
  print(rv.COMPARTMENTS$compartments)
  logs$variable.debug.button <- print(rv.COMPARTMENTS$compartments)
  logs$variable.debug.table  <- rv.COMPARTMENTS$compartments.df
})

observeEvent(input$debug_view_equations, {
  print(eqns$eqn.info.df)
  print("Equations: Chemical Dataframe")
  print(eqns$eqn.info.chem.df)
  print("Equations: Enzyme Dataframe")
  print(eqns$eqn.info.enz.df)
  print("Equations: Synthesis Dataframe")
  print(eqns$eqn.info.syn.df)
  print("Equations: Degradation Dataframe")
  print(eqns$eqn.info.deg.df)
  print("Equations: Main Latex Dataframe")
  print(eqns$eqn.main.latex)
  print("Equations: Additional Equations Dataframe")
  print(eqns$additional.eqns)
  
  logs$variable.debug.button <- print(eqns$eqn.info)
  logs$variable.debug.table  <- eqns$eqn.info.df
})

observeEvent(input$debug_view_ids, {
  print(id$id.df)
  logs$variable.debug.button <- print(id$id.df)
  logs$variable.debug.table  <- id$id.df
})

# Debug Input Output
observeEvent(input$debug_view_IO, {
  logs$variable.debug.button <- print(IO$IO.info)
  logs$variable.debug.table  <- IO$IO.df
})

observeEvent(input$debug_view_parameters, {
  print(params$par.info)
  logs$variable.debug.button <- print(params$par.info)
  logs$variable.debug.table  <- params$par.df
})

output$debug_text_view <- renderPrint(
  print(logs$variable.debug.button)
)

output$debug_table_view <- renderRHandsontable(
  rhandsontable(logs$variable.debug.table,
                width = "100%",
                readOnly = TRUE)
)
