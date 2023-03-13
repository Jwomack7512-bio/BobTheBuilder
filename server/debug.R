
# View Variables ---------------------------------------------------------------
observeEvent(input$debug_view_variables, {
  print(vars$var.info)
  logs$variable.debug.button <- print(vars$var.info)
  logs$variable.debug.table  <- vars$var.df
})

observeEvent(input$debug_view_compartments, {
  print(vars$compartments.info)
  logs$variable.debug.button <- print(vars$compartments.info)
  logs$variable.debug.table  <- vars$compartments.df
})

observeEvent(input$debug_view_equations, {
  print(eqns$eqn.info)
  print("Equations: Chemical Dataframe")
  print(eqns$eqn.chem)
  print("Equations: Enzyme Dataframe")
  print(eqns$eqn.enzyme)
  print("Equations: Synthesis Dataframe")
  print(eqns$eqn.syn)
  print("Equations: Degradation Dataframe")
  print(eqns$eqn.deg)
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
  print("IO")
  print(IO)
  print("IO.info")
  print(IO$IO.info)
  print("IO.df")
  print(IO$IO.df)
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
