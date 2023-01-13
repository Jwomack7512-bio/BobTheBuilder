

# View IDs ---------------------------------------------------------------------
observeEvent(input$view_ids, {
  print(id$id.df)
  print(id$id.variables)
  print(id$id.parameters)
  print(id$id.equations)
  print(id$id.diffeq)
  print(id$id.compartments)
})

# View Variables ---------------------------------------------------------------
observeEvent(input$view_variables, {
  print(vars$var.info)
  print(vars$compartments.info)
})