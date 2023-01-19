# Server for global options

observeEvent(input$GO_base_duration, {
  units$selected.units$Duration <- input$GO_base_duration
  
  # updatePickerInput(
  #   session = session,
  #   "execute_time_unit",
  #   selected = input$GO_base_duration
  # )
})

observeEvent(input$GO_base_energy, {
  units$selected.units$Energy <- input$GO_base_energy
})

observeEvent(input$GO_base_length, {
  units$selected.units$Length <- input$GO_base_length
})

observeEvent(input$GO_base_mass, {
  units$selected.units$Mass <- input$GO_base_mass
})

observeEvent(input$GO_base_volume, {
  units$selected.units$Volume <- input$GO_base_volume
})

observeEvent(input$GO_base_flow, {
  units$selected.units$Flow <- input$GO_base_flow
})

observeEvent(input$GO_base_count, {
  units$selected.units$Count <- input$GO_base_count
})

observeEvent(input$GO_species_unit_choice, {
  if (input$GO_species_unit_choice == "Mol") {
    units$selected.units$For.Var <- units$base.units$Count
    units$possible.units$For.Var <- units$possible.units$Count
  } else if (input$GO_species_unit_choice == "Mass") {
    units$selected.units$For.Var <- units$base.units$Mass
    units$possible.units$For.Var <- units$possible.units$Mass
  }
})


observe({
  print("selected units")
  print(units$selected.units)
})