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
  PrintVar(input$GO_base_duration)
  PrintVar(input$GO_base_energy)
  PrintVar(input$GO_base_length)
  PrintVar(input$GO_base_mass)
  PrintVar(input$GO_base_volume)
  PrintVar(input$GO_base_flow)
  PrintVar(input$GO_base_count)
  
  PrintVar(units$base.units$Duration)
  PrintVar(units$base.units$Mass)
  PrintVar(units$base.units$Volume)
  PrintVar(units$base.units$Count)
  PrintVar(units$base.units$For.Var)
  
  PrintVar(units$selected.units$Duration)
  PrintVar(units$selected.units$Mass)
  PrintVar(units$selected.units$Volume)
  PrintVar(units$selected.units$Count)
  PrintVar(units$selected.units$For.Var)
})

