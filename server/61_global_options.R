# Server for global options

observeEvent(input$GO_base_duration, {
  units$base.values$Duration <- input$GO_base_duration
})

observeEvent(input$GO_base_energy, {
  units$base.values$Energy <- input$GO_base_energy
})

observeEvent(input$GO_base_length, {
  units$base.values$Length <- input$GO_base_length
})

observeEvent(input$GO_base_mass, {
  units$base.values$Mass <- input$GO_base_mass
})

observeEvent(input$GO_base_volume, {
  units$base.values$Volume <- input$GO_base_volume
})

observeEvent(input$GO_base_flow, {
  units$base.values$Flow <- input$GO_base_flow
})

observeEvent(input$GO_base_count, {
  units$base.values$Count <- input$GO_base_count
})

observeEvent(input$GO_species_unit_choice, {
  if (input$GO_species_unit_choice == "Mol") {
    units$base.values$For.Var <- units$base.values$Count
    units$possible.units$For.Var <- units$possible.units$Count
  } else if (input$GO_species_unit_choice == "Mass") {
    units$base.values$For.Var <- units$base.values$Mass
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
})

