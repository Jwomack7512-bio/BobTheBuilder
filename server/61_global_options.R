# Server for global options

observeEvent(input$GO_base_duration, {
  units$base.values[1] <- input$GO_base_duration
})

observeEvent(input$GO_base_energy, {
  units$base.values[2] <- input$GO_base_energy
})

observeEvent(input$GO_base_length, {
  units$base.values[3] <- input$GO_base_length
})

observeEvent(input$GO_base_mass, {
  units$base.values[4] <- input$GO_base_mass
})

observeEvent(input$GO_base_volume, {
  units$base.values[5] <- input$GO_base_volume
})

observeEvent(input$GO_base_flow, {
  units$base.values[6] <- input$GO_base_flow
})

observeEvent(input$GO_base_count, {
  units$base.values[7] <- input$GO_base_count
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

