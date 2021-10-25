
#store model options on button press

observeEvent(input$options_store_options, {
  model.options$time.start <- input$execute_time_start
  model.options$time.end <- input$execute_time_end
  model.options$time.step <- input$execute_time_step
  model.options$time.scale.bool <- input$execute_turnOn_time_scale_var
  model.options$time.scale.value <- input$execute_time_scale_var
  model.options$ode.solver.type <- input$execute_ode_solver_type
  
  session$sendCustomMessage(type = 'testmessage',
                            message = 'Options Stored')
})