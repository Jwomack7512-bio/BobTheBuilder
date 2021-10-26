
#store model options on button press

observeEvent(input$options_store_options, {
  options$time.start <- input$execute_time_start
  options$time.end <- input$execute_time_end
  options$time.step <- input$execute_time_step
  options$time.scale.bool <- input$execute_turnOn_time_scale_var
  options$time.scale.value <- input$execute_time_scale_var
  options$ode.solver.type <- input$execute_ode_solver_type
  
  session$sendCustomMessage(type = 'testmessage',
                            message = 'Options Stored')
})