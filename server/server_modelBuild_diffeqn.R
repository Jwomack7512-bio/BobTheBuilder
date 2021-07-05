############################## DiffEQ Server #################################
source("./server/generating_diffEqs_functions.R") #load functions to solve differential equations

observeEvent(input$diffeq_generate_equations, {
  # observe({
  #   print(data$eqn_info)
  #   print(rv$vars_in_model)
  # })
  rv$diffEQs <- calc_differential_equations(data$eqn_info, rv$vars_in_model, rv$inputOutputs_df, rv$In_out_added)
  
})

output$diffeq_display_diffEqs <- renderText({
  # paste(paste0('d(', rv$vars_in_model, ")/dt = ", rv$diffEQs), collapse="<br><br>")
  
  if(length(rv$vars_in_model)==0)
  {
    "No variables entered"
  }
  else
  {
    n_eqns = length(rv$vars_in_model)
    eqns_to_display <- c()
    for(i in seq(n_eqns))
    {
      new_eqn <- paste0("(",i, ") ", 'd(', rv$vars_in_model[i], ")/dt = ", rv$diffEQs[i])
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse="<br><br>")
  }
})

