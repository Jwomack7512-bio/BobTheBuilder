


#variable data table 
output$summary_variable_table <- renderDT({ 
  DT::datatable(ICs$ICs.table[, 1:2],
                class = "cell-border stripe",
                rownames = FALSE,
                colnames = c("Species", "Conc"),
                options = list(autoWidth = TRUE,
                                pageLength = -1,
                                ordering = FALSE,
                                dom = "t",
                                scrollY = "370px",
                                initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': 'white', 'color': 'black'});",
                                 "}")
                                )
                )
  })

#parameter data table
output$summary_parameter_table <- renderDT({ 
  DT::datatable(params$param.table[, 1:2],
                class = "cell-border stripe",
                rownames = FALSE,
                options = list(autoWidth = TRUE,
                               pageLength = -1,
                               ordering = FALSE,
                               dom = "t",
                               scrollY = "370px",
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': 'white', 'color': 'black'});",
                                 "}")
                                )
                )
})

#differential equations viewer
output$summary_differential_equations <- renderText({
  # paste(paste0('d(', vars$species, ")/dt = ", DE$eqns), collapse="<br><br>")
  
  if (length(vars$species) == 0) {
    "No variables entered"
  }
  else {
    n_eqns = length(vars$species)
    eqns_to_display <- c()
    for (i in seq(n_eqns)) {
      if (input$diffeq_option_simplify) {
        new_eqn <- paste0("(",i, ") ", 'd(', vars$species[i], ")/dt = ", Deriv::Simplify(DE$eqns[i]))
      } else {
        new_eqn <- paste0("(",i, ") ", 'd(', vars$species[i], ")/dt = ", DE$eqns[i])
      }
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse = "<br><br>")
  }
})

#plot summary
output$summary_plot <- renderPlot({
  print(plotLineplotInput(gatherData(ModelToUse())))
})

output$summary_reaction_equations <- renderText({
  if (length(eqns$main) == 0) {
    paste("No equations entered")
  } else {
    n_eqns = seq(length(eqns$main))
    eqns_to_display <- c()
    for (i in n_eqns) {
      new_eqn <- paste0("(",i, ") ", eqns$main[i])
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse = "<br><br>")
  }
})