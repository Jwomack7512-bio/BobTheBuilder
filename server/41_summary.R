
forPub <- reactiveValues(
  
)

#variable data table 
output$summary_variable_table <- renderDT({ 
  unit.row <- rep("nM", nrow(ICs$ICs.table))
  jPrint(unit.row)
  my.table <- cbind(ICs$ICs.table[,1:2], unit.row)
  jPrint(my.table)
  #my.table <- data.frame(ICs$ICs.table, unit.row)
  #jPrint(my.table)
  
  DT::datatable(my.table,
                class = "cell-border stripe",
                rownames = FALSE,
                colnames = c("Species", "IC", "Units"),
                editable = TRUE,
                options = list(autoWidth = TRUE,
                               columnDefs = list(list(className = "dt-center", targets = "_all")),
                                pageLength = -1,
                                ordering = FALSE,
                                dom = "t",
                                scrollY = "370px",
                                initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': 'white', 'color': 'black', 'font-size': '20px'});",
                                 "}")
                                )
                ) %>%
    formatStyle(columns = c("Variable", "Value", "unit.row"), fontSize = "115%")
  })

#parameter data table
output$summary_parameter_table <- renderDT({ 
  units <- rep("min^-1", (nrow(params$param.table)))
  my.table <- cbind(params$param.table[,1:2], units)
  DT::datatable(my.table,
                class = "cell-border stripe",
                rownames = FALSE,
                colnames = c("Parameter", "Value", "Units"),
                editable = TRUE,
                options = list(autoWidth = TRUE,
                               columnDefs = list(list(className = "dt-center", targets = "_all")),
                               pageLength = -1,
                               ordering = FALSE,
                               dom = "t",
                               scrollY = "370px",
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': 'white', 'color': 'black', 'font-size': '20px'});",
                                 "}")
                                )
                ) %>%
    formatStyle(columns = c("Parameter", "Value", "units"), fontSize = "115%")
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
  print(plotLineplotInput(gatherData(results$model.final)))
})

output$summary_plotly <- renderPlotly({
  data <- gatherData(results$model.final)
  ggplotly(plotLineplotInput(data), tooltip = c("x", "y", "colour"))
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