
# Equation Summary -------------------------------------------------------------
output$ReactionEquationsBox <- renderUI({
  text.size <- as.character(input$sum_box_size)
  box(
    title = HTML("<b>Reaction Equations</b>"),
    width = 12,
    div(style = 'height:370px;
                  overflow-y: scroll;',
        htmlOutput(outputId = "summary_reaction_equations")),
    tags$head(
    tags$style(paste0("#summary_reaction_equations {
                             font-size:", text.size, "px;
                                             }")
    )
    )
  )
})

output$summary_reaction_equations <- renderText({
  # Grab the reactiosn
  to.display <- unname(sapply(rv.REACTIONS$reactions,
                              get,
                              x = "Equation.Text"))
  
  if (length(to.display) == 0) {
    paste("No equations entered")
  } else {
    eqns_to_display <- c()
    for (i in seq_along(to.display)) {
      new_eqn <- paste0("(",i, ") ", to.display[i])
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse = "<br><br>")
  }
})

# Differential Equations Summary -----------------------------------------------
output$DifferentialEquationsBox <- renderUI({
  text.size <- as.character(input$sum_box_size)
  
  box(
    title =  HTML("<b>Differential Equations</b></font size>"),
    width = 12,
    div(style = 'height:325px;
                                           overflow-y: scroll;',
        htmlOutput(outputId = "summary_differential_equations")),
    tags$head(
      tags$style(paste0("#summary_differential_equations {
                                                      font-size:", text.size,"px;

                                                                      };")
      )
    ),
    tags$head(
      tags$style(".card-title {font-size:25px};"
      )
    )
    
  )
})

#differential equations viewer
output$summary_differential_equations <- renderText({
  # paste(paste0('d(', rv.SPECIES$species.names, ")/dt = ", DE$de.eqns),
  # collapse="<br><br>")
  
  if (length(rv.SPECIES$species.names) == 0) {
    "No variables entered"
  }
  else {
    n_eqns = length(rv.SPECIES$species.names)
    eqns_to_display <- c()
    for (i in seq(n_eqns)) {
      if (input$diffeq_option_simplify) {
        new_eqn <- paste0("(",i, ") ", 
                          'd(', rv.SPECIES$species.names[i], ")/dt = ", 
                          Deriv::Simplify(DE$de.eqns[i]))
      } else {
        new_eqn <- paste0("(",i, ") ", 
                          'd(', rv.SPECIES$species.names[i], ")/dt = ", 
                          DE$de.eqns[i])
      }
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse = "<br><br>")
  }
})

# Variable Summary -------------------------------------------------------------
output$summary_variable_table <- renderDT({ 
  unit.row <- rep("nM", nrow(ICs$ICs.table))
  my.table <- cbind(ICs$ICs.table[,1:2], unit.row)
  #my.table <- data.frame(ICs$ICs.table, unit.row)
  font.size <- paste0(as.character(input$sum_table_font_size), "%")
  
  DT::datatable(
    my.table,
    class = "cell-border stripe",
    rownames = FALSE,
    colnames = c("Species", "IC", "Units"),
    editable = TRUE,
    options = list(
      autoWidth = TRUE,
      columnDefs = list(list(className = "dt-center", targets = "_all")),
      pageLength = -1,
      ordering = FALSE,
      dom = "t",
      scrollY = "370px",
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': 'white', 
        'color': 'black', 'font-size': '25px'});",
        "}"
      )
    )
  ) %>%
  formatStyle(
    columns = c("Variable", "Value", "unit.row"), fontSize = font.size)
  })

# Parameter Summary ------------------------------------------------------------
output$summary_parameter_table <- renderDT({ 
  units <- rep("min^-1", (nrow(rv.PARAMETERS$param.table)))
  my.table <- cbind(rv.PARAMETERS$param.table[,1:2], units)
  
  font.size <- paste0(as.character(input$sum_table_font_size), "%")
  
  DT::datatable(
    my.table,
    class = "cell-border stripe",
    rownames = FALSE,
    colnames = c("Parameter", "Value", "Units"),
    editable = TRUE,
    options = list(
      autoWidth = TRUE,
      columnDefs = list(list(className = "dt-center", targets = "_all")),
      pageLength = -1,
      ordering = FALSE,
      dom = "t",
      scrollY = "370px",
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': 'white', 
        'color': 'black', 'font-size': '25px'});",
        "}"
      )
    )
  ) %>% 
  formatStyle(columns = c("Parameter", "Value", "units"), fontSize = font.size)
})



# Plot Summary -----------------------------------------------------------------
output$summary_plot <- renderPlot({
  print(plotLineplotInput(gatherData(rv.RESULTS$model.final)))
})

output$summary_plotly <- renderPlotly({
  data <- gatherData(rv.RESULTS$model.final)
  ggplotly(plotLineplotInput(data), tooltip = c("x", "y", "colour"))
})


