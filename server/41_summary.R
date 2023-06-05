
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
  
  # Mathjax Output
  uiOutput("summary_DE_mathjax")
  
  # text.size <- as.character(input$sum_box_size)
  # box(
  #   title =  HTML("<b>Differential Equations</b></font size>"),
  #   width = 12,
  #   div(style = 'height:325px; overflow-y: scroll;',
  #       htmlOutput(outputId = "summary_differential_equations")),
  #   tags$head(
  #     tags$style(
  #       paste0("#summary_differential_equations {font-size:", 
  #              text.size,
  #              "px;}")
  #     )
  #   ),
  #   tags$head(
  #     tags$style(".card-title {font-size:25px}")
  #   )
  # )
})


output$summary_DE_mathjax <- renderUI({
  box(
    id = "box_summary_diff_eqns",
    width = 12,
    title = HTML("<b>Differential Equations</b></font size>"),
    lapply(seq(length(rv.DE$de.equations.list)), function(i){
      div(
        style = "overflow-y:auto",
        withMathJax(
          buildMathjaxEqn(rv.DE$de.equations.list[[i]],
                          i,
                          rv.DE$de.equations.list[[i]]$Compartment.vol,
                          TRUE)
        )
      )
    })
  )
})

output$summary_differential_equations <- renderText({
  # paste(paste0('d(', rv.SPECIES$species.names, ")/dt = ", DE$de.eqns),
  # collapse="<br><br>")
  
  if (length(rv.DE$de.equations.list) == 0) {
    "No Solved Differential Equations"
  }
  else {
    # Get species names 
    spec.names <- unname(sapply(rv.DE$de.equations.list,
                                get,
                                x = "Name"))
    diff.eqns <- unname(sapply(rv.DE$de.equations.list,
                               get, 
                               x = "ODES.eqn.string"))
    
    eqns_to_display <- c()
    for (i in seq_along(rv.DE$de.equations.list)) {
      if (input$diffeq_option_simplify) {
        new_eqn <- paste0("(",i, ") ", 
                          'd(', spec.names[i], ")/dt = ", 
                          Deriv::Simplify(diff.eqns[i]))
      } else {
        new_eqn <- paste0("(",i, ") ", 
                          'd(', spec.names[i], ")/dt = ", 
                          diff.eqns[i])
      }
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse = "<br><br>")
  }
})

# Variable Summary -------------------------------------------------------------
output$summary_variable_table <- renderDT({ 
  
  # Build Variable Table
  my.table <- rv.SPECIES$species.df %>%
    select(Name, Value, Unit)
  
  colnames(my.table) <- c("Species", "Value", "Unit")
  
  font.size <- paste0(as.character(input$sum_table_font_size), "%")
  
  DT::datatable(
    my.table,
    class = "cell-border stripe",
    rownames = FALSE,
    editable = TRUE,
    options = list(
      autoWidth = TRUE,
      # colnames = c("Species", "Value", "Unit"),
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
    columns = c("Species", "Value", "Unit"), fontSize = font.size)
  })

# Parameter Summary ------------------------------------------------------------
output$summary_parameter_table <- renderDT({ 
  # Build Paramter Table
  my.table <- rv.PARAMETERS$parameters.df %>%
    select("Name", "Value", "Unit")
  
  colnames(my.table) <- c("Parameter", "Value", "Unit")
  
  font.size <- paste0(as.character(input$sum_table_font_size), "%")
  
  DT::datatable(
    my.table,
    class = "cell-border stripe",
    rownames = FALSE,
    # colnames = c("Parameter", "Value", "Unit"),
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
  formatStyle(columns = c("Parameter", "Value", "Unit"), fontSize = font.size)
})



# Plot Summary -----------------------------------------------------------------
output$summary_plot <- renderPlot({
  to.plot <- CreatePlot(rv.RESULTS$results.model.final,
                        input$lineplot_yvar,
                        input$choose_color_palette,
                        input$line_size_options,
                        input$line_legend_title,
                        input$line_show_dots,
                        input$line_axis_confirm,
                        input$line_xaxis_min,
                        input$line_xaxis_max,
                        input$line_xstep,
                        input$line_yaxis_min,
                        input$line_yaxis_max,
                        input$line_ystep,
                        input$line_title,
                        input$line_xlabel,
                        input$line_xtitle_location,
                        input$line_axis_text_size,
                        input$line_axis_title_size,
                        input$line_ylabel,
                        input$line_ytitle_location,
                        input$line_axis_text_size,
                        input$line_axis_title_size,
                        input$line_title_text_size,
                        input$line_title_location,
                        input$line_legend_position,
                        input$line_legend_title_size,
                        input$line_legend_font_size,
                        input$line_panel_colorPicker_checkbox,
                        input$line_panel_colorPicker,
                        input$line_plotBackground_color_change,
                        input$line_plotBackground_colorPicker,
                        input$show_overlay_data,
                        data.scatter(),
                        input$plot_data_import_x,
                        input$plot_data_import_y)
})

output$summary_plotly <- renderPlotly({
  to.plot <- CreatePlot(rv.RESULTS$results.model.final,
                        input$lineplot_yvar,
                        input$choose_color_palette,
                        input$line_size_options,
                        input$line_legend_title,
                        input$line_show_dots,
                        input$line_axis_confirm,
                        input$line_xaxis_min,
                        input$line_xaxis_max,
                        input$line_xstep,
                        input$line_yaxis_min,
                        input$line_yaxis_max,
                        input$line_ystep,
                        input$line_title,
                        input$line_xlabel,
                        input$line_xtitle_location,
                        input$line_axis_text_size,
                        input$line_axis_title_size,
                        input$line_ylabel,
                        input$line_ytitle_location,
                        input$line_axis_text_size,
                        input$line_axis_title_size,
                        input$line_title_text_size,
                        input$line_title_location,
                        input$line_legend_position,
                        input$line_legend_title_size,
                        input$line_legend_font_size,
                        input$line_panel_colorPicker_checkbox,
                        input$line_panel_colorPicker,
                        input$line_plotBackground_color_change,
                        input$line_plotBackground_colorPicker,
                        input$show_overlay_data,
                        data.scatter(),
                        input$plot_data_import_x,
                        input$plot_data_import_y
  )
  
  ggplotly(to.plot, 
           tooltip = c("x", "y", "colour"))
})


