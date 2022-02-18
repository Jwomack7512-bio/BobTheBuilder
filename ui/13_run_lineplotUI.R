

TAB_RUN_LINEPLOT <- tabItem(
  tabName = "TAB_RUN_LINEPLOT",
  fluidRow(
    column(
      width = 3,
      pickerInput(inputId = "lineplot_choose_plot_mode"
                   ,label = "Choose Plot Mode"
                   ,choices = c("Normal Plot" = "normal_plot"
                                ,"Loop Mode" = "loop_mode"
                                ,"Side-by-Side Comparison" = "compare_mode"
                                ,"Overlay Data" = "overlay_data_mode"))
    )
  ),  
  br(),
  fluidRow(
    column(
      width = 6,
#-------------------------Input Dropdown Button---------------------------------
      dropdownButton(
        label = "Inputs",
        icon = icon("sliders"),
        circle = FALSE,
        status = "success",
        width = "400px",
        selectInput(inputId = 'lineplot_xvar',
                    label = 'x variable',
                    choices = character()),
        selectInput(inputId = 'lineplot_yvar',
                    label = 'y variable(s)',
                    choices = character(),
                    multiple = TRUE)
      ) #endDropDown
    ), #end column width=6
    column(
      width = 6,
#______________________Dropdown Axis Options Button______________________________
      #These divs are used to place the buttons inline with eachother in the column.
      div(style = "display:inline-block; text_align:right;",
          #OPTIONS BUTTON
          dropdownButton(label = "Axis Options",
                         icon = icon("gear"), 
                         circle = FALSE, 
                         status = "success", 
                         right = TRUE,
                         textInput(inputId = "line_title",
                                   label = "Title",
                                   value = ""),
                         conditionalPanel(condition = "input.lineplot_comparision_mode",
                                          textInput(inputId = "line_title_comparisonPlot",
                                                    label = "Title for Plot 2",
                                                    value = "")
                         ),
                         textInput(inputId = "line_xlabel",
                                   label = "X Label",
                                   value = ""),
                         textInput(inputId = "line_ylabel",
                                   label = "Y Label",
                                   value = "")
          ) #end Dropdown Button
      ), #end div
      div(style = "display:inline-block; text_align:right;",
          #_______________________________________________________________________________
          
          #Dropdown Download Button
          
          #_______________________________________________________________________________
          dropdownButton(label = "Download",
                         icon = icon("download"),
                         circle = FALSE,
                         status = "success",
                         right = TRUE,
                         textInput(
                           inputId = "line_download_title",
                           label = NULL,
                           value = "",
                           placeholder = "Type Download TItle",
                           width = NULL
                         ),
                         radioGroupButtons(
                           inputId = "line_download_radiobuttons",
                           label = NULL,
                           choices = c(".jpg",
                                       ".png", ".pdf"),
                           individual = TRUE,
                           checkIcon = list(
                             yes = tags$i(class = "fa fa-circle",
                                          style = "color: steelblue"),
                             no = tags$i(class = "fa fa-circle-o",
                                         style = "color: steelblue"))
                         ),
                         downloadBttn(outputId = "downloadLine",
                                      label = "Download",
                                      style = "unite",
                                      color = "success",
                                      size = "sm",
                                      block = FALSE,
                                      no_outline = FALSE)
          ) #end dropdownButton
      ), #end Div
      div(style = "display:inline-block; text_align:right;",
          dropdownButton(label = "Customize", status = "success", right = TRUE, circle = FALSE
                         ,sliderInput(inputId = "line_size_options",
                                      label = "Size of Lines",
                                      min = 0,
                                      max = 3,
                                      step = 0.2,
                                      value = 1)
                         ,prettyCheckbox(inputId = "line_show_dots",
                                         label = "Show Points",
                                         value = FALSE)
          )
      )
      ,align = 'right'
    )#end Column width=6
  )#end FluidRow
  ,fluidRow(
      column(
        width = 12,
        conditionalPanel(
          condition = "input.lineplot_choose_plot_mode == 'normal_plot'",
          jqui_resizable(plotOutput("LinePlot"))
      )
    ),
    column(width = 6,
           conditionalPanel(
             condition = "input.lineplot_choose_plot_mode == 'compare_mode'",
             jqui_resizable(plotOutput("LinePlot_compare1"))
           )
          ),
    column(width = 6,
           conditionalPanel(
             condition = "input.lineplot_choose_plot_mode == 'compare_mode'",
             jqui_resizable(plotOutput("LinePlot_compare2"))
           )
    )
  )
  ,br()
  ,fluidRow(
    column(
      width = 12,
      conditionalPanel(
        condition = "input.lineplot_choose_plot_mode == 'compare_mode'",
        box(
          title = NULL,
          width = 12,
          collapsible = FALSE,
          fluidRow(
            column(
              width = 3,
              pickerInput(inputId = "compare_models_select_vars",
                          label = "Select variables to compare",
                          choices = c())
            ),
            column(
              width = 3,
              offset = 6,
              actionBttn(inputId = "run_compared_model",
                         label = "Solve Models")
            )
          ),
          fluidRow(
            column(
              width = 12,
              
            )
          )
        )
      )
    )
  )
#----------------------Options Below Plot---------------------------------------
  ,fluidRow(
    column(
      width = 12,
      tabBox(
        title = NULL,
        width = 12,
        #____________________________________
        #Color Options
        #____________________________________
        tabPanel("Color Options",
                 fluidRow(
                   #add line color options
                   column(width = 12,
                          fluidRow(
                            column(width = 3,
                                   uiOutput("line_color_options_popdown")),
                            column(width = 3,
                                   uiOutput("line_type_options_popdown")))
                   ) #end column
                 ) #end fluidRow
        ), #end tabPanel
        #____________________________________
        #Background Options
        #____________________________________                        
        tabPanel("Background Options",
                 fluidRow(
                   column(width = 5,
                          selectInput(
                            inputId = "theme_output_line",
                            label = "Background Theme", 
                            choices = c("gray"
                                        ,"bw"
                                        ,"linedraw"
                                        ,"light"
                                        ,"minimal"
                                        ,"classic"
                                        ,"void"
                                        ,"dark")
                          ),
                          fluidRow(
                            div(style = "display:inline-block; text_align:right;", 
                                prettyCheckbox(inputId = "line_panel_colorPicker_checkbox", 
                                               label = NULL, 
                                               value = FALSE)),
                            div(style = "display:inline-block; text_align:right;", 
                                colourInput(inputId = "line_panel_colorPicker", 
                                            label = "Select Color", 
                                            value = "grey"))))
                 )   
        ),#end tabPanel
        #____________________________________
        #Legend Options
        #____________________________________
        tabPanel("Legend Options",
                 fluidRow(
                   column(width = 5,
                          selectInput(inputId = "line_legend_position",
                                      label = "Location of Legend",
                                      choices = c("Left" = "left", 
                                                  "Right" = "right", 
                                                  "Top" = "top", 
                                                  "Bottom" = "bottom", 
                                                  "No Legend" = "none"),
                                      selected = "right"),
                          textInput(inputId = "line_legend_title",
                                    label = "Legend Title",
                                    value = "")))
        ) #end tabPanel
      )#End tabBox
    )
  )

)