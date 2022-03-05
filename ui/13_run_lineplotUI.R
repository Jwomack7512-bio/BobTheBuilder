js2 <- paste0(c(
  "var selectinput = document.getElementById('lineplot_yvar');",
  "selectinput.selectize.setValue(-1, false);",
  "selectinput.selectize.selectall();",
  "$('#select + .selectize-control .item').removeClass('active');"),
  collapse = "\n")

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
      # pickerInput(inputId = 'lineplot_yvar',
      #             label = NULL,
      #             choices = character(),
      #             multiple = TRUE,
      #             options = list(
      #               `actions-box` = TRUE,
      #               title = "Variables To Plot")
      #             )
#-------------------------Input Dropdown Button---------------------------------
      dropdownButton(
        label = "Inputs",
        icon = icon("sliders"),
        circle = FALSE,
        status = "dropdownbutton",
        size = "lg",
        # pickerInput(inputId = 'lineplot_xvar',
        #             label = 'x variable',
        #             choices = character()),
        div(id = "form",
            selectizeInput(inputId = 'lineplot_yvar',
                           label = NULL,
                           choices = character(),
                           multiple = TRUE,
                           options = list(
                             placeholder = "Select Variables",
                             plugins = list('remove_button')
                             )
                          )
            ),
        fluidRow(
          column(
            width = 6,
            actionButton("select_all", "Select All", onclick = js2)
          ),
          column(
            width = 6,
            actionButton("reset_input", "Reset")
          )
        )
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
                         status = "dropdownbutton", 
                         right = TRUE,
                         size = "lg",
                         radioGroupButtons(inputId="line_axis_options",
                                           label="Edit",
                                           choices=c("Labels" = "Labels"
                                                     ,"Range" = "Axis Range"
                                                     ,"Size" = "Size"
                                                     ,"Position" = "Position")),
                         conditionalPanel(condition="input.line_axis_options == 'Labels'",
                                          textInput(inputId="line_title", 
                                                    label="Title", 
                                                    value = ""),
                                          textInput(inputId="line_xlabel", 
                                                    label="X Label", 
                                                    value = ""),
                                          textInput(inputId="line_ylabel", 
                                                    label="Y Label", 
                                                    value = "")
                         )
                         ,conditionalPanel(condition="input.line_axis_options=='Axis Range'",
                                           fluidRow(
                                             column(width=4,
                                                    numericInput(inputId="line_xaxis_min", 
                                                                 label="x-axis min",
                                                                 value=1)),
                                             column(width=4,
                                                    numericInput(inputId="line_xaxis_max", 
                                                                 label="x-axis max", 
                                                                 value=10)),
                                             column(width=4,
                                                    numericInput(inputId="line_xstep",
                                                                 label="x step",
                                                                 value=2))
                                           ),#end fluidRow
                                           fluidRow(
                                             column(width=4,
                                                    numericInput(inputId="line_yaxis_min",
                                                                 label="y-axis min", 
                                                                 value=1)),
                                             column(width=4,
                                                    numericInput(inputId="line_yaxis_max", 
                                                                 label="y-axis max", 
                                                                 value=10)),
                                             column(width=4,
                                                    numericInput(inputId="line_ystep",
                                                                 label="y step",
                                                                 value=2))
                                           ),#end fluidRow
                                           switchInput(inputId="line_axis_confirm", 
                                                       label="Change Axis", 
                                                       labelWidth='80px')
                         )#end ConditionalPanel
                         ,conditionalPanel(condition="input.line_axis_options == 'Size'"
                                           ,numericInput(inputId = "line_title_text_size"
                                                         ,label = "title font size"
                                                         ,value = 22
                                                         ,min = 1)
                                           ,hr()
                                           ,numericInput(inputId = "line_x_axis_title_size"
                                                         ,label = "x-axis label font size"
                                                         ,value = 12
                                                         ,min = 1)
                                           ,numericInput(inputId = "line_x_axis_text_size"
                                                         ,label = "x-axis plot text label font size"
                                                         ,value = 10
                                                         ,min = 1)
                                           ,hr()
                                           ,numericInput(inputId = "line_y_axis_title_size"
                                                         ,label = "y-axis label font size"
                                                         ,value = 12
                                                         ,min = 1)
                                           ,numericInput(inputId = "line_y_axis_text_size"
                                                         ,label = "y-axis plot text label font size"
                                                         ,value = 10
                                                         ,min = 1)
                         )
                         ,conditionalPanel(condition="input.line_axis_options=='Position'"
                                           ,sliderInput(inputId = "line_title_location"
                                                        ,label = "Title Position"
                                                        ,min = 0
                                                        ,max = 1
                                                        ,value = 0.5
                                                        ,step = 0.1)
                                           ,sliderInput(inputId = "line_xtitle_location"
                                                        ,label = "x-axis Title Position"
                                                        ,min = 0
                                                        ,max = 1
                                                        ,value = 0.5
                                                        ,step = 0.1)
                                           ,sliderInput(inputId = "line_ytitle_location"
                                                        ,label = "y-axis Title Position"
                                                        ,min = 0
                                                        ,max = 1
                                                        ,value = 0.5
                                                        ,step = 0.1)
                         ) #end conditional panel: Position
          ) #end Dropdown Button
      ), #end div
      div(style = "display:inline-block; text_align:right;",
          #_______________________________________________________________________________
          
          #Dropdown Download Button
          
          #_______________________________________________________________________________
          dropdownButton(label = "Download",
                         icon = icon("download"),
                         circle = FALSE,
                         status = "dropdownbutton",
                         right = TRUE,
                         size = "lg",
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
                                      color = "primary",
                                      size = "sm",
                                      block = FALSE,
                                      no_outline = FALSE)
          ) #end dropdownButton
      ), #end Div
      div(style = "display:inline-block; text_align:right;",
          dropdownButton(label = "Customize", 
                         status = "dropdownbutton",
                         icon = icon("bookmark", lib = "glyphicon"),
                         right = TRUE, 
                         circle = FALSE,
                         size = "lg",
                         sliderInput(inputId = "line_size_options",
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
                          choices = c(),
                          multiple = TRUE)
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
              DTOutput("compare_models_DT")
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
,tags$head(tags$style(HTML(".btn-dropdownbutton {
                      background-color: #343a40 !important;
                      color: white;
                      }
               ")))

)