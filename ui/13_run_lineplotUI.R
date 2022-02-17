#This tab corresponds to the "BoxPlot" SubTab (under Graphing Tab) of the App
#-------------------------------------------------------------------------
#  Justin Womack
#  July 28, 2020
#  Last Update: August 15, 2020
#  ZSPH/MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------
#__________________________________________________________________________________________________________
#SUBTAB: BOXPLOT
#__________________________________________________________________________________________________________
TAB_RUN_LINEPLOT <- tabItem(tabName = "TAB_RUN_LINEPLOT"
                            ,fluidRow(column(width = 3
                                             ,tags$h2("Line Plot"))
                                      ,column(width = 2
                                              ,offset = 6
                                              ,pickerInput(inputId = "lineplot_choose_plot_mode"
                                                          ,label = "Choose Plot Mode"
                                                          ,choices = c("Normal Plot" = "normal_plot"
                                                                       ,"Loop Mode" = "loop_mode"
                                                                       ,"Side-by-Side Comparison" = "compare_mode"
                                                                       ,"Overlay Data" = "overlay_data_mode"))
                                              ))  
                            
                            ,br()
                            ,fluidRow(
                              column(width = 12,
                                     fluidRow(
                                       column(width = 6,
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
                                       column(width = 6,
                                              #____________________________________
                                              #Dropdown Axis Options Button
                                              #____________________________________
                                              #These divs are used to place the buttons inline with eachother in the column.
                                              div(style = "display:inline-block; text_align:right;",
                                                  #OPTIONS BUTTON
                                                  dropdownButton(label = "Axis Options",icon = icon("gear"), circle = FALSE, status = "success", right = TRUE,
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
                                                  #DOWNLOAD BUTTON
                                                  #____________________________________
                                                  #Dropdown Download Button
                                                  #____________________________________
                                                  dropdownButton(label = "Download",icon = icon("download"),circle = FALSE,status = "success",right = TRUE,
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
                                     ),#end FluidRow
                                     #____________________________________
                                     #PlotOutput Line Plot
                                     #____________________________________
                                     uiOutput("model_plotType")
                                     #jqui_resizable(plotOutput(outputId = 'LinePlot'))
                                     #jqui_resizable(plotlyOutput(outputId = 'plotlyLinePlot'))
                              ) #end column width=9
                            ), #end FluidRow
                            br(),
                            conditionalPanel(condition = "input.lineplot_choose_plot_mode == 'loop_mode' "
                                             ,box(title = NULL
                                                  ,status = "success"
                                                  ,solidHeader = FALSE
                                                  ,collapsible = TRUE
                                                  ,width = NULL,
                                                  tabBox(title = "Change Values"
                                                         ,width = 12
                                                         ,tabPanel("Parameters"
                                                                   ,uiOutput("loop_parameters_eqns_header")
                                                                   ,uiOutput("loop_parameters_eqns")
                                                                   ,uiOutput("loop_parameters_inputs_header")
                                                                   ,uiOutput("loop_parameters_inputs")
                                                                   ,uiOutput("loop_parameters_outputs_header")
                                                                   ,uiOutput("loop_parameters_outputs"))
                                                         ,tabPanel("Initial Conditions"
                                                                   ,uiOutput("loop_ICs_UI"))
                                                         ,tabPanel("Run Model"
                                                                   ,fluidRow(column(width=3
                                                                                    ,textInput(inputId="loop_execute_time_start"
                                                                                               ,label="Starting Time"
                                                                                               ,value = ""))
                                                                             ,column(width=3
                                                                                     ,textInput(inputId="loop_execute_time_end"
                                                                                                ,label="End Time"
                                                                                                ,value = ""))
                                                                             ,column(width=3
                                                                                     ,textInput(inputId="loop_execute_time_step"
                                                                                                ,label="Time Step"
                                                                                                ,value = "")))
                                                                   ,fluidRow(column(width=1
                                                                                    ,offset=10
                                                                                    ,actionButton(inputId="loop_execute_run_model"
                                                                                                  ,label="Run Solver"
                                                                                                  ,style="color: #fff; background-color: green; border-color: #2e6da4")))
                                                                   
                                                         )
                                                  )
                                             )
                            )#end conditional Panel
                            ,fluidRow(
                              uiOutput(outputId = "line_box_options")
                              ,conditionalPanel(condition = "input.lineplot_choose_plot_mode == 'compare_mode'" 
                                                ,column(width=6,
                                                        box(title=NULL
                                                            ,status="success"
                                                            ,solidHeader=FALSE
                                                            ,collapsible=TRUE
                                                            ,width=NULL,
                                                            tabBox(title="Change Values"
                                                                   ,width=12
                                                                   ,tabPanel("Parameters"
                                                                             ,uiOutput("compare_parameters_eqns_header")
                                                                             ,uiOutput("compare_parameters_eqns")
                                                                             ,uiOutput("compare_parameters_inputs_header")
                                                                             ,uiOutput("compare_parameters_inputs")
                                                                             ,uiOutput("compare_parameters_outputs_header")
                                                                             ,uiOutput("compare_parameters_outputs")
                                                                             ,uiOutput("compare_parameters_rate_header")
                                                                             ,uiOutput("compare_parameters_rates")
                                                                             )
                                                                   ,tabPanel("Initial Conditions"
                                                                             ,uiOutput("compare_ICs_UI"))
                                                                   ,tabPanel("Run Model"
                                                                            ,"As of now make sure you click on the 'Initial Conditions' tab or the comparison model will not run"
                                                                             ,fluidRow(column(width=1
                                                                                              ,offset=8
                                                                                              ,actionButton(inputId="compare_execute_run_model"
                                                                                                            ,label="Run Solver"
                                                                                                            ,style="color: #fff; background-color: green; border-color: #2e6da4")))
                                                                   ) #end tabpanel
                                                            ) #end tab box
                                                        ) #end box
                                                      ) #end column
                                                ) #end conditionalPanel
                            )
                            
                            # #____________________________________
                            # #Options containing Tabs
                            # #____________________________________
                            # ,box(
                            #   #this is a box that holds the import data options.
                            #   title=NULL, status="success", solidHeader=FALSE, collapsible=TRUE, width=NULL,
                            #   
                            #   tabBox(
                            #     title="Options",
                            #     width=12,
                            #     #____________________________________
                            #     #Color Options
                            #     #____________________________________
                            #     tabPanel("Color Options",
                            #              fluidRow(
                            #                #add line color options
                            #                column(width=12,
                            #                       fluidRow(
                            #                         column(width=3,
                            #                                uiOutput("line_color_options_popdown")),
                            #                         column(width=3,
                            #                                uiOutput("line_type_options_popdown")))
                            #                ) #end column
                            #              ) #end fluidRow
                            #     ), #end tabPanel
                            #     #____________________________________
                            #     #Background Options
                            #     #____________________________________                        
                            #     tabPanel("Background Options",
                            #              
                            #              fluidRow(
                            #                column(width=5,
                            #                       selectInput(
                            #                         inputId = "theme_output_line",
                            #                         label = "Background Theme", 
                            #                         choices = c("gray"
                            #                                     ,"bw"
                            #                                     ,"linedraw"
                            #                                     ,"light"
                            #                                     ,"minimal"
                            #                                     ,"classic"
                            #                                     ,"void"
                            #                                     ,"dark")
                            #                       ),
                            #                       fluidRow(
                            #                         div(style="display:inline-block; text_align:right;", 
                            #                             prettyCheckbox(inputId="line_panel_colorPicker_checkbox", 
                            #                                            label=NULL, 
                            #                                            value = FALSE)),
                            #                         div(style="display:inline-block; text_align:right;", 
                            #                             colourInput(inputId="line_panel_colorPicker", 
                            #                                         label="Select Color", 
                            #                                         value="grey"))))
                            #              )
                            #              
                            #              
                            #     ),#end tabPanel
                            #     #____________________________________
                            #     #Legend Options
                            #     #____________________________________
                            #     tabPanel("Legend Options",
                            #              fluidRow(
                            #                column(width=5,
                            #                       selectInput(inputId="line_legend_position",
                            #                                   label = "Location of Legend",
                            #                                   choices = c("Left" = "left", 
                            #                                               "Right" = "right", 
                            #                                               "Top" = "top", 
                            #                                               "Bottom" = "bottom", 
                            #                                               "No Legend" = "none"),
                            #                                   selected = "right"),
                            #                       textInput(inputId="line_legend_title",
                            #                                 label = "Legend Title",
                            #                                 value = "")))
                            #     ) #end tabPanel
                            #   )#End tabBox
                            # ) #End box
                            
) # end tabItem