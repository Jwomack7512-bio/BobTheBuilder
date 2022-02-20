


output$lineplot_plot_normal <- renderUI({
    div(
        fluidRow(
            column(width = ifelse(input$lineplot_comparision_mode, 6, 12),
                   fluidRow(
                       column(width = ifelse(input$lineplot_comparision_mode, 2, 6),
                              dropdown(
                                  label = "Inputs",
                                  icon = icon("sliders"),
                                  circle = FALSE, 
                                  status = "success",    
                                  selectInput(inputId = 'lineplot_xvar',
                                              label = 'x variable',
                                              choices = colnames(model_output())[1]),
                                  pickerInput(inputId = 'lineplot_yvar',
                                              label = 'y variable(s)'
                                              ,choices  = colnames(model_output())[2:ncol(model_output())]
                                              ,selected = colnames(model_output())[2:ncol(model_output())]
                                              ,options=list('actions-box'=TRUE) 
                                              ,multiple=TRUE)
                              ) #endDropDown
                       ), #end column width=6
                       column(
                         width = ifelse(input$lineplot_comparision_mode, 10, 6),
                              #____________________________________
                              #Dropdown Axis Options Button
                              #____________________________________
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
                                  dropdownButton(label = "Customize", 
                                                 status = "success", 
                                                 right = TRUE, 
                                                 circle = FALSE
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
                   jqui_resizable(plotOutput(outputId = 'LinePlot'))
                   #plotOutput(outputId = 'LinePlot')
                   
                   #jqui_resizable(plotlyOutput(outputId = 'plotlyLinePlot'))
            ) #end column width=9
            ,if (input$lineplot_comparision_mode) {
                column(width = 6,
                       fluidRow(
                           column(width = 2,
                                  dropdown(
                                      label = "Inputs",
                                      icon = icon("sliders"),
                                      circle = FALSE, 
                                      status = "success",    
                                      selectInput(inputId = 'lineplot_xvar_compare',
                                                  label = 'x variable',
                                                  choices = colnames(model_output())[1]),
                                      pickerInput(inputId = 'lineplot_yvar_compare',
                                                  label = 'y variable(s)'
                                                  ,choices  = colnames(model_output())[2:ncol(model_output())]
                                                  ,selected = colnames(model_output())[2:ncol(model_output())]
                                                  ,options = list('actions-box' = TRUE) 
                                                  ,multiple = TRUE)
                                  ) #endDropDown
                           ), #end column width=6
                           column(width = 10,
                                  #____________________________________
                                  #Dropdown Axis Options Button
                                  #____________________________________
                                  #These divs are used to place the buttons inline with eachother in the column.
                                  div(style = "display:inline-block; text_align:right;",
                                      #OPTIONS BUTTON 
                                      dropdownButton(
                                        label = "Axis Options",
                                        icon = icon("gear"),
                                        circle = FALSE,
                                        status = "success",
                                        right = TRUE,
                                        textInput(
                                          inputId = "line_title_compare",
                                          label = "Title",
                                          value = ""
                                        ),
                                        textInput(
                                          inputId = "line_xlabel_compare",
                                          label = "X Label",
                                          value = ""
                                        ),
                                        textInput(
                                          inputId = "line_ylabel_compare",
                                          label = "Y Label",
                                          value = ""
                                        )
                                      ) #end Dropdown Button
                                  ), #end div
                                  div(style = "display:inline-block; text_align:right;",
                                      #DOWNLOAD BUTTON
                                      #____________________________________
                                      #Dropdown Download Button
                                      #____________________________________
                                      dropdownButton(
                                        label = "Download",
                                        icon = icon("download"),
                                        circle = FALSE,
                                        status = "success",
                                        right = TRUE,
                                        textInput(
                                          inputId = "line_download_title_compare",
                                          label = NULL,
                                          value = "",
                                          placeholder = "Type Download TItle",
                                          width = NULL
                                        ),
                                        radioGroupButtons(
                                          inputId = "line_download_radiobuttons_compare",
                                          label = NULL,
                                          choices = c(".jpg",
                                                      ".png", ".pdf"),
                                          individual = TRUE,
                                          checkIcon = list(
                                            yes = tags$i(class = "fa fa-circle",
                                                         style = "color: steelblue"),
                                            no = tags$i(class = "fa fa-circle-o",
                                                        style = "color: steelblue")
                                          )
                                        ),
                                        downloadBttn(
                                          outputId = "downloadLine_compare",
                                          label = "Download",
                                          style = "unite",
                                          color = "success",
                                          size = "sm",
                                          block = FALSE,
                                          no_outline = FALSE
                                        )
                                      ) #end dropdownButton
                                  ), #end Div
                                  div(
                                    style = "display:inline-block; text_align:right;",
                                    dropdownButton(
                                      label = "Customize",
                                      status = "success",
                                      right = TRUE,
                                      circle = FALSE,
                                      sliderInput(
                                        inputId = "line_size_options_compare",
                                        label =  "Size of Lines",
                                        min = 0,
                                        max = 3,
                                        step = 0.2,
                                        value = 1
                                      ),
                                      prettyCheckbox(
                                        inputId = "line_show_dots_compare",
                                        label = "Show Points",
                                        value = FALSE
                                      )
                                    )
                                  )
                                  , align = 'right'
                           )#end Column width=6
                       ), #end FluidRow
                       jqui_resizable(plotOutput(outputId = 'LinePlot_compared'))
                       )
            }
        ) #end FluidRow 
    )
})
