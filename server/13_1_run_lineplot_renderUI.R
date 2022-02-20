#This would be the server to rendering varying UI parts for the plotting module.
#Controls if normal plot Ui is shown or multiple plots.  Changes plots around, etc. 

# 
observeEvent(params$vars.all, {
    updatePickerInput(session,
                      "compare_models_select_vars",
                      choices = params$vars.all, 
                      selected = NULL)
})
#storage for compare model values to be put into a datatable
compareModelTable <- reactiveValues(
  df = data.frame(matrix(ncol = 3,
                         nrow = 0,
                         dimnames = list(NULL, c("Variable",
                                                 "Model 1 Value",
                                                 "Model 2 Value")))),
  no.values = TRUE
)

observeEvent(input$compare_models_select_vars, {
  req(length(input$compare_models_select_vars) > 0)
  #check to add vars to table if they aren't in
  for (var in input$compare_models_select_vars) {
    df.2.vec <- pull(compareModelTable$df, "Variable")
    if (!(var %in% df.2.vec)) {
      #find vars parameter value
      idx = match(var, params$vars.all)
      value = params$vals.all[idx]
      row.to.df <- c(var, value, value)
      if (compareModelTable$no.values) {
        compareModelTable$no.values = FALSE
        compareModelTable$df[1,] <- row.to.df
      } else {
        compareModelTable$df <- rbind(compareModelTable$df, row.to.df)
      }
    }
  }
  #check to see if current variables are no longer in pickerinput
  df.2.vec <- pull(compareModelTable$df, "Variable")
  for (var in df.2.vec) {
    if (!(var %in% input$compare_models_select_vars)) {
      idx = match(var, df.2.vec)
      #remove row from RV
      compareModelTable$df <- compareModelTable$df[-idx, 1:ncol(compareModelTable$df)]
    }
  }
})

output$compare_models_DT <- renderDT({
  DT::datatable(compareModelTable$df,
                editable = list(target = "column", disable = list(columns = 0)),
                class = "cell-border stripe",
                options = list(autoWidth = TRUE,
                               pageLength = -1,
                               ordering = FALSE,
                               dom = 't')
                )
})

proxy_compare_models_DT = dataTableProxy("compare_models_DT")

observeEvent(input$compare_models_DT_cell_edit, {
  info = input$compare_models_DT_cell_edit
  compareModelTable$df <- editData(compareModelTable$df, info)
  replaceData(proxy_compare_models_DT, compareModelTable$df, resetPaging = FALSE)
})


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
