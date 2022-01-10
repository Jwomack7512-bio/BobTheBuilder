#This tab corresponds to the "Parameters" tab
#-------------------------------------------------------------------------
#  Justin Womack
#  January 5, 2021
#  Last Update: January 5, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_Parameters <- tabItem(tabName = "TAB_Parameters"
                         ,h2("Parameters"),
                         HTML("The table below hosts all parameters and values for your model. <br>"),
                         tags$b("To Use:"),
                         tags$div(
                           tags$ul(
                             tags$li("Double click cell in column you wish to edit. This will open the edit option for the column"),
                             tags$li("Change all desired values in that column."),
                             tags$li("Press <b>\"Ctrl+Enter\"<\b> to save changes or <b>\"Esc\"</b> to cancel")
                           )
                         ),
                         tags$b("Do not:"),
                         tags$div(
                           tags$ul(
                             tags$li("Edit more than one column at a time. Only changes to the first column save.  I.E do all values, \"Ctrl+Enter\", then do the Descriptions"),
                             tags$li("It is recommended that you do not change the parameter value at this time until I program a proper failsafe."),
                             tags$li("Or until Viren programs it.")
                           )
                         ),
                         "ToDo:",
                         tags$div(
                           tags$ul(
                             tags$li("Add option to which between table view and panel view?"),
                             tags$li("Add sort option to sort variables by parameter type: equations, IO, and other breaks")
                           )
                         )
                         # ,fluidRow(column(width = 2
                         #                  ,offset = 8
                         #                  ,actionButton(inputId = "param_store_parameters"
                         #                                ,label = "Store Parameters"
                         #                                ,style = "color: #fff; background-color: green; border-color: #2e6da4")
                         #                  
                         # ))
                         ,DTOutput("parameters_DT")
                         #,h3("Parameters From Equations")
                         # ,uiOutput("parameters_eqns_header")
                         # ,uiOutput("parameters_eqns")
                         # ,uiOutput("parameters_inputs_header")
                         # ,uiOutput("parameters_inputs")
                         # ,uiOutput("parameters_outputs_header")
                         # ,uiOutput("parameters_outputs")
                         # ,uiOutput("parameters_rateEqns_header")
                         # ,uiOutput("parameters_rateEqns")
                         # ,uiOutput("parameters_TD_eqns_header")
                         # ,uiOutput("parameters_TD_eqns")
                  )