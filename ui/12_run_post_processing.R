



TAB_run_post_processing <- tabItem(tabName = "TAB_run_post_processing",
                                   "Note: Tab not mean to perform serious calculations. Simple addition and subtraction of specific variables"
                                   ,"However, a custom option is provided it you are truly daring"
                                   ,hr()
                                   ,box(title = NULL
                                        ,closable = FALSE
                                        ,width = 11
                                        ,status = "success"
                                        ,solidHeader = FALSE
                                        ,collapsible = TRUE
                                        ,enable_dropdown = FALSE
                                       ,fluidRow(
                                         column(
                                           width = 3,
                                           textInput(inputId = "pp_new_var"
                                                     ,label = "New Variable Name"
                                                     ,value = "")
                                         )
                                         ,column(
                                           width = 1,
                                           div(style = "padding-top:30px",
                                               "=")
                                         ),
                                         column(
                                           width = 3
                                           ,pickerInput(inputId = "pp_add_vars"
                                                        ,label = "Variables to Add"
                                                        ,choices = c()
                                                        ,multiple = TRUE
                                                        ,options = list(
                                                          title = "Select Variables To Add"
                                                        ))
                                         )
                                         ,column(
                                           width = 3
                                           ,pickerInput(inputId = "pp_sub_vars"
                                                         ,label = "Variables to Subtract"
                                                         ,choices = c()
                                                        ,multiple = TRUE
                                                        ,options = list(
                                                          title = "Select Variables To Subtract"
                                                        ))
                                         )
                                       ),
                                       fluidRow(
                                         column(
                                           width = 10,
                                           verbatimTextOutput(outputId = "pp_built_equation"
                                                              ,placeholder = TRUE)
                                         )
                                       )
                                       ,fluidRow(
                                         column(
                                           width = 3,
                                           actionBttn(inputId = "pp_submit_new_var"
                                                      ,label = "Submit")
                                           )
                                         )
                                     ) # end box
                                   ,hr()
                                   ,rHandsontableOutput("pp_data_table")
                                   
)
