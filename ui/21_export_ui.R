#This tab corresponds to the "Export" tab
#-------------------------------------------------------------------------
#  Justin Womack
#  January 5, 2021
#  Last Update: January 5, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_export <- 
  tabItem(tabName = "TAB_export",
                   h2("Export")
                   ,fluidRow(
                     column(
                       width = 4
                       ,box(
                         title = "Code"
                         ,solidHeader = FALSE
                         #,background="#000"
                         ,collapsible = FALSE
                         ,closable = FALSE
                         ,width = 12
                         ,fluidRow(
                           column(
                             width = 12,
                             textInput(inputId = "export_code_file_name"
                                      ,label = NULL
                                      ,value = ""
                                      ,placeholder = "File Name (no extension)")
                           )
                         )
                         ,downloadBttn(outputId = "export_data_to_matlab_script"
                                       ,label = "MatLab Code"
                                       ,style = "unite"
                                       ,color = "success")
                         ,downloadBttn(outputId = "export_data_to_R_script"
                                      ,label = "R Code"
                                      ,style = "unite"
                                      ,color = "success")
                       )
                     )
                     ,column(
                       width = 4
                       ,box(
                         title = "Model File"
                         ,solidHeader = FALSE
                         #,background="#000"
                         ,collapsible = FALSE
                         ,closable = FALSE
                         ,width = 12
                         ,fluidRow(
                           column(
                             width = 12,
                             textInput(inputId = "export_model_file_name"
                                       ,label = NULL
                                       ,value = ""
                                       ,placeholder = "File Name (no extension)")
                           )
                         )
                         ,downloadBttn(outputId = "export_save_data"
                                       ,label = "Save Model"
                                       ,style = "unite"
                                       ,color = "success")
                       )
                     )
                     ,column(
                       width = 4
                       ,box(
                         title = "Latex Document"
                         ,solidHeader = FALSE
                         #,background="#000"
                         ,collapsible = FALSE
                         ,closable = FALSE
                         ,width = 12
                         ,fluidRow(
                           column(
                             width = 12,
                             textInput(inputId = "export_eqns_file_name"
                                       ,label = NULL
                                       ,value = ""
                                       ,placeholder = "File Name (no extension)")
                           )
                         )
                         ,downloadBttn(outputId = "export_latex_document"
                                      ,label = "Latex Document"
                                      ,style = "unite"
                                      ,color = "success")
                         ,dropdownMenu = boxDropdown(
                           "Pages to Add"
                           ,checkboxInput("latex_add_variables"
                                          ,"Variables"
                                          ,TRUE)
                           ,checkboxInput("latex_add_equations"
                                          ,"Equations"
                                          ,TRUE)
                           ,checkboxInput("latex_add_additionalEqns"
                                          ,"Additional Equations"
                                          ,TRUE)
                           ,checkboxInput("latex_add_IO"
                                          ,"Input/Output"
                                          ,TRUE)
                           ,checkboxInput("latex_add_paramTable"
                                          ,"Parameter Table"
                                          ,TRUE)
                           ,checkboxInput("latex_add_diffEqns"
                                          ,"Differential Eqns"
                                          ,TRUE)
                         )
                         ,sidebar = boxSidebar(
                           id = "latexSideBar"
                           #,width = 25
                           ,checkboxInput(inputId = "latex_equation_headers"
                                          ,label = "Equation Types Shown"
                                          ,value = FALSE)
                           ,checkboxInput(inputId = "latex_equation_description"
                                          ,label = "Equation Descriptions Shown"
                                          ,value = TRUE)
                           ,checkboxInput(inputId = "latex_option_test"
                                          ,label = "Advanced Options"
                                          ,value = FALSE)
                         )
                       )
                     )
                    )
                   ,hr()
                   ,box(title = NULL
                            ,solidHeader = FALSE
                            #,background="#000"
                            ,collapsible = FALSE
                            ,closable = FALSE
                            ,width = 12
                            ,tabBox(width = 12
                                    ,tabPanel("Parameters"
                                              ,DTOutput("table_parameters_export"))
                                    ,tabPanel("Equations"
                                              ,DTOutput("table_equations_export"))
                                    ,tabPanel("Initial Conditions"
                                              ,DTOutput("table_ICs_export"))
                                    ) #end tabbox
                            )#end box
                   )#end tabitem