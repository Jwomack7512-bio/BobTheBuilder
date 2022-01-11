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
                         )
                         ,sidebar = boxSidebar(
                           id = "latexSideBar"
                           #,width = 25
                           ,checkboxInput(inputId = "latex_option_test"
                                          ,label = "Advanced Options"
                                          ,value = FALSE)
                         )
                       )
                     )
                    )
          
                   # ,fluidRow(column(width = 12,
                   #                  "Export to Matlab -> "
                   #                  
                   #                  ,downloadBttn(outputId = "export_data_to_matlab_script"
                   #                                ,label = "To matlab"
                   #                                ,style = "unite"
                   #                                ,color = "success")))#end fluidRow
                   # ,hr()
                   # ,fluidRow(column(width = 12,
                   #                  "Export to R ->"
                   #                  ,downloadBttn(outputId = "export_data_to_R_script"
                   #                                ,label = "To R"
                   #                                ,style = "unite"
                   #                                ,color = "success"))
                   #  )#end fluidRow
                   ,hr()
                   # ,fluidRow(column(width = 2,
                   #                  "SAVE MODEL -> ")
                   #           ,column(width = 3
                   #                   ,textInput(inputId = "export_data_rds_file_name"
                   #                              ,label = "Save File Name"
                   #                              ,value = "My_model"))
                   #           ,column(width = 3
                   #                   ,downloadBttn(outputId = "export_save_data"
                   #                                  ,label = "Save Model (.RDS file)"
                   #                                  ,style = "unite"
                   #                                  ,color = "success"))
                   #  )
                   # ,hr()
                   # ,fluidRow(
                   #   column(
                   #     width = 4
                   #     ,downloadBttn(outputId = "export_latex_document"
                   #                   ,label = "Latex Document"
                   #                   ,style = "unite"
                   #                   ,color = "success")
                   #   ) # end column
                   # ) # end fluidRow
                   ,box(title = NULL
                            ,solidHeader = FALSE
                            #,background="#000"
                            ,collapsible = FALSE
                            ,closable = FALSE
                            ,width = 12
                            ,fluidRow(column(width = 3
                                             ,offset = 9
                                             ,actionButton(inputId = "export_generate_output_tables"
                                                           ,label = "Generate Tables"
                                                           ,style = "color: #fff; background-color: green; border-color: #2e6da4")
                                             )
                              
                            )
                            
                            ,tabBox(width = 12
                                    ,tabPanel("Parameters"
                                              ,DT::dataTableOutput("table_parameters"))
                                    ,tabPanel("Equations"
                                              ,DT::dataTableOutput("table_equations"))
                                    ,tabPanel("Initial Conditions"
                                              ,DT::dataTableOutput("table_ICs"))
                                    ) #end tabbox
                            )#end box
                   )#end tabitem