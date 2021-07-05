#This tab corresponds to the "Export" tab
#-------------------------------------------------------------------------
#  Justin Womack
#  January 5, 2021
#  Last Update: January 5, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_export <- tabItem(tabName="TAB_export",
                   h2("Export")
                   ,fluidRow(column(width=12,
                                    "Export to Matlab ->"
                                    
                                    ,downloadBttn(outputId = "export_data_to_matlab_script"
                                                  ,label = "To matlab"
                                                  ,style = "unite"
                                                  ,color = "success")))#end fluidRow
                   ,fluidRow(column(width=12,
                                    "Export to R ->"
                                    ,downloadBttn(outputId = "export_data_to_R_script"
                                                  ,label = "To R"
                                                  ,style = "unite"
                                                  ,color = "success"))
                    )#end fluidRow
                   ,fluidRow(column(width=2,
                                    "SAVE MODEL ->")
                             ,column(width=3
                                     ,textInput(inputId = "export_data_rds_file_name"
                                                ,label = "Save File Name"
                                                ,value = "My_model"))
                             ,column(width=3
                                     ,downloadBttn(outputId = "export_save_data"
                                                   ,label = "Save Model (.RDS file)"
                                                   ,style = "unite"
                                                   ,color = "success"))
                    )
                   ,hr()
                   
                   ,boxPlus(title=NULL
                            ,solidHeader=FALSE
                            #,background="#000"
                            ,collapsible = FALSE
                            ,closable=FALSE
                            ,width=12
                            ,fluidRow(column(width=3
                                             ,offset=9
                                             ,actionButton(inputId="export_generate_output_tables"
                                                           ,label="Generate Tables"
                                                           ,style="color: #fff; background-color: green; border-color: #2e6da4")
                                             )
                              
                            )
                            
                            ,tabBox(width=12
                                    ,tabPanel("Parameters"
                                              ,DT::dataTableOutput("table_parameters"))
                                    ,tabPanel("Equations"
                                              ,DT::dataTableOutput("table_equations"))
                                    ,tabPanel("Initial Conditions"
                                              ,DT::dataTableOutput("table_ICs"))
                                    ) #end tabbox
                            )#end boxplus
                   )#end tabitem