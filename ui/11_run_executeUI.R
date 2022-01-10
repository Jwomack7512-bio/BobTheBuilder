#This tab corresponds to the "Execute Model" under Run Model Tab
#  Justin Womack
#  January 26, 2021
#  Last Update: January 26, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_RUN_EXECUTE <- tabItem(tabName = "TAB_RUN_EXECUTE"
                           ,fluidRow(
                             column(
                               width = 2
                               ,wellPanel(
                                 textInput(inputId = "execute_time_start"
                                           ,label = "Starting Time"
                                           ,value = "0")
                                 ,textInput(inputId = "execute_time_end"
                                            ,label = "End Time"
                                            ,value = "10")
                                 ,textInput(inputId = "execute_time_step"
                                            ,label = "Time Step"
                                            ,value = "0.1")
                                 ,actionButton(inputId = "execute_run_model"
                                               ,label = "Run Solver"
                                               ,style = "color: #fff; background-color: green; border-color: #2e6da4")
                               )
                             )
                            ,column(
                              width = 10
                              ,box(title = "Model Description"
                                   ,solidHeader = TRUE
                                   ,collapsible = TRUE
                                   ,closable = FALSE
                                   ,status = "success"
                                   ,width = 12
                                   
                                   
                                   ,tabBox(width = 12
                                           ,tabPanel("Equations"
                                                     ,htmlOutput("execute_equations_show"))
                                           ,tabPanel("Parameters"
                                                     ,htmlOutput("execute_parameters_show"))
                                           ,tabPanel("Initial Conditions"
                                                     ,htmlOutput("execute_ICS_show"))
                                   )#end tabbox
                                   
                              )#end box
                            )
                           )#end fluidRow
                           

                              
                            ,rHandsontableOutput("execute_table_for_model")
                            
                            
)