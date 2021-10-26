#This tab corresponds to the "Execute Model" under Run Model Tab
#  Justin Womack
#  January 26, 2021
#  Last Update: January 26, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_RUN_EXECUTE <- tabItem(tabName = "TAB_RUN_EXECUTE"
                            ,fluidRow(
                              box(title="Model Description"
                                      ,solidHeader=TRUE
                                      ,collapsible=TRUE
                                      ,closable=FALSE
                                      ,status="success"
                                      ,width=12
                                      ,tabBox(width=12
                                              ,tabPanel("Options"
                                                        ,h2("MOVED")
                                                        ) #end tabPanel
                                              ,tabPanel("Equations"
                                                        ,htmlOutput("execute_equations_show"))
                                              ,tabPanel("Parameters"
                                                        ,htmlOutput("execute_parameters_show"))
                                              ,tabPanel("Initial Conditions"
                                                        ,htmlOutput("execute_ICS_show"))
                                      )#end tabbox
                                      ,fluidRow(column(width=1
                                                       ,offset=10
                                                       ,actionButton(inputId="execute_run_model"
                                                                     ,label="Run Solver"
                                                                     ,style="color: #fff; background-color: green; border-color: #2e6da4")))
                              )#end box
                            )#end fluidRow
                            ,rHandsontableOutput("execute_table_for_model")
                            
                            
)