#This tab corresponds to the "Execute Model" under Run Model Tab
#  Justin Womack
#  January 26, 2021
#  Last Update: January 26, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_RUN_EXECUTE <- tabItem(tabName = "TAB_RUN_EXECUTE"
                            ,fluidRow(
                              boxPlus(title="Model Description"
                                      ,solidHeader=TRUE
                                      ,collapsible=TRUE
                                      ,closable=FALSE
                                      ,status="success"
                                      ,width=12
                                      ,tabBox(width=12
                                              ,tabPanel("Options"
                                                        ,h4("Options go here such as type of ode solver, constraints, etc")
                                                        ,fluidRow(column(width=3
                                                                         ,textInput(inputId="execute_time_start"
                                                                                    ,label="Starting Time"
                                                                                    ,value = ""))
                                                                  ,column(width=3
                                                                          ,textInput(inputId="execute_time_end"
                                                                                     ,label="End Time"
                                                                                     ,value = ""))
                                                                  ,column(width=3
                                                                         ,textInput(inputId="execute_time_step"
                                                                                    ,label="Time Step"
                                                                                    ,value = ""))
                                                                  ) #end fluidRow
                                                        ,fluidRow(column(width = 1
                                                                         ,checkboxInput(inputId = "execute_turnOn_time_scale_var"
                                                                                        ,label = "Scale Output"
                                                                                        ,value = FALSE))
                                                                  ,column(width = 3
                                                                         ,textInput(inputId = "execute_time_scale_var"
                                                                                    ,label = "Time Scale Variable"
                                                                                    ,value = "1")))
                                                        ,fluidRow(column(width = 3
                                                                         ,pickerInput(inputId="execute_ode_solver_type"
                                                                                      ,label = "Select ODE solver"
                                                                                      ,choices = c("lsoda"
                                                                                                   ,"lsode"
                                                                                                   ,"lsodes"
                                                                                                   ,"lsodar"
                                                                                                   ,"vode"
                                                                                                   ,"daspk"
                                                                                                   ,"euler"
                                                                                                   ,"rk4"
                                                                                                   ,"ode23"
                                                                                                   ,"ode45"
                                                                                                   ,"radau")))
                                                                  )
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
                              )#end boxplus
                            )#end fluidRow
                            ,rHandsontableOutput("execute_table_for_model")
                            
                            
)