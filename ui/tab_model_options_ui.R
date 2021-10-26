

TAB_MODEL_OPTIONS <- tabItem(tabName = "TAB_MODEL_OPTIONS",
                             fluidRow(
                               column(
                                 width = 9
                                 ,box(title = NULL
                                      ,solidHeader = FALSE
                                      ,collapsible = FALSE
                                      ,closable = FALSE
                                      ,width = 12
                                      ,fluidRow(column(width = 3
                                                       ,textInput(inputId = "execute_time_start"
                                                                  ,label = "Starting Time"
                                                                  ,value = ""))
                                                ,column(width = 3
                                                        ,textInput(inputId = "execute_time_end"
                                                                   ,label = "End Time"
                                                                   ,value = ""))
                                                ,column(width = 3
                                                        ,textInput(inputId = "execute_time_step"
                                                                   ,label = "Time Step"
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
                                                       ,pickerInput(inputId = "execute_ode_solver_type"
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
                                      ) # end fluidrow
                                 ) # end box
                               ) #end column
                               ,column(
                                 width = 3
                                 ,box(title = NULL
                                      ,solidHeader = FALSE
                                      ,collapsible = FALSE
                                      ,closable = FALSE
                                      ,width = 12
                                      ,"Here lies a button"
                                      ,fluidRow(
                                        column(
                                          width = 12,
                                          actionButton(inputId = "options_store_options"
                                                       ,label = "Store Options"
                                                       ,style = "color: #fff; background-color: green; border-color: #2e6da4")
                                        )
                                      )
                                      
                                 ) #end box
                               ) #end column
                             ) #end fluidRow
                             
)