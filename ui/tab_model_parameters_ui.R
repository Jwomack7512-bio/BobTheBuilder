#This tab corresponds to the "Parameters" tab
#-------------------------------------------------------------------------
#  Justin Womack
#  January 5, 2021
#  Last Update: January 5, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_Parameters <- tabItem(tabName="TAB_Parameters"
                         ,h2("Parameters")
                         ,fluidRow(column(width=2
                                          ,offset=8
                                          ,actionButton(inputId="param_store_parameters"
                                                        ,label="Store Parameters"
                                                        ,style="color: #fff; background-color: green; border-color: #2e6da4")
                                          ,actionButton(inputId="param_view_parameters"
                                                        ,label="View Parameters"
                                                        ,style="color: #fff; background-color: green; border-color: #2e6da4")
                                          ,actionButton(inputId="param_remove_duplicate_parameters"
                                                        ,label="Delete Duplicates"
                                                        ,style="color: #fff; background-color: green; border-color: #2e6da4"))
                         )
                         #,h3("Parameters From Equations")
                         ,uiOutput("parameters_eqns_header")
                         ,uiOutput("parameters_eqns")
                         ,uiOutput("parameters_inputs_header")
                         ,uiOutput("parameters_inputs")
                         ,uiOutput("parameters_outputs_header")
                         ,uiOutput("parameters_outputs")
                         ,uiOutput("parameters_rateEqns_header")
                         ,uiOutput("parameters_rateEqns")
                         ,uiOutput("parameters_TD_eqns_header")
                         ,uiOutput("parameters_TD_eqns")
                  )