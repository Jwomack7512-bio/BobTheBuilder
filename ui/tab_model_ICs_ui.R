#This tab corresponds to the "Initial Conditions" tab
#-------------------------------------------------------------------------
#  Justin Womack
#  January 5, 2021
#  Last Update: January 5, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_ICs <- tabItem(tabName="TAB_ICs"
                   ,h2("Create ICs")
                   ,fluidRow(column(width=2
                                    ,offset=8
                                    ,actionButton(inputId="ICs_store_ICs"
                                                  ,label="Store ICs"
                                                  ,style="color: #fff; background-color: red; border-color: #2e6da4"))
                   )
                   ,uiOutput("ICs_UI"))