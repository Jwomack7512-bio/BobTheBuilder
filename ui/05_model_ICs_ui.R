#This tab corresponds to the "Initial Conditions" tab
#-------------------------------------------------------------------------
#  Justin Womack
#  January 5, 2021
#  Last Update: January 5, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_ICs <- tabItem(tabName = "TAB_ICs"
                   ,DTOutput("ICs_DT"),
                   hr(),
                   rHandsontableOutput("ICs_RHT"),
                   tags$style(type="text/css", "#ICs_RHT th {font-weight:bold;}")
                   )