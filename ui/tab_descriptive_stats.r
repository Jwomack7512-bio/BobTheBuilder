#This tab corresponds to the "Tableone" Tab of the App
#-------------------------------------------------------------------------
#  Justin Womack
#  August 14, 2020
#  Last Update: August 15, 2020
#  ZSPH/MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_DESCRIPTIVE_STATS <- tabItem(tabName = "TAB_DESCRIPTIVE_STATS", 
                           tags$h2("Table Analysis"),
                           br(),
                           box(title="Statisitcal Summary of Table", status="primary", solidHeader=TRUE, collapsible=TRUE, width=NULL,
                               #radio buttons allowing user to select which table one ouput they want
                               fluidRow(
                                 column(width = 5,
                                        radioGroupButtons(inputId="TABLEONE_SUMMARY_OPTIONS", 
                                                          label="Type of Summary", 
                                                          choices=c("mean/sd", "summary"), 
                                                          justified=T)),
                                 column(width=1),
                                 column(width=5,
                                        radioGroupButtons(inputId="TABLEONE_ACTIVATE_SUBCLASSES", 
                                                          label="View", 
                                                          choices=c('All', "Categorical Variables", "Continuous Variables"), 
                                                          justified = TRUE, 
                                                          checkIcon = list(yes = icon("ok", lib = "glyphicon")))
                                 )#end Column
                               ),#end fluidRow
                               fluidRow(
                                 column(width=7,
                                        div(style = "display: inline-block;"),
                                        #dropdown allowing user to choose categorical variables for tableone output
                                        pickerInput(inputId="TABLEONE_CHOOSE_CATEGORICAL", 
                                                    label="Select Categorical Variables", 
                                                    choices=character(), 
                                                    options=list('actions-box'=TRUE),
                                                    multiple=TRUE))
                               ), #end fluidRow
                               fluidRow(
                                 column(width=2,
                                        div(style = "display: inline-block;"), #center checkbox
                                        #checkbox to turn on the categorical separation of variables (maybe not needed. Not exactly sure)
                                        materialSwitch(inputId="TABLEONE_ACTIVATE_STRATA", 
                                                       label="Activate Strata", 
                                                       value=FALSE,
                                                       status="primary",
                                                       right=FALSE)),
                                 column(width=5,
                                        div(style = "display: inline-block;"),
                                        #dropdown allowing user to choose categorical variables for tableone output
                                        pickerInput(inputId="TABLEONE_CHOOSE_STRATA", 
                                                    label=NULL, 
                                                    choices=character(),
                                                    options=list(title="Select Strata Variable")))
                               ), #end fluidRow
                               fluidRow(
                                 column(width=7,
                                        div(style = "display: inline-block;"),
                                        #attaches a tip to this variable on how to use it properly
                                        bs_embed_tooltip(
                                          #creates select inputs for user to choose variables that would follow median instead of mean
                                          pickerInput(inputId="TABLEONE_CHOOSE_NONNORMAL", 
                                                      label="Select NonNormal Variables", 
                                                      choices=character(), 
                                                      options=list('actions-box'=TRUE),
                                                      multiple=TRUE),
                                          title="Select continuous variables here that you would wish to rather undergo median instead of mean. Useful for highly skewed data",
                                          placement="right"))
                               )#end FluidRow
                               
                               
                               # downloadBttn(outputId="TABLEONE_DOWNLOAD_DATA",
                               #              label="Download", 
                               #              style="unite", 
                               #              color="primary", 
                               #              size="sm", 
                               #              block=F, 
                               #              no_outline=F)
                           )#end box
                           ,conditionalPanel(condition = "!input.tableone_view_text_output"
                                             ,h3("Continous Variables", align = "center")
                                             ,DT::dataTableOutput("tableone_data_output_continous")
                                             #,tableOutput("tableone_data_output_continous")
                                             ,hr()
                                             ,h3("Categorical Variables", align = "center")
                                             ,DT::dataTableOutput("tableone_data_output_categorical")
                           )
                           ,checkboxInput(inputId="tableone_view_text_output" 
                                          ,label = "View as raw text instead"
                                          ,value = FALSE)
                           ,conditionalPanel(condition = "input.tableone_view_text_output"
                                             ,verbatimTextOutput('tableone_summary_output', placeholder=TRUE)
                           )
                           
                           
                           # radioGroupButtons(inputId="TABLEONE_TABLE_OPTIONS", label="Select Table Type", choices=c('text', 'table1', "table2"), justified=T),
                           # conditionalPanel(condition="input.TABLEONE_TABLE_OPTIONS=='text'",
                           #                  verbatimTextOutput('tableone_summary_output', placeholder=TRUE)),
                           # conditionalPanel(condition="input.TABLEONE_TABLE_OPTIONS=='table1'",
                           #                  rHandsontableOutput("tableone_sumamry_rhandsomeTable"))
                           
                           
) # end tabItem
