#This tab corresponds to the "Input/Output" tab
#-------------------------------------------------------------------------
#  Justin Womack
#  January 20, 2021
#  Last Update: January 20, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_LoadModel <- tabItem(tabName="TAB_LoadModel",
                     h2("Load File")
                     ,fluidRow(
                       column(width=12
                              ,boxPlus(title = "Load Model"
                                ,closable = FALSE 
                                ,width = NULL
                                ,status = "success" 
                                ,solidHeader = TRUE 
                                ,collapsible = TRUE
                                ,enable_dropdown = TRUE
                                ,dropdown_icon = "wrench"
                                #my fucking goodness this took forever for me to get. The divs place the two
                                #widgets together and the middle div adds a tiny bit of space between them. 
                                #The first div adds padding to help center align the button a bit.
                               ,div(style="display: inline-block;vertical-align:top;padding-top:3px"
                                    #shiny import button that allows user to select db to use
                                    ,shinyFilesButton("find_model_location" 
                                                     ,label = "Import Database" 
                                                     ,title = 'test' 
                                                     ,multiple = FALSE))
                                ,div(style="display: inline-block;vertical-align:top; width: 5px;",HTML("<br>"))
                                ,div(style="display: inline-block;vertical-align:top;"
                                    #text output that shows the user with db they have selected
                                ,verbatimTextOutput(outputId="model_name_location_output"))
                                ,actionButton(inputId = "Button_load_model"
                                             ,label = "Load Database")
                              )#end box
                       ) #end column
                     ) #end fluidRow
                     # ,pickerInput(inputId="InOut_selectVar"
                     #              ,label = "Select Variable to Add Input or Output to"
                     #              ,choices = c())
                     # ,boxPlus(title = "Put input or Output to the Overall System"
                     #          ,closable = FALSE 
                     #          ,width = NULL
                     #          ,status = "success" 
                     #          ,solidHeader = TRUE 
                     #          ,collapsible = TRUE
                     #          ,enable_dropdown = TRUE
                     #          ,dropdown_icon = "wrench"
                     #          
                     #          ,tabBox(title=NULL
                     #                  ,id="InOut_tabbox"
                     #                  ,width=12
                     #                  ,tabPanel("Input"
                     #                            ,verbatimTextOutput("InOut_showInForVar")
                     #                            ,fluidRow(column(width=4
                     #                                             ,pickerInput(inputId="InOut_typeOfIn"
                     #                                                          ,label = "Select Input Type"
                     #                                                          ,choices = c("Rate" = "Rate"
                     #                                                                       ,"Simple Diffusion" = "simp_diff"
                     #                                                                       , "Synthesis" = "Synthesis")))
                     #                                      #,uiOutput("InOut_InOptions")
                     #                            ) #end FluidRow
                     #                            
                     #                            ,fluidRow(column(width=3
                     #                                             ,textInput(inputId="InOut_varIn"
                     #                                                        ,label = "Variable Name"
                     #                                                        ,value = ""))
                     #                                      ,column(width=3
                     #                                              ,textInput(inputId="InOut_varInValue"
                     #                                                         ,label = "Value"
                     #                                                         ,value = "0"))
                     #                                      ,column(width=3
                     #                                              ,textInput(inputId="InOut_VarInComment"
                     #                                                         ,label = "Comment"
                     #                                                         ,value=""))
                     #                            )#end fluidRow
                     #                            ,actionButton(inputId="Inout_addInVarToDf"
                     #                                          ,label="Add Input"
                     #                                          ,style="color: #fff; background-color: green; border-color: #2e6da4")
                     #                  )#end tabPanel
                     #                  ,tabPanel("Outputs"
                     #                            ,verbatimTextOutput("InOut_showOutForVar")
                     #                            ,fluidRow(column(width=4
                     #                                             ,pickerInput(inputId="InOut_typeOfOut"
                     #                                                          ,label = "Select Output Type"
                     #                                                          ,choices = c("Rate" = "Rate"
                     #                                                                       ,"Simple Diffusion" = "simp_diff"
                     #                                                                       , "Degradation" = "Degradation")))
                     #                            ) #end FluidRow
                     #                            ,fluidRow(column(width=3
                     #                                             ,textInput(inputId="InOut_varOut"
                     #                                                        ,label = "Variable Name"
                     #                                                        ,value = ""))
                     #                                      ,column(width=3
                     #                                              ,textInput(inputId="InOut_varOutValue"
                     #                                                         ,label = "Value"
                     #                                                         ,value = "0"))
                     #                                      ,column(width=3
                     #                                              ,textInput(inputId="InOut_VarOutComment"
                     #                                                         ,label = "Comment"
                     #                                                         ,value=""))
                     #                            )#end fluidRow
                     #                            ,actionButton(inputId="Inout_addOutVarToDf"
                     #                                          ,label="Add Output"
                     #                                          ,style="color: #fff; background-color: green; border-color: #2e6da4")
                     #                  )#end tabPanel
                     #          )#end tabbox
                     # )#end boxplus
)#end tabitem