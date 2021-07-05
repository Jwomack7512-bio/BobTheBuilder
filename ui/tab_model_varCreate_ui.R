#This tab corresponds to the "Data Management" Tab of the App
#-------------------------------------------------------------------------
#  Justin Womack
#  January 5, 2021
#  Last Update: January 5, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_VAR_CREATE <- tabItem(tabName="TAB_VAR_CREATE",
                          h2("Create Variables")
                          ,fluidRow(column(width=8
                                           ,textInput(inputId="createVar_varInput"
                                                     ,label="Type Name of variable"
                                                     ,value=""))
                                    ,column(width=2
                                            ,div(style="display: inline-block;vertical-align:top;padding-top:23px;padding-left:-10px"
                                            ,actionButton(inputId="createVar_addVarToList"
                                                          ,label="Add Variable"
                                                          ,style="color: #fff; background-color: green; border-color: #2e6da4")))
 
                          )#end fluidRow
                          ,boxPlus(title=NULL
                                   ,solidHeader=FALSE
                                   #,background="#000"
                                   ,collapsible = FALSE
                                   ,closable=FALSE
                                   ,htmlOutput(outputId="createVar_displayVars"))
                          ,actionButton(inputId="createVar_removeVarFromList"
                                        ,label="Remove Last Added"
                                        ,style="color: #fff; background-color: red; border-color: #2e6da4")
                          )#end tabItem
