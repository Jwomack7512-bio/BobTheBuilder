#This tab corresponds to the "Data Management" Tab of the App
#-------------------------------------------------------------------------
#  Justin Womack
#  January 5, 2021
#  Last Update: January 5, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_VAR_CREATE <- tabItem(tabName="TAB_VAR_CREATE"
                          # h2("Create Variables")
                          # ,hr()
                          ,fluidRow(column(width = 6
                                           ,box(title=NULL
                                               ,solidHeader=FALSE
                                               ,collapsible = FALSE
                                               ,closable=FALSE
                                               ,width = 12
                                               ,fluidRow
                                               (
                                                 column(width=8
                                                         ,textInput(inputId="createVar_varInput"
                                                                    ,label="Enter Model Variable"
                                                                    ,value=""))
                                                 ,column(width=4
                                                         ,div(style="display: inline-block;vertical-align:top;padding-top:23px;padding-left:-10px"
                                                              ,actionButton(inputId="createVar_addVarToList"
                                                                            ,label="Add Variable"
                                                                            ,style="color: #fff; background-color: green; border-color: #2e6da4"))))
                                               )
                                               )
                            
                                    # ,column(width=6
                                    #         ,h3("Variables in Model")
                                    #         ,fluidRow(
                                    #           box(title=NULL
                                    #               ,solidHeader=FALSE
                                    #               #,background="#000"
                                    #               ,collapsible = FALSE
                                    #               ,closable=FALSE
                                    #               ,htmlOutput(outputId="createVar_displayVars"))
                                    #         )
                                    #         
                                    #         ,actionButton(inputId="createVar_removeVarFromList"
                                    #                       ,label="Remove Last Added"
                                    #                       ,style="color: #fff; background-color: red; border-color: #2e6da4"))
                                    
                          )#end fluidRow
                          ,fluidRow(column(width = 6
                                           ,h3("Variables in Model")
                                          ,box(title=NULL
                                               ,solidHeader=FALSE
                                               ,collapsible = FALSE
                                               ,closable=FALSE
                                               ,width = 12
                                               ,fluidRow(
                                                 box(title=NULL
                                                     ,solidHeader=FALSE
                                                     #,background="#000"
                                                     ,collapsible = FALSE
                                                     ,closable=FALSE
                                                     ,width = 12
                                                     ,htmlOutput(outputId="createVar_displayVars"))
                                               )
                                               
                                               ,actionButton(inputId="createVar_removeVarFromList"
                                                             ,label="Remove Last Added"
                                                             ,style="color: #fff; background-color: red; border-color: #2e6da4")))
                          )#end fluidRow
                                  
                          )#end tabItem
