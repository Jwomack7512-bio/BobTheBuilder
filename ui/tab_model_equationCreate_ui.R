#This tab corresponds to the "Equation Creation" tab
#-------------------------------------------------------------------------
#  Justin Womack
#  January 5, 2021
#  Last Update: January 5, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_Equation_Create <- tabItem(tabName="TAB_Equation_Create"
                               ,fluidRow(column(width=10,
                                                boxPlus(title="Equation Creator"
                                                        ,solidHeader=TRUE
                                                        ,collapsible=TRUE
                                                        ,closable=FALSE
                                                        ,status="success"
                                                        ,width=NULL
                                                        ,tabBox(width=12
                                                                ,tabPanel("New"
                                                                          ,fluidRow(column(width=3
                                                                                           ,pickerInput(inputId="eqnCreate_type_of_equation"
                                                                                                        ,label="Select Type"
                                                                                                        ,choices=c("Chemical Rxn" = "chem_rxn"
                                                                                                                   ,"Enzyme-Catalyzed Rxn" = "enzyme_rxn"
                                                                                                                   ,"Simple Diffusion" = "simp_diff"
                                                                                                                   ,"Rate Equation" = "rate_eqn")))
                                                                                    ,conditionalPanel(condition="input.eqnCreate_type_of_equation=='chem_rxn'"
                                                                                                      ,column(width=3
                                                                                                              ,numericInput(inputId="eqnCreate_num_of_eqn_LHS"
                                                                                                                            ,label="Number of Variable on LHS"
                                                                                                                            ,value=1
                                                                                                                            ,min=1
                                                                                                                            ,step=1))
                                                                                                      ,column(width=3
                                                                                                              ,numericInput(inputId="eqnCreate_num_of_eqn_RHS"
                                                                                                                            ,label="Number of Variable on RHS"
                                                                                                                            ,value=1
                                                                                                                            ,min=1
                                                                                                                            ,step=1))
                                                                                    )#end conditional Panel on chem_rxn
                                                                                    
                                                                          )#end fluidRow
                                                                          ,hr()
                                                                          ,verbatimTextOutput(outputId="eqnCreate_showEquationBuilding",
                                                                                              placehold=TRUE)                                                   
                                                                          ,hr()
                                                                          ,conditionalPanel(condition="input.eqnCreate_type_of_equation=='chem_rxn'"
                                                                                            ,uiOutput("eqnCreate_equationBuilder_chem"))
                                                                          ,conditionalPanel(condition="input.eqnCreate_type_of_equation=='enzyme_rxn'"
                                                                                            ,uiOutput("eqnCreate_equationBuilder_enzyme"))
                                                                          ,conditionalPanel(condition="input.eqnCreate_type_of_equation=='simp_diff'"
                                                                                            ,uiOutput("eqnCreate_equationBuilder_simp_diff"))
                                                                          ,conditionalPanel(condition="input.eqnCreate_type_of_equation=='rate_eqn'"
                                                                                            ,fluidRow(column(width = 4
                                                                                                             ,textInput(inputId = "eqnCreate_rate_new_parameter"
                                                                                                                        ,label = "Parameter for model"
                                                                                                                        ,value = ""))
                                                                                                      ,column(width = 2
                                                                                                             ,actionButton(inputId = "eqnCreate_rate_store_new_parameter"
                                                                                                                           ,label = "Store Parameter"
                                                                                                                           ,style="color: #fff; background-color: green; border-color: #2e6da4"))
                                                                                                      )
                                                                                            ,hr()
                                                                                            ,fluidRow(column(width=4
                                                                                                             ,textInput(inputId = "eqnCreate_rate_firstvar"
                                                                                                                        ,label = "Rate Var"
                                                                                                                        ,value = ""))
                                                                                                      ,column(width = 1
                                                                                                              ,"=")
                                                                                                      ,column(width=7
                                                                                                              ,textInput(inputId="eqnCreate_rate_equation"
                                                                                                                         ,label = "Rate Equation"
                                                                                                                         ,value = ""))
                                                                                                      )
                                                                                            
                                                                                            #,uiOutput("eqnCreate_equationBuilder_rate")
                                                                                            )
                                                                          ,fluidRow(column(width=1
                                                                                           ,offset=10
                                                                                           ,actionButton(inputId="eqnCreate_addEqnToVector"
                                                                                                         ,label="Add Equation"
                                                                                                         ,style="color: #fff; background-color: green; border-color: #2e6da4")))
                                                                )#end tabitem new
                                                                ,tabPanel("Edit"
                                                                          ,fluidRow(column(width=6
                                                                                            ,pickerInput(inputId = "eqnCreate_edit_select_equation"
                                                                                                        ,label = "Select Equation Number to Edit"
                                                                                                        ,choices = "")
                                                                                           )
                                                                                    ,column(width=2
                                                                                            ,div
                                                                                            (
                                                                                                style="display: inline-block;vertical-align:top;padding-top:25px;padding-left:-35px"
                                                                                                ,actionButton(inputId="createEqn_edit_equation_button"
                                                                                                              ,label="Edit")
                                                                                            )
                                                                                    )
                                                                          )
                                                                          ,hr()
                                                                          ,uiOutput('eqnCreate_renderingUIcomponents')
                                                                          ,hr()
                                                                          ,verbatimTextOutput("build_equation_edit")
                                                                          
                                                                )
                                                                ,tabPanel("Delete"
                                                                          ,fluidRow(column(width=6
                                                                                           ,pickerInput(inputId = "eqnCreate_delete_equation"
                                                                                                        ,label = "Select Equation Number to delete"
                                                                                                        ,choices = "")
                                                                          )
                                                                          ,column(width=2
                                                                                  ,div
                                                                                  (
                                                                                      style="display: inline-block;vertical-align:top;padding-top:25px;padding-left:-35px"
                                                                                      ,actionButton(inputId="createEqn_delete_equation_button"
                                                                                                    ,label="Delete")
                                                                                  )
                                                                          )
                                                                          ))
                                                                ,tabPanel("View"
                                                                          ,"This section is to show the differential outputs that would result from each of the selected equations"
                                                                          ,conditionalPanel(condition="input.eqnCreate_type_of_equation=='enzyme_rxn'"
                                                                                            ,uiOutput("test_mathJax"))
                                                                )
                                                        )#end tabbox
                                                ) #end boxplus
                               )
                               ,column(width=2
                                      ,boxPlus(title="Options"
                                              ,solidHeader=TRUE
                                              ,collapsible=TRUE
                                              ,closable=FALSE
                                              ,status="success"
                                              ,width=NULL
                                              ,uiOutput("eqnCreate_Options"))
                               )
                               ,fluidRow(column(width=12
                                                ,boxPlus(title=NULL
                                                        ,solidHeader=FALSE
                                                        #,background="#000"
                                                        ,collapsible = FALSE
                                                        ,closable=FALSE
                                                        ,width=10
                           
                                                         
                                                        ,tabBox(width=12
                                                                ,tabPanel("Equations"
                                                                          ,htmlOutput(outputId = "eqnCreate_showEquations"))
                                                                ,tabPanel("Rates"
                                                                          ,htmlOutput(outputId = "eqnCreate_showRateEquations"))
                                                                )
                                                        ,fluidRow(column(width=12,
                                                                         align = "right"
                                                                         ,actionButton(inputId="createEqn_removeEqnFromList"
                                                                                       ,label="Remove Last Added"
                                                                                       ,style="color: #fff; background-color: red; border-color: #2e6da4"))
                                                        )
                                                        )
                                                )
                                        )
                               
                                        )
                               ,actionButton(inputId="createEqn_removeFirstRate"
                                             ,label="Remove First Rate"
                                             ,style="color: #fff; background-color: red; border-color: #2e6da4")
                          )#end TabItem


