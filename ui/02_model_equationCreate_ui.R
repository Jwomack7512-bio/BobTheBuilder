#This tab corresponds to the "Equation Creation" tab
#-------------------------------------------------------------------------
#  Justin Womack
#  January 5, 2021
#  Last Update: January 5, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_Equation_Create <- tabItem(tabName = "TAB_Equation_Create"
                               ,fluidRow(
                                 column(
                                  width = 12,
                                  tabBox(
                                    id = "tabbox_build_equations",
                                    width = 12,
                                    tabPanel("New"
                                            ,fluidRow(
                                              column(
                                                width = 12
                                                ,fluidRow(
                                                  column(
                                                    width = 3
                                                   ,pickerInput(inputId = "eqnCreate_type_of_equation"
                                                                ,label = "Equation Type"
                                                                ,choices = c("Chemical Reaction" = "chem_rxn"
                                                                             ,"Enzyme-Catalyzed Rxn" = "enzyme_rxn"
                                                                             ,"Simple Diffusion" = "simp_diff"
                                                                             ,"Rate Equation" = "rate_eqn"
                                                                             ,"Time Dependent Equation" = "time_dependent")
                                                                )
                                                   )
                                                  ,column(
                                                    width = 3
                                                    ,conditionalPanel(condition = "input.eqnCreate_type_of_equation=='chem_rxn'"
                                                                      ,numericInput(inputId = "eqnCreate_num_of_eqn_LHS"
                                                                                    ,label = "Number of Reactants"
                                                                                    ,value = 1
                                                                                    ,min = 1
                                                                                    ,step = 1)
                                                          )
                                                  )
                                                  ,column(width = 3
                                                          ,conditionalPanel(condition = "input.eqnCreate_type_of_equation=='chem_rxn'"
                                                                            ,numericInput(inputId = "eqnCreate_num_of_eqn_RHS"
                                                                                          ,label = "Number of Products"
                                                                                          ,value = 1
                                                                                          ,min = 1
                                                                                          ,step = 1)
                                                          )
                                                  )
                                                )#end fluidRow
                                                ,hr()
                                                ,fluidRow(
                                                  column(
                                                    width = 9
                                                    ,verbatimTextOutput(outputId = "eqnCreate_showEquationBuilding",
                                                                         placehold = TRUE) 
                                                  )
                                                  ,column(
                                                    offset = 1
                                                    ,width = 2
                                                    ,align = "right"
                                                    ,div(style = "display:inline-block; vertical-align:top; padding-top:20px",
                                                         actionButton(inputId = "eqnCreate_addEqnToVector"
                                                                       ,label = "Add Equation"
                                                                       ,style = "color: #fff; background-color: #007BFF; border-color: #2e6da4; padding-top:10px; padding-bottom:10px")
                                                         )
                                                  )
                                                  )
                                              )
                                              # ,column(
                                              #   width = 3
                                              #   ,fluidRow(
                                              #     column(
                                              #       width = 12
                                              #       ,style = "border-bottom:1px solid"
                                              #       ,align = "center"
                                              #       ,h4("Options")
                                              #     )
                                              #   )
                                              #   ,style = "border:1px solid;"
                                              #   ,uiOutput("eqnCreate_Options")
                                              # )
                                            )
                                               
                                      )#end tabitem new
                                      ,tabPanel("Edit"
                                                ,fluidRow(column(width = 6
                                                                 ,pickerInput(inputId = "eqnCreate_edit_select_equation"
                                                                              ,label = "Select Equation Number to Edit"
                                                                              ,choices = "")
                                                )
                                                ,column(width = 2
                                                        ,div
                                                        (
                                                          style = "display: inline-block;vertical-align:top;padding-top:25px;padding-left:-35px"
                                                          ,actionButton(inputId = "createEqn_edit_equation_button"
                                                                        ,label = "Edit")
                                                        )
                                                )
                                                )
                                                ,hr()
                                                ,uiOutput('eqnCreate_renderingUIcomponents')
                                                ,hr()
                                                ,verbatimTextOutput("build_equation_edit")
                                                
                                      )
                                      ,tabPanel("Delete"
                                                ,fluidRow(column(width = 6
                                                                 ,pickerInput(inputId = "eqnCreate_delete_equation"
                                                                              ,label = "Select Equation Number to delete"
                                                                              ,choices = "")
                                                )
                                                ,column(width = 2
                                                        ,div
                                                        (
                                                          style = "display: inline-block;vertical-align:top;padding-top:25px;padding-left:-35px"
                                                          ,actionButton(inputId = "createEqn_delete_equation_button"
                                                                        ,label = "Delete")
                                                        )
                                                )
                                                ))
                                      ,tabPanel("View"
                                                ,"This section is to show the differential outputs that would result from each of the selected equations"
                                                ,conditionalPanel(condition = "input.eqnCreate_type_of_equation=='enzyme_rxn'"
                                                                  ,uiOutput("test_mathJax"))
                                      )
                                  )#end tabbox
                                )
                              ),
                              conditionalPanel(condition = "input.tabbox_build_equations == 'New'",
                                               fluidRow(
                                                 column(
                                                   width = 12,
                                                   box(
                                                     title = "Equation Builder",
                                                     solidHeader = FALSE,
                                                     collapsible = TRUE,
                                                     closable = FALSE,
                                                     width = 12,
                                                     conditionalPanel(condition = "input.eqnCreate_type_of_equation=='chem_rxn'"
                                                                      ,uiOutput("eqnCreate_equationBuilder_chem"))
                                                     #,uiOutput("eqnCreate_equationBuilder_chem_forward_modifiers")
                                                     #,uiOutput("eqnCreate_equationBuilder_chem_reverse_modifiers")
                                                     ,conditionalPanel(condition = "input.eqnCreate_type_of_equation=='enzyme_rxn'"
                                                                       ,uiOutput("eqnCreate_equationBuilder_enzyme"))
                                                     ,conditionalPanel(condition = "input.eqnCreate_type_of_equation=='simp_diff'"
                                                                       ,uiOutput("eqnCreate_equationBuilder_simp_diff"))
                                                     ,conditionalPanel(condition = "input.eqnCreate_type_of_equation=='rate_eqn'"
                                                                       ,fluidRow(column(width  = 5
                                                                                        ,textInput(inputId = "eqnCreate_rate_new_parameter"
                                                                                                   ,label = "Additional Paramters for Model"
                                                                                                   ,value = ""
                                                                                                   ,placeholder = "Ex. Var1, Var2, Var3"))
                                                                                 # ,column(width = 2
                                                                                 #         ,actionButton(inputId = "eqnCreate_rate_store_new_parameter"
                                                                                 #                       ,label = "Store Parameter"
                                                                                 #                       ,style="color: #fff; background-color: green; border-color: #2e6da4"))
                                                                       )
                                                                       ,hr()
                                                                       ,fluidRow(column(width = 4
                                                                                        ,pickerInput(inputId = "eqnCreate_rate_firstvar"
                                                                                                     ,label = "Rate Variable"
                                                                                                     ,choices = c()
                                                                                                     ,options = list(
                                                                                                       'live-search' = TRUE)
                                                                                        ))
                                                                                 ,column(width = 1
                                                                                         ,div(style = "padding-top:30px",
                                                                                              "="))
                                                                                 ,column(width = 7
                                                                                         ,textInput(inputId = "eqnCreate_rate_equation"
                                                                                                    ,label = "Rate Equation"
                                                                                                    ,value = ""))
                                                                       )
                                                     )
                                                     ,conditionalPanel(condition = "input.eqnCreate_type_of_equation=='time_dependent'"
                                                                       ,fluidRow(column(width = 4
                                                                                        ,textInput(inputId = "eqnCreate_time_dependent_parameters"
                                                                                                   ,label = "Parameter for time dependent equations"
                                                                                                   ,value = ""))
                                                                                 ,column(width = 2
                                                                                         ,actionButton(inputId = "eqnCreate_time_dependent_store_new_parameter"
                                                                                                       ,label = "Store Parameter"
                                                                                                       ,style = "color: #fff; background-color: green; border-color: #2e6da4"))
                                                                       )
                                                                       ,hr()
                                                                       ,fluidRow(column(width = 4
                                                                                        ,textInput(inputId = "eqnCreate_time_dependent_firstvar"
                                                                                                   ,label = "Time Dependent Variable"
                                                                                                   ,value = ""))
                                                                                 ,column(width = 1
                                                                                         ,"=")
                                                                                 ,column(width = 7
                                                                                         ,textInput(inputId = "eqnCreate_time_dependent_equation"
                                                                                                    ,label = "Equation"
                                                                                                    ,value = ""))
                                                                       )
                                                     ),
                                                     sidebar = boxSidebar(
                                                       id = "eqnCreate_sideBar",
                                                       uiOutput("eqnCreate_Options")
                                                     )
                                                   ) #end box
                                                 ) #end column
                                               ) #end fluidRow
                                               )
                              
                              ,fluidRow(
                                column(
                                  width = 12
                                 ,tabBox(width = 12
                                         ,tabPanel("Equations"
                                                   ,htmlOutput(outputId = "eqnCreate_showEquations"))
                                         ,tabPanel("Additional Equations"
                                                   ,htmlOutput(outputId = "eqnCreate_showAdditionalEquations"))
                                         ,tabPanel("Equation Descriptions",
                                                   radioGroupButtons(inputId = "eqnCreate_EqnDescriptionDisplayType"
                                                                     ,"View Type"
                                                                     ,choices = c("Single View" = "single",
                                                                                     "Flow View" = "flow")
                                                                     ,checkIcon = list(
                                                                       yes = icon("ok", 
                                                                                  lib = "glyphicon"))
                                                                     )
                                                  ,conditionalPanel(condition = "input.eqnCreate_EqnDescriptionDisplayType == 'single'",
                                                                    pickerInput("eqnCreate_selectEqnForDescription",
                                                                                "Select Equation",
                                                                                choices = c())
                                                                    ,uiOutput("eqnCreate_eqnDescription"),
                                                                    actionButton(inputId = "eqnCreate_storeEqnDescription"
                                                                                 ,label = "Store Description"
                                                                                 ,style = "color: #fff; background-color: green; border-color: #2e6da4")
                                                                    )
                                                  ,conditionalPanel(condition = "input.eqnCreate_EqnDescriptionDisplayType == 'flow'",
                                                                    uiOutput("eqnCreate_eqnDescriptionFlow")
                                                                    )
                                                   )
                                 ) #end tabbox
                                ) #end column
                              ) #end fluidRow
)#end TabItem


