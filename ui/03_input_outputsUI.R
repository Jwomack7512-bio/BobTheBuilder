#This tab corresponds to the "Input/Output" tab
#-------------------------------------------------------------------------
#  Justin Womack
#  January 20, 2021
#  Last Update: January 20, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_InOut <- tabItem(tabName="TAB_InOut"
                     ,fluidRow(column(width=3
                                      ,box(title = "Select Variable"
                                               ,closable = FALSE
                                               ,width = NULL
                                               ,status = "success"
                                               ,solidHeader = FALSE
                                               ,collapsible = TRUE
                                               ,enable_dropdown = FALSE
                                               ,pickerInput(inputId = "InOut_selectVar"
                                                            ,label = "Select Variable to Add Input or Output To"
                                                            ,choices = c()
                                                            ,options = pickerOptions(liveSearch = TRUE
                                                                                     ,liveSearchStyle = "startsWith"
                                                                                     ,dropupAuto = FALSE))
                                               ,awesomeRadio(inputId = "InOut_radio"
                                                            ,label = "Select Condition"
                                                            ,choices = c("Input" = "Input"
                                                                         ,"Output" = "Output")
                                                            ,selected = "Input")
                                      
                                               
                                               ) #end box
                                      
                                      ) #endColumn
                               ,column(width = 9
                                       ,box(title = "Put input or Output to the Overall System"
                                                ,closable = FALSE 
                                                ,width = NULL
                                                ,status = "success" 
                                                ,solidHeader = FALSE 
                                                ,collapsible = TRUE
                                                ,enable_dropdown = TRUE
                                                ,dropdown_icon = "wrench"
#Condition = input
                                                ,conditionalPanel(condition = "input.InOut_radio == 'Input'"
                                                                  ,verbatimTextOutput("InOut_showInForVar")
                                                                  ,fluidRow(column(width=4
                                                                                   ,pickerInput(inputId="InOut_typeOfIn"
                                                                                                ,label = "Select Input Type"
                                                                                                ,choices = c("Rate" = "Rate"
                                                                                                             ,"Simple Diffusion" = "simp_diff"
                                                                                                             , "Synthesis" = "Synthesis")))
                                                                            #,uiOutput("InOut_InOptions")
                                                                  ) #end FluidRow
                                                                  ,hr()
#condition = input ->rate
                                                                  ,conditionalPanel(condition="input.InOut_typeOfIn == 'Rate'"
                                                                                    ,p("Type the name of the rate constant you wish to put into the model.  Edit the value in Parameter tab.")
                                                                                    ,hr()
                                                                                    ,fluidRow(column(width=3
                                                                                                     ,textInput(inputId="In_rate_id"
                                                                                                                ,label = "Rate Constant Name"
                                                                                                                ,value = ""
                                                                                                                ,placeholder = "r_in"))
                                                                                              ,column(width=9
                                                                                                      ,checkboxInput(inputId = "In_rate_multiply_with_species"
                                                                                                                     ,label = "Multiply rate constant by the concentration of this variable"
                                                                                                                     ,value = FALSE))
                                                                                    )#end fluidRow
                                                                  )#end conditional Panel 'rate'
                                                                  
                                                                  ,actionButton(inputId="Inout_addInVarToDf"
                                                                                ,label="Add Input"
                                                                                ,style="color: #fff; background-color: green; border-color: #2e6da4")
                                                                  ) #end Conditional Pane
#condition  = output
                                                ,conditionalPanel(condition = "input.InOut_radio == 'Output'"
                                                                  ,verbatimTextOutput("InOut_showOutForVar")
                                                                  ,fluidRow(column(width=4
                                                                                   ,pickerInput(inputId="InOut_typeOfOut"
                                                                                                ,label = "Select Output Type"
                                                                                                ,choices = c("Rate" = "Rate"
                                                                                                             ,"Simple Diffusion" = "simp_diff"
                                                                                                             , "Degradation by Enzyme" = "Enzyme_Degradation"
                                                                                                             ,"Mass Action Removal" = "mass_action")))
                                                                  ) #end FluidRow
                                                                  ,hr()
#condition = output -> rate
                                                                  ,conditionalPanel(condition="input.InOut_typeOfOut == 'Rate'"
                                                                                    ,p("Type the name of the rate constant you wish to put into the model.  Edit the value in Parameter tab.")
                                                                                    ,hr()
                                                                                    ,fluidRow(column(width=3
                                                                                                     ,textInput(inputId="Out_rate_id"
                                                                                                                ,label = "Rate Constant Name"
                                                                                                                ,value = ""
                                                                                                                ,placeholder = "r_out"))
                                                                                              ,column(width=9
                                                                                                      ,checkboxInput(inputId = "Out_rate_multiply_with_species"
                                                                                                                     ,label = "Multiply rate constant by the concentration of this variable"
                                                                                                                     ,value = FALSE))
                                                                                    )#end fluidRow
                                                                  )#end conditional Panel 'rate'
                                                                  ,conditionalPanel(condition="input.InOut_typeOfOut == 'Enzyme_Degradation'"
                                                                                    # ,pickerInput(inputId = "enzyme_deg_substrate"
                                                                                    #              ,label = "Substrate"
                                                                                    #              ,choices = c())
                                                                                    ,textInput(inputId = "enzyme_deg_km"
                                                                                               ,label = "Km"
                                                                                               ,value = "",
                                                                                               placeholder = "Km_1")
                                                                                    ,conditionalPanel(condition = "!input.enzyme_deg_vmax_opt"
                                                                                                     ,textInput(inputId = "enzyme_deg_Vmax"
                                                                                                                ,label = "Vmax"
                                                                                                                ,value = ""
                                                                                                                ,placeholder = "Vmax_1"))
                                                                                    
                                                                                    ,checkboxInput(inputId = "enzyme_deg_vmax_opt"
                                                                                                   ,label = "Expand Vm"
                                                                                                   ,value = FALSE),
                                                                                    conditionalPanel(condition = "input.enzyme_deg_vmax_opt"
                                                                                                     ,textInput(inputId = "enzyme_deg_kcat"
                                                                                                                ,label = "kcat"
                                                                                                                ,value = ""
                                                                                                                ,placeholder = "kcat_1")
                                                                                                     ,pickerInput(inputId = "enzyme_deg_enzyme"
                                                                                                                  ,label = "Enzyme",
                                                                                                                  choices = c()))
                                                                                    
                                                                                    )
                                                                ,conditionalPanel(condition="input.InOut_typeOfOut == 'mass_action'"
                                                                                  ,pickerInput(inputId = "MA_species"
                                                                                               ,label = "Species that remove substrate"
                                                                                               ,choices = c()
                                                                                               ,multiple = TRUE)
                                                                                  ,textInput(inputId = "MA_deg_rate_constant"
                                                                                             ,label = "Rate Constant"
                                                                                             ,value = "")
                                                                )
                                                                  ,actionButton(inputId = "Inout_addOutVarToDf"
                                                                                ,label = "Add Output"
                                                                                ,style = "color: #fff; background-color: green; border-color: #2e6da4"))
                                       )#end box
                                    )
                               ) #end fluidRow
                       ,fluidRow(column(width=9
                                        ,offset=3
                                         ,h3("Logs")
                                         ,box(title=NULL
                                                  ,solidHeader=FALSE
                                                  #,background="#000"
                                                  ,collapsible = FALSE
                                                  ,closable=FALSE
                                                  ,width=12
                                                  ,htmlOutput(outputId="IO_Display_Logs")
                                                  ,hr()
                                                  ,fluidRow(column(width=4
                                                                   ,pickerInput(inputId = "Inout_delete_IO_eqn"
                                                                                ,label = "Select Eqn Number to delete"
                                                                                ,choices = c()))
                                                            ,column(width=2
                                                                   ,actionButton(inputId="Inout_button_delete_IO_eqn"
                                                                                 ,label="Delete"
                                                                                 ,style="color: #fff; background-color: red; border-color: #2e6da4"))
                                                            )
                                                  )
                                        # ,fluidRow(column(width = 7
                                        #                   ,pickerInput(inputId = "Inout_delete_IO_eqn"
                                        #                                ,label = "Select Eqn Number to delete"
                                        #                                ,choices = c())
                                        #                  )
                                        #           ,column(width = 3
                                        #                  ,actionButton(inputId="Inout_button_delete_IO_eqn"
                                        #                                ,label="Remove Last Added"
                                        #                                ,style="color: #fff; background-color: red; border-color: #2e6da4"))
                                        #           )
                                )
                       )
                     
)#end tabitem