#This tab corresponds to the "Differential Equations" tab
#-------------------------------------------------------------------------
#  Justin Womack
#  January 5, 2021
#  Last Update: January 5, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_diffEqs <- tabItem(tabName = "TAB_diffEqs"
                      ,fluidRow
                      (
                        box(title = "System of Differential Equations"
                               ,solidHeader = TRUE
                               ,status = "success"
                               #,background="#000"
                               ,collapsible = FALSE
                               ,closable = FALSE
                               ,width = 12
                               ,htmlOutput(outputId = "diffeq_display_diffEqs")
                               ,sidebar = boxSidebar(
                                  id = "diffeq_sidebar"
                                  ,icon = icon("bars", lib = "font-awesome")
                                  ,width = 25
                                  ,checkboxInput(inputId = "diffeq_option_simplify"
                                                ,label = "Simplify Equations"
                                                ,value = FALSE)
                                  )
                      )
                      )
                      ,fluidRow(column(width = 4
                                      ,actionButton(inputId = "diffeq_generate_equations"
                                                   ,label = "Generate"
                                                   ,style = "color: #fff; background-color: green; border-color: #2e6da4")
                                       )  
                                )
                      ,hr()
                      ,fluidRow
                      (
                          column
                          (
                              width = 12
                              ,p
                              (
                                  "*Note if you generate equations and then add more variables, the new variables will show up here with a repeating list of eqns. \n
                                    This is a visual bug that will need to be fixed.  Simply click the button to regenerate the equations to fix it."  
                              )
                          )
                      )
                      

)#end tabItem



