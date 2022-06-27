
TAB_SUMMARY <- tabItem(tabName = "TAB_SUMMARY", 
                       
                       fluidRow(
                         column(
                           width = 4,
                           box(
                             title = "Reaction Equations",
                             width = 12,
                             div(style = 'height:370px;
                                           overflow-y: scroll;',
                                 htmlOutput(outputId = "summary_reaction_equations"))
                             # tags$head(
                             # tags$style("#summary_reaction_equations {
                             #                          font-size: 20px;
                             #                          
                             #                                          }"
                             # )
                             # )
                           )
                         ),
                         column(
                           width = 8,
                           jqui_resizable(plotOutput("summary_plot"))
                         )
                       ),
                       fluidRow(
                         column(
                           width = 4,
                           box(
                             title = "Differential Equations",
                             width = 12,
                             div(style = 'height:325px;
                                           overflow-y: scroll;',
                                 htmlOutput(outputId = "summary_differential_equations"))
                           )
                         ),
                         column(
                           width = 4,
                             DTOutput("summary_variable_table")
                         ),
                         column(
                           width = 4,
                             DTOutput("summary_parameter_table")
                         )

                       )
)
