Tab_Parameter_Estimation <-
  tabItem(tabName = "Tab_Parameter_Estimation", 
          "Parameter Estimation",
          fluidRow(
            column(
              width = 3,
              box(
                width = 12,
                fileInput("pe_obs_data",
                          "Import Observed Data"),
                radioButtons(
                  inputId = "pe_view_import",
                  label = "View:",
                  choices = c("Plot", "Table")
                  )
                )
              ),
            column(
              width = 9,
              box(
                width = 12,
                fluidRow(
                  column(
                    width = 4,
                    pickerInput(
                      "pe_select_par",
                      "Parameters to Estimate:",
                      choices = c(),
                      multiple = TRUE
                    )
                  )
                )
              )
            )
            ),
          fluidRow(
            rHandsontableOutput(
              outputId = "pe_parameter_value_table"
            ),
            actionBttn(
              inputId = "pe_run_parameter_estimation",
              label = "Run"
              )
          ),
          fluidRow( 
            column(
              width = 12,
              conditionalPanel(
                condition = "input.pe_view_import == 'Table'",
                rHandsontableOutput(
                  outputId = "pe_import_data_table"
                )
              ),
              conditionalPanel(
                condition = "input.pe_view_import == 'Plot'", 
                plotOutput(
                  outputId = "pe_parameter_estimation_plot"
                )
              )
            )
          ),
          actionBttn(
            inputId = "pe_store_estimated_parameters",
            label = "Store"
          ),
          verbatimTextOutput(
            outputId = "pe_logs"
          )
          )
  











