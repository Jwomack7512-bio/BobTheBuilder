
TAB_COMPARTMENT_IO <- 
  tabItem(
    tabName = "TAB_COMPARTMENT_IO",
    fluidRow(
      column(
        width = 4,
        box(
          width = 12,
          solidHeader = FALSE,
          collapsible = FALSE,
          closable = FALSE,
          pickerInput(
            inputId = "CIO_IO_options",
            label = "Options",
            choices = c("Flow Between Compartments" = "FLOW",
                        "Clearance" = "CLEARANCE")
          )
        )
      ),
      column(
        width = 8,
        fluidRow(
          column(
            width = 12,
            box(
              width = 12,
              solidHeader = FALSE,
              collapsible = FALSE,
              closable = FALSE,
              conditionalPanel(
                condition = "input.CIO_IO_options == 'FLOW'",
                fluidRow(
                  column(
                    width = 3,
                    pickerInput(
                      inputId = "CIO_flow_compartment_out",
                      label = "Flow Out",
                      choices = c()
                    )
                  ),
                  column(
                    width = 3,
                    pickerInput(
                      inputId = "CIO_flow_compartment_in",
                      label = "Flow In",
                      choices = c()
                    )
                  ),
                  column(
                    width = 3,
                    textInput(
                      inputId = "CIO_flow_rate",
                      label = "Rate",
                      value = 1
                    )
                  ),
                ),
                fluidRow(
                  column(
                    width = 3,
                    pickerInput(
                      inputId = "CIO_flow_species",
                      label = "Species",
                      choices = c(),
                      multiple = TRUE
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.CIO_IO_options == 'CLEARANCE'",
                fluidRow(
                  column(
                    width = 3,
                    pickerInput(
                      inputId = "CIO_clearance_compartment",
                      label = "Compartment",
                      choices = c()
                    )
                  ),
                  column(
                    width = 3,
                    pickerInput(
                      inputId = "CIO_clearance_species",
                      label = "Species",
                      choices = c(),
                      multiple = TRUE
                    )
                  ),
                  column(
                    width = 3,
                    textInput(
                      inputId = "CIO_clearance_rate",
                      label = "Rate",
                      value = 1
                    )
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            offset = 8,
            align = "right",
            actionBttn(
              inputId = "CIO_add_IO",
              label = "Add"
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        box(
          width = 12,
          solidHeader = FALSE,
          collapsible = FALSE,
          closable = FALSE,
          br(),
          htmlOutput(
            outputId = "CIO_IO_Logs"
          )
        )
      )
    )
  )