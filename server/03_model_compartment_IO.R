
TAB_COMPARTMENT_IO <- 
  tabItem(
    tabName = "TAB_COMPARTMENT_IO",
    fluidRow(
      column(
        width = 3,
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
        width = 9,
        fluidRow(
          column(
            width = 12,
            box(
              width = 12,
              solidHeader = FALSE,
              collapsible = FALSE,
              closable = FALSE,
              conditionalPanel(
                condition = "CIO_IO_options == 'FLOW'",
                splitLayout(
                  cellWidths = c("33%", "33%", "33%"),
                  pickerInput(
                    inputId = "CIO_flow_out",
                    label = "Flow Out",
                    choices = c()
                  ),
                  pickerInput(
                    inputId = "CIO_flow_in",
                    label = "Flow In",
                    choices = c()
                  ),
                  textInput(
                    inputId = "CIO_flow_rate",
                    label = "Rate",
                    value = 1
                  )
                )
              )
            )
          ),
          column(
            width = 4,
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
          verbatimTextOutput(
            outputId = "CIO_IO_Logs",
            placeholder = "My logs for input and output of compartments"
          )
        )
      )
    )
  )