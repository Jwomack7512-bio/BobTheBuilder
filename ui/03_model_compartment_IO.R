
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
            choices = c("Flow In" = "FLOW_IN",
                        "Flow Out" = "FLOW_OUT",
                        "Flow Between Compartments" = "FLOW_BETWEEN",
                        "Clearance" = "CLEARANCE",
                        "Simple Diffusion" = "SIMPDIFF",
                        "Facillitated Diffusion" = "FACILITATED_DIFF")
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
                condition = "input.CIO_IO_options == 'FLOW_IN'",
                fluidRow(
                  column(
                    width = 3,
                    pickerInput(
                      inputId = "CIO_flow_in_compartment",
                      label = "Compartment",
                      choices = c()
                    )
                  ),
                  column(
                    width = 3,
                    pickerInput(
                      inputId = "CIO_flow_in_species",
                      label = "Species",
                      choices = c()
                    )
                  ),
                  column(
                    width = 3,
                    textInput(
                      inputId = "CIO_flow_in_rate_constant",
                      label = "Flow Rate",
                      value = 1
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.CIO_IO_options == 'FLOW_OUT'",
                fluidRow(
                  column(
                    width = 3,
                    pickerInput(
                      inputId = "CIO_flow_out_compartment",
                      label = "Compartment",
                      choices = c()
                    )
                  ),
                  column(
                    width = 3,
                    pickerInput(
                      inputId = "CIO_flow_out_species",
                      label = "Species",
                      choices = c()
                    )
                  ),
                  column(
                    width = 3,
                    textInput(
                      inputId = "CIO_flow_out_rate_constant",
                      label = "Flow Rate",
                      value = 1
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.CIO_IO_options == 'FLOW_BETWEEN'",
                fluidRow(
                  column(
                    width = 3,
                    pickerInput(
                      inputId = "CIO_flowbetween_compartment_out",
                      label = "Flow Out",
                      choices = c()
                    )
                  ),
                  column(
                    width = 3,
                    pickerInput(
                      inputId = "CIO_flowbetween_compartment_in",
                      label = "Flow In",
                      choices = c()
                    )
                  ),
                  column(
                    width = 3,
                    textInput(
                      inputId = "CIO_flowbetween_rate_constant",
                      label = "Rate",
                      value = 1
                    )
                  ),
                ),
                fluidRow(
                  column(
                    width = 3,
                    pickerInput(
                      inputId = "CIO_flowbetween_species",
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
                      choices = c()
                    )
                  ),
                  column(
                    width = 3,
                    textInput(
                      inputId = "CIO_clearance_rate_constant",
                      label = "Rate",
                      value = 1
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.CIO_IO_options == 'SIMPDIFF'",
                fluidRow(
                  column(
                    width = 3,
                    pickerInput(
                      inputId = "CIO_simpdiff_compartment1",
                      label = "Compartment",
                      choices = c()
                    )
                  ),
                  column(
                    width = 3,
                    pickerInput(
                      inputId = "CIO_simpdiff_species1",
                      label = "Species",
                      choices = c()
                    )
                  ),
                  column(
                    width = 3,
                    textInput(
                      inputId = "CIO_simpdiff_rate_constant",
                      label = "PS",
                      value = 1
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 3,
                    pickerInput(
                      inputId = "CIO_simpdiff_compartment2",
                      label = "Compartment",
                      choices = c()
                    )
                  ),
                  column(
                    width = 3,
                    pickerInput(
                      inputId = "CIO_simpdiff_species2",
                      label = "Species",
                      choices = c()
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.CIO_IO_options == 'FACILITATED_DIFF'",
                fluidRow(
                  column(
                    width = 3,
                    pickerInput(
                      inputId = "CIO_facilitatedDiff_compartment1",
                      label = "From Compartment",
                      choices = c()
                    )
                  ),
                  column(
                    width = 3,
                    pickerInput(
                      inputId = "CIO_facilitatedDiff_species1",
                      label = "From Species",
                      choices = c()
                    )
                  ),
                  column(
                    width = 3,
                    textInput(
                      inputId = "CIO_facilitatedDiff_Vmax",
                      label = "Vmax",
                      value = 1
                    )
                  ),
                  column(
                    width = 3,
                    textInput(
                      inputId = "CIO_facilitatedDiff_Km",
                      label = "Km",
                      value = 1
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 3,
                    pickerInput(
                      inputId = "CIO_facilitatedDiff_compartment2",
                      label = "To Compartment",
                      choices = c()
                    )
                  ),
                  column(
                    width = 3,
                    pickerInput(
                      inputId = "CIO_facilitatedDiff_species2",
                      label = "To Species",
                      choices = c()
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