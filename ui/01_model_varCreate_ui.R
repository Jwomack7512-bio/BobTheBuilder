#This tab corresponds to the "Data Management" Tab of the App
#-------------------------------------------------------------------------
#  Justin Womack
#  January 5, 2021
#  Last Update: January 5, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_VAR_CREATE <- 
  tabItem(
    tabName = "TAB_VAR_CREATE",
    bsModal(
      id = "modal_create_compartment",
      title = "Create Compartment",
      trigger = "createVar_add_compartment",
      textInput(
        inputId = "modal_createCompartment_compartment_name",
        label = "Compartment Name",
        value = ""
      ),
      textInput(
        inputId = "modal_createCompartment_volume_variable",
        label = "Volume Variable",
        value = ""
      ),
      textInput(
        inputId = "modal_createCompartment_volume_value",
        label = "Volume Value",
        value = ""
      ),
      pickerInput(
        inputId = "modal_createCompartment_volume_unit",
        label = "Volume Unit",
        choices = c()
      ),
      textAreaInput(
        inputId = "modal_createCompartment_description",
        label = "Description",
        value = "",
        width = NULL,
        height = "200px"
      ),
      hr(),
      fluidRow(
        column(
          width = 12,
          align = "right",
          actionButton("modal_createCompartment_add_button",
                       "Delete")
        )
      )
    ),
    bsModal(
      id = "modal_create_variable",
      title = NULL,
      trigger = "createVar_add_variable_to_all_button",
      "Enter the name of the variable to add to all compartments and choose the 
      subsetting term.",
      textInput(
        inputId = "modal_variable_name",
        label = "Name",
        value = ""
      ),
      br(),
      br(),
      br(),
      radioButtons(
        inputId = "modal_variable_name_subset",
        label = "How to subset variable",
        choices = c("Compartment Name" = "COMPNAME",
                    "Numerical" = "COMPNUMBER"),
        inline = TRUE
      ),
      hr(),
      fluidRow(
        column(
          width = 12,
          align = "right",
          div(
            actionButton("modal_createVariable_add_button",
                         "Add"),
            actionButton("modal_createVariable_cancel_button",
                         "Cancel")
          )
        )
      )
    ),
    tags$head(
      tags$style(
        "#modal_create_variable .modal-footer{ display:none}")
      ),
    jqui_sortable(
      div(
    # Info Box -----------------------------------------------------------------
        fluidRow(
          column(
            width = 12,
            box(
              id = "create_var_info_box",
              title = "Info",
              collapsible = TRUE,
              collapsed = TRUE,
              width = 12,
              h3("How To Use")
              ,tags$div(
                tags$ul(
                  tags$li("Type variable name in following naming conventions below."),
                  tags$li("Variables can me multi-entered by using the following conventions:"),
                  tags$div(
                    tags$ul(
                      tags$li("Space separated variables: Var1 Var2 Var3"),
                      tags$li("Comma separated variables: Var1, Var2, Var3")
                    )              
                  ),
                  tags$li("Press the \"Add Variable\" button. You should see the variable(s) add to the table below."),
                  tags$li("Double click on correpsonding 'Description' tab on table to add a description to the variable. This is 
                                                            useful for you and others to remember what the variable is and will be used in the export methods."),
                  tags$li("Variable names can be changed here as well. But note that they do not currently change elsewhere in model.
                                                            So if they are placed in an equation an error will occur.  This will be fixed at a later time.")
                )
              )
              ,h3("Naming Conventions")
              ,tags$div(
                tags$ul(
                  tags$li("Do not start variable name with a number or special character"),
                  tags$li("Special Characters allowed are: \"_\" and \".\" (eg my_var, my.var, my.weird_var)"),
                  tags$li("Variables are case sensitive (ie Var1 is different from var1)")
                )
              )
            )
          )
        ),
    # Compartment Box ----------------------------------------------------------
        fluidRow(
          column(
            width = 12,
            box(
              id = "createVar_compartment_box",
              width = 12,
              title = "Compartments",
              collapsible = TRUE,
              div(
                rHandsontableOutput("createVar_compartment_table"),
                fluidRow(
                  column(
                    offset = 9,
                    width = 3,
                    align = "right",
                    actionBttn(
                      inputId = "createVar_add_compartment_button",
                      label = NULL,
                      style = "material-circle",
                      color = "primary",
                      icon = icon("plus"),
                      size = "xs"
                    ),
                    actionBttn(
                      inputId = "createVar_remove_compartment_button",
                      label = NULL,
                      style = "material-circle",
                      color = "danger",
                      icon = icon("minus"),
                      size = "xs"
                    ) 
                  )
                )
              )
            )
          )
        ),
    # Variable Box -------------------------------------------------------------
        fluidRow(
          column(
            width = 12,
            box(
              id = "createVar_variable_box",
              width = 12,
              title = "Species",
              collapsible = TRUE,
              div(style = "background-color:#F9F9F9;
                       border: 1px solid #c5c5c5;
                       border-radius: 12px;
                       padding: 10px 10px 10px 10px;",
                  fluidRow(
                    column(
                      width = 3,
                      pickerInput(
                        inputId = "createVar_active_compartment",
                        label = "Active Compartment",
                        choices = c()
                      )
                    ),
                    column(
                      width = 3,
                      prettyCheckbox(
                        inputId = "createVar_show_active_compartment_only",
                        label = "Show Active Compartment Only",
                        value = TRUE
                      ),
                      prettyCheckbox(
                        inputId = "createVar_add_to_all_compartments",
                        label = "Add To All Compartments",
                        value = FALSE
                      )
                    )
                  ) 
              ),
              hr(),
              fluidRow(
                column(
                  width = 12,
                  div(
                    rHandsontableOutput("myVariables_DT"),
                    fluidRow(
                      column(
                        offset = 9,
                        width = 3,
                        align = "right",
                        hidden(
                          actionBttn(
                            inputId = "createVar_add_variable_to_all_button",
                            label = NULL,
                            style = "material-circle",
                            color = "primary",
                            icon = icon("plus"),
                            size = "xs"
                          )
                        ),
                        actionBttn(
                          inputId = "createVar_add_variable_button",
                          label = NULL,
                          style = "material-circle",
                          color = "primary",
                          icon = icon("plus"),
                          size = "xs"
                        ),
                        actionBttn(
                          inputId = "createVar_remove_variable_button",
                          label = NULL,
                          style = "material-circle",
                          color = "danger",
                          icon = icon("minus"),
                          size = "xs"
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ),
    # Equation Box -------------------------------------------------------------
        fluidRow(
          column(
            width = 12,
            box(
              id = "createVar_equation_box",
              width = 12,
              title = "Equations",
              collapsible = TRUE,
              "pH"
            )
          )
        ),
    # Input/Output Box ---------------------------------------------------------
        fluidRow(
          column(
            width = 12,
            box(
              id = "createVar_input_output_box",
              width = 12,
              title = "Input/Output",
              collapsible = TRUE,
              collapsed = TRUE,
              fluidRow(
                column(
                  width = 3,
                  div(
                    style = "background-color:#F9F9F9;
                       border: 1px solid #c5c5c5;
                       border-radius: 12px;
                       padding: 10px 10px 10px 10px;",
                    pickerInput(
                      inputId = "CIO_IO_options",
                      label = "Options",
                      choices = c(
                        "Flow In" = "FLOW_IN",
                        "Flow Out" = "FLOW_OUT",
                        #"Flow Between Compartments" = "FLOW_BETWEEN",
                        "Clearance" = "CLEARANCE",
                        "Simple Diffusion" = "SIMPDIFF",
                        "Facillitated Diffusion" = "FACILITATED_DIFF"
                      )
                    )
                  )
                ),
                column(
                  width = 9,
                  div(
                    style = "background-color:#F9F9F9;
                       border: 1px solid #c5c5c5;
                       border-radius: 12px;
                       padding: 10px 10px 10px 10px;",
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
                  actionBttn(inputId = "CIO_add_IO",
                             label = "Add")
                )
              ),
              br(),
              fluidRow(
                column(
                  width = 12, 
                  htmlOutput(
                    outputId = "CIO_IO_Logs"
                  )
                )
              )
            )
          )
        ),
    # Parameter Box ------------------------------------------------------------
        fluidRow(
          column(
            width = 12,
              box(
                id = "createVar_parameter_box",
                width = 12,
                title = "Parameters",
                collapsible = TRUE,
                collapsed = TRUE,
                div(style = "background-color:#F9F9F9;
                       border: 1px solid #c5c5c5;
                       border-radius: 12px;
                       padding: 10px 10px 10px 10px;",
                    fluidRow(
                      column(
                        width = 3,
                        pickerInput(
                          inputId = "parameters_filter_type",
                          label = "View By:",
                          choices = c("All",
                                      "Eqns",
                                      "Inputs",
                                      "Outputs")
                        )
                      )
                    )
                ),
                hr(),
                fluidRow(
                  column(
                    width = 12,
                    div(
                      rHandsontableOutput("parameters_DT")
                    )
                  )
                )
              )
            )
        ),
    # Differential Equations ---------------------------------------------------
        fluidRow(
          column(
            width = 12,
            box(
              id = "createVar_differential_equations_box",
              width = 12,
              title = "Differential Equations",
              collapsible = TRUE,
              collapsed = TRUE,
              htmlOutput(outputId = "diffeq_display_diffEqs"),
              hr(),
              fluidRow(
                column(
                  width = 12,
                  align = "right",
                  actionButton(
                    inputId = "diffeq_generate_equations",
                    label = "Generate")
                )
              ),
              conditionalPanel(
                condition = "input.diffeq_custom_option",
                hr(),
                fluidRow(
                  column(
                    width = 3,
                    pickerInput(
                      inputId = "diffeq_var_to_custom",
                      label = "Species For Customization",
                      choices = c()
                    )
                  ),
                  column(
                    width = 7,
                    textInput(
                      inputId = "diffeq_custom_eqn",
                      label = "Custom Equation",
                      value = "",
                      placeholder = "k_f1*A-k_r1*B"
                    )
                  ),
                  column(
                    width = 2,
                    div(style = "padding-top:28px",
                      actionButton(
                        inputId = "diffeq_custom_eqn_button",
                        label = "Customize")
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    "Note that any custom equation will disable that equation 
                    to be solved for by the computer. You can choose to allow 
                    equation to be generated by adding it below"
                  )
                ),
                fluidRow(
                  column(
                    width = 3,
                    pickerInput(
                      inputId = "diffeq_multi_custom_eqns",
                      label = "Custom Equations to Ignore",
                      choices = c(),
                      multiple = TRUE
                    )
                  )
                )
              ),
              sidebar = boxSidebar(
                id = "diffeq_sidebar",
                icon = icon("bars", lib = "font-awesome"),
                width = 25,
                checkboxInput(
                  inputId = "diffeq_option_simplify",
                  label = "Simplify Equations",
                  value = FALSE), 
                checkboxInput(
                  inputId = "diffeq_custom_option",
                  label = "Create Custom Equation",
                  value = FALSE
                )
              )
            )
          )
        )
      ) # end div
    ),#end sortable

    tags$head(tags$style('#html_table_vars .box-header{ display: none}')),  
    tags$head(tags$style('#box1 .box-header{ display: none}')),
    tags$head(
      tags$style(".PE_variable_UI_table label {display: table-cell; 
                             text-align: center;
                             vertical-align: top; } 

.PE_variable_UI_table .form-group {display: table-cell;}")
    )
  )#end tabItem
