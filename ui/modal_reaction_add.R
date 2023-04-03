shinyBS::bsModal(
  id = "modal_create_equations",
  title = NULL,
  trigger = "eqns_add_open_modal",
  size = "large",
  fluidRow(
    column(
      width = 3,
      pickerInput(
        inputId = "eqnCreate_active_compartment",
        label = "Active Compartment",
        choices = c()
      )
    )
  ),
  fluidRow(
    column(
      width = 3,
      ### Equation Builder Main Sidebar----------------------------------
      box(
        id = "eqnbuilder_sidebar",
        solidHeader = FALSE,
        width = 12,
        collapsible = FALSE,
        pickerInput(
          inputId = "eqnCreate_type_of_equation",
          label = "Equation Type",
          choices = c("Chemical Reaction" = "chem_rxn",
                      "Enzyme Based Reaction" = "enzyme_rxn",
                      "Synthesis" = "syn",
                      "Degradation" = "deg",
                      "Custom Equation" = "rate_eqn",
                      "Time Dependent Equation" = "time_dependent"
          )
        ),
        conditionalPanel(
          condition = 
            "input.eqnCreate_type_of_equation == 'chem_rxn'",
          pickerInput(
            inputId = "eqn_chem_law",
            label = "Law",
            choices = c("Mass Action" = "MA",
                        "Regulated Mass Action" = "MAwR"
            )
          ),
          pickerInput(
            inputId = "eqn_chem_forward_or_both",
            label = "Reaction Direction",
            choices = c("Reversible" = "both_directions",
                        "Forward" = 'forward_only'),
            choicesOpt = 
              list(
                icon = c("glyphicon glyphicon-resize-horizontal",
                         "glyphicon glyphicon-arrow-right"
                )
              )
          ),
          conditionalPanel(
            condition = "input.eqn_chem_law == 'MAwR'",
            hr(),
            prettyCheckbox(
              inputId = "eqn_options_chem_modifier_forward",
              label = "Add Forward Regulator(s)",
              value = FALSE
            ),
            conditionalPanel(
              condition = "input.eqn_options_chem_modifier_forward",
              numericInput(
                inputId = "eqn_options_chem_num_forward_regulators",
                label = "# of Forward Regulators",
                value = 1,
                min = 1,
                step = 1)
            ),
            conditionalPanel(
              condition = "input.eqn_chem_forward_or_both == 
                                                          'both_directions'",
              prettyCheckbox(
                inputId = "eqn_options_chem_modifier_reverse",
                label = "Add Reverse Regulator(s)",
                value = FALSE
              ),
              conditionalPanel(
                condition = 
                  "input.eqn_options_chem_modifier_reverse",
                numericInput(
                  inputId = 
                    "eqn_options_chem_num_reverse_regulators",
                  label = "# of Reverse Regulators",
                  value = 1,
                  min = 1,
                  step = 1)
              )
            )
          )
        ),
        conditionalPanel(
          condition = 
            "input.eqnCreate_type_of_equation == 'enzyme_rxn'",
          pickerInput(
            inputId = "eqn_enzyme_law",
            label = "Law",
            choices = c("Michaelis Menten Kinetics" = "MM",
                        "Other" = "Other")
          ),
          hr(),
          prettyCheckbox(
            inputId = "eqn_options_enzyme_useVmax",
            label = "Use Vmax",
            value = FALSE
          )
        ),
        conditionalPanel(
          condition = "input.eqnCreate_type_of_equation == 'syn'",
          pickerInput(
            inputId = "eqn_syn_law",
            label = "Law",
            choices = c("Rate" = "rate",
                        "By Factor" = "byFactor")
          )
        ),
        conditionalPanel(
          condition = "input.eqnCreate_type_of_equation == 'deg'",
          pickerInput(
            inputId = "eqn_deg_law",
            label = "Law",
            choices = c("Rate" = "rate",
                        "By Enzyme" = "byEnzyme")
          ),
          hr(),
          prettyCheckbox(
            inputId = "eqn_deg_to_products",
            label = "Degrades to species",
            value = FALSE
          ),
          conditionalPanel(
            condition = "input.eqn_deg_to_products",
            numericInput(
              inputId = "eqn_deg_num_products",
              label = "Number of Species",
              value = 1,
              min = 1,
              step = 1
            )
          ),
          conditionalPanel(
            condition = "input.eqn_deg_law == 'byEnzyme'",
            hr(),
            prettyCheckbox(
              inputId = "eqn_deg_use_Vmax",
              label = "Use Vmax",
              value = FALSE
            )
          )
        )
      )
    ),
    ### Equation Builder Tabbox -------------------------------------
    column(
      width = 9,
      box(
        width = 12,
        solidHeader = FALSE,
        collapsible = FALSE,
        id = "tabbox_equation_builder",
        conditionalPanel(
          condition = 
            "input.eqnCreate_type_of_equation=='chem_rxn'",
          fluidRow(
            column(
              width = 3, 
              numericInput(
                inputId = "eqnCreate_num_of_eqn_LHS",
                label = "Number of Reactants",
                value = 1,
                min = 1,
                step = 1)
            ), 
            column(
              width = 3,
              numericInput(
                inputId = "eqnCreate_num_of_eqn_RHS",
                label = "Number of Products",
                value = 1,
                min = 1,
                step = 1)
            )
          ),
          hr(),
          uiOutput("eqnCreate_equationBuilder_chem"),
          tags$head(tags$style(HTML("
                          .shiny-split-layout > div {
                            overflow: visible;
                          }
                          ")))
        ),
        conditionalPanel(
          condition = 
            "input.eqnCreate_type_of_equation == 'enzyme_rxn'",
          uiOutput("eqnCreate_equationBuilder_enzyme")
        ),
        conditionalPanel(
          condition = 
            "input.eqnCreate_type_of_equation == 'syn'",
          uiOutput("eqnCreate_equationBuilder_synthesis")
        ),
        conditionalPanel(
          condition = 
            "input.eqnCreate_type_of_equation == 'deg'",
          uiOutput("eqnCreate_equationBuilder_degradation")
        ),
        conditionalPanel(
          condition = 
            "input.eqnCreate_type_of_equation == 'simp_diff'",
          uiOutput("eqnCreate_equationBuilder_simp_diff")
        ),
        conditionalPanel(
          condition = 
            "input.eqnCreate_type_of_equation == 'rate_eqn'",
          uiOutput("eqnCreate_equationBuilder_custom_rate")
        ),
        conditionalPanel(
          condition = 
            "input.eqnCreate_type_of_equation == 
                                                          'time_dependent'",
          uiOutput("eqnCreate_equationBuilder_time_equation")
        ),
        hr(),
        fluidRow(
          column(
            width = 12,
            uiOutput("eqnCreate_showEquationBuilding")
          )
          # column(
          #   width = 2,
          #   align = "right",
          #   div(style = "padding-top:6px",
          #       )
          # )
        )
        # tabPanel(
        #   "Info",
        #   # conditionalPanel(
        #   #   condition = 
        #   #"input.eqnCreate_type_of_equation == 'chem_rxn'",
        #   #   conditionalPanel(
        #   #     condition = "input.eqn_chem_law == 'MA'",
        #   #     uiOutput("mathjax_MA")
        #   #   ),
        #   #   conditionalPanel(
        #   #     condition = "input.eqn_chem_law == 'MAwR'",
        #   #     uiOutput("mathjax_MA_with_regulators")
        #   #   ),
        #   # ),
        #   # conditionalPanel(
        #   #   condition = 
        #   #"input.eqnCreate_type_of_equation =='enzyme_rxn'",
        #   #   uiOutput("enzyme_MM")
        #   #   )
        #   "Coming Soon..."
        # )
      )
    )
    
  ),
  hr(),
  fluidRow(
    column(
      width = 6,
      checkboxInput(
        inputId = "checkbox_modal_keep_active_add_eqn",
        label = "Close on Add",
        value = FALSE
      )
    ),
    column(
      width = 6,
      align = "right",
      actionButton(
        inputId = "eqnCreate_addEqnToVector",
        label = "Add Equation",
        width = "auto")
    )
  )
)