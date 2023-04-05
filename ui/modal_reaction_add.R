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
# Main Sidebar -----------------------------------------------------------------
      box(
        id = "eqnbuilder_sidebar",
        solidHeader = FALSE,
        width = 12,
        collapsible = FALSE,
        pickerInput(
          inputId = "eqnCreate_type_of_equation",
          label = "Reaction Type",
          choices = c("All" = "All",
                      "Chemical Reaction" = "chemical_reaction",
                      "Enzyme Based Reaction" = "enzyme_reaction",
                      # "Synthesis" = "syn",
                      # "Degradation" = "deg",
                      "Custom Equation" = "rate_eqn",
                      "Time Dependent Equation" = "time_dependent"
          )
        ),
        pickerInput(
          inputId = "eqnCreate_reaction_law",
          label = "Reaction Law",
          choices = c()
        ),
        
## Mass Action -----------------------------------------------------------------
        conditionalPanel(
          condition = "input.eqnCreate_reaction_law == 'mass_action'",
          pickerInput(
            inputId = "PI_mass_action_reverisble_option",
            label = "Reversability?",
            choices = c("Reversible" = "both_directions",
                        "Irreversible" = "forward_only"),
            choicesOpt = list(icon = c("glyphicon glyphicon-resize-horizontal",
                                       "glyphicon glyphicon-arrow-right"))
          )
        ),
## Mass Action w Regulation ----------------------------------------------------
        conditionalPanel(
          condition = "input.eqnCreate_reaction_law == 'mass_action_w_reg'",
          pickerInput(
            inputId = "reaction_mass_action_wReg_reverisble",
            label = "Reversability?",
            choices = c("Reversible" = "both_directions",
                        "Irreversible" = 'forward_only'),
            choicesOpt = 
              list(
                icon = c("glyphicon glyphicon-resize-horizontal",
                         "glyphicon glyphicon-arrow-right"
                )
              )
          ),
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
            condition = "input.reaction_mass_action_wReg_reverisble == 
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
        ),
## Synthesis -------------------------------------------------------------------
        conditionalPanel(
          condition = "input.eqnCreate_reaction_law == 'synthesis'",
          prettyCheckbox(
            inputId = "synthesis_factor_checkbox",
            label = "Factor Driving Synthesis?",
            value = FALSE
          )
        ),

## Degradation By Rate ---------------------------------------------------------
        conditionalPanel(
          condition = "input.eqnCreate_reaction_law == 'degradation_rate'",
          prettyCheckbox(
            inputId = "degradation_rate_toProducts_checkbox",
            label = "Degrade Into Products?",
            value = FALSE
          )
        ),
## Degradation By Enzyme -------------------------------------------------------
        conditionalPanel(
          condition = "input.eqnCreate_reaction_law == 'degradation_by_enzyme'",
          prettyCheckbox(
            inputId = "degradation_enzyme_toProducts_checkbox",
            label = "Degrade Into Products?",
            value = FALSE
          ),
          hr(),
          prettyCheckbox(
            inputId = "degradation_enzyme_useVmax_checkbox",
            label = "Use Vmax",
            value = FALSE
          )
        ),
## Michaelis Menten -----------------------------------------------------------
        conditionalPanel(
          condition = "input.eqnCreate_reaction_law == 'michaelis_menten'",
          prettyCheckbox(
            inputId = "michaelis_menten_useVmax_checkbox",
            label = "Use Vmax",
            value = FALSE
          )
        )
      )
    ),
# Equation Builder Tabbox ------------------------------------------------------
    column(
      width = 9,
      box(
        width = 12,
        solidHeader = FALSE,
        collapsible = FALSE,
        id = "tabbox_equation_builder",
        conditionalPanel(
          condition = 
            "input.eqnCreate_reaction_law == 'mass_action'",
          fluidRow(
            column(
              width = 3, 
              numericInput(
                inputId = "NI_mass_action_num_reactants",
                label = "Number of Reactants",
                value = 1,
                min = 1,
                step = 1)
            ), 
            column(
              width = 3,
              numericInput(
                inputId = "NI_mass_action_num_products",
                label = "Number of Products",
                value = 1,
                min = 1,
                step = 1
              )
            )
          ),
          hr(),
          uiOutput("equationBuilder_mass_action"),
          tags$head(tags$style(HTML("
                          .shiny-split-layout > div {
                            overflow: visible;
                          }
                          ")))
        ),
        conditionalPanel(
          condition = 
            "input.eqnCreate_reaction_law == 'mass_action_w_reg'",
          fluidRow(
            column(
              width = 3, 
              numericInput(
                inputId = "NI_mass_action_wReg_num_reactants",
                label = "Number of Reactants",
                value = 1,
                min = 1,
                step = 1)
            ), 
            column(
              width = 3,
              numericInput(
                inputId = "NI_mass_action_wReg_num_products",
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