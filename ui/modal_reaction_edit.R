shinyBS::bsModal(
  id = "modal_edit_equations",
  title = NULL,
  trigger = "eqns_edit_open_modal",
  size = "large",
  fluidRow(
    column(
      width = 12,
      "To edit equations, select the equation you wish to edit. Then change
         the variables or parameter values. Note that you cannot change the 
         compartment with edit."
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 12, 
      box(
        width = 12,
        solidHeader = FALSE,
        collapsible = FALSE,
        fluidRow(
          column(
            width = 3,
            pickerInput(
              inputId = "eqnCreate_edit_select_equation",
              label = "Select Equation Number to Edit",
              choices = ""
            )
          ),
          column(
            width = 9,
            uiOutput("build_equation_edit")
          )
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 3,
      div(style = "cursor: not-allowed;",
          div(style = "background-color:#AAAFB4;
                   border: 1px solid #c5c5c5;
                   border-radius: 12px;
                   padding: 10px 10px 10px 10px;
                   pointer-events: none;
                   cursor: not-allowed",
              uiOutput("eqnCreate_renderingUIcomponents")
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
          condition =
            "input.eqnCreate_type_of_equation_edit == 'chem_rxn'",
          uiOutput("eqnCreate_equationBuilder_chem_edit")
        ),
        conditionalPanel(
          condition =
            "input.eqnCreate_type_of_equation_edit == 'enzyme_rxn'",
          uiOutput("eqnCreate_equationBuilder_enzyme_edit")
        ),
        conditionalPanel(
          condition =
            "input.eqnCreate_type_of_equation_edit == 'syn'",
          uiOutput("eqnCreate_equationBuilder_synthesis_edit")
        ),
        conditionalPanel(
          condition =
            "input.eqnCreate_type_of_equation_edit == 'deg'",
          uiOutput("eqnCreate_equationBuilder_degradation_edit")
        ),
        conditionalPanel(
          condition =
            "input.eqnCreate_type_of_equation_edit == 'rate_eqn'",
          uiOutput("eqnCreate_equationBuilder_custom_rate_edit")
        ),
        conditionalPanel(
          condition =
            "input.eqnCreate_type_of_equation_edit == 'time_dependent'",
          uiOutput("eqnCreate_equationBuilder_time_equation_edit")
        )
      )
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 12,
      align = "right",
      div(
        actionButton("modal_editEqn_edit_button",
                     "Confirm Edit")
      )
    )
  )
)