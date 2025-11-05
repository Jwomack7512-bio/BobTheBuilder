shinyBS::bsModal(
  id = "modal_delete_custom_eqn",
  title = "Delete Custom Equation",
  trigger = "bttn_custom_eqn_delete",
  size = "large",
  fluidRow(
    column(
      width = 12,
      box(
        width = 12,
        solidHeader = FALSE,
        collapsible = FALSE,
        fluidRow(
          column(
            width = 6,
            pickerInput(
              inputId = "PI_custom_eqn_delete_select",
              label = "Select Custom Equation(s) to Delete",
              choices = "",
              multiple = TRUE,
              options = pickerOptions(liveSearch = TRUE,
                                      liveSearchStyle = "startsWith")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            rHandsontableOutput("RHT_custom_eqn_delete_preview")
          )
        )
      )
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 6,
      checkboxInput(
        inputId = "checkbox_custom_eqn_delete_keep_modal_active",
        label = "Close on Delete",
        value = TRUE
      )
    ),
    column(
      width = 6,
      align = "right",
      div(
        actionButton("bttn_custom_eqn_delete_confirm",
                     "Delete")
      )
    )
  )
)

