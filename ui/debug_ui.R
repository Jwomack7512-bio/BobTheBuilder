TAB_DEBUG <-   
  tabItem(
    tabName = "TAB_DEBUG",
    fluidRow(
      column(
        width = 3,
        h4(tags$b(tags$u("View"))),
        verticalLayout(
          radioGroupButtons(inputId = "debug_text_or_table",
                            label = "View by:",
                            choices = c("List View", "Table")),
          actionButton(inputId = "debug_view_variables",
                       label = "Species",
                       width = "50%"),
          actionButton(inputId = "debug_view_compartments",
                       label = "Compartments",
                       width = "50%"),
          actionButton(inputId = "debug_view_equations",
                       label = "Equations",
                       width = "50%"),
          actionButton(inputId = "debug_view_ids",
                       label = "ID Database",
                       width = "50%"),
          actionButton(inputId = "debug_view_IO",
                       label = "I/O",
                       width = "50%"),
          actionButton(inputId = "debug_view_parameters",
                       label = "Parameters",
                       width = "50%")
        )
      ),
      column(
        width = 9,
        conditionalPanel(
          condition = "input.debug_text_or_table == 'List View'",
          verbatimTextOutput("debug_text_view")
        ),
        conditionalPanel(
          condition = "input.debug_text_or_table == 'Table'",
          rHandsontableOutput("debug_table_view")
        )
        
      )
    ),
    hr(), hr(), hr(),
    fluidRow(
      column(
        width = 3,
        verticalLayout(
          "Summary Size Tab",
          numericInput(
            inputId = "sum_box_size",
            label = "Text Size (px)",
            value = 25,
            step = 1,
            min = 1
          ),
          numericInput(
            inputId = "sum_table_font_size",
            label = "Table Font (%)",
            value = 135,
            min = 5,
            max = 200,
            step = 5
          ),
          pickerInput(
            inputId = "css_selector",
            label = "Select Skin",
            choices = c("Default",
                        "Night",
                        "RoyalBlue"),
            select = "Default"
          )
        )
      )
    )
)