TAB_MODEL_REPOSITORY <-
  tabItem(
    tabName = "TAB_MODEL_REPOSITORY",
    fluidRow(
      column(
        width = 4,
        selectInput(
          inputId = "SI_repos_base_choices",
          label = "Select Model To Load:",
          choices <- c(),
          selectize = FALSE,
          size = 12
        )
      ), 
      column(
        width = 8,
        verticalLayout(
          fluidRow(
            column(
              width = 12,
              align = "right",
              actionButton(
                inputId = "bttn_load_model_from_base_repo",
                label = "Load Model"
              )
            )
          ),
          textOutput(outputId = "TO_repos_model_name"),
          textOutput(outputId = "TO_repos_model_description")
        )
      )
    )
  )