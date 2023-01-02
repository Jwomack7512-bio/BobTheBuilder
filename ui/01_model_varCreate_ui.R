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
    box(
      id = "create_var_info_box",
      title = "Info",
      collapsible = TRUE,
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
    ),
    br()
    ,fluidRow(
      column(
        width = 4,
        box(
          title = "Compartments",
          solidHeader = TRUE,
          collapsible = FALSE,
          closable = FALSE,
          headerBorder = FALSE,
          width = 12,
          fluidRow(
            column(
              width = 8,
              textInput(
                inputId = "createVar_compartment_input",
                label = "Compartment Name",
                value = ""
              )
            ),
            column(
              width = 4,
              div(style = "display: inline-block;
                          vertical-align:top;
                          padding-top:32px;
                          padding-left:-10px",
                  actionButton(
                    inputId = "createVar_add_compartment",
                    label = "Add",
                    width = "100px")
                  )
            )
          ),
          fluidRow(
            column(
              width = 12,
              pickerInput(
                inputId = "createVar_active_compartment",
                label = "Active Compartment",
                choices = c()
              ),
              prettyCheckbox(
                inputId = "createVar_show_active_compartment_only",
                label = "Show Active Compartment Only",
                value = TRUE
              )
            )
          )
        )
        ,box(
          title = "Add Variables"
          ,solidHeader = TRUE
          ,collapsible = FALSE
          ,closable = FALSE
          ,headerBorder = FALSE
          ,width = 12
          ,fluidRow(
            column(
              width = 8
              ,textInput(
                inputId = "createVar_varInput"
                ,label = "Enter Model Variable"
                ,value = "")
              )
            ,column(
              width = 4
              ,div(style = "display: inline-block;vertical-align:top;padding-top:32px;padding-left:-10px"
                ,actionButton(
                  inputId = "createVar_addVarToList"
                  ,label = "Add"
                  ,width = "100px"))
            )
          )
          ,br()
          ,fluidRow(
            column(
              width = 8,
              #,style = "border-left: 1px solid"
              pickerInput(inputId = "createVar_deleteVarPicker"
                          ,label = "Variable to Delete"
                          ,choices = c()
                          #,width = "auto"
                          ,options = pickerOptions(liveSearch = TRUE
                                                   ,liveSearchStyle = "startsWith")))
              
            ,column(width = 4
                    ,div(style = "display: inline-block;vertical-align:top;padding-top:32px;padding-left:-10px"
                         ,actionButton(inputId = "createVar_deleteVarButton"
                                       ,label = "Delete"
                                       ,width = "100px")))
          )
        ),
        box(
          title = textOutput("createVar_PE_box_title"),
          solidHeader = TRUE,
          collapsible = FALSE,
          closable = FALSE,
          headerBorder = FALSE,
          width = 12,
          uiOutput(
            outputId = "createVar_PE_variables"
          )
        )
      ),
      column(
        width = 8,
        # fluidRow(
        #   column(
        #     width = 3,
        #     offset = 9,
        #     pickerInput(
        #       inputId = "createVar_table_filter",
        #       label = "Show",
        #       choices = c()
        #     )
        #   )
        # ),
        rHandsontableOutput("myVariables_DT")
        # rHandsontableOutput("myVariables")
      )
    ), #end fluidRow




    
    tags$head(tags$style('#html_table_vars .box-header{ display: none}')),  
    tags$head(tags$style('#box1 .box-header{ display: none}')),
    tags$head(
      tags$style(".PE_variable_UI_table label {display: table-cell; 
                             text-align: center;
                             vertical-align: top; } 

.PE_variable_UI_table .form-group {display: table-cell;}")
    )
  )#end tabItem
