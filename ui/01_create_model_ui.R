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
    ## Create Variable Modal ---------------------------------------------------
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
        selected = "COMPNUMBER",
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
    
    shinyBS::bsModal(
      id = "modal_delete_species",
      title = NULL,
      trigger = "species_del_open_modal",
      size = "large",
      fluidRow(
        column(
          width = 4, 
          pickerInput(
            inputId = "PI_modal_delete_species",
            label = "Select Species To Remove",
            choices = c()
          )
        )
      ),
      hr(),
      fluidRow(
        column(
          width = 12,
          align = "right",
          div(
            actionButton("button_modal_delete_species",
                         "Delete")
          )
        )
      )
    ),
    ## Create Equations Modal --------------------------------------------------
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
          width = 12,
          align = "right",
          actionButton(
            inputId = "eqnCreate_addEqnToVector",
            label = "Add Equation",
            width = "auto")
        )
      )
    ),
    # Edit Equations Modal -----------------------------------------------------
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
   ),
   ## Delete Equation Modal ----------------------------------------------------
    shinyBS::bsModal(
      id = "modal_delete_equations",
      title = NULL,
      trigger = "eqns_delete_open_modal",
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
                  inputId = "eqnCreate_delete_select_equation",
                  label = "Select Equation Number to Delete",
                  choices = "",
                  multiple = TRUE
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                rHandsontableOutput("deleteEquations_table_viewer")
              )
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
            actionButton("modal_delete_eqn_button",
                         "Delete")
          )
        )
      )
   ), 
   shinyBS::bsModal(
     id = "modal_add_IO",
     title = NULL,
     trigger = "io_add_open_modal",
     size = "large",
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
               "Flow Between Compartments" = "FLOW_BETWEEN",
               "Clearance" = "CLEARANCE",
               "Simple Diffusion" = "SIMPDIFF",
               "Facillitated Diffusion" = "FACILITATED_DIFF"
             )
           )
         ),
         conditionalPanel(
           condition = "input.CIO_IO_options == 'FLOW_BETWEEN'",
           br(),
           div(
             style = "background-color:#F9F9F9;
                       border: 1px solid #c5c5c5;
                       border-radius: 12px;
                       padding: 10px 10px 10px 10px;",
             checkboxInput(
               inputId = "CIO_flowbetween_split",
               label = "Split Flow",
               value = FALSE
             ),
             numericInput(
               inputId = "CIO_flowbetween_number_split",
               label = "Number of Splits",
               value = 2,
               min = 2,
               step = 1
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
           ## Flow in -------------------------------------------------
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
                   label = "Flow Rate Variable",
                   value = "",
                   placeholder = "F"
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
                   label = "Flow Rate Variable",
                   value = "",
                   placeholder = "F"
                 )
               )
             )
           ),
           ## Flow Between --------------------------------------------
           conditionalPanel(
             condition = "input.CIO_IO_options == 'FLOW_BETWEEN'",
             # Compartment Out
             fluidRow(
               column(
                 width = 3,
                 style = "padding:0px; padding-left: 7.5px",
                 pickerInput(
                   inputId = "CIO_flowbetween_compartment_out",
                   label = "Flow Out Of",
                   choices = c()
                 )
               ),
               column(
                 width = 3,
                 style = "padding-left:0px; padding-right:0px",
                 pickerInput(
                   inputId = "CIO_flowbetween_species_out",
                   label = "Species Out",
                   choices = c()
                 )
               ),
               column(
                 width = 3,
                 style = "padding:0px;",
                 textInput(
                   inputId = "CIO_flowbetween_flow_variable_out",
                   label = "Flow Variable",
                   value = "",
                   placeholder = "F"
                 )
               ),
               column(
                 width = 3,
                 style = "padding:0px; padding-right: 7.5px",
                 textInput(
                   inputId = "CIO_flowbetween_flow_value_out",
                   label = textOutput("CIO_fb_vo_text"),
                   value = 1
                 )
               )
             ),
             hr(),
             # Flow in 1
             fluidRow(
               column(
                 width = 3,
                 style = "padding-left:7.5px; padding-right:0px",
                 pickerInput(
                   inputId = "CIO_flowbetween_compartment_in_1",
                   label = "Flow Into",
                   choices = c()
                 )
               ),
               column(
                 width = 3,
                 style = "padding-left:0px; padding-right:0px",
                 pickerInput(
                   inputId = "CIO_flowbetween_species_in_1",
                   label = "Species In",
                   choices = c()
                 )
               ),
               column(
                 width = 3,
                 style = "padding-left:0px; padding-right:0px",
                 conditionalPanel(
                   condition = "input.CIO_flowbetween_split",
                   textInput(
                     inputId = "CIO_flowbetween_flow_variable_in_1",
                     label = "Flow Variable",
                     value = "",
                     placeholder = "F_1"
                   )
                 )
               ),
               column(
                 width = 3,
                 style = "padding-left:0px; padding-right:7.5px",
                 conditionalPanel(
                   condition = "input.CIO_flowbetween_split",
                   textInput(
                     inputId = "CIO_flowbetween_flow_value_in_1",
                     label = textOutput("CIO_fb_sv1_text"),
                     value = 1
                   )
                 )
               )
             ),
             # Flow Split Renders
             fluidRow(
               column(
                 width = 3,
                 style = "padding-left:7.5px; padding-right:0px",
                 uiOutput("CIO_flow_between_render_compartments")
               ),
               column(
                 width = 3,
                 style = "padding-left:0px; padding-right:0px",
                 uiOutput("CIO_flow_between_render_species")
               ),
               column(
                 width = 3,
                 style = "padding-left:0px; padding-right:0px",
                 uiOutput("CIO_flow_between_render_flow_variables")
               ),
               column(
                 width = 3,
                 style = "padding-left:0px; padding-right:7.5px",
                 uiOutput("CIO_flow_between_render_flow_values")
               )
             )
           ),
           ## Clearance -----------------------------------------------
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
                   label = "Rate Variable",
                   value = "",
                   placeholder = "ke"
                 )
               )
             )
           ),
           ## Simple Diffusion ----------------------------------------
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
                   label = "Diffusivity Coefficient",
                   value = "",
                   placeholder = "PS_1"
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
           ## Facilitated Diffusion -----------------------------------
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
                   label = "Maximum Velocity",
                   value = "",
                   placeholder = "Vmax"
                 )
               ),
               column(
                 width = 3,
                 textInput(
                   inputId = "CIO_facilitatedDiff_Km",
                   label = "Michaelis Constnat",
                   value = "", 
                   placeholder = "Km"
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
     hr(),
     fluidRow(
       column(
         width = 12,
         align = "right",
         actionButton(inputId = "CIO_add_IO",
                      label = "Add")
       )
     )
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
                    # color = "royal",
                    icon = icon("plus"),
                    size = "xs"
                  ),
                  actionBttn(
                    inputId = "createVar_remove_compartment_button",
                    label = NULL,
                    style = "material-circle",
                    # color = "success",
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
            # uiOutput("createVar_species_compartment_options"),
            shinyjs::hidden(
              div(id = "species_hide_in_single_compartment",
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
                  hr()
              )
            ),
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
                        # color = "primary",
                        icon = icon("plus"),
                        size = "xs"
                      ),
                      actionBttn(
                        inputId = "species_del_open_modal",
                        label = NULL,
                        style = "material-circle",
                        # color = "danger",
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
      # Equation Box ------------------------------------------------------------
      fluidRow(
        column(
          width = 12,
          box(
            id = "createVar_equation_box",
            width = 12,
            title = "Equations",
            collapsible = TRUE,
            ## Eqn Display ---------------------------------------------------
            fluidRow(
              column(
                width = 12,
                rHandsontableOutput(outputId = "main_eqns_table")
              ) #end column
            ), #end fluidRow
            fluidRow(
              column(
                width = 12,
                align = "right",
                div(
                  actionBttn(
                    inputId = "eqns_add_open_modal",
                    label = NULL,
                    style = "material-circle",
                    icon = icon("plus"),
                    size = "xs"
                  ),
                  actionBttn(
                    inputId = "eqns_delete_open_modal",
                    label = NULL,
                    style = "material-circle",
                    icon = icon("minus"),
                    size = "xs"
                  )
                )
              )
            ),
            tags$head(
              tags$style("#eqnbuilder_sidebar {min-height:480px")),
            tags$head(
              tags$style("#tabbox_equation_builder_box {min-height:480px")),
            tags$head(
              tags$style("#eqnCreate_addEqnToVector {margin-top: 15px")),
            tags$head(
              tags$style("#eqnCreate_showEquationBuilding {margin-top: 15px"))
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
                width = 12, 
                rHandsontableOutput("createModel_IO_logs_table")
              )
            ),
            fluidRow(
              column(
                width = 12,
                align = "right",
                div(
                  actionBttn(
                    inputId = "io_add_open_modal",
                    label = NULL,
                    style = "material-circle",
                    icon = icon("plus"),
                    size = "xs"
                  ),
                  actionBttn(
                    inputId = "io_delete_open_modal",
                    label = NULL,
                    style = "material-circle",
                    icon = icon("minus"),
                    size = "xs"
                  )
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
                                  "Equation Parameters" = "Reaction",
                                  "Input/Output Parameters" = "Input/Output",
                                  "Compartment Volumes" = "Compartment")
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
