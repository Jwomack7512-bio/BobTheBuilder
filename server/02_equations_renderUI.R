# This script holds the renderUIs for the equation building suite
# w.load.MA.vars <- Waiter$new(id = "eqnCreate_equationBuilder_chem")
# w.load.MA.vars$show()
# w.load.MA.vars$hide()

output$equationBuilder_mass_action <- renderUI({
  number.reactants <- as.numeric(input$NI_mass_action_num_reactants)
  number.products  <- as.numeric(input$NI_mass_action_num_products)
  
  div(
    fluidRow(
      column(
        style = "border-right: 1px solid #e5e5e5; padding-right:20px",
        width = 4,
        lapply(seq(number.reactants), function(i){
          div(
            HTML(paste0("<b>Reactant ", as.character(i), "</b>")),
            splitLayout(
              numericInput(
                inputId = paste0("NI_MA_r_stoichiometry_", as.character(i)),
                label = NULL,
                value = 1,
                min = 1,
                step = 1),
              pickerInput(
                inputId = paste0("PI_MA_reactant_", as.character(i)),
                label = NULL,
                choices = sort(rv.SPECIES$df.by.compartment$Name),
                options = pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = "startsWith",
                                        dropupAuto = FALSE)
              ),
              cellWidths = c("25%", "75%")
            )
          )
        })
      ), #end Column
      column(
        style = "border-right: 1px solid #e5e5e5; 
                 padding-right: 20px; 
                 padding-left: 20px;",
        width = 4,
        lapply(seq(number.products), function(i){
          div(
            HTML(paste0("<b>Product ", as.character(i), "</b>")),
            splitLayout(
              numericInput(
                inputId = paste0("NI_MA_p_stoichiometry_", as.character(i)),
                label = NULL,
                value = 1,
                min = 1,
                step = 1),
              pickerInput(
                inputId = paste0("PI_MA_product_", as.character(i)),
                label = NULL,
                choices = sort(rv.SPECIES$df.by.compartment$Name),
                options = pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = "startsWith",
                                        dropupAuto = FALSE)
              ),
              cellWidths = c("25%", "75%")
            )
          )
        })
      ), #end Column
      column(
        style = "padding-left: 20px; padding-right: 0px",
        width = 3,
        textInput(
          inputId = "TI_mass_action_forward_k",
          label = "Forward Rate Constant",
          value = paste0("k_f", 
                         as.character(rv.REACTIONS$reaction.id.counter + 1))
        ),
        tags$head(tags$style("#TI_mass_action_forward_k {margin-top: -7px;}")),
        conditionalPanel(
          condition = 
            "input.PI_mass_action_reverisble_option == 'both_directions'",
          textInput(
            inputId = "TI_mass_action_reverse_k",
            label = "Reverse Rate Constant",
            value = paste0("k_r", 
                           as.character(rv.REACTIONS$reaction.id.counter + 1))
          ),
          tags$head(tags$style("#TI_mass_action_reverse_k {margin-top: -7px;}"))
        )
      ), #end column
      column(
        style = "padding-left: 0px",
        width = 1,
        textInput(
          inputId = "TI_mass_action_forward_k_value",
          label = "Value",
          value = 0
        ),
        conditionalPanel(
          condition = 
            "input.PI_mass_action_reverisble_option == 'both_directions'",
          textInput(
            inputId = "TI_mass_action_reverse_k_value",
            label = "Value",
            value = 0)
          )
        ),
      tags$head(
        tags$style("#TI_mass_action_forward_k_value {margin-top: -7px;}")),
      tags$head(
        tags$style("#TI_mass_action_reverse_k_value {margin-top: -7px;}"))
    ) #end fluidRow`
  )
})

output$equationBuilder_mass_action_w_regulation <- renderUI({
  number.reactants <- as.numeric(input$NI_mass_action_wReg_num_reactants)
  number.products  <- as.numeric(input$NI_mass_action_wReg_num_products)
  
  n.forward.regulators <- as.numeric(input$NI_MAwR_n_forward_regulators)
  n.reverse.regulators <- as.numeric(input$NI_MAwR_n_reverse_regulators)
  
  #Sys.sleep(0.5) 
  div(
    fluidRow(
      column(
        style = "border-right: 1px solid #e5e5e5; padding-right:20px",
        width = 4,
        lapply(seq(number.reactants), function(i){
          div(
            HTML(paste0("<b>Reactant ", as.character(i), "</b>")),
            splitLayout(
              numericInput(
                inputId = paste0("NI_MAwR_r_stoichiometry_", as.character(i)),
                label = NULL,
                value = 1,
                min = 1,
                step = 1),
              pickerInput(
                inputId = paste0("PI_MAwR_reactant_", as.character(i)),
                label = NULL,
                choices = sort(rv.SPECIES$df.by.compartment$Name),
                options = pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = "startsWith",
                                        dropupAuto = FALSE)
              ),
              cellWidths = c("25%", "75%")
            )
          )
        })
      ), #end Column
      column(
        style = "border-right: 1px solid #e5e5e5; 
               padding-right: 20px; 
               padding-left: 20px;",
        width = 4,
        lapply(seq(number.products), function(i){
          div(
            HTML(paste0("<b>Product ", as.character(i), "</b>")),
            splitLayout(
              numericInput(
                inputId = paste0("NI_MAwR_p_stoichiometry_", as.character(i)),
                label = NULL,
                value = 1,
                min = 1,
                step = 1),
              pickerInput(
                inputId = paste0("PI_MAwR_product_", as.character(i)),
                label = NULL,
                choices = sort(rv.SPECIES$df.by.compartment$Name),
                options = pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = "startsWith",
                                        dropupAuto = FALSE)
              ),
              cellWidths = c("25%", "75%")
            )
          )
        })
      ), #end Column
      column(
        style = "padding-left: 20px; padding-right: 0px",
        width = 3,
        conditionalPanel(
          condition = "!input.CB_MAwR_chem_modifier_forward",
          textInput(
            inputId = "TI_MAwR_forward_k",
            label = "Forward Rate Constant",
            value = paste0("k_f", 
                           as.character(rv.REACTIONS$reaction.id.counter + 1)
                           )
           ),
          tags$head(
            tags$style(
              "#TI_MAwR_forward_k {margin-top: -7px;}"))
        ),
        conditionalPanel(
          condition = 
            "input.reaction_mass_action_wReg_reverisble == 'both_directions' && 
             !input.CB_MAwR_chem_modifier_reverse",
          textInput(
            inputId = "TI_MAwR_reverse_k",
            label = "Reverse Rate Constant",
            value = paste0("k_r", 
                           as.character(rv.REACTIONS$reaction.id.counter + 1)
                           )
          ),
          tags$head(tags$style("#TI_MAwR_reverse_k {margin-top: -7px;}"))
        )
      ), #end column
      column(
        style = "padding-left: 0px",
        width = 1,
        conditionalPanel(
          condition = "!input.CB_MAwR_chem_modifier_forward",
          textInput(
            inputId = "TI_MAwR_forward_k_value",
            label = "Value",
            value = 0
          )
        ),
        conditionalPanel(
          condition = 
            "input.reaction_mass_action_wReg_reverisble == 'both_directions' && 
             !input.CB_MAwR_chem_modifier_reverse",
          textInput(
            inputId = "TI_MAwR_reverse_k_value",
            label = "Value",
            value = 0)
        )
      ),
      tags$head(
        tags$style("#TI_MAwR_forward_k_value {margin-top: -7px;}")),
      tags$head(
        tags$style("#TI_MAwR_reverse_k_value {margin-top: -7px;}"))
    ), #end fluidRow`
    conditionalPanel(
      condition = "input.CB_MAwR_chem_modifier_forward || 
                   input.CB_MAwR_chem_modifier_reverse",
      hr()
    ),
    fluidRow(
      column(
        width = 3,
        conditionalPanel(
          condition = "input.CB_MAwR_chem_modifier_forward",
          lapply(seq(n.forward.regulators), function(i){
            pickerInput(
              inputId = paste0("PI_MAwR_forward_regulator_", as.character(i)),
              label = paste0("Forward Regulator ", as.character(i)),
              choices = sort(rv.SPECIES$df.by.compartment$Name),
              options = pickerOptions(liveSearch = TRUE,
                                      liveSearchStyle = "startsWith"))
          })
        )
      ),
      column(
        width = 3,
        conditionalPanel(
          condition = "input.CB_MAwR_chem_modifier_forward",
          lapply(seq(n.forward.regulators), function(i){
            textInput(
              inputId = paste0("TI_MAwR_forward_regulator_RC_", 
                               as.character(i)),
              label = "Rate Constant",
              value = paste0("k_f", 
                             as.character(rv.REACTIONS$reaction.id.counter + 1),
                             ".", 
                             as.character(i)
              )
            )
          })
        )
      ),
      column(
        width = 3,
        conditionalPanel(
          condition = "input.CB_MAwR_chem_modifier_forward",
          lapply(seq(n.forward.regulators), function(i){
            textInput(
              inputId = paste0("TI_MAwR_forward_regulator_RC_value_",
                               as.character(i)),
              label = "Value",
              value = 0
            )
          })
        )
      )
    ),
    fluidRow(
      column(
        width = 3,
        conditionalPanel(
          condition = "input.CB_MAwR_chem_modifier_reverse",
          lapply(seq(n.reverse.regulators), function(i){
            pickerInput(
              inputId = paste0("PI_MAwR_reverse_regulator_", as.character(i)),
              label = paste0("Reverse Regulator ", as.character(i)),
              choices = sort(rv.SPECIES$df.by.compartment$Name),
              options = pickerOptions(liveSearch = TRUE
                                      ,liveSearchStyle = "startsWith")
            )
          })
        )
      ),
      column(
        width = 3,
        conditionalPanel(
          condition = "input.CB_MAwR_chem_modifier_reverse",
          lapply(seq(n.reverse.regulators), function(i){
            textInput(
              inputId = paste0("TI_MAwR_reverse_regulator_RC_", 
                               as.character(i)),
              label = "Rate Constant",
              value = paste0("k_r",
                             as.character(rv.REACTIONS$reaction.id.counter + 1),
                             ".",
                             as.character(i))
            )
          })
        )
      ),
      column(
        width = 3,
        conditionalPanel(
          condition = "input.CB_MAwR_chem_modifier_reverse",
          lapply(seq(n.reverse.regulators), function(i){
            textInput(
              inputId = paste0("TI_MAwR_reverse_regulator_RC_value_",
                               as.character(i)),
              label = "Value",
              value = 0
            )
          })
        )
      )
    )
  )#end div
})

output$equationBuilder_synthesis <- renderUI({
  
  div(
    conditionalPanel(
      condition = "!input.CB_synthesis_factor_checkbox",
      fluidRow(
        column(
          width = 3,
          pickerInput(
            inputId = "PI_synthesis_rate_var",
            label   = "Species to synthesize",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith") 
          )
        )
      ),
      fluidRow(
        column(
          width = 3,
          textInput(
            inputId = "TI_synthesis_rate_RC",
            label = "Rate Constant",
            value = paste0("k_syn",
                           as.character(rv.REACTIONS$reaction.id.counter + 1))
            
          )
        ),
        column(
          width = 3, 
          textInput(
            inputId = "TI_synthesis_rate_RC_value",
            label = "Value",
            value = 1
          )
        )
      )
    ), 
    conditionalPanel(
      condition = "input.CB_synthesis_factor_checkbox",
      fluidRow(
        column(
          width = 3,
          pickerInput(
            inputId = "PI_synthesis_byFactor_var",
            label   = "Species to synthesize",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith") 
          )
        ),
        column(
          width = 3, 
          pickerInput(
            inputId = "PI_synthesis_byFactor_factor",
            label = "Factor causing synthesis",
            choices = sort(rv.SPECIES$df.by.compartment$Name)
          )
        )
      ),
      fluidRow(
        column(
          width = 3, 
          textInput(
            inputId = "TI_synthesis_byFactor_RC",
            label = "Rate Constant",
            value = paste0("k_syn", 
                           as.character(rv.REACTIONS$reaction.id.counter + 1))
          )
        ),
        column(
          width = 3, 
          textInput(
            inputId = "TI_synthesis_byFactor_RC_value",
            label = "Value",
            value = 1
          )
        )
      )
    )
  )
})

output$equationBuilder_degradation_rate <- renderUI({
  
  div(
    fluidRow(
      column(
        width = 4,
        pickerInput(
          inputId = "PI_degradation_rate_species",
          label   = "Species to degrade",
          choices = sort(rv.SPECIES$df.by.compartment$Name),
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = "startsWith") 
        )
      ),
      column(
        width = 4,
        conditionalPanel(
          condition = "input.CB_degradation_rate_toProducts",
          lapply(seq(input$NI_degradation_rate_num_products), function(i){
            pickerInput(
              inputId = paste0("PI_degradation_rate_product_", as.character(i)),
              label = paste0("Product ", as.character(i)),
              choices = sort(rv.SPECIES$df.by.compartment$Name),
              options = pickerOptions(liveSearch = TRUE,
                                      liveSearchStyle = "startsWith"))
          })
        )
      )
    ),
    hr(),
    fluidRow(
      column(
        width = 8,
        splitLayout(
          textInput(
            inputId = "TI_degradation_rate_RC",
            label = "Rate Constant",
            value = paste0("k_d", 
                           as.character(rv.REACTIONS$reaction.id.counter + 1))
          ),
          textInput(
            inputId = "TI_degradation_rate_RC_value",
            label = "Value",
            value = 0
          ),
          div(
            style = "padding-top:38px; padding-left:15px;",
            checkboxInput(inputId = "CB_degradation_rate_conc_dependent",
                          label = "Concentration Dependent",
                          value = TRUE)
          )
        )
      )  
    )
  )
})

output$equationBuilder_degradation_by_enzyme <- renderUI({
  
  div(
    fluidRow(
      column(
        width = 3,
        pickerInput(
          inputId = "PI_degradation_enzyme_species",
          label   = "Species to degrade",
          choices = sort(rv.SPECIES$df.by.compartment$Name),
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = "startsWith") 
        ),
        conditionalPanel(
          condition = "!input.CB_degradation_enzyme_useVmax",
          pickerInput(
            inputId = "PI_degradation_enzyme_enzyme",
            label = "Enzyme",
            choices = sort(rv.SPECIES$df.by.compartment$Name)
          )
        )
      ),
      column(
        width = 3,
        offset = 1,
        conditionalPanel(
          condition = "input.CB_degradation_enzyme_toProducts",
          lapply(seq(input$NI_degradation_enzyme_num_products), function(i){
            pickerInput(
              inputId = paste0("PI_degradation_enzyme_product_", 
                               as.character(i)),
              label = paste0("Product ", as.character(i)),
              choices = sort(rv.SPECIES$df.by.compartment$Name),
              options = pickerOptions(liveSearch = TRUE,
                                      liveSearchStyle = "startsWith"))
          })
        )
      )
    ),
    hr(),
    conditionalPanel(
      condition = "!input.CB_degradation_enzyme_useVmax",
      fluidRow(
        column(
          style = "padding-right: 0px;",
          width = 3,
          textInput(
            inputId = "TI_degradation_enzyme_kcat",
            label = "kcat",
            value = paste0("k_d", 
                           as.character(rv.REACTIONS$reaction.id.counter+1))
          )
        ),
        column(
          style = "padding-left: 0px;",
          width = 3,
          textInput(
            inputId = "TI_degradation_enzyme_kcat_value",
            label = "Value",
            value = 1
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input.CB_degradation_enzyme_useVmax",
      fluidRow(
        column(
          style = "padding-right: 0px;",
          width = 3,
          textInput(
            inputId = "TI_degradation_enzyme_Vmax",
            label = "Vmax",
            value = paste0("Vmax_", 
                           as.character(rv.REACTIONS$reaction.id.counter+1))
          )
        ),
        column(
          style = "padding-left: 0px;",
          width = 3,
          textInput(
            inputId = "TI_degradation_enzyme_Vmax_value",
            label = "Value",
            value = 1
          )
        )
      )
    ),
    fluidRow(
      column(
        style = "padding-right: 0px;",
        width = 3,
        textInput(
          inputId = "TI_degradation_enzyme_Km",
          label = "Km",
          value = paste0("Km_", 
                         as.character(rv.REACTIONS$reaction.id.counter + 1))
        )
      ),
      column(
        style = "padding-left: 0px;",
        width = 3,
        textInput(
          inputId = "TI_degradation_enzyme_Km_value",
          label = "Value",
          value = 1
        )
      )
    )
  )
})

output$equationBuilder_michaelis_menten <- renderUI({
  
  div(
    fluidRow(
      column(
        width = 3,
        pickerInput(
          inputId = "PI_michaelis_menten_substrate",
          label = "Substrate",
          choices = sort(rv.SPECIES$df.by.compartment$Name),
          options = pickerOptions(
            liveSearch = TRUE,
            liveSearchStyle = "startsWith",
            dropupAuto = FALSE
          )
        )
      ),
      column(
        width = 3,
        offset = 1,
        pickerInput(
          inputId = "PI_michaelis_menten_product",
          label = "Product",
          choices = sort(rv.SPECIES$df.by.compartment$Name),
          options = pickerOptions(
            liveSearch = TRUE,
            liveSearchStyle = "startsWith",
            dropupAuto = FALSE
          )
        )
      ),
      column(
        width = 3, 
        offset = 1,
        conditionalPanel(
          condition = "!input.CB_michaelis_menten_useVmax",
          pickerInput(
            inputId = "PI_michaelis_menten_enzyme",
            label = "Enzyme",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")
          )
        )
      )
    ),
    hr(),
    fluidRow(
      column(
        style = "padding-right: 0px",
        width = 3,
        textInput(
          inputId = "TI_michaelis_menten_Km",
          label = "Km",
          value = paste0("Km_", 
                         as.character(rv.REACTIONS$reaction.id.counter + 1))
        )
      ),
      column(
        style = "padding-left: 0px",
        width = 3,
        textInput(
          inputId = "TI_michaelis_menten_Km_value",
          label = "Value",
          value = 1
        )
      )
    ),
    fluidRow(
      column(
        width = 3,
        style = "padding-right: 0px",
        conditionalPanel(
          condition = "input.CB_michaelis_menten_useVmax",
          textInput(
            inputId = "TI_michaelis_menten_vmax",
            label = "Vmax",
            value = paste0("Vmax_", 
                           as.character(rv.REACTIONS$reaction.id.counter + 1))
          )
        ),
        conditionalPanel(
          condition = "!input.CB_michaelis_menten_useVmax",
          textInput(
            inputId = "TI_michaelis_menten_kcat",
            label = "kcat",
            value = paste0("kcat_", 
                           as.character(rv.REACTIONS$reaction.id.counter + 1))
          )
        )
      ),
      column(
        width = 3,
        style = "padding-left: 0px",
        conditionalPanel(
          condition = "input.CB_michaelis_menten_useVmax",
          textInput(
            inputId = "TI_michaelis_menten_vmax_value",
            label = "Value",
            value = 0
          )
        ),
        conditionalPanel(
          condition = "!input.CB_michaelis_menten_useVmax",
          textInput(
            inputId = "TI_michaelis_menten_kcat_value",
            label = "Value",
            value = 0
          )
        )
      )
    )
  )#end div
})

output$eqnCreate_equationBuilder_custom_rate <- renderUI({
  
  div(
    "This is a custom equation entering. Please make sure things are spelled correctly as errors will occur if these equations are improper.",
    "Add any parameters in the 'Parameter Values' tab that would be used in this equation.",
    "These are often used for algebraic equations that need to be added to the model.",
    hr(),
    fluidRow(
      column(
        width = 4,
        textInput(
          inputId = "eqnCreate_custom_eqn_lhs",
          label = "Variable",
          value = "")
    ),
    column(
      width = 1,
      div(style = "padding-top:35px; padding-left: 12px;",
          "=")), 
    column(
      width = 7,
      textInput(
        inputId = "eqnCreate_custom_eqn_rhs",
        label = "Equation",
        value = ""
      )
    ))
  )
})

output$eqnCreate_equationBuilder_time_equation <- renderUI({
  
  div(
    "This is a custom equation entering. Please make sure things are spelled correctly as errors will occur if these equations are improper.",
    "There is a box below to add any parameters that could be used in your time dependent equations.",
    "Please note that 't' is used for time",
    hr(),
    fluidRow(
      column(
        width = 4,
        textInput(inputId = "eqnCreate_time_dependent_parameters",
                  label = "Parameters to add",
                  value = "")
        ),
      column(
        width = 4,
        align = "left",
        div(style = "padding-top: 30px;",
            actionButton(
              inputId = "eqnCreate_time_dependent_store_new_parameter",
              label = "Store Parameter"))
        
      )
    ),
    hr(),
    fluidRow(
      column(
        width = 4,
        textInput(
          inputId = "eqnCreate_time_dependent_firstvar",
          label = "Time Dependent Variable",
          value = ""
        )
      ),
      column(width = 1,
             div(style = "padding-top: 38px; padding-left: 12px",
                 "=")
             ),
      column(
        width = 7,
        textInput(
          inputId = "eqnCreate_time_dependent_equation",
          label = "Equation",
          value = ""
        )
      )
    )
  )
})



