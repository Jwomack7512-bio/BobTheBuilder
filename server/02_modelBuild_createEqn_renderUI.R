# This script holds the renderUIs for the equation building suite

output$eqnCreate_equationBuilder_chem <- renderUI({
  number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS)
  number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS)
  number_forward_regulators = as.numeric(input$eqn_options_chem_num_forward_regulators)
  number_reverse_regulators = as.numeric(input$eqn_options_chem_num_reverse_regulators)
  
  div(
    fluidRow(
      column(
        width = 4,
        lapply(seq(number_LHS_equations), function(i){
          div(
            HTML(paste0("<b>Reactant ", as.character(i), "</b>")),
          splitLayout(
            numericInput(inputId = paste0("LHS_Coeff_", as.character(i))
                       ,label = NULL
                      #,label =  paste0("Reactant ", as.character(i))
                      ,value = 1
                      ,min = 1
                      ,step = 1),
            pickerInput(inputId = paste0("LHS_Var_", as.character(i))
                        ,label = NULL
                        # ,label =  paste0("Reactant ", as.character(i))
                        ,choices = sort(vars$species)
                        ,options = pickerOptions(liveSearch = TRUE
                                                 ,liveSearchStyle = "startsWith"
                                                 ,dropupAuto = FALSE))
            ,cellWidths = c("25%", "75%")
          )
          )
        })
    ), #end Column
    column(
      width = 4,
      offset = 4,
      lapply(seq(number_RHS_equations), function(i){
        div(
          HTML(paste0("<b>Product ", as.character(i), "</b>")),
          splitLayout(
            numericInput(inputId = paste0("RHS_Coeff_", as.character(i))
                         ,label = NULL
                         ,value = 1
                         ,min = 1
                         ,step = 1),
            pickerInput(inputId = paste0("RHS_Var_", as.character(i))
                        ,label = NULL
                        ,choices = sort(vars$species)
                        ,options = pickerOptions(liveSearch = TRUE
                                                 ,liveSearchStyle = "startsWith"
                                                 ,dropupAuto = FALSE))
            ,cellWidths = c("25%", "75%")
          )
        )
      })
      
    ) #end Column

    # ,column(width = 3
    #         ,conditionalPanel(condition = "!input.eqn_options_chem_modifier_forward"
    #                           ,textInput(inputId = "eqn_chem_forward_k"
    #                                      ,label = "Forward Rate Constant"
    #                                      ,value = paste0("k_f", as.character(eqns$n.eqns + 1)))
    #         )
    #         ,conditionalPanel(condition = "input.eqn_chem_forward_or_both=='both_directions' && !input.eqn_options_chem_modifier_reverse"
    #                           ,textInput(inputId = "eqn_chem_back_k"
    #                                      ,label = "Reverse Rate Constant"
    #                                      ,value = paste0("k_r", as.character(eqns$n.eqns + 1))))
    # )#end column
    ), #end fluidRow`
    hr(),
    fluidRow(
      column(
        width = 3,
        conditionalPanel(
          condition = "!input.eqn_options_chem_modifier_forward"
          ,textInput(inputId = "eqn_chem_forward_k"
                     ,label = "Forward Rate Constant"
                     ,value = paste0("k_f", as.character(eqns$n.eqns + 1)))
        ),
        conditionalPanel(
          condition = "input.eqn_options_chem_modifier_forward"
          ,lapply(seq(number_forward_regulators), function(i){
            pickerInput(
              inputId = paste0("eqn_forward_regulator_", as.character(i))
              ,label = paste0("Forward Regulator ", as.character(i))
              ,choices = sort(vars$species)
              ,options = pickerOptions(liveSearch = TRUE
                                       ,liveSearchStyle = "startsWith"))
          })
        )
        ),
      column(
        width = 3,
        conditionalPanel(
          condition = "input.eqn_options_chem_modifier_forward"
          ,lapply(seq(number_forward_regulators), function(i){
            textInput(
              inputId = paste0("eqn_forward_rateConstant_", as.character(i))
              ,label = paste0("Rate Constant ", as.character(i))
              ,value = "")
         })
       )
     )
    ),
    fluidRow(
      column(
        width = 3,
        conditionalPanel(
          condition = "input.eqn_chem_forward_or_both=='both_directions' && 
                    !input.eqn_options_chem_modifier_reverse"
          ,textInput(inputId = "eqn_chem_back_k"
                     ,label = "Reverse Rate Constant"
                     ,value = paste0("k_r", 
                                     as.character(eqns$n.eqns + 1)))
        ),
        conditionalPanel(
          condition = "input.eqn_options_chem_modifier_reverse"
          ,lapply(seq(number_reverse_regulators), function(i){
            pickerInput(
              inputId = paste0("eqn_reverse_regulator_", as.character(i))
              ,label = paste0("Reverse Regulator ", as.character(i))
              ,choices = sort(vars$species)
              ,options = pickerOptions(liveSearch = TRUE
                                       ,liveSearchStyle = "startsWith"))
          })
        )
      ),
      column(
        width = 3,
        conditionalPanel(
          condition = "input.eqn_options_chem_modifier_reverse"
          ,lapply(seq(number_reverse_regulators), function(i){
            textInput(
              inputId = paste0("eqn_reverse_rateConstant_", as.character(i))
              ,label = "Rate Constant"
              ,value = "")
          })
        )
      )
    ),

  )#end div
})




#-------------------------------------------------------------------------------

# Options UI

#-------------------------------------------------------------------------------
output$eqnCreate_Options <- renderUI({
  # div
  # (
  #   fluidRow(
  #     column(
  #       width = 12
  #       
  #         
  #         ,conditionalPanel(
  #           condition = "input.eqn_chem_forward_or_both=='both_directions'"
  #           ,hr()
  #           
  # 
  #           )
  #       )
  #       ,conditionalPanel(
  #         condition = "input.eqnCreate_type_of_equation=='enzyme_rxn'"
  #         ,checkboxInput(
  #           inputId = "eqn_options_enzyme_noVmax"
  #           ,label = "Split Vmax to kcat and enzyme"
  #           ,value = TRUE
  #         )
  #       )
  #     ) #end column
  #   ) #end fluidRow
  # ) #end div
})



