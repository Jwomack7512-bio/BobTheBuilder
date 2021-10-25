source("./server/helper_createEqns.R") #load functions to solve differential equations


StoreParamsEqn <- function(parameterToAdd) {
  
  #NEED TO ADD CHECK IF PARAM ALREADY EXISTS
  if (!(parameterToAdd %in% rv$parameters_in_model) &&
        !(parameterToAdd %in% rv$rate_params)) {
    rv$param_eqns <- append(rv$param_eqns, parameterToAdd)
    rv$param_eqns_values <- append(rv$param_eqns_values, 0)
    rv$param_eqns_comments <- append(rv$param_eqns_comments, "")
    
    rv$parameters_in_model <- append(rv$parameters_in_model, parameterToAdd)
    rv$parameter_values <- append(rv$parameter_values, 0)
    rv$parameter_descriptions <- append(rv$parameter_descriptions, "")
  }
}

StoreParamsRate <- function(parameterToAdd) {
  
  if (!rv$first_param_rateEqn_stored) rv$first_param_rateEqn_stored = TRUE
  
  #NEED TO ADD CHECK IF PARAM ALREADY EXISTS
  if (!(parameterToAdd %in% rv$parameters_in_model)) {
    rv$param_rateEqn <- append(rv$param_rateEqn, parameterToAdd)
    rv$param_rateEqn_values <- append(rv$param_rateEqn_values, 0)
    rv$param_rateEqn_comments <- append(rv$param_rateEqn_comments, "")
    
    rv$parameters_in_model <- append(rv$parameters_in_model, parameterToAdd)
    rv$parameter_values <- append(rv$parameter_values, 0)
    rv$parameter_descriptions <- append(rv$parameter_descriptions, "")
  }
}

build_db_row <- function(eqn_type, RHS_coef, RHS_var, LHS_coef, LHS_var,arrow_type, kf, kr, description){
  row_out <- c(eqn_type, RHS_coef, RHS_var, LHS_coef, LHS_var,arrow_type, kf, kr, description)
}

observeEvent(input$createVar_addVarToList, {
  updatePickerInput(session, "eqnCreate_recep", choices = sort(rv$vars_in_model))
  updatePickerInput(session, "eqnCreate_lig", choices = sort(rv$vars_in_model))

})

observeEvent(input$eqnCreate_recep, {
  updateTextInput(session, 
                  "eqnCreate_lig_recep_product", 
                  value = paste0(input$eqnCreate_recep, input$eqnCreate_lig))
})

observeEvent(input$eqnCreate_lig, {
  updateTextInput(session, 
                  "eqnCreate_lig_recep_product", 
                  value = paste0(input$eqnCreate_recep, input$eqnCreate_lig))
})


#-------------------------------------------------------------------------------

# Extract data and store equation elements into a df to solve ODEs from

#-------------------------------------------------------------------------------
observeEvent(input$eqnCreate_addEqnToVector, {
  rv$number_of_equations <- rv$number_of_equations + 1
  eqn_type <- input$eqnCreate_type_of_equation
  
  if (eqn_type == "chem_rxn") {
    number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS) #number of variables on RHS of equation
    number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS) #number of variables on LHS of equation
    number_forward_regulators = as.numeric(input$eqn_options_chem_num_forward_regulators) #number of regulators for forward reaction
    number_reverse_regulators = as.numeric(input$eqn_options_chem_num_reverse_regulators) #number of regulators for reverse reaction
    
    ###Left Hand side of equation
    coef_LHS <- vector() #initialize
    var_LHS <- vector()  #initialize
    for (i in seq(number_LHS_equations)) { #find all coefficients and variables on left hand side of equation and add them to vectors
      coef <- eval(parse(text = paste0("input$LHS_Coeff_", as.character(i))))
      var <- eval(parse(text = paste0("input$LHS_Var_", as.character(i))))
      coef_LHS <- append(coef_LHS, coef)
      var_LHS <- append(var_LHS, var)
    }
    coef_LHS <- paste(coef_LHS, collapse = " ") #paste vectors into space separated variables (ex k1 k2 k3)
    var_LHS <- paste(var_LHS, collapse = " ") #paste vectors into space separated variables
    
    ###Right Hand side of equation
    coef_RHS <- vector() #initialize
    var_RHS <- vector()  #initialize
    for (i in seq(number_RHS_equations)) { #find all coefficients and variables on right hand side of equation and add them to vectors
      coef <- eval(parse(text = paste0("input$RHS_Coeff_", as.character(i))))
      var <- eval(parse(text = paste0("input$RHS_Var_", as.character(i))))
      coef_RHS <- append(coef_RHS, coef)
      var_RHS <- append(var_RHS, var)
    }
    coef_RHS <- paste(coef_RHS, collapse = " ") #paste vectors into space separated variables
    var_RHS <- paste(var_RHS, collapse = " ")   #paste vectors into space separated variables
    
    #rate constants
    arrow_direction <- input$eqn_chem_forward_or_both
    if (arrow_direction == "both_directions") {
      ###Checks if regulator was used in forward reaction, hence removing kr and updating the appropriate values for the regulator 
      if (input$eqn_options_chem_modifier_forward) {
        kf <- NA
        forward_modifier_bool <- TRUE
        f_regulators_coef <- vector()
        f_regulators_rateConstants <- vector()
        for (i in seq(number_forward_regulators)) { #find all forward regulators and their rate constants and store them to vectors
          coef <- eval(parse(text = paste0("input$eqn_forward_regulator_", as.character(i))))
          rc <- eval(parse(text = paste0("input$eqn_forward_rateConstant_", as.character(i))))
          #rv$param_eqns <- append(rv$param_eqns, rc)
          StoreParamsEqn(rc)
          f_regulators_coef <- append(f_regulators_coef, coef)
          f_regulators_rateConstants <- append(f_regulators_rateConstants, rc)
        }
        f_regulators_coef <- paste(f_regulators_coef, collapse = " ")
        f_regulators_rateConstants <- paste(f_regulators_rateConstants, collapse = " ")
      }
      else{
        forward_modifier_bool <- FALSE
        f_regulators_coef <- NA
        f_regulators_rateConstants <- NA
        kf <- input$eqn_chem_forward_k
        #rv$param_eqns <- append(rv$param_eqns, kf)
        StoreParamsEqn(kf)
      }
      ###Checks if regulator was used in reverse reaction, hence removing kr and updating the appropriate values for the regulator 
      if (input$eqn_options_chem_modifier_reverse) {
        kr <- NA
        reverse_modifier_bool <- TRUE
        r_regulators_coef <- vector()
        r_regulators_rateConstants <- vector()
        for (i in seq(number_reverse_regulators)) { #find all forward regulators and their rate constants and store them to vectors
          coef <- eval(parse(text = paste0("input$eqn_reverse_regulator_", as.character(i))))
          rc <- eval(parse(text = paste0("input$eqn_reverse_rateConstant_", as.character(i))))
          #rv$param_eqns <- append(rv$param_eqns, rc)
          StoreParamsEqn(rc)
          
          r_regulators_coef <- append(r_regulators_coef, coef)
          r_regulators_rateConstants <- append(r_regulators_rateConstants, rc)
        }
        r_regulators_coef <- paste(r_regulators_coef, collapse = " ")
        r_regulators_rateConstants <- paste(r_regulators_rateConstants, collapse = " ")
      }
      else{
        kr <- input$eqn_chem_back_k
        reverse_modifier_bool <- FALSE
        r_regulators_coef <- NA
        r_regulators_rateConstants <- NA
        #rv$param_eqns <- append(rv$param_eqns, kr)
        StoreParamsEqn(kr)
      }
    }
    else if (arrow_direction == "forward_only") {
      if (input$eqn_options_chem_modifier_forward) {
        kf <- NA
        forward_modifier_bool <- TRUE
        f_regulators_coef <- vector()
        f_regulators_rateConstants <- vector()
        for (i in seq(number_forward_regulators)) { #find all forward regulators and their rate constants and store them to vectors
          coef <- eval(parse(text = paste0("input$eqn_forward_regulator_", as.character(i))))
          rc <- eval(parse(text = paste0("input$eqn_forward_rateConstant_", as.character(i))))
          #rv$param_eqns <- append(rv$param_eqns, rc)
          StoreParamsEqn(rc)
          f_regulators_coef <- append(f_regulators_coef, coef)
          f_regulators_rateConstants <- append(f_regulators_rateConstants, rc)
        }
        f_regulators_coef <- paste(f_regulators_coef, collapse = " ")
        f_regulators_rateConstants <- paste(f_regulators_rateConstants, collapse = " ")
      }
      else{
        kf <- input$eqn_chem_forward_k
        #rv$param_eqns <- append(rv$param_eqns, kf)
        StoreParamsEqn(kf)
        forward_modifier_bool <- FALSE
        f_regulators_coef <- NA
        f_regulators_rateConstants <- NA
      }
      kr <- NA
      reverse_modifier_bool <- FALSE
      r_regulators_coef <- NA
      r_regulators_rateConstants <- NA
    }
    kcat = NA
    Vmax = NA 
    Km = NA 
    enzyme = NA
    row_to_df <- c(eqn_type, coef_LHS, var_LHS, coef_RHS, var_RHS, arrow_direction, kf, kr, 
                   kcat, Vmax, Km, enzyme,
                   forward_modifier_bool, f_regulators_coef, f_regulators_rateConstants,
                   reverse_modifier_bool, r_regulators_coef, r_regulators_rateConstants)
    #print(row_to_df)
    
  }#end if chem_rxn
  else if (eqn_type == "enzyme_rxn") {
    coef_LHS <- 1
    coef_RHS <- 1
    var_LHS = input$eqn_enzyme_substrate
    var_RHS = input$eqn_enzyme_product
    arrow_direction <- "forward_only"
    Km = input$eqn_enzyme_Km
    #rv$param_eqns <- append(rv$param_eqns, Km)
    StoreParamsEqn(Km)
    
    if (input$eqn_options_enzyme_noVmax) {
      kcat = input$eqn_enzyme_kcat
      enzyme = input$eqn_enzyme_enzyme
      Vmax = NA
      #rv$param_eqns <- append(rv$param_eqns, kcat)
      #rv$param_eqns <- append(rv$param_eqns, Km)
      StoreParamsEqn(kcat)
      StoreParamsEqn(Km)
    } else if (!input$eqn_options_enzyme_noVmax) {
      Vmax = input$eqn_enzyme_Vmax
      kcat = NA
      enzyme = NA
      #rv$param_eqns <- append(rv$param_eqns, Vmax)
      StoreParamsEqn(Vmax)
    }
    
    kf = NA
    kr = NA
    forward_modifier_bool <- FALSE
    f_regulators_coef <- NA
    f_regulators_rateConstants <- NA
    reverse_modifier_bool <- FALSE
    r_regulators_coef <- NA
    r_regulators_rateConstants <- NA
    row_to_df <- c(eqn_type, coef_LHS, var_LHS, coef_RHS, var_RHS, arrow_direction, kf, kr, 
                   kcat, Vmax, Km, enzyme,
                   forward_modifier_bool, f_regulators_coef, f_regulators_rateConstants,
                   reverse_modifier_bool, r_regulators_coef, r_regulators_rateConstants)
  } 
  else if (eqn_type == "simp_diff") {
    coef_LHS <- 1
    coef_RHS <- 1
    var_LHS = input$simp_diff_var1
    var_RHS = input$simp_diff_var2
    diff_coef <- input$simp_diff_PS_Var
    if (input$simp_diff_wayOfDiffusion) {
      arrow_direction <- "forward_only"
      kf = diff_coef
      kr = NA
    }else{
      arrow_direction <- "both_directions"
      kf = diff_coef
      kr = diff_coef
    }
    kcat = NA
    Vmax = NA 
    Km = NA 
    enzyme = NA
    forward_modifier_bool <- FALSE
    f_regulators_coef <- NA
    f_regulators_rateConstants <- NA
    reverse_modifier_bool <- FALSE
    r_regulators_coef <- NA
    r_regulators_rateConstants <- NA
    row_to_df <- c(eqn_type, coef_LHS, var_LHS, coef_RHS, var_RHS, arrow_direction, kf, kr, 
                   kcat, Vmax, Km, enzyme,
                   forward_modifier_bool, f_regulators_coef, f_regulators_rateConstants,
                   reverse_modifier_bool, r_regulators_coef, r_regulators_rateConstants)    # if(rv$first_run){
    #   rv$first_run <- FALSE
    #   data$eqn_info[1,] <- row_to_df
    # }else{
    #   data$eqn_info  <- rbind(data$eqn_info , row_to_df)
    #}
    #store parameter value
    #rv$param_eqns <- append(rv$param_eqns, kf)
    StoreParamsEqn(kf)
  }
  else if (eqn_type == "rate_eqn") {
    rate_left <- input$eqnCreate_rate_firstvar
    rate_right <- input$eqnCreate_rate_equation
    rate_eqn <- paste0(rate_left, " = ", rate_right)
    rv$additional_eqns <- c(rv$additional_eqns, rate_eqn)
    rv$parameters_based_on_other_values <- rate_left
    #remove rate_left from parameters-----------------------------------------------------------------------------------------------------------------------
    #split_rate_to_components()
    
    #search all parameters lists for parameter and remove it from each. (input, output, eqn, total)
    parameter_to_remove <- rate_left
    if (parameter_to_remove %in% rv$param_inputs) {
      rv$param_inputs <- rv$param_inputs[!rv$param_inputs %in% parameter_to_remove]
    }
    if (parameter_to_remove %in% rv$param_outputs) {
      rv$param_outputs <- rv$param_outputs[!rv$param_outputs %in% parameter_to_remove]
    }
    if (parameter_to_remove %in% rv$param_eqns) {
      rv$param_eqns <- rv$param_eqns[!rv$param_eqns %in% parameter_to_remove]
    }
    if (parameter_to_remove %in% rv$parameters_in_model) {
      rv$parameters_in_model <- rv$parameters_in_model[!rv$parameters_in_model %in% parameter_to_remove]
    }
    #remove all excess variables from created lists if they exist (ie. we generated ui for parameter values and comments.  Will need to remove those)
  }
  else if (eqn_type == "time_dependent")
  {
    TD_left <- input$eqnCreate_time_dependent_firstvar
    TD_right <- input$eqnCreate_time_dependent_equation
    TD_eqn <- paste0(TD_left, "=", TD_right)
    rv$additional_eqns <- c(rv$additional_eqns, TD_eqn)
    rv$parameters_based_on_other_values <- TD_left
  }
  if (eqn_type != "rate_eqn" && eqn_type != "time_dependent")
  {
    if (rv$first_run)
    {
      rv$first_run <- FALSE
      data$eqn_info[1,] <- row_to_df
    }
    else
    {
      data$eqn_info  <- rbind(data$eqn_info , row_to_df)
    }
  }

})
observe(print(data$eqn_info))

#-------------------------------------------------------------------------------

# Build Text Equation for User to See

#-------------------------------------------------------------------------------
equationBuilder <- reactive({
  if (input$eqnCreate_type_of_equation == "chem_rxn") {
    number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS)
    number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS)
    number_forward_regulators = as.numeric(input$eqn_options_chem_num_forward_regulators)
    number_reverse_regulators = as.numeric(input$eqn_options_chem_num_reverse_regulators)
    
    eqn_LHS <- ""
    for (i in seq(number_LHS_equations)) {
      coef <- eval(parse(text = paste0("input$LHS_Coeff_", as.character(i))))
      var <- eval(parse(text = paste0("input$LHS_Var_", as.character(i))))
      if (coef != "1") {eqn_LHS <- paste0(eqn_LHS, coef, "*")}
      if (i == as.numeric(number_LHS_equations)) {eqn_LHS <- paste0(eqn_LHS, var)}
      else{eqn_LHS <- paste0(eqn_LHS, var, " + ")}
    }
    
    eqn_RHS <- ""
    for (i in seq(number_RHS_equations)) {
      coef <- eval(parse(text = paste0("input$RHS_Coeff_", as.character(i))))
      var <- eval(parse(text = paste0("input$RHS_Var_", as.character(i))))
      if (coef != "1") {eqn_RHS <- paste0(eqn_RHS, coef, "*")}
      if (i == as.numeric(number_RHS_equations)) {eqn_RHS <- paste0(eqn_RHS, var)}
      else{eqn_RHS <- paste0(eqn_RHS, var, " + ")}
    }
    
    if (input$eqn_chem_forward_or_both == "both_directions") {
      arrow <- "<-->"
      if (input$eqn_options_chem_modifier_forward && input$eqn_options_chem_modifier_reverse) {
        #find regulators and add them together in form ([regulator/constant, regulator2/constant2, etc...])
        forwardModifiers <- c()
        for (i in seq(number_forward_regulators)) {
          regulator <- eval(parse(text = paste0("input$eqn_forward_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_forward_rateConstant_", as.character(i))))
          modifierExpression <- paste0(regulator, "/", rateConstant)
          forwardModifiers <- c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ",")
        
        reverseModifiers <- c()
        for (i in seq(number_reverse_regulators)) {
          regulator <- eval(parse(text = paste0("input$eqn_reverse_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_reverse_rateConstant_", as.character(i))))
          modifierExpression <- paste0(regulator, "/", rateConstant)
          reverseModifiers <- c(reverseModifiers, modifierExpression)
        }
        reverseModifiers <- paste(reverseModifiers, collapse = ",")
        
        arrow <- paste0("([", reverseModifiers, "])", arrow, "([",forwardModifiers ,"])")
      }
      else if (input$eqn_options_chem_modifier_forward && !input$eqn_options_chem_modifier_reverse) {
        forwardModifiers <- c()
        for (i in seq(number_forward_regulators)) {
          regulator <- eval(parse(text = paste0("input$eqn_forward_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_forward_rateConstant_", as.character(i))))
          modifierExpression <- paste0(regulator, "/", rateConstant)
          forwardModifiers <- c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ",")
        
        arrow <- paste0("(", input$eqn_chem_back_k, ")", arrow, "([",forwardModifiers ,"])")
      }
      else if (!input$eqn_options_chem_modifier_forward && input$eqn_options_chem_modifier_reverse) {
        reverseModifiers <- c()
        for (i in seq(number_reverse_regulators)) {
          regulator <- eval(parse(text = paste0("input$eqn_reverse_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_reverse_rateConstant_", as.character(i))))
          modifierExpression <- paste0(regulator, "/", rateConstant)
          reverseModifiers <- c(reverseModifiers, modifierExpression)
        }
        reverseModifiers <- paste(reverseModifiers, collapse = ",")
        arrow <- paste0("([", reverseModifiers, "])", arrow, "(", input$eqn_chem_forward_k, ")")
      }
      else
      {
        arrow <- paste0("(", input$eqn_chem_back_k, ")", arrow, "(", input$eqn_chem_forward_k, ")")
      }
    }
    else if (input$eqn_chem_forward_or_both == "forward_only") {
      arrow = "--->"
      if (input$eqn_options_chem_modifier_forward) {
        forwardModifiers <- c()
        for (i in seq(number_forward_regulators)) {
          regulator <- eval(parse(text = paste0("input$eqn_forward_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_forward_rateConstant_", as.character(i))))
          modifierExpression <- paste0(regulator, "/", rateConstant)
          forwardModifiers <- c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ",")
        arrow <- paste0(arrow, "([",forwardModifiers ,"])")
      }
      else
      {
        arrow <- paste0(arrow, "(", input$eqn_chem_forward_k, ")")
      }
    }
    
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
  }
  
  else if (input$eqnCreate_type_of_equation == "enzyme_rxn") {
    substrate = input$eqn_enzyme_substrate
    product = input$eqn_enzyme_product
    arrow = "-->"
    enzyme = input$eqn_enzyme_enzyme
    Km = input$eqn_enzyme_Km
    
    if (input$eqn_options_enzyme_noVmax) {
      kcat = input$eqn_enzyme_kcat
      textOut <- paste0(substrate," + ", enzyme,  " (", kcat, ")", arrow, "(", Km, ") ", product)
    }
    else if (!input$eqn_options_enzyme_noVmax) {
      Vmax = input$eqn_enzyme_Vmax
      textOut <- paste0(substrate, " (", Vmax, ", Enzyme)", arrow, "(", Km, ") ", product)
      
    }
  }
  else if (input$eqnCreate_type_of_equation == "simp_diff") {
    var_left = input$simp_diff_var1
    var_right = input$simp_diff_var2
    diff_coef <- input$simp_diff_PS_Var
    ifelse(input$simp_diff_wayOfDiffusion, symbol <- "-->", symbol <- "<-->")
    
    textOut <- paste0(var_left, " ", symbol, "(", diff_coef, ") ", var_right)
  }
  else if (input$eqnCreate_type_of_equation == "rate_eqn")
  {
    rate_left <- input$eqnCreate_rate_firstvar
    rate_right <- input$eqnCreate_rate_equation
    textOut <- paste0(rate_left, " = ", rate_right)
  }
  else if (input$eqnCreate_type_of_equation == "time_dependent")
  {
    TD_left <- input$eqnCreate_time_dependent_firstvar
    TD_right <- input$eqnCreate_time_dependent_equation
    textOut <- paste0(TD_left, "=", TD_right)
  }
  else if (input$eqnCreate_type_of_equation == "mass_bal") {
    textOut <- "MASS BAL"
  }
  else if (input$eqnCreate_type_of_equation == "lig_recep") {
    textOut <- paste0(input$eqnCreate_recep, "+", input$eqnCreate_stoch_coef, input$eqnCreate_lig, "=", input$eqnCreate_stoch_coef, input$eqnCreate_lig_recep_product)
  }
  else{textOut <- "ERROR"}
  return(textOut)
})

#-------------------------------------------------------------------------------

# Equation Building UI

#-------------------------------------------------------------------------------
output$eqnCreate_equationBuilder_chem <- renderUI({
  number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS)
  number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS)
  number_forward_regulators = as.numeric(input$eqn_options_chem_num_forward_regulators)
  number_reverse_regulators = as.numeric(input$eqn_options_chem_num_reverse_regulators)

  div(
    fluidRow(column(width = 2
                    ,lapply(seq(number_LHS_equations), function(i){
                      numericInput(inputId = paste0("LHS_Coeff_", as.character(i))
                                   ,label = "Coefficient"
                                   ,value = 1
                                   ,min = 1
                                   ,step = 1)
                    })
      )#end Column
      ,column(width = 2
              ,lapply(seq(number_LHS_equations), function(i){
                pickerInput(inputId = paste0("LHS_Var_", as.character(i))
                            ,label = "Variable"
                            ,choices = sort(rv$vars_in_model))
              })
      )#end column
      ,column(width = 3
              #,offset=1
              ,pickerInput(inputId = "eqn_chem_forward_or_both"
                           ,label = "Reaction Direction"
                           ,choices = c("Forward" = 'forward_only'
                                      ,"Both" = "both_directions"))
              ,conditionalPanel(condition = "!input.eqn_options_chem_modifier_forward"
                                ,textInput(inputId = "eqn_chem_forward_k"
                                           ,label = "Forward Rate Constant"
                                           ,value = paste0("kf", as.character(rv$number_of_equations + 1)))
              )
              ,conditionalPanel(condition = "input.eqn_chem_forward_or_both=='both_directions' && !input.eqn_options_chem_modifier_reverse"
                                ,textInput(inputId = "eqn_chem_back_k"
                                           ,label = "Reverse Rate Constant"
                                           ,value = paste0("kr", as.character(rv$number_of_equations + 1))))
      )#end column
      ,column(width = 2
              #,offset=1
              ,lapply(seq(number_RHS_equations), function(i){
                numericInput(inputId = paste0("RHS_Coeff_", as.character(i))
                             ,label = "Coefficient"
                             ,value = 1
                             ,min = 1
                             ,step = 1)
              })
      )#end Column
      ,column(width = 2
              ,lapply(seq(number_RHS_equations), function(i){
                pickerInput(inputId = paste0("RHS_Var_", as.character(i))
                            ,label = "Variable"
                            ,choices = sort(rv$vars_in_model))
              })
      )#end column
    )#end fluidRow``
    ,fluidRow(column(width = 12
                    ,conditionalPanel(condition = "input.eqn_options_chem_modifier_forward"
                                      ,hr()
                                      ,h5("Forward Regulators")
                                      ,hr()
                    )
    )
    )
    ,fluidRow(conditionalPanel(condition = "input.eqn_options_chem_modifier_forward"
                               ,column(width = 3
                                       #,offset=1
                                       ,lapply(seq(number_forward_regulators), function(i){
                                         pickerInput(inputId = paste0("eqn_forward_regulator_", as.character(i))
                                                     ,label = paste0("Forward Regulator ", i)
                                                     ,choices = sort(rv$vars_in_model))
                                       })
                               )#end Column
                               ,column(width = 2
                                       ,lapply(seq(number_forward_regulators), function(i){
                                         textInput(inputId = paste0("eqn_forward_rateConstant_", as.character(i))
                                                   ,label = paste0("Rate Constant ", i)
                                                   ,value = "")
                                       })
                               )
    )
    )
    ,fluidRow(column(width = 12
                    ,conditionalPanel(condition = "input.eqn_options_chem_modifier_reverse"
                                      ,hr()
                                      ,h5("Reverse Regulators")
                                      ,hr()
                    )
    )
    )
    ,fluidRow(conditionalPanel(condition = "input.eqn_options_chem_modifier_reverse"
                               ,h5("Reverse Regulators")
                               ,column(width = 3
                                       #,offset=1
                                       ,lapply(seq(number_reverse_regulators), function(i){
                                         pickerInput(inputId = paste0("eqn_reverse_regulator_", as.character(i))
                                                     ,label = paste0("Reverse Regulator ", i)
                                                     ,choices = sort(rv$vars_in_model))
                                       })
                               )#end Column
                               ,column(width = 2
                                       ,lapply(seq(number_reverse_regulators), function(i){
                                         textInput(inputId = paste0("eqn_reverse_rateConstant_", as.character(i))
                                                   ,label = paste0("Rate Constant ", i)
                                                   ,value = "")
                                       })
                               )
    )
    )
  )#end div
  
})

output$eqnCreate_equationBuilder_enzyme <- renderUI({

  div(
    fluidRow(column(width=3
                    ,pickerInput(inputId="eqn_enzyme_substrate"
                                 ,label="Substrate"
                                 ,choices=rv$vars_in_model)
                    ,conditionalPanel(condition="input.eqn_options_enzyme_noVmax"
                                      ,pickerInput(inputId="eqn_enzyme_enzyme"
                                                   ,label="Enzyme"
                                                   ,choices=sort(rv$vars_in_model)))
                    )
             ,column(width=3
                     ,offset = 1
                     ,conditionalPanel(condition="!input.eqn_options_enzyme_noVmax"
                                       ,textInput(inputId="eqn_enzyme_Vmax"
                                                  ,label = "Vmax"
                                                  ,value = paste0("Vmax_", as.character(rv$number_of_equations+1))))
                     ,conditionalPanel(condition="input.eqn_options_enzyme_noVmax"
                                       ,textInput(inputId="eqn_enzyme_kcat"
                                                  ,label = "kcat"
                                                  ,value = paste0("kcat_", as.character(rv$number_of_equations+1))))
                     
                     ,textInput(inputId="eqn_enzyme_Km"
                                ,label = "Km"
                                ,value = paste0("Km_", as.character(rv$number_of_equations+1)))
                     )
             ,column(width=3
                     ,offset=1
                     ,pickerInput(inputId="eqn_enzyme_product"
                                  ,label="Product"
                                  ,choices=sort(rv$vars_in_model)))
    )#end fluidRow
  )#end div
})

output$eqnCreate_equationBuilder_simp_diff <- renderUI({
  #number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS)
  #number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS)
  
  div(
    fluidRow(column(width=3
                    ,pickerInput(inputId="simp_diff_var1"
                                 ,label="Var1"
                                 ,choices=sort(rv$vars_in_model)))
             ,column(width=3
                     ,textInput(inputId="simp_diff_PS_Var"
                                ,label = "Diffusion Constant"
                                ,value = paste0("PS", as.character(rv$number_of_equations+1))))
             ,column(width=3
                     ,pickerInput(inputId="simp_diff_var2"
                                  ,label="Var2"
                                  ,choices=sort(rv$vars_in_model)))
    )#end fluidRow
    ,fluidRow(column(width=4,
                     checkboxInput(inputId="simp_diff_wayOfDiffusion"
                                   ,label="This diffusion is one way"
                                   ,value = FALSE)
    ))
  )#end div
})


#-------------------------------------------------------------------------------

# Rate Equation Store Parameter/Time Dependent

#-------------------------------------------------------------------------------

# observeEvent(input$eqnCreate_rate_store_new_parameter, {
#   new_parameter <- input$eqnCreate_rate_new_parameter
#   rv$param_rateEqn <- append(rv$param_rateEqn, new_parameter)
#   updateTextInput(session
#                   ,"eqnCreate_rate_new_parameter"
#                   ,value = "")
# })

observeEvent(input$eqnCreate_time_dependent_store_new_parameter, {
  new_parameter <- input$eqnCreate_time_dependent_parameters
  rv$param_timeDependentEqn <- append(rv$param_timeDependentEqn, new_parameter)
  updateTextInput(session
                  ,"eqnCreate_time_dependent_parameters"
                  ,value = "")
})

#-------------------------------------------------------------------------------

# Options UI

#-------------------------------------------------------------------------------
output$eqnCreate_Options <- renderUI({
  div
  (
    fluidRow(
      column(
        width = 12
        ,conditionalPanel(condition = "input.eqnCreate_type_of_equation=='chem_rxn'"
                          ,p("Chemical Reaction")
                          ,checkboxInput(inputId = "eqn_options_chem_modifier_forward"
                                         ,label = "Add Forward Regulator(s)"
                                         ,value = FALSE)
                          ,conditionalPanel(condition = "input.eqn_options_chem_modifier_forward"
                                            ,numericInput(inputId = "eqn_options_chem_num_forward_regulators"
                                                          ,label = "# of Forward Regulators"
                                                          ,value = 1
                                                          ,min = 1 
                                                          ,step = 1))
                          ,conditionalPanel(condition = "input.eqn_chem_forward_or_both=='both_directions'"
                                            ,hr()
                                            ,checkboxInput(inputId = "eqn_options_chem_modifier_reverse"
                                                           ,label = "Add Reverse Regulator(s)"
                                                           ,value = FALSE)
                                            ,conditionalPanel(condition = "input.eqn_options_chem_modifier_reverse"
                                                               ,numericInput(inputId = "eqn_options_chem_num_reverse_regulators"
                                                                             ,label = "# of Reverse Regulators"
                                                                             ,value = 1
                                                                             ,min = 1 
                                                                             ,step = 1))
                                            )
                          
        ),
        conditionalPanel(condition = "input.eqnCreate_type_of_equation=='enzyme_rxn'"
                         ,p("Enzyme Reaction")
                         ,checkboxInput(inputId = "eqn_options_enzyme_noVmax"
                                        ,label = "Split Vmax to kcat and enzyme"
                                        ,value = FALSE)
        )
      ) #end column
    ) #end fluidRow
  ) #end div
  
  # if(input$eqnCreate_type_of_equation=='chem_rxn')
  # {
  #   div(
  #     p("Chemical Reaction")
  #     ,checkboxInput(inputId = "eqn_options_chem_modifier_forward"
  #                   ,label = "Add Forward Regulators"
  #                   ,value = FALSE)
  #     ,checkboxInput(inputId = "eqn_options_chem_modifier_reverse"
  #                   ,label = "Add Reverse Regulators"
  #                   ,value = FALSE)
  #   )
  # }
  # else if(input$eqnCreate_type_of_equation=='enzyme_rxn')
  # {
  #   div
  #   (
  #     p("Enzyme Reaction")
  #     ,p("TEST")
  #     # ,checkboxInput(inputId = "eqn_options_enzyme_noVmax"
  #     #                ,label = "Split Vmax to kcat and enzyme"
  #     #                ,value = FALSE)
  #   )
  # }
                   
  # conditionalPanel(condition="input.eqnCreate_type_of_equation=='enzyme_rxn'"
  #                  ,p("Enzyme Reaction")
  #                  ,checkboxInput(inputId = "eqn_options_enzyme_noVmax"
  #                                 ,label = "Split Vmax to kcat and enzyme"
  #                                 ,value = FALSE)
  # )
})

output$eqnCreate_equationBuilder_chem_forward_modifiers <- renderUI({
  number_forward_regulators = as.numeric(input$eqn_options_chem_num_forward_regulators)
  number_reverse_regulators = as.numeric(input$eqn_options_chem_num_reverse_regulators)
  
  div(
    fluidRow(column(width = 12
                     ,conditionalPanel(condition = "input.eqn_options_chem_modifier_forward"
                                       ,hr()
                                       ,h5("Forward Regulators")
                                       ,hr()
                     )
    )
    )
    ,fluidRow(conditionalPanel(condition = "input.eqn_options_chem_modifier_forward"
                               ,column(width = 3
                                       #,offset=1
                                       ,lapply(seq(number_forward_regulators), function(i){
                                         pickerInput(inputId = paste0("eqn_forward_regulator_", as.character(i))
                                                     ,label = paste0("Forward Regulator ", i)
                                                     ,choices = sort(rv$vars_in_model))
                                       })
                               )#end Column
                               ,column(width = 2
                                       ,lapply(seq(number_forward_regulators), function(i){
                                         textInput(inputId = paste0("eqn_forward_rateConstant_", as.character(i))
                                                   ,label = paste0("Rate Constant ", i)
                                                   ,value = "")
                                       })
                               )
    )
    )
  )
})

output$eqnCreate_equationBuilder_chem_reverse_modifiers <- renderUI({
  number_forward_regulators = as.numeric(input$eqn_options_chem_num_forward_regulators)
  number_reverse_regulators = as.numeric(input$eqn_options_chem_num_reverse_regulators)
  
  div(
    fluidRow(column(width = 12
                     ,conditionalPanel(condition = "input.eqn_options_chem_modifier_reverse"
                                       ,hr()
                                       ,h5("Reverse Regulators")
                                       ,hr()
                     )
    )
    )
    ,fluidRow(conditionalPanel(condition = "input.eqn_options_chem_modifier_reverse"
                               ,h5("Reverse Regulators")
                               ,column(width = 3
                                       #,offset=1
                                       ,lapply(seq(number_reverse_regulators), function(i){
                                         pickerInput(inputId = paste0("eqn_reverse_regulator_", as.character(i))
                                                     ,label = paste0("Reverse Regulator ", i)
                                                     ,choices = sort(rv$vars_in_model))
                                       })
                               )#end Column
                               ,column(width = 2
                                       ,lapply(seq(number_reverse_regulators), function(i){
                                         textInput(inputId = paste0("eqn_reverse_rateConstant_", as.character(i))
                                                   ,label = paste0("Rate Constant ", i)
                                                   ,value = "")
                                       })
                               )
    )
    )
  )
})
#-------------------------------------------------------------------------------

# When Equation Add button pressed, store vars to respective places

#-------------------------------------------------------------------------------
observeEvent(input$eqnCreate_addEqnToVector, {
  eqn_type <- input$eqnCreate_type_of_equation
  
  #Add additional parameters in rate equation to proper rv
  if (eqn_type == "rate_eqn") {
    observe({print("rate eqn")})
    if (isTruthy(input$eqnCreate_rate_new_parameter)) { #if new parameters were entered (var1,var2,var3)
      observe({print("truthy statement")})
      num.param.to.add <- length(str_split(input$eqnCreate_rate_new_parameter, ","))
      parameters.to.add <- str_split(input$eqnCreate_rate_new_parameter, ",")
      for (i in seq(num.param.to.add)) {
        new.parameter <- gsub(" ", "", parameters.to.add[[i]], fixed = TRUE)
        phrase <- paste0("Added Param ", new.parameter)
        observe({print(phrase)})
        #rv$param_rateEqn <- append(rv$param_rateEqn, new.parameter)
        StoreParamsRate(new.parameter)
      }
      
      #remove parameter and value and comment from paramter vectors 
      param.to.remove = input$eqnCreate_rate_firstvar
      rv$rate_params <- append(rv$rate_params, param.to.remove)
      #search named vector for this parameter and remove
      if (param.to.remove %in% rv$param_eqns) {
        idx.of.param = which(rv$param_eqns == param.to.remove)
        rv$param_eqns = rv$param_eqns[-idx.of.param]
        rv$param_eqns_values = rv$param_eqns_values[-idx.of.param]
        rv$param_eqns_comments = rv$param_eqns_comments[-idx.of.param]
        if (length(rv$param_eqns) == 0) {
          rv$first_param_eqn_stored = FALSE
        }
      }
      #remove corresponding idices from list. 
      updateTextInput(session
                      ,"eqnCreate_rate_new_parameter"
                      ,value = "")
    }
    updateTextInput(session
                    ,"eqnCreate_rate_equation"
                    ,value = "")
  }
  if (eqn_type != "rate_eqn" && eqn_type != "time_dependent") {
    rv$eqns_in_model <- append(rv$eqns_in_model, equationBuilder())   #store selected variable to list of variables
  }
  #rate equation added in different part of code
  
  #reset text input to blank when variable entered
  eqn_type <- input$eqnCreate_type_of_equation
  number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS)
  number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS)
  nums <- c(number_RHS_equations, number_LHS_equations)
  out_list <- list(eqn_type, nums)
  #rv$eqn_info <- append(rv$eqn_info, out_list)
  updateNumericInput(session, "eqnCreate_num_of_eqn_LHS", value = 1)
  updateNumericInput(session, "eqnCreate_num_of_eqn_RHS", value = 1)
  
  updatePickerInput(session,'eqnCreate_edit_select_equation',choices = seq(length(rv$eqns_in_model)))
  updateCheckboxInput(session,"eqn_options_chem_modifier_forward",value = FALSE)
  updateNumericInput(session, "eqn_options_chem_num_forward_regulators", value = 1)
  updateCheckboxInput(session,"eqn_options_chem_modifier_reverse",value = FALSE)
  updateNumericInput(session, "eqn_options_chem_num_reverse_regulators", value = 1)
  updatePickerInput(session, "eqnCreate_rate_firstvar", choices = rv$parameters_in_model)

})

#-------------------------------------------------------------------------------

# Equation Text outputs

#-------------------------------------------------------------------------------

output$eqnCreate_showEquationBuilding <- renderText({equationBuilder()})
#output$eqnCreate_showEquations <- renderPrint({data$eqn_info})
output$eqnCreate_showEquations <- renderText({
  if (length(rv$eqns_in_model) == 0) {
    paste("No equations entered")
  } else {
    n_eqns = seq(length(rv$eqns_in_model))
    eqns_to_display <- c()
    for (i in n_eqns) {
      new_eqn <- paste0("(",i, ") ", rv$eqns_in_model[i])
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse = "<br>")
  }

})

output$eqnCreate_showAdditionalEquations <- renderText({
  if (length(rv$additional_eqns) == 0) {
    "No additional equations entered"
  } else{
    eqns_to_display <- c()
    n_eqns = seq(length(rv$additional_eqns))

    for (i in n_eqns) {
      new_eqn <- paste0("(",n_eqns[i], ") ", rv$additional_eqns[i])
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse = "<br>")
  }
})

#-------------------------------------------------------------------------------

# Removing last Equation from list

#-------------------------------------------------------------------------------
#when back button is pressed
observeEvent(input$createEqn_removeEqnFromList, {
  rv$eqns_in_model <- rv$eqns_in_model[-length(rv$eqns_in_model)] #removes equanation from equation list
  
  #need to remove parameters
  param1 = data$eqn_info[nrow(data$eqn_info), 7] #kf
  param2 = data$eqn_info[nrow(data$eqn_info), 8] #kr
  if(!is.na(param1)){
    rv$param_eqns <- rv$param_eqns[-length(rv$param_eqns)]
    rv$param_eqns_values <- rv$param_eqns_values[-length(rv$param_eqns_values)]
    rv$param_eqns_comments <- rv$param_eqns_comments[-length(rv$param_eqns_comments)]
  }
  if(!is.na(param2)){
    rv$param_eqns <- rv$param_eqns[-length(rv$param_eqns)]
    rv$param_eqns_values <- rv$param_eqns_values[-length(rv$param_eqns_values)]
    rv$param_eqns_comments <- rv$param_eqns_comments[-length(rv$param_eqns_comments)]
  }
  
  #removes equation from its data matrix
  if(nrow(data$eqn_info)==1){ #if only row in matrix
    data$eqn_info <- data$eqn_info[-nrow(data$eqn_info), ] #remove equation info from data base
    rv$first_run = TRUE #reset to be no equations
  }else{
    data$eqn_info <- data$eqn_info[-nrow(data$eqn_info), ] #remove equation info from data base
  }
  rv$number_of_equations <- rv$number_of_equations - 1
})

observeEvent(input$createEqn_removeFirstRate, {
  rv$additional_eqns <- rv$additional_eqns[-1]
})

#-------------------------------------------------------------------------------

# Delete Equation from Model

#-------------------------------------------------------------------------------
observeEvent(input$eqnCreate_addEqnToVector, {
  updatePickerInput(session
                    ,"eqnCreate_delete_equation"
                    ,choices = seq(rv$number_of_equations))
})


observeEvent(input$createEqn_delete_equation_button, {
  #delete the number equation in the list
  number_of_eqn_to_delete <- as.numeric(input$eqnCreate_delete_equation)
  data$eqn_info <- data$eqn_info[-number_of_eqn_to_delete, 1:ncol(data$eqn_info)] #delete equation from dataframe
  rv$eqns_in_model <- rv$eqns_in_model[-number_of_eqn_to_delete] #removes equanation from equation list
  rv$number_of_equations <- rv$number_of_equations - 1
  
  updatePickerInput(session
                    ,"eqnCreate_delete_equation"
                    ,choices = seq(rv$number_of_equations))
})

#-------------------------------------------------------------------------------

# Edit Tab Controlling the editing of equations

#-------------------------------------------------------------------------------

#currently returns the type of equation the user selected
testernum <- eventReactive(input$createEqn_edit_equation_button , {
  eqn_number = input$eqnCreate_edit_select_equation #eqn number to edit
  eqn_to_edit <- data$eqn_info[eqn_number, 1:ncol(data$eqn_info)] #extract equation
  return(eqn_to_edit[1])
})

#prints the type of equation
output$build_equation_edit <- renderPrint({
  req(input$createEqn_edit_equation_button)
  equationBuilder_edit()
})

output$eqnCreate_renderingUIcomponents <- renderUI({
  req(input$createEqn_edit_equation_button)
  
  eqn_number = input$eqnCreate_edit_select_equation #eqn number to edit
  eqn_to_edit <- data$eqn_info[eqn_number, 1:ncol(data$eqn_info)] #extract equation
  
  div(
    fluidRow(column(width = 3,
                    pickerInput(inputId="eqnCreate_type_of_equation_edit"
                                ,label="Select Type"
                                ,choices=c("Chemical Rxn" = "chem_rxn"
                                           ,"Enzyme-Catalyzed Rxn" = "enzyme_rxn"
                                           ,"Simple Diffusion" = "simp_diff"
                                           ,"Rate Equation" = "rate_eqn")
                                ,selected = testernum()))
             ,conditionalPanel(condition="input.eqnCreate_type_of_equation_edit=='chem_rxn'"
                               ,column(width=3
                                       ,numericInput(inputId="eqnCreate_num_of_eqn_LHS_edit"
                                                     ,label="Number of Variable on LHS"
                                                     ,value=length(str_split(eqn_to_edit[2], " ")[[1]]) 
                                                     ,min=1
                                                     ,step=1))
                               ,column(width=3
                                       ,numericInput(inputId="eqnCreate_num_of_eqn_RHS_edit"
                                                     ,label="Number of Variable on RHS"
                                                     ,value=length(str_split(eqn_to_edit[4], " ")[[1]])
                                                     ,min=1
                                                     ,step=1))
             )#end conditional Panel on chem_rxn
    )
    ,conditionalPanel(condition = "input.eqnCreate_type_of_equation_edit =='chem_rxn'"
                      ,hr()
                      ,uiOutput("edit_chemical_reaction")
    )
    ,conditionalPanel(condition = "input.eqnCreate_type_of_equation_edit =='enzyme_rxn'"
                      ,hr()
                      ,uiOutput("edit_enzyme_reaction"))
    ,hr()
    ,fluidRow(column(width=12
                      ,actionButton(inputId="edit_save_changes_button"
                                     ,label="Save Changes"))
              ,align = "right")
  ) #end div
  

})

output$edit_chemical_reaction <- renderUI({
  req(input$createEqn_edit_equation_button)
  number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS_edit)
  number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS_edit)
  eqn_number = input$eqnCreate_edit_select_equation #eqn number to edit
  eqn_to_edit <- data$eqn_info[eqn_number, 1:ncol(data$eqn_info)] #extract equation
  
  fluidRow(column(width=2
                  ,lapply(seq(number_LHS_equations), function(i){
                    numericInput(inputId=paste0("LHS_Coeff_edit_", as.character(i))
                                 ,label="Coefficient"
                                 ,value = str_split(eqn_to_edit[2], " ")[[1]][i]
                                 ,min = 1
                                 ,step=1)
                  })
  )#end Column
  ,column(width=2
          ,lapply(seq(number_LHS_equations), function(i){
            pickerInput(inputId=paste0("LHS_Var_edit_", as.character(i))
                        ,label="Choose Var"
                        ,choices=sort(rv$vars_in_model)
                        ,selected = str_split(eqn_to_edit[3], " ")[[1]][i])
          })
  )#end column
  ,column(width=3
          #,offset=1
          ,pickerInput(inputId="eqn_chem_forward_or_both_edit"
                       ,label="Reaction Direction"
                       ,choices=c("Forward" = 'forward_only'
                                  ,"Both" = "both_directions")
                       ,selected = ifelse(eqn_to_edit[6] == "forward_only", "forward_only", "both_directions"))
          ,textInput(inputId="eqn_chem_forward_k_edit"
                     ,label="Forward Rate Constant"
                     ,value=eqn_to_edit[7])
          ,conditionalPanel(condition="input.eqn_chem_forward_or_both_edit=='both_directions'"
                            ,textInput(inputId = "eqn_chem_back_k_edit"
                                       ,label="Reverse Rate Constant"
                                       ,value=eqn_to_edit[8]))
  )#end column
  ,column(width=2
          #,offset=1
          ,lapply(seq(number_RHS_equations), function(i){
            numericInput(inputId=paste0("RHS_Coeff_edit_", as.character(i))
                         ,label="Coefficient"
                         ,value = str_split(eqn_to_edit[4], " ")[[1]][i]
                         ,min = 1
                         ,step=1)
          })
  )#end Column
  ,column(width=2
          ,lapply(seq(number_RHS_equations), function(i){
            pickerInput(inputId=paste0("RHS_Var_edit_", as.character(i))
                        ,label="Choose Var"
                        ,choices=sort(rv$vars_in_model)
                        ,selected = str_split(eqn_to_edit[5], " ")[[1]][i])
          })
  )#end column
  )#end fluidRow
  
})

output$edit_enzyme_reaction <- renderUI({
  req(input$createEqn_edit_equation_button)
  eqn_number = input$eqnCreate_edit_select_equation #eqn number to edit
  eqn_to_edit <- data$eqn_info[eqn_number, 1:ncol(data$eqn_info)] #extract equation
  div(
    fluidRow(column(width=3
                    ,pickerInput(inputId="eqn_enzyme_substrate_edit"
                                 ,label="Substrate"
                                 ,choices=sort(rv$vars_in_model)
                                 ,selected=eqn_to_edit[3])
                    ,conditionalPanel(condition="input.eqn_options_enzyme_noVmax"
                                      ,pickerInput(inputId="eqn_enzyme_enzyme_edit"
                                                   ,label="Enzyme"
                                                   ,choices=sort(rv$vars_in_model)
                                                   ,selected=eqn_to_edit[12]))
    )
    ,column(width=3
            ,offset = 1
            ,conditionalPanel(condition="!input.eqn_options_enzyme_noVmax"
                              ,textInput(inputId="eqn_enzyme_Vmax_edit"
                                         ,label = "Vmax"
                                         ,value = eqn_to_edit[10]))
            ,conditionalPanel(condition="input.eqn_options_enzyme_noVmax"
                              ,textInput(inputId="eqn_enzyme_kcat_edit"
                                         ,label = "kcat"
                                         ,value = eqn_to_edit[9]))
            
            ,textInput(inputId="eqn_enzyme_Km_edit"
                       ,label = "Km"
                       ,value = eqn_to_edit[11])
    )
    ,column(width=3
            ,offset=1
            ,pickerInput(inputId="eqn_enzyme_product_edit"
                         ,label="Product"
                         ,choices=sort(rv$vars_in_model)
                         ,selected=eqn_to_edit[5]))
    )#end fluidRow
  )#end div
})
# 
# ,conditionalPanel(condition="input.eqnCreate_type_of_equation=='chem_rxn'"
#                   ,uiOutput("eqnCreate_equationBuilder_chem"))

equationBuilder_edit <- reactive({
  if(input$eqnCreate_type_of_equation_edit=="chem_rxn"){
    number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS_edit)
    number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS_edit)
    
    eqn_LHS <- ""
    for(i in seq(number_LHS_equations)){
      coef <- eval(parse(text=paste0("input$LHS_Coeff_edit_", as.character(i))))
      var <- eval(parse(text=paste0("input$LHS_Var_edit_", as.character(i))))
      if(coef!="1"){eqn_LHS <- paste0(eqn_LHS, coef, "*")}
      if(i==as.numeric(number_LHS_equations)){eqn_LHS <- paste0(eqn_LHS, var)}
      else{eqn_LHS <- paste0(eqn_LHS, var, " + ")}
    }
    
    eqn_RHS <- ""
    for (i in seq(number_RHS_equations)) {
      coef <- eval(parse(text = paste0("input$RHS_Coeff_edit_", as.character(i))))
      var <- eval(parse(text = paste0("input$RHS_Var_edit_", as.character(i))))
      if (coef != "1") {eqn_RHS <- paste0(eqn_RHS, coef, "*")}
      if (i == as.numeric(number_RHS_equations)) {eqn_RHS <- paste0(eqn_RHS, var)}
      else{eqn_RHS <- paste0(eqn_RHS, var, " + ")}
    }
    
    if (input$eqn_chem_forward_or_both_edit == "both_directions") {
      arrow <- "<-->"
      arrow <- paste0("(", input$eqn_chem_back_k_edit, ")", arrow, "(", input$eqn_chem_forward_k_edit, ")")
    }
    else if (input$eqn_chem_forward_or_both_edit == "forward_only") {
      arrow = "--->"
      arrow <- paste0(arrow, "(", input$eqn_chem_forward_k_edit, ")")
    }
    
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
  }
  
  else if (input$eqnCreate_type_of_equation_edit == "enzyme_rxn") {
    substrate = input$eqn_enzyme_substrate_edit
    product = input$eqn_enzyme_product_edit
    arrow = "-->"
    enzyme = input$eqn_enzyme_enzyme_edit
    Km = input$eqn_enzyme_Km_edit
    
    if (input$eqn_options_enzyme_noVmax) {
      kcat = input$eqn_enzyme_kcat_edit
      textOut <- paste0(substrate," + ", enzyme,  " (", kcat, ")", arrow, "(", Km, ") ", product)
    } else if (!input$eqn_options_enzyme_noVmax) {
      Vmax = input$eqn_enzyme_Vmax_edit
      textOut <- paste0(substrate, " (", Vmax, ", Enzyme)", arrow, "(", Km, ") ", product)
    }
  }
  
  # else if(input$eqnCreate_type_of_equation_edit=="simp_diff"){
  #   var_left = input$simp_diff_var1
  #   var_right = input$simp_diff_var2
  #   diff_coef <- input$simp_diff_PS_Var
  #   ifelse(input$simp_diff_wayOfDiffusion, symbol <- "-->", symbol <- "<-->")
  #   
  #   textOut <- paste0(var_left, " ", symbol, "(", diff_coef, ") ", var_right)
  # }
  # else if(input$eqnCreate_type_of_equation=="mass_bal"){
  #   textOut <- "MASS BAL"
  # }
  else{textOut <- "ERROR"}
  return(textOut)
})
#-------------------------------------------------------------------------------

# Edit Tab rewriting of Equations from Equation UI

#-------------------------------------------------------------------------------

observeEvent(input$edit_save_changes_button, {
  #find which equation we are editing. 
  eqn_number = input$eqnCreate_edit_select_equation #eqn number to edit
  eqn_to_edit <- data$eqn_info[eqn_number, 1:ncol(data$eqn_info)] #extract equation
  
  #delete components
  
  eqn_type <- input$eqnCreate_type_of_equation_edit
  #add new components to equation sheet.
  
  if (eqn_type == "chem_rxn") {
    number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS_edit)
    number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS_edit)
    
    coef_LHS <- vector()
    var_LHS <- vector()
    for (i in seq(number_LHS_equations)) {
      coef <- eval(parse(text = paste0("input$LHS_Coeff_edit_", as.character(i))))
      var <- eval(parse(text = paste0("input$LHS_Var_edit_", as.character(i))))
      coef_LHS <- append(coef_LHS, coef)
      var_LHS <- append(var_LHS, var)
    }
    coef_LHS <- paste(coef_LHS, collapse = " ")
    var_LHS <- paste(var_LHS, collapse = " ")
    
    coef_RHS <- vector()
    var_RHS <- vector()
    for (i in seq(number_RHS_equations)) {
      coef <- eval(parse(text = paste0("input$RHS_Coeff_edit_", as.character(i))))
      var <- eval(parse(text = paste0("input$RHS_Var_edit_", as.character(i))))
      coef_RHS <- append(coef_RHS, coef)
      var_RHS <- append(var_RHS, var)
    }
    coef_RHS <- paste(coef_RHS, collapse = " ")
    var_RHS <- paste(var_RHS, collapse = " ")
    
    arrow_direction <- input$eqn_chem_forward_or_both_edit
    if (arrow_direction == "both_directions") {
      kf <- input$eqn_chem_forward_k_edit
      kr <- input$eqn_chem_back_k_edit
      # rv$param_eqns <- append(rv$param_eqns, kf)
      # rv$param_eqns <- append(rv$param_eqns, kr)
      StoreParamsEqn(kf)
      StoreParamsEqn(kr)
    }
    else if (arrow_direction == "forward_only") {
      kf <- input$eqn_chem_forward_k_edit
      kr <- NA
      #rv$param_eqns <- append(rv$param_eqns, kf)
      StoreParamsEqn(kf)
    }
    kcat = NA
    Vmax = NA 
    Km = NA 
    enzyme = NA
    forward_modifier_bool <- FALSE
    f_regulators_coef <- NA
    f_regulators_rateConstants <- NA
    reverse_modifier_bool <- FALSE
    r_regulators_coef <- NA
    r_regulators_rateConstants <- NA
    row_to_df <- c(eqn_type, coef_LHS, var_LHS, coef_RHS, var_RHS, arrow_direction, kf, kr, 
                   kcat, Vmax, Km, enzyme,
                   forward_modifier_bool, f_regulators_coef, f_regulators_rateConstants,
                   reverse_modifier_bool, r_regulators_coef, r_regulators_rateConstants)
    
  }#end if chem_rxn
  else if (eqn_type == "enzyme_rxn") {
    coef_LHS <- 1
    coef_RHS <- 1
    var_LHS = input$eqn_enzyme_substrate_edit
    var_RHS = input$eqn_enzyme_product_edit
    arrow_direction <- "forward_only"
    Km = input$eqn_enzyme_Km_edit
    #rv$param_eqns <- append(rv$param_eqns, Km)
    StoreParamsEqn(Km)
    
    if (input$eqn_options_enzyme_noVmax) {
      kcat = input$eqn_enzyme_kcat_edit
      enzyme = input$eqn_enzyme_enzyme_edit
      Vmax = NA
      # rv$param_eqns <- append(rv$param_eqns, kcat)
      # rv$param_eqns <- append(rv$param_eqns, Km)
      StoreParamsEqn(kcat)
      StoreParamsEqn(Km)
    } else if (!input$eqn_options_enzyme_noVmax) {
      Vmax = input$eqn_enzyme_Vmax_edit
      kcat = NA
      enzyme = NA
      #rv$param_eqns <- append(rv$param_eqns, Vmax)
      StoreParamsEqn(Vmax)
    }
    
    kf = NA
    kr = NA
    forward_modifier_bool <- FALSE
    f_regulators_coef <- NA
    f_regulators_rateConstants <- NA
    reverse_modifier_bool <- FALSE
    r_regulators_coef <- NA
    r_regulators_rateConstants <- NA
    row_to_df <- c(eqn_type, coef_LHS, var_LHS, coef_RHS, var_RHS, arrow_direction, kf, kr, 
                   kcat, Vmax, Km, enzyme,
                   forward_modifier_bool, f_regulators_coef, f_regulators_rateConstants,
                   reverse_modifier_bool, r_regulators_coef, r_regulators_rateConstants)
  }
  data$eqn_info[eqn_number, 1:ncol(data$eqn_info)]  <- row_to_df
  rv$eqns_in_model[as.numeric(eqn_number)] <- equationBuilder_edit()
  # updatePickerInput(session,
  #                   'eqnCreate_edit_select_equation'
  #                   ,choices = seq(length(rv$eqns_in_model)))
})
observe({print(rv$eqns_in_model)})
#-------------------------------------------------------------------------------

# View Tab controlling the equations view

#-------------------------------------------------------------------------------

output$test_mathJax <- renderUI({
  withMathJax(
    # sprintf('The resulting enzyme kinetic reaction
    #            $$\\frac{d}{dt} = \\frac{V_{max}S}{K_{m} + S}
    #         = %s$$', input$eqn_enzyme_Vmax))
    tags$b("The resulting enzyme kinetic reaction:"),
    br(),
    paste0("$$\\frac{d}{dt} = \\frac{V_{max}S}{K_{m} + S} = $$ ", sprintf("$$\\frac{%s*%s}{%s + %s}$$",input$eqn_enzyme_Vmax, input$eqn_enzyme_substrate, input$eqn_enzyme_Km, input$eqn_enzyme_substrate)),
    br()
  )
})

# output$test_orderInputs <- renderUI({
#   orderInput("source1", "Vars", items =rv$vars_in_model, as_source = TRUE, connect = "test_eqn")
#   orderInput("test_eqn", "Eqn", items = NULL, placeholder = "Drag Here")
# })

