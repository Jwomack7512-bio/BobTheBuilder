CheckParametersForErrors <- function(paramsToCheck, allParamVariables) {
  # takes input of all parameters inputs for chem, enyzme, etc..only some will be active
  passed.test = TRUE #set true by default and change if error found
  for (var in paramsToCheck) {
    varCheck <- variableCheck(var, allParamVariables)
    pass.check <- varCheck[[1]]
    error.message <- varCheck[[2]]
    error.code <- varCheck[[3]]
    if (!pass.check) {
      if (error.code == 2 | error.code == 3 | error.code == 4) {
        #Sweet alert notifying user that the equation cannot be added with appropriate error message
        passed.test = FALSE
        sendSweetAlert(
          session = session,
          title = "Error...",
          text = error.message,
          type = "error"
        )
        
      } else if (error.code == 1) { #currently this is a warning and not an error because this is something that may be a thing
        sendSweetAlert(
          session = session,
          title = "Warning !!!",
          text = error.message,
          type = "warning"
        )
        # ask_confirmation(
        #   inputId = "myconfirmation1",
        #   type = "warning",
        #   title = "Want to confirm ?"
        # )
      }
    }
  }
  return(passed.test)
}

StoreParamsEqn <- function(parameterToAdd) {
  
  #NEED TO ADD CHECK IF PARAM ALREADY EXISTS
  if (!(parameterToAdd %in% params$vars.all) &&
        !(parameterToAdd %in% params$rate.params)) {
    params$eqns.vars <- append(params$eqns.vars, parameterToAdd)
    params$eqns.vals <- append(params$eqns.vals, 0)
    params$eqns.comments <- append(params$eqns.comments, "")
    
    params$vars.all <- append(params$vars.all, parameterToAdd)
    params$vals.all <- append(params$vals.all, 0)
    params$comments.all <- append(params$comments.all, "")
    
    #add parameter to parameter table
    row.to.add <- c(parameterToAdd, 0, "")
    if (nrow(params$param.table) == 0) {
      params$param.table[1,] <- row.to.add
    } else {
      params$param.table <- rbind(params$param.table, row.to.add)
    }
  }
}

StoreParamsRate <- function(parameterToAdd) {
  
  if (!params$first.rate.eqn.stored) params$first.rate.eqn.stored = TRUE
  
  #NEED TO ADD CHECK IF PARAM ALREADY EXISTS
  if (!(parameterToAdd %in% params$vars.all)) {
    params$rate.eqn.vars <- append(params$rate.eqn.vars, parameterToAdd)
    params$rate.eqn.vals <- append(params$rate.eqn.vals, 0)
    params$rate.eqn.comments <- append(params$rate.eqn.comments, "")
    
    params$vars.all <- append(params$vars.all, parameterToAdd)
    params$vals.all <- append(params$vals.all, 0)
    params$comments.all <- append(params$comments.all, "")
  }
  
  #add parameter to parameter table
  row.to.add <- c(parameterToAdd, 0, "")
  if (nrow(params$param.table) == 0) {
    params$param.table[1,] <- row.to.add
  } else {
    params$param.table <- rbind(params$param.table, row.to.add)
  }
}

build_db_row <- function(eqn_type, RHS_coef, RHS_var, LHS_coef, LHS_var,arrow_type, kf, kr, description){
  row_out <- c(eqn_type, RHS_coef, RHS_var, LHS_coef, LHS_var,arrow_type, kf, kr, description)
}

observeEvent(input$createVar_addVarToList, {
  updatePickerInput(session, "eqnCreate_recep", choices = sort(vars$species))
  updatePickerInput(session, "eqnCreate_lig", choices = sort(vars$species))

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
  eqns$n.eqns <- eqns$n.eqns + 1
  eqn_type <- input$eqnCreate_type_of_equation
  params.to.add <- c()
  passed.error.check = TRUE
  
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
        f.regulators <- c()
        f.regulators.RC <- c()
        for (i in seq(number_forward_regulators)) {
          f.regulator <- eval(parse(text = paste0("input$eqn_forward_regulator_", as.character(i))))
          f.regulators <- c(f.regulators, f.regulator)
          
          f.rc <- eval(parse(text = paste0("input$eqn_forward_rateConstant_", as.character(i))))
          f.regulators.RC <- c(f.regulators.RC, f.rc)
          params.to.add <- c(params.to.add, f.rc)
        }
      }
      else{
        forward_modifier_bool <- FALSE
        f.regulators <- NA
        f.regulators.RC <- NA
        kf <- input$eqn_chem_forward_k
        
        params.to.add <- c(params.to.add, kf)
        #StoreParamsEqn(kf)
      }
      ###Checks if regulator was used in reverse reaction, hence removing kr and updating the appropriate values for the regulator 
      if (input$eqn_options_chem_modifier_reverse) {
        kr <- NA
        reverse_modifier_bool <- TRUE 
        r.regulators <- c()
        r.regulators.RC <- c()
        for (i in seq(number_reverse_regulators)) {
          r.regulator <- eval(parse(text = paste0("input$eqn_reverse_regulator_", as.character(i))))
          r.regulators <- c(r.regulators, r.regulator)
          
          r.rc <- eval(parse(text = paste0("input$eqn_reverse_rateConstant_", as.character(i))))
          jPrint(r.rc)
          r.regulators.RC <- c(r.regulators.RC, r.rc)
          params.to.add <- c(params.to.add, r.rc)
        }
      }
      else{
        kr <- input$eqn_chem_back_k
        reverse_modifier_bool <- FALSE
        r.regulators <- NA
        r.regulators.RC <- NA
        params.to.add <- c(params.to.add, kr)
      }
    }
    else if (arrow_direction == "forward_only") {
      if (input$eqn_options_chem_modifier_forward) {
        kf <- NA
        forward_modifier_bool <- TRUE
        f.regulators <- c()
        f.regulators.RC <- c()
        for (i in seq(number_forward_regulators)) {
          f.regulator <- eval(parse(text = paste0("input$eqn_forward_regulator_", as.character(i))))
          f.regulators <- c(f.regulators, f.regulator)
          
          f.rc <- eval(parse(text = paste0("input$eqn_forward_rateConstant_", as.character(i))))
          f.regulators.RC <- c(f.regulators.RC, f.rc)
          params.to.add <- c(params.to.add, f.rc)
        }
      }
      else{
        kf <- input$eqn_chem_forward_k
        params.to.add <- c(params.to.add, kf)
        #StoreParamsEqn(kf)
        forward_modifier_bool <- FALSE
        f.regulators <- NA
        f.regulators.RC <- NA
      }
      kr <- NA
      reverse_modifier_bool <- FALSE
      r.regulators <- NA
      r.regulators.RC <- NA
    }
    kcat = NA
    Vmax = NA 
    Km = NA 
    enzyme = NA
    passed.error.check <- CheckParametersForErrors(params.to.add, params$vars.all)
    
    if (passed.error.check) {
      for (var in params.to.add) {
        StoreParamsEqn(var)
      }
      f.regulators <- paste(f.regulators, collapse = " ")
      r.regulators <- paste(r.regulators, collapse = " ")
      f.regulators.RC <- paste(f.regulators.RC, collapse = " ")
      r.regulators.RC <- paste(r.regulators.RC, collapse = " ")
      jPrint("Regulators")
      jPrint(r.regulators.RC)
      row_to_df <- c(eqn_type, coef_LHS, var_LHS, coef_RHS, var_RHS, arrow_direction, kf, kr, 
                     kcat, Vmax, Km, enzyme,
                     forward_modifier_bool, f.regulators, f.regulators.RC,
                     reverse_modifier_bool, r.regulators, r.regulators.RC)
      
      eqns$eqn.descriptions <- c(eqns$eqn.descriptions, "")
    }
    
    #print(row_to_df)
    
  }#end if chem_rxn
  else if (eqn_type == "enzyme_rxn") {
    coef_LHS <- 1
    coef_RHS <- 1
    var_LHS = input$eqn_enzyme_substrate
    var_RHS = input$eqn_enzyme_product
    arrow_direction <- "forward_only"
    Km = input$eqn_enzyme_Km
    StoreParamsEqn(Km)
    
    if (input$eqn_options_enzyme_noVmax) {
      kcat = input$eqn_enzyme_kcat
      enzyme = input$eqn_enzyme_enzyme
      Vmax = NA
      StoreParamsEqn(kcat)
      #StoreParamsEqn(Km)
    } else if (!input$eqn_options_enzyme_noVmax) {
      Vmax = input$eqn_enzyme_Vmax
      kcat = NA
      enzyme = NA
      #params$eqns.vars <- append(params$eqns.vars, Vmax)
      StoreParamsEqn(Vmax)
    }
    
    kf = NA
    kr = NA
    forward_modifier_bool <- FALSE
    f_regulators_coef <- NA
    f_regulators_rateConstants <- NA
    reverse_modifier_bool <- FALSE
    r.regulators <- NA
    r.regulators.RC <- NA
    row_to_df <- c(eqn_type, coef_LHS, var_LHS, coef_RHS, var_RHS, arrow_direction, kf, kr, 
                   kcat, Vmax, Km, enzyme,
                   forward_modifier_bool, f_regulators_coef, f_regulators_rateConstants,
                   reverse_modifier_bool, r.regulators, r.regulators.RC)
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
    r.regulators <- NA
    r.regulators.RC <- NA
    row_to_df <- c(eqn_type, coef_LHS, var_LHS, coef_RHS, var_RHS, arrow_direction, kf, kr, 
                   kcat, Vmax, Km, enzyme,
                   forward_modifier_bool, f_regulators_coef, f_regulators_rateConstants,
                   reverse_modifier_bool, r.regulators, r.regulators.RC)    

    StoreParamsEqn(kf)
  }
  else if (eqn_type == "rate_eqn") {
    rate_left <- input$eqnCreate_rate_firstvar
    rate_right <- input$eqnCreate_rate_equation
    rate_eqn <- paste0(rate_left, " = ", rate_right)
    eqns$additional.eqns <- c(eqns$additional.eqns, rate_eqn)
    params$parameters.based.on.other.values <- rate_left
    
    #remove rate_left from parameters-----------------------------------------------------------------------------------------------------------------------
    #split_rate_to_components()
    
    #search all parameters lists for parameter and remove it from each. (input, output, eqn, total)
    parameter_to_remove <- rate_left
    if (parameter_to_remove %in% params$inputs.vars) {
      params$inputs.vars <- params$inputs.vars[!params$inputs.vars %in% parameter_to_remove]
    }
    if (parameter_to_remove %in% params$outputs.vars) {
      params$outputs.vars <- params$outputs.vars[!params$outputs.vars %in% parameter_to_remove]
    }
    if (parameter_to_remove %in% params$eqns.vars) {
      params$eqns.vars <- params$eqns.vars[!params$eqns.vars %in% parameter_to_remove]
    }
    if (parameter_to_remove %in% params$vars.all) {
      params$vars.all <- params$vars.all[!params$vars.all %in% parameter_to_remove]
    }
    #remove all excess variables from created lists if they exist (ie. we generated ui for parameter values and comments.  Will need to remove those)
  }
  else if (eqn_type == "time_dependent")
  {
    TD_left <- input$eqnCreate_time_dependent_firstvar
    TD_right <- input$eqnCreate_time_dependent_equation
    TD_eqn <- paste0(TD_left, "=", TD_right)
    eqns$additional.eqns <- c(eqns$additional.eqns, TD_eqn)
    params$parameters.based.on.other.values <- TD_left
  }
  if (eqn_type != "rate_eqn" && eqn_type != "time_dependent")
  {
    if (eqns$first.run)
    {
      if (passed.error.check) {
        eqns$first.run <- FALSE
        eqns$eqn.info[1,] <- row_to_df
      }
    }
    else
    {
      if (passed.error.check) {
        eqns$eqn.info  <- rbind(eqns$eqn.info , row_to_df)
      }
      
    }
  }
  if (passed.error.check) {
    if (eqn_type != "rate_eqn" && eqn_type != "time_dependent") {
      eqns$main <- append(eqns$main, equationBuilder())   #store selected variable to list of variables
    }
  }

})
observe(print(eqns$eqn.info))

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
    fluidRow(column(width = 1
                    ,lapply(seq(number_LHS_equations), function(i){
                      numericInput(inputId = paste0("LHS_Coeff_", as.character(i))
                                   ,label = "#"
                                   ,value = 1
                                   ,min = 1
                                   ,step = 1)
                    })
      )#end Column
      ,column(width = 3
              ,lapply(seq(number_LHS_equations), function(i){
                pickerInput(inputId = paste0("LHS_Var_", as.character(i))
                            ,label =  paste0("Reactant ", as.character(i))
                            ,choices = sort(vars$species)
                            ,options = pickerOptions(liveSearch = TRUE
                                                    ,liveSearchStyle = "startsWith"
                                                    ,dropupAuto = FALSE))
              })
      )#end column
      ,column(width = 3
              #,offset=1
              ,pickerInput(inputId = "eqn_chem_forward_or_both"
                           ,label = "Reaction Direction"
                           ,choices = c("Reversible" = "both_directions",
                                        "Forward" = 'forward_only'
                                       )
                           ,choicesOpt = list(icon = c("glyphicon glyphicon-resize-horizontal",
                                                       "glyphicon glyphicon-arrow-right"
                                                       )
                                              )
                           )
              ,conditionalPanel(condition = "!input.eqn_options_chem_modifier_forward"
                                ,textInput(inputId = "eqn_chem_forward_k"
                                           ,label = "Forward Rate Constant"
                                           ,value = paste0("k_f", as.character(eqns$n.eqns + 1)))
              )
              ,conditionalPanel(condition = "input.eqn_chem_forward_or_both=='both_directions' && !input.eqn_options_chem_modifier_reverse"
                                ,textInput(inputId = "eqn_chem_back_k"
                                           ,label = "Reverse Rate Constant"
                                           ,value = paste0("k_r", as.character(eqns$n.eqns + 1))))
      )#end column
      ,column(width = 1
              #,offset=1
              ,lapply(seq(number_RHS_equations), function(i){
                numericInput(inputId = paste0("RHS_Coeff_", as.character(i))
                             ,label = "#"
                             ,value = 1
                             ,min = 1
                             ,step = 1)
              })
      )#end Column
      ,column(width = 3
              ,lapply(seq(number_RHS_equations), function(i){
                pickerInput(inputId = paste0("RHS_Var_", as.character(i))
                            ,label = paste0("Product ", as.character(i))
                            ,choices = sort(vars$species)
                            ,options = pickerOptions(liveSearch = TRUE
                                                     ,liveSearchStyle = "startsWith"
                                                     ,dropupAuto = FALSE))
              })
      )#end column
    )#end fluidRow`
    # Forward Modifier UI ------------------------------------------------------`
    ,fluidRow(
      column(
        offset = 1
        ,width = 3
          ,conditionalPanel(
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
      )
      ,column(
        width = 3
        ,conditionalPanel(
          condition = "input.eqn_options_chem_modifier_forward"
          ,lapply(seq(number_forward_regulators), function(i){
                textInput(
                  inputId = paste0("eqn_forward_rateConstant_", as.character(i))
                  ,label = paste0("Rate Constant ", as.character(i))
                  ,value = "")
          })
          )
        )
      )
    # Reverse Modifier UI ------------------------------------------------------
    ,fluidRow(
      column(
        offset = 1
        ,width = 3
        ,conditionalPanel(
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
      )
      ,column(
        width = 3
        ,conditionalPanel(
          condition = "input.eqn_options_chem_modifier_reverse"
          ,lapply(seq(number_reverse_regulators), function(i){
            textInput(
              inputId = paste0("eqn_reverse_rateConstant_", as.character(i))
              ,label = "Rate Constant"
              ,value = "")
          })
        )
      )
    )
  )#end div
  
})

output$eqnCreate_equationBuilder_enzyme <- renderUI({

  div(
    fluidRow(column(width = 3
                    ,pickerInput(inputId = "eqn_enzyme_substrate"
                                 ,label = "Substrate"
                                 ,choices = vars$species
                                 ,options = pickerOptions(liveSearch = TRUE
                                                          ,liveSearchStyle = "startsWith"
                                                          ,dropupAuto = FALSE))
                    ,conditionalPanel(condition = "input.eqn_options_enzyme_noVmax"
                                      ,pickerInput(inputId = "eqn_enzyme_enzyme"
                                                   ,label = "Enzyme"
                                                   ,choices = sort(vars$species)
                                                   ,options = pickerOptions(liveSearch = TRUE
                                                                            ,liveSearchStyle = "startsWith")))
                    )
             ,column(
               width = 3
               ,offset = 1
               ,conditionalPanel(
                 condition = "!input.eqn_options_enzyme_noVmax"
                 ,textInput(
                   inputId = "eqn_enzyme_Vmax"
                   ,label = "Vmax"
                   ,value = paste0("Vmax_", as.character(eqns$n.eqns + 1))))
                   ,conditionalPanel(
                     condition = "input.eqn_options_enzyme_noVmax"
                     ,textInput(inputId = "eqn_enzyme_kcat"
                     ,label = "kcat"
                     ,value = paste0("kcat_", as.character(eqns$n.eqns + 1))))
                     
                     ,textInput(inputId = "eqn_enzyme_Km"
                                ,label = "Km"
                                ,value = paste0("Km_", as.character(eqns$n.eqns + 1)))
                     )
             ,column(width = 3
                     ,offset = 1
                     ,pickerInput(inputId = "eqn_enzyme_product"
                                  ,label = "Product"
                                  ,choices = sort(vars$species)
                                  ,options = pickerOptions(liveSearch = TRUE
                                                           ,liveSearchStyle = "startsWith"
                                                           ,dropupAuto = FALSE)))
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
                                 ,choices=sort(vars$species)
                                 ,options = pickerOptions(liveSearch = TRUE
                                                          ,liveSearchStyle = "startsWith")))
             ,column(width=3
                     ,textInput(inputId="simp_diff_PS_Var"
                                ,label = "Diffusion Constant"
                                ,value = paste0("PS", as.character(eqns$n.eqns+1))))
             ,column(width=3
                     ,pickerInput(inputId="simp_diff_var2"
                                  ,label="Var2"
                                  ,choices=sort(vars$species)
                                  ,options = pickerOptions(liveSearch = TRUE
                                                           ,liveSearchStyle = "startsWith")))
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
#   params$rate.eqn.vars <- append(params$rate.eqn.vars, new_parameter)
#   updateTextInput(session
#                   ,"eqnCreate_rate_new_parameter"
#                   ,value = "")
# })

observeEvent(input$eqnCreate_time_dependent_store_new_parameter, {
  new_parameter <- input$eqnCreate_time_dependent_parameters
  params$time.dep.vars <- append(params$time.dep.vars, new_parameter)
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
        ,conditionalPanel(
          condition = "input.eqnCreate_type_of_equation=='chem_rxn'"
          ,checkboxInput(
            inputId = "eqn_options_chem_modifier_forward"
            ,label = "Add Forward Regulator(s)"
            ,value = FALSE
          )
          ,conditionalPanel(condition = "input.eqn_options_chem_modifier_forward"
                            ,numericInput(inputId = "eqn_options_chem_num_forward_regulators"
                                          ,label = "# of Forward Regulators"
                                          ,value = 1
                                          ,min = 1 
                                          ,step = 1)
                            )
          ,conditionalPanel(
            condition = "input.eqn_chem_forward_or_both=='both_directions'"
            ,hr()
            ,checkboxInput(
              inputId = "eqn_options_chem_modifier_reverse"
              ,label = "Add Reverse Regulator(s)"
              ,value = FALSE
            )
            ,conditionalPanel(condition = "input.eqn_options_chem_modifier_reverse"
                              ,numericInput(inputId = "eqn_options_chem_num_reverse_regulators"
                                            ,label = "# of Reverse Regulators"
                                            ,value = 1
                                            ,min = 1 
                                            ,step = 1)
            )
          )
        )
        ,conditionalPanel(
          condition = "input.eqnCreate_type_of_equation=='enzyme_rxn'"
          ,checkboxInput(
            inputId = "eqn_options_enzyme_noVmax"
            ,label = "Split Vmax to kcat and enzyme"
            ,value = FALSE
          )
        )
      ) #end column
    ) #end fluidRow
  ) #end div
})

# output$eqnCreate_equationBuilder_chem_forward_modifiers <- renderUI({
#   # number_forward_regulators = as.numeric(input$eqn_options_chem_num_forward_regulators)
#   # number_reverse_regulators = as.numeric(input$eqn_options_chem_num_reverse_regulators)
#   number_forward_regulators = 1
#   number_reverse_regulators = 1
#   
#   div(
#     fluidRow(column(width = 12
#                      ,conditionalPanel(condition = "input.eqn_options_chem_modifier_forward"
#                                        ,hr()
#                                        ,h5("Forward Regulators")
#                                        ,hr()
#                      )
#     )
#     )
#     ,fluidRow(conditionalPanel(condition = "input.eqn_options_chem_modifier_forward"
#                                ,column(width = 3
#                                        #,offset=1
#                                        ,lapply(seq(number_forward_regulators), function(i){
#                                          pickerInput(inputId = paste0("eqn_forward_regulator_", as.character(i))
#                                                      ,label = paste0("Forward Regulator ", i)
#                                                      ,choices = sort(vars$species))
#                                        })
#                                )#end Column
#                                ,column(width = 2
#                                        ,lapply(seq(number_forward_regulators), function(i){
#                                          textInput(inputId = paste0("eqn_forward_rateConstant_", as.character(i))
#                                                    ,label = paste0("Rate Constant ", i)
#                                                    ,value = "")
#                                        })
#                                )
#     )
#     )
#   )
# })
# 
# output$eqnCreate_equationBuilder_chem_reverse_modifiers <- renderUI({
#   number_forward_regulators = as.numeric(input$eqn_options_chem_num_forward_regulators)
#   number_reverse_regulators = as.numeric(input$eqn_options_chem_num_reverse_regulators)
#   
#   div(
#     fluidRow(column(width = 12
#                      ,conditionalPanel(condition = "input.eqn_options_chem_modifier_reverse"
#                                        ,hr()
#                                        ,h5("Reverse Regulators")
#                                        ,hr()
#                      )
#     )
#     )
#     ,fluidRow(conditionalPanel(condition = "input.eqn_options_chem_modifier_reverse"
#                                ,h5("Reverse Regulators")
#                                ,column(width = 3
#                                        #,offset=1
#                                        ,lapply(seq(number_reverse_regulators), function(i){
#                                          pickerInput(inputId = paste0("eqn_reverse_regulator_", as.character(i))
#                                                      ,label = paste0("Reverse Regulator ", i)
#                                                      ,choices = sort(vars$species))
#                                        })
#                                )#end Column
#                                ,column(width = 2
#                                        ,lapply(seq(number_reverse_regulators), function(i){
#                                          textInput(inputId = paste0("eqn_reverse_rateConstant_", as.character(i))
#                                                    ,label = paste0("Rate Constant ", i)
#                                                    ,value = "")
#                                        })
#                                )
#     )
#     )
#   )
# })
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
        jPrint(phrase)
        #params$rate.eqn.vars <- append(params$rate.eqn.vars, new.parameter)
        StoreParamsRate(new.parameter)
      }
      
      #remove parameter and value and comment from paramter vectors 
      param.to.remove = input$eqnCreate_rate_firstvar
      params$rate.params <- append(params$rate.params, param.to.remove)
      #search named vector for this parameter and remove
      if (param.to.remove %in% params$eqns.vars) {
        idx.of.param = which(params$eqns.vars == param.to.remove)
        params$eqns.vars = params$eqns.vars[-idx.of.param]
        params$eqns.vals = params$eqns.vals[-idx.of.param]
        params$eqns.comments = params$eqns.comments[-idx.of.param]
        if (length(params$eqns.vars) == 0) {
           params$first.param.eqn.stored = FALSE
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

  #rate equation added in different part of code
  
  #reset text input to blank when variable entered
  eqn_type <- input$eqnCreate_type_of_equation
  number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS)
  number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS)
  nums <- c(number_RHS_equations, number_LHS_equations)
  out_list <- list(eqn_type, nums)

  updateNumericInput(session, "eqnCreate_num_of_eqn_LHS", value = 1)
  updateNumericInput(session, "eqnCreate_num_of_eqn_RHS", value = 1)
  
  updatePickerInput(session,'eqnCreate_edit_select_equation',choices = seq(length(eqns$main)))
  updateCheckboxInput(session,"eqn_options_chem_modifier_forward",value = FALSE)
  updateNumericInput(session, "eqn_options_chem_num_forward_regulators", value = 1)
  updateCheckboxInput(session,"eqn_options_chem_modifier_reverse",value = FALSE)
  updateNumericInput(session, "eqn_options_chem_num_reverse_regulators", value = 1)
  updatePickerInput(session, "eqnCreate_rate_firstvar", choices = params$vars.all)

})

#-------------------------------------------------------------------------------

# Equation Text outputs

#-------------------------------------------------------------------------------

output$eqnCreate_showEquationBuilding <- renderText({equationBuilder()})
#output$eqnCreate_showEquations <- renderPrint({eqns$eqn.info})
output$eqnCreate_showEquations <- renderText({
  if (length(eqns$main) == 0) {
    paste("No equations entered")
  } else {
    n_eqns = seq(length(eqns$main))
    eqns_to_display <- c()
    for (i in n_eqns) {
      new_eqn <- paste0("(",i, ") ", eqns$main[i])
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse = "<br>")
  }

})

output$eqnCreate_showAdditionalEquations <- renderText({
  if (length(eqns$additional.eqns) == 0) {
    "No additional equations entered"
  } else{
    eqns_to_display <- c()
    n_eqns = seq(length(eqns$additional.eqns))

    for (i in n_eqns) {
      new_eqn <- paste0("(",n_eqns[i], ") ", eqns$additional.eqns[i])
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
  eqns$main <- eqns$main[-length(eqns$main)] #removes equanation from equation list
  
  #need to remove parameters
  param1 = eqns$eqn.info[nrow(eqns$eqn.info), 7] #kf
  param2 = eqns$eqn.info[nrow(eqns$eqn.info), 8] #kr
  if(!is.na(param1)){
    params$eqns.vars <- params$eqns.vars[-length(params$eqns.vars)]
    params$eqns.vals <- params$eqns.vals[-length(params$eqns.vals)]
    params$eqns.comments <- params$eqns.comments[-length(params$eqns.comments)]
  }
  if(!is.na(param2)){
    params$eqns.vars <- params$eqns.vars[-length(params$eqns.vars)]
    params$eqns.vals <- params$eqns.vals[-length(params$eqns.vals)]
    params$eqns.comments <- params$eqns.comments[-length(params$eqns.comments)]
  }
  
  #removes equation from its data matrix
  if(nrow(eqns$eqn.info)==1){ #if only row in matrix
    eqns$eqn.info <- eqns$eqn.info[-nrow(eqns$eqn.info), ] #remove equation info from data base
    eqns$first.run = TRUE #reset to be no equations
  }else{
    eqns$eqn.info <- eqns$eqn.info[-nrow(eqns$eqn.info), ] #remove equation info from data base
  }
  eqns$n.eqns<- eqns$n.eqns- 1
})

observeEvent(input$createEqn_removeFirstRate, {
  eqns$additional.eqns <- eqns$additional.eqns[-1]
})

#-------------------------------------------------------------------------------

# Delete Equation from Model

#-------------------------------------------------------------------------------
observeEvent(eqns$main, {
  jPrint(eqns$n.eqns)
  jPrint("eqns above")
  updatePickerInput(session
                    ,"eqnCreate_delete_equation"
                    ,choices = as.character(seq(eqns$n.eqns)))
})


observeEvent(input$createEqn_delete_equation_button, {
  #delete the number equation in the list
  number_of_eqn_to_delete <- as.numeric(input$eqnCreate_delete_equation)
  eqns$eqn.info <- eqns$eqn.info[-number_of_eqn_to_delete, 1:ncol(eqns$eqn.info)] #delete equation from dataframe
  eqns$main <- eqns$main[-number_of_eqn_to_delete] #removes equanation from equation list
  eqns$n.eqns <- eqns$n.eqns - 1
  eqns$eqn.descriptions <- eqns$eqn.descriptions[-number_of_eqn_to_delete]
  
  my.choices <- paste0(seq(eqns$n.eqns), ") ", eqns$main)
  updatePickerInput(session,
                    "eqnCreate_selectEqnForDescription",
                    choices = my.choices)
  # updatePickerInput(session
  #                   ,"eqnCreate_delete_equation"
  #                   ,choices = seq(eqns$n.eqns))
})

#-------------------------------------------------------------------------------

# Edit Tab Controlling the editing of equations

#-------------------------------------------------------------------------------

#currently returns the type of equation the user selected
testernum <- eventReactive(input$createEqn_edit_equation_button , {
  eqn_number = input$eqnCreate_edit_select_equation #eqn number to edit
  eqn_to_edit <- eqns$eqn.info[eqn_number, 1:ncol(eqns$eqn.info)] #extract equation
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
  eqn_to_edit <- eqns$eqn.info[eqn_number, 1:ncol(eqns$eqn.info)] #extract equation
  
  div(
    fluidRow(column(width = 3,
                    pickerInput(inputId = "eqnCreate_type_of_equation_edit"
                                ,label = "Select Type"
                                ,choices = c("Chemical Rxn" = "chem_rxn"
                                           ,"Enzyme-Catalyzed Rxn" = "enzyme_rxn"
                                           ,"Simple Diffusion" = "simp_diff"
                                           ,"Rate Equation" = "rate_eqn")
                                ,selected = testernum()))
             ,column(width = 3,
                     conditionalPanel(condition = "input.eqnCreate_type_of_equation_edit=='chem_rxn'"
                                       ,numericInput(inputId = "eqnCreate_num_of_eqn_LHS_edit"
                                                     ,label = "Number of Variable on LHS"
                                                     ,value = length(str_split(eqn_to_edit[2], " ")[[1]]) 
                                                     ,min = 1
                                                     ,step = 1)))
            ,column(width = 3,
                    conditionalPanel(condition = "input.eqnCreate_type_of_equation_edit=='chem_rxn'"
                                       ,numericInput(inputId = "eqnCreate_num_of_eqn_RHS_edit"
                                                     ,label = "Number of Variable on RHS"
                                                     ,value = length(str_split(eqn_to_edit[4], " ")[[1]])
                                                     ,min = 1
                                                     ,step = 1)))
    )
    ,conditionalPanel(condition = "input.eqnCreate_type_of_equation_edit =='chem_rxn'"
                      ,hr()
                      ,uiOutput("edit_chemical_reaction")
    )
    ,conditionalPanel(condition = "input.eqnCreate_type_of_equation_edit =='enzyme_rxn'"
                      ,hr()
                      ,uiOutput("edit_enzyme_reaction"))
    ,hr()
    ,fluidRow(column(width = 12
                      ,actionButton(inputId = "edit_save_changes_button"
                                     ,label = "Save Changes"))
              ,align = "right")
  ) #end div
  

})

output$edit_chemical_reaction <- renderUI({
  req(input$createEqn_edit_equation_button)
  number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS_edit)
  number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS_edit)
  eqn_number = input$eqnCreate_edit_select_equation #eqn number to edit
  eqn_to_edit <- eqns$eqn.info[eqn_number, 1:ncol(eqns$eqn.info)] #extract equation
  
  fluidRow(column(width = 2
                  ,lapply(seq(number_LHS_equations), function(i){
                    numericInput(inputId = paste0("LHS_Coeff_edit_", as.character(i))
                                 ,label = "Coefficient"
                                 ,value = str_split(eqn_to_edit[2], " ")[[1]][i]
                                 ,min = 1
                                 ,step = 1)
                  })
  )#end Column
  ,column(width = 2
          ,lapply(seq(number_LHS_equations), function(i){
            pickerInput(inputId = paste0("LHS_Var_edit_", as.character(i))
                        ,label = "Choose Var"
                        ,choices = sort(vars$species)
                        ,selected = str_split(eqn_to_edit[3], " ")[[1]][i])
          })
  )#end column
  ,column(width = 3
          #,offset=1
          ,pickerInput(inputId = "eqn_chem_forward_or_both_edit"
                       ,label = "Reaction Direction"
                       ,choices = c("Forward" = 'forward_only'
                                  ,"Both" = "both_directions")
                       ,selected = ifelse(eqn_to_edit[6] == "forward_only", "forward_only", "both_directions"))
          ,textInput(inputId = "eqn_chem_forward_k_edit"
                     ,label = "Forward Rate Constant"
                     ,value = eqn_to_edit[7])
          ,conditionalPanel(condition = "input.eqn_chem_forward_or_both_edit=='both_directions'"
                            ,textInput(inputId = "eqn_chem_back_k_edit"
                                       ,label = "Reverse Rate Constant"
                                       ,value = eqn_to_edit[8]))
  )#end column
  ,column(width = 2
          #,offset=1
          ,lapply(seq(number_RHS_equations), function(i){
            numericInput(inputId = paste0("RHS_Coeff_edit_", as.character(i))
                         ,label = "Coefficient"
                         ,value = str_split(eqn_to_edit[4], " ")[[1]][i]
                         ,min = 1
                         ,step = 1)
          })
  )#end Column
  ,column(width = 2
          ,lapply(seq(number_RHS_equations), function(i){
            pickerInput(inputId = paste0("RHS_Var_edit_", as.character(i))
                        ,label = "Choose Var"
                        ,choices = sort(vars$species)
                        ,selected = str_split(eqn_to_edit[5], " ")[[1]][i])
          })
  )#end column
  )#end fluidRow
  
})

output$edit_enzyme_reaction <- renderUI({
  req(input$createEqn_edit_equation_button)
  eqn_number = input$eqnCreate_edit_select_equation #eqn number to edit
  eqn_to_edit <- eqns$eqn.info[eqn_number, 1:ncol(eqns$eqn.info)] #extract equation
  div(
    fluidRow(column(width = 3
                    ,pickerInput(inputId = "eqn_enzyme_substrate_edit"
                                 ,label = "Substrate"
                                 ,choices = sort(vars$species)
                                 ,selected = eqn_to_edit[3])
                    ,conditionalPanel(condition = "input.eqn_options_enzyme_noVmax"
                                      ,pickerInput(inputId = "eqn_enzyme_enzyme_edit"
                                                   ,label = "Enzyme"
                                                   ,choices = sort(vars$species)
                                                   ,selected = eqn_to_edit[12]))
    )
    ,column(width = 3
            ,offset = 1
            ,conditionalPanel(condition = "!input.eqn_options_enzyme_noVmax"
                              ,textInput(inputId = "eqn_enzyme_Vmax_edit"
                                         ,label = "Vmax"
                                         ,value = eqn_to_edit[10]))
            ,conditionalPanel(condition = "input.eqn_options_enzyme_noVmax"
                              ,textInput(inputId = "eqn_enzyme_kcat_edit"
                                         ,label = "kcat"
                                         ,value = eqn_to_edit[9]))
            
            ,textInput(inputId = "eqn_enzyme_Km_edit"
                       ,label = "Km"
                       ,value = eqn_to_edit[11])
    )
    ,column(width = 3
            ,offset = 1
            ,pickerInput(inputId = "eqn_enzyme_product_edit"
                         ,label = "Product"
                         ,choices = sort(vars$species)
                         ,selected = eqn_to_edit[5]))
    )#end fluidRow
  )#end div
})
# 
# ,conditionalPanel(condition="input.eqnCreate_type_of_equation=='chem_rxn'"
#                   ,uiOutput("eqnCreate_equationBuilder_chem"))

equationBuilder_edit <- reactive({
  if (input$eqnCreate_type_of_equation_edit == "chem_rxn") {
    number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS_edit)
    number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS_edit)
    
    eqn_LHS <- ""
    for (i in seq(number_LHS_equations)) {
      coef <- eval(parse(text = paste0("input$LHS_Coeff_edit_", as.character(i))))
      var <- eval(parse(text = paste0("input$LHS_Var_edit_", as.character(i))))
      if (coef != "1") {eqn_LHS <- paste0(eqn_LHS, coef, "*")}
      if (i == is.numeric(number_LHS_equations)) {eqn_LHS <- paste0(eqn_LHS, var)}
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
  eqn_to_edit <- eqns$eqn.info[eqn_number, 1:ncol(eqns$eqn.info)] #extract equation
  
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
      # params$eqns.vars <- append(params$eqns.vars, kf)
      # params$eqns.vars <- append(params$eqns.vars, kr)
      StoreParamsEqn(kf)
      StoreParamsEqn(kr)
    }
    else if (arrow_direction == "forward_only") {
      kf <- input$eqn_chem_forward_k_edit
      kr <- NA
      #params$eqns.vars <- append(params$eqns.vars, kf)
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
    r.regulators <- NA
    r.regulators.RC <- NA
    row_to_df <- c(eqn_type, coef_LHS, var_LHS, coef_RHS, var_RHS, arrow_direction, kf, kr, 
                   kcat, Vmax, Km, enzyme,
                   forward_modifier_bool, f_regulators_coef, f_regulators_rateConstants,
                   reverse_modifier_bool, r.regulators, r.regulators.RC)
    
  }#end if chem_rxn
  else if (eqn_type == "enzyme_rxn") {
    coef_LHS <- 1
    coef_RHS <- 1
    var_LHS = input$eqn_enzyme_substrate_edit
    var_RHS = input$eqn_enzyme_product_edit
    arrow_direction <- "forward_only"
    Km = input$eqn_enzyme_Km_edit
    #params$eqns.vars <- append(params$eqns.vars, Km)
    StoreParamsEqn(Km)
    
    if (input$eqn_options_enzyme_noVmax) {
      kcat = input$eqn_enzyme_kcat_edit
      enzyme = input$eqn_enzyme_enzyme_edit
      Vmax = NA
      # params$eqns.vars <- append(params$eqns.vars, kcat)
      # params$eqns.vars <- append(params$eqns.vars, Km)
      StoreParamsEqn(kcat)
      StoreParamsEqn(Km)
    } else if (!input$eqn_options_enzyme_noVmax) {
      Vmax = input$eqn_enzyme_Vmax_edit
      kcat = NA
      enzyme = NA
      #params$eqns.vars <- append(params$eqns.vars, Vmax)
      StoreParamsEqn(Vmax)
    }
    
    kf = NA
    kr = NA
    forward_modifier_bool <- FALSE
    f_regulators_coef <- NA
    f_regulators_rateConstants <- NA
    reverse_modifier_bool <- FALSE
    r.regulators <- NA
    r.regulators.RC <- NA
    row_to_df <- c(eqn_type, coef_LHS, var_LHS, coef_RHS, var_RHS, arrow_direction, kf, kr, 
                   kcat, Vmax, Km, enzyme,
                   forward_modifier_bool, f_regulators_coef, f_regulators_rateConstants,
                   reverse_modifier_bool, r.regulators, r.regulators.RC)
  }
  eqns$eqn.info[eqn_number, 1:ncol(eqns$eqn.info)]  <- row_to_df
  eqns$main[as.numeric(eqn_number)] <- equationBuilder_edit()
  # updatePickerInput(session,
  #                   'eqnCreate_edit_select_equation'
  #                   ,choices = seq(length(eqns$main)))
})
observe({print(eqns$main)})
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


observeEvent(input$eqnCreate_addEqnToVector, {
  my.choices <- paste0(seq(eqns$n.eqns), ") ", eqns$main)
  updatePickerInput(session,
                    "eqnCreate_selectEqnForDescription",
                    choices = my.choices)
})

observeEvent(input$eqnCreate_storeEqnDescription, {
  #store current description to description vector 
  
  # find index
  idx = eqn.num <- as.numeric(str_split(input$eqnCreate_selectEqnForDescription, "\\)")[[1]][1])
  
  # find description
  text.to.store <- eval(parse(text = paste0("input$eqnDescription_", as.character(idx))))
  jPrint(text.to.store)
  jPrint(paste0("input$eqnDescription_", as.character(idx)))
  
  # store it to descriptions
  eqns$eqn.descriptions[idx] <- text.to.store
})

output$eqnCreate_eqnDescription <- renderUI({
  req(eqns$n.eqns > 0)
  eqn.num <- as.numeric(str_split(input$eqnCreate_selectEqnForDescription, "\\)")[[1]][1])
  #eqn.num = as.numeric(input$eqnCreate_selectEqnForDescription)
  
  textAreaInput(inputId = paste0("eqnDescription_", eqn.num),
                label = paste0("Description of \"", eqns$main[eqn.num], "\""),
                value = eqns$eqn.descriptions[eqn.num], 
                width = NULL,
                height = '200px')
})

output$eqnCreate_eqnDescriptionFlow <- renderUI({
  req(eqns$n.eqns > 0)
  #n.eqns <- length(eqns$main)
  n.eqns <- eqns$n.eqns
  
  lapply(seq(n.eqns), function(i){
    textAreaInput(inputId = paste0("eqnDescriptionFlow_", i),
                  label = paste0(i,") Description of \"", eqns$main[i], "\""),
                  value = eqns$eqn.descriptions[i], 
                  width = NULL,
                  height = '200px')
  })
})

# observe({
#   n.eqns <- eqns$n.eqns
#   for (i in seq(n.eqns)) {
#       observeEvent(input[[paste0("eqnDescriptionFlow_", i)]], {
# 
#         # find description
#         text.to.store <- eval(parse(text = paste0("input$eqnDescription_", as.character(i))))
#         jPrint(text.to.store)
#         jPrint(paste0("input$eqnDescription_", as.character(i)))
#         
#         # store it to descriptions
#         eqns$eqn.descriptions[i] <- text.to.store
#     })
#   }
# })
# output$test_orderInputs <- renderUI({
#   orderInput("source1", "Vars", items =vars$species, as_source = TRUE, connect = "test_eqn")
#   orderInput("test_eqn", "Eqn", items = NULL, placeholder = "Drag Here")
# })


#--------------------------Random----------------------------------------------

