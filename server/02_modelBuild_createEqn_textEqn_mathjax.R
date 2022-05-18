Var2MathJ <- function(var = NULL){
  # Converts 
  # Args:
  #   var: variable to change to mathjax format converting subscripts properly
  #
  # Returns:
  #   var in latex readable form
  #
  # Ex: var = my_var -> var = my_{var} 
  

  latex.var = ""
  
  if (!is.null(var)) {
    split.var = strsplit(var, "")[[1]]
    has.underscore = FALSE

    for (i in seq(length(split.var))) {
      if (split.var[i] == "_" & !has.underscore) {
        has.underscore = TRUE
        latex.var = paste0(latex.var, split.var[i], "{")
      }else{
        latex.var = paste0(latex.var, split.var[i])
      }
    }
    if (has.underscore) {
      latex.var = paste0(latex.var, "}")
    }

  }
  
  return(latex.var)
}


equationBuilder_MathJax <- reactive({
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
      if (i == as.numeric(number_LHS_equations)) {eqn_LHS <- paste0(eqn_LHS, Var2MathJ(var))}
      else{eqn_LHS <- paste0(eqn_LHS, Var2MathJ(var), " + ")}
    }
    
    eqn_RHS <- ""
    for (i in seq(number_RHS_equations)) {
      coef <- eval(parse(text = paste0("input$RHS_Coeff_", as.character(i))))
      var <- eval(parse(text = paste0("input$RHS_Var_", as.character(i))))
      if (coef != "1") {eqn_RHS <- paste0(eqn_RHS, coef, "*")}
      if (i == as.numeric(number_RHS_equations)) {eqn_RHS <- paste0(eqn_RHS, Var2MathJ(var))}
      else{eqn_RHS <- paste0(eqn_RHS, Var2MathJ(var), " + ")}
    }
    
    if (input$eqn_chem_forward_or_both == "both_directions") {
      arrow <- "<->"
      if (input$eqn_options_chem_modifier_forward && input$eqn_options_chem_modifier_reverse) {
        #find regulators and add them together in form ([regulator/constant, regulator2/constant2, etc...])
        forwardModifiers <- c()
        for (i in seq(number_forward_regulators)) {
          regulator <- eval(parse(text = paste0("input$eqn_forward_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_forward_rateConstant_", as.character(i))))
          modifierExpression <- paste0("(",
                                       Var2MathJ(regulator),
                                       ":", 
                                       Var2MathJ(rateConstant),
                                       ")"
                                       )
          forwardModifiers <- c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ", ")
        
        reverseModifiers <- c()
        for (i in seq(number_reverse_regulators)) {
          regulator <- eval(parse(text = paste0("input$eqn_reverse_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_reverse_rateConstant_", as.character(i))))
          modifierExpression <- paste0("(",
                                       Var2MathJ(regulator),
                                       ":", 
                                       Var2MathJ(rateConstant),
                                       ")"
                                       )
          reverseModifiers <- c(reverseModifiers, modifierExpression)
        }
        reverseModifiers <- paste(reverseModifiers, collapse = ", ")
        
        arrow <- paste0(arrow, "[{",forwardModifiers ,"}]", "[{", reverseModifiers, "}]")
      }
      else if (input$eqn_options_chem_modifier_forward && !input$eqn_options_chem_modifier_reverse) {
        forwardModifiers <- c()
        for (i in seq(number_forward_regulators)) {
          regulator <- eval(parse(text = paste0("input$eqn_forward_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_forward_rateConstant_", as.character(i))))
          modifierExpression <- paste0("(",
                                       Var2MathJ(regulator), 
                                       ":",
                                       Var2MathJ(rateConstant),
                                       ")"
                                       )
          forwardModifiers <- c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ", ")
        
        arrow <- paste0(arrow, 
                        "[{",
                        forwardModifiers,
                        "}]",
                        "[{", 
                        Var2MathJ(input$eqn_chem_back_k),
                        "}]"
                        )
      }
      else if (!input$eqn_options_chem_modifier_forward && input$eqn_options_chem_modifier_reverse) {
        jPrint("Reverse Equation Build")
        reverseModifiers <- c()
        for (i in seq(number_reverse_regulators)) {
          regulator <- eval(parse(text = paste0("input$eqn_reverse_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_reverse_rateConstant_", as.character(i))))
          modifierExpression <- paste0("(",
                                       Var2MathJ(regulator),
                                       ":",
                                       Var2MathJ(rateConstant),
                                       ")"
                                       )
          reverseModifiers <- c(reverseModifiers, modifierExpression)
        }
        reverseModifiers <- paste(reverseModifiers, collapse = ",")
        arrow <- paste0( arrow, 
                         "[{", 
                         Var2MathJ(input$eqn_chem_forward_k),
                         "}]",
                         "[{", 
                         reverseModifiers, 
                         "}]"
                         )
      }
      else
      {
        arrow <- paste0(arrow, 
                        "[{", 
                        Var2MathJ(input$eqn_chem_forward_k), 
                        "}]", 
                        "[{", 
                        Var2MathJ(input$eqn_chem_back_k), 
                        "}]")
      }
    }
    else if (input$eqn_chem_forward_or_both == "forward_only") {
      arrow = "->"
      if (input$eqn_options_chem_modifier_forward) {
        forwardModifiers <- c()
        for (i in seq(number_forward_regulators)) {
          regulator <- eval(parse(text = paste0("input$eqn_forward_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_forward_rateConstant_", as.character(i))))
          modifierExpression <- paste0("(",
                                       Var2MathJ(regulator),
                                       ":",
                                       Var2MathJ(rateConstant),
                                       ")")
          forwardModifiers <- c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ",")
        arrow <- paste0(arrow,
                        "[{",
                        forwardModifiers,
                        "}]")
      }
      else
      {
        arrow <- paste0(arrow, 
                        "[{", 
                        Var2MathJ(input$eqn_chem_forward_k), 
                        "}]")
      }
    }
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
  }
  else if (input$eqnCreate_type_of_equation == "enzyme_rxn") {
    substrate <- Var2MathJ(input$eqn_enzyme_substrate)
    product   <- Var2MathJ(input$eqn_enzyme_product)
    arrow     <- "->"
    enzyme    <- Var2MathJ(input$eqn_enzyme_enzyme)
    Km        <- Var2MathJ(input$eqn_enzyme_Km)
    
    if (!input$eqn_options_enzyme_useVmax) {
      kcat    <- Var2MathJ(input$eqn_enzyme_kcat)
      textOut <- paste0(substrate,
                        " + ",
                        enzyme,
                        arrow,
                        "[{", Km ,"}]",
                        "[{", kcat, "}]",
                        product)
    }
    else if (input$eqn_options_enzyme_useVmax) {
      Vmax = input$eqn_enzyme_Vmax
      textOut <- paste0(substrate, 
                        arrow,
                        "[{", Vmax, "}]",
                        "[{", Km, "}]",
                        product
                        )
    }
  }
  else if (input$eqnCreate_type_of_equation == "syn") {
    
    if (input$eqn_syn_law == "rate") {
      arrow <- "->"
      var   <- Var2MathJ(input$eqn_syn_rate_var)
      rc    <- Var2MathJ(input$eqn_syn_rate_RC)
      type  <- "syn"
      textOut <- paste0(arrow,
                        "[{", rc, "}]",
                        "[{", type, "}]",
                        var
                        )
    } else if (input$eqn_syn_law == "byFactor") {
      arrow  <- "->"
      var    <- Var2MathJ(input$eqn_syn_sby_var)
      rc     <- Var2MathJ(input$eqn_syn_sby_RC)
      factor <- Var2MathJ(input$eqn_syn_sby_factor)
      type   <- "syn"
      textOut <- paste0(factor,
                        arrow,
                        "[{", rc, "}]",
                        "[{", type, "}]",
                        var
      )
    }
  } else if (input$eqnCreate_type_of_equation == "deg") {
    
    if (input$eqn_deg_law == "rate") {
      arrow <- "->"
      var   <- Var2MathJ(input$eqn_deg_var)
      rc    <- Var2MathJ(input$eqn_deg_rate_RC)
      type  <- "deg"
      textOut <- paste0(var,
                        arrow,
                        "[{", rc, "}]",
                        "[{", type, "}]",
                        "\\bigotimes"
      )
      
      
    } else if (input$eqn_deg_law == "byEnzyme") {
      arrow <- "->"
      var   <- Var2MathJ(input$eqn_deg_var)
      Km    <- Var2MathJ(input$eqn_deg_Km)
      type  <- "deg"
      
      if (input$eqn_deg_use_Vmax) {
        Vmax <- Var2MathJ(input$eqn_deg_Vmax)
        textOut <- paste0(var,
                          arrow,
                          "[{", Km, ",\\ ", Vmax, "}]",
                          "[{", type, "}]"
        )
      } else {
        enz  <- Var2MathJ(input$eqn_deg_enzyme)
        kcat <- Var2MathJ(input$eqn_deg_kcat)
        textOut <- paste0(var,
                          arrow,
                          "[{", Km, ",\\ ", kcat, ",\\ ", enz, "}]",
                          "[{", type, "}]"
        )
      }
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
  textOut <- paste0("$$\\ce{", textOut, "}$$")
  return(textOut)
})
