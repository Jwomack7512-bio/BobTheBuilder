

differentialEqnsMathjax <- reactive({
  # Displays the differential equations in mathjax form
  # Have multiple options
  #   @newline - each differential is displayed with a newline after each step
  
  # require equations or IO to be greater than one
  #req()
  
  if (input$diffeq_newline_diffeq) {
    separator <- " \\\\ "
    aligner   <- "&"
  } else {
    separator <- ""
    aligner   <- ""
  }
  
  beginning.align <- "\\begin{aligned} "
  diff.eqns <- vector("character", length = length(rv.DE$de.equations.list))
  # Cycle through de equations list.
  for (i in seq_along(rv.DE$de.equations.list)) {
    
    # Get compartment vol
    comp.vol <- rv.DE$de.equations.list[[i]]$Compartment.vol
    
    # create fraction for each (d[var1]/dt = )
    begin.fract <- paste0(Var2MathJ(comp.vol),
                          "*",
                          "\\frac{d[", 
                          rv.DE$de.equations.list[[i]]$Name,
                          "]}{dt} &= ")
    
    # Check if equations mathjax expressions have been created for this variable
    if (isTruthy(rv.DE$de.equations.list[[i]]$ODES.mathjax.vector)) {
      
      # Create align function
      current.diff <- "\\begin{aligned}[t] "
      # Cycle through all vectors, adding to function
      for (j in seq_along(rv.DE$de.equations.list[[i]]$ODES.mathjax.vector)) {
        mj.expression <- rv.DE$de.equations.list[[i]]$ODES.mathjax.vector[j]
        current.diff <- paste0(current.diff, 
                               aligner,
                               mj.expression,
                               " ")
        # Add the newline for all equations that aren't the last one
        if (j != length(rv.DE$de.equations.list[[i]]$ODES.mathjax.vector)) {
          current.diff <- paste0(current.diff, separator)
        }
      }
      
      current.diff <- paste0(current.diff, "\\end{aligned}")
    } else {
      current.diff <- "0"
    }
    
    # Combine fraction with diffeqn
    current.diff <- paste0(begin.fract, current.diff)
    print("Mathjax test differentials")
    print(current.diff)
    diff.eqns[i] <- current.diff
  }
  
  print("Diff.eqns")
  print(diff.eqns)
  
  out <- paste0(diff.eqns, collapse = " \\\\\\\\ ")
  # out <- paste0("$$", out, "$$")
  out <- paste0("$$\\begin{aligned} ", out, "\\end{aligned}$$")
  print(out)
  

  
  
  # 
  # # Store each individual in a vector
  # 
  # # Collapse vector with mathjax newline (//)
  # for (i in seq_along(rv.DE$de.equations.list)) {
  #   print("DE TESTS ITER")
  #   print(rv.DE$de.equations.list[[i]]$ODES.mathjax.vector)
  #   if (isTruthy(rv.DE$de.equations.list[[i]]$ODES.mathjax.vector)) {
  #     textOut <- paste0(textOut, 
  #                    rv.DE$de.equations.list[[i]]$ODES.mathjax.vector, 
  #                    "\n")
  #   }
  # }
  # textOut <- paste0("$$", textOut, "$$")
  # print("Mathjax Test")
  # print(textOut)
  return(out)
})

equationMathJaxBuilder <- reactive({
  
  if (input$eqnCreate_reaction_law == "mass_action") {
    number.reactants <- as.numeric(input$NI_mass_action_num_reactants)
    number.products  <- as.numeric(input$NI_mass_action_num_products)

    eqn_LHS <- ""
    for (i in seq(number.reactants)) {
      coef <- eval(parse(text = paste0("input$NI_MA_r_stoichiometry_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MA_reactant_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_LHS <- paste0(eqn_LHS, coef, "*")
        }
      } else {
        eqn_LHS <- ""
      }

      if (i == as.numeric(number.reactants)) {
        eqn_LHS <- paste0(eqn_LHS, Var2MathJ(var))
      } else {
        eqn_LHS <- paste0(eqn_LHS, Var2MathJ(var), " + ")
      }
    }
    
    eqn_RHS <- ""
    for (i in seq(number.products)) {
      coef <- eval(parse(text = paste0("input$NI_MA_p_stoichiometry_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MA_product_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_RHS <- paste0(eqn_RHS, coef, "*")
        }
      } else {
        eqn_RHS <- ""
      }
      
      if (i == as.numeric(number.products)) {
        eqn_RHS <- paste0(eqn_RHS, Var2MathJ(var))
      }
      else{
        eqn_RHS <- paste0(eqn_RHS, Var2MathJ(var), " + ")
      }
    }
    
    if (input$PI_mass_action_reverisble_option == "both_directions") {
      arrow <- "<->"

      arrow <- paste0("\\ce{",
                      arrow, 
                      "[{", 
                      Var2MathJ(input$TI_mass_action_forward_k), 
                      "}]", 
                      "[{", 
                      Var2MathJ(input$TI_mass_action_reverse_k), 
                      "}]",
                      "}")
    }
    else if (input$PI_mass_action_reverisble_option == "forward_only") {
      arrow = "->"

      arrow <- paste0("\\ce{",
                      arrow, 
                      "[{", 
                      Var2MathJ(input$TI_mass_action_forward_k), 
                      "}]",
                      "}")
    }
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
  }
  else if (input$eqnCreate_reaction_law == "mass_action_w_reg") {
    arrow <- "->"
    
    number.reactants <- as.numeric(input$NI_mass_action_wReg_num_reactants)
    number.products  <- as.numeric(input$NI_mass_action_wReg_num_products)
    
    has.f.reg <- input$CB_MAwR_chem_modifier_forward
    has.r.reg <- input$CB_MAwR_chem_modifier_reverse
    
    number_forward_regulators = as.numeric(input$NI_MAwR_n_forward_regulators)
    number_reverse_regulators = as.numeric(input$NI_MAwR_n_reverse_regulators)
    
    reversible <- input$PI_mass_action_reverisble_option
    
    # Build Reactant Equation Side
    eqn_LHS <- ""
    for (i in seq(number.reactants)) {
      coef <- eval(parse(text = paste0("input$NI_MAwR_r_stoichiometry_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MAwR_reactant_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_LHS <- paste0(eqn_LHS, coef, "*")
        }
      } else {
        eqn_LHS <- ""
      }
      
      if (i == as.numeric(number.reactants)) {
        eqn_LHS <- paste0(eqn_LHS, Var2MathJ(var))
      } else {
        eqn_LHS <- paste0(eqn_LHS, Var2MathJ(var), " + ")
      }
    }
    
    # Build Product Equation Side
    eqn_RHS <- ""
    for (i in seq(number.products)) {
      coef <- eval(parse(text = paste0("input$NI_MAwR_p_stoichiometry_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MAwR_product_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_RHS <- paste0(eqn_RHS, coef, "*")
        }
      } else {
        eqn_RHS <- ""
      }
      
      if (i == as.numeric(number.products)) {
        eqn_RHS <- paste0(eqn_RHS, Var2MathJ(var))
      }
      else{
        eqn_RHS <- paste0(eqn_RHS, Var2MathJ(var), " + ")
      }
    }
    
    # Check For Forward Regulators
  
    if (has.f.reg) {
      #find regulators and add them together in form ([regulator/constant, 
      #regulator2/constant2, etc...])
      forwardModifiers <- c()
      for (i in seq(number_forward_regulators)) {
        regulator <-
          eval(parse(text = paste0(
            "input$PI_MAwR_forward_regulator_", as.character(i)
          )))
        rateConstant <-
          eval(parse(text = paste0(
            "input$TI_MAwR_forward_regulator_RC_", as.character(i)
          )))
        modifierExpression <- paste0("(",
                                     Var2MathJ(regulator),
                                     ":",
                                     Var2MathJ(rateConstant),
                                     ")")
        forwardModifiers <-
          c(forwardModifiers, modifierExpression)
      }
      forwardModifiers <- paste(forwardModifiers, collapse = ", ")
    } 
    else {
      # If no forward regulators, use kf
      forwardModifiers <- Var2MathJ(input$TI_MAwR_forward_k)
    }
    forwardModifiers <- paste0("[{",
                               forwardModifiers,
                               "}]")
    # Check If Reaction Is Reversible
    if (reversible == "both_directions") {
      arrow <- "<->"
      # Check if Reverse Regulator is used
      if (has.r.reg) {
        reverseModifiers <- c()
        for (i in seq(number_reverse_regulators)) {
          regulator <-
            eval(parse(text = paste0(
              "input$PI_MAwR_reverse_regulator_", as.character(i)
            )))
          rateConstant <-
            eval(parse(text = paste0(
              "input$TI_MAwR_reverse_regulator_RC_", as.character(i)
            )))
          modifierExpression <- paste0("(",
                                       Var2MathJ(regulator),
                                       ":",
                                       Var2MathJ(rateConstant),
                                       ")")
          reverseModifiers <-
            c(reverseModifiers, modifierExpression)
        }
        reverseModifiers <- paste(reverseModifiers, collapse = ", ")
      }
      else {
        # If no regulators, use kr
        reverseModifiers <- Var2MathJ(input$TI_MAwR_reverse_k)
      }
      reverseModifiers <- paste0("[{", 
                                 reverseModifiers, 
                                 "}]")
    } 
    else {
      reverseModifiers <- ""
    }
    
    arrow <- paste0("\\ce{",
                    arrow,
                    forwardModifiers,
                    reverseModifiers,
                    "}")

    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
    
  }
  else if (input$eqnCreate_reaction_law == "synthesis") {
    
    if (input$CB_synthesis_factor_checkbox) {
      arrow  <- "->"
      var    <- Var2MathJ(input$PI_synthesis_byFactor_var)
      rc     <- Var2MathJ(input$TI_synthesis_byFactor_RC)
      factor <- Var2MathJ(input$PI_synthesis_byFactor_factor)
      type   <- "syn"
      textOut <- paste0(factor,
                        "\\ce{",
                        arrow,
                        "[{", rc, "}]",
                        "[{", type, "}]",
                        "}",
                        var
      )
    } else {
      arrow <- "->"
      var   <- Var2MathJ(input$PI_synthesis_rate_var)
      rc    <- Var2MathJ(input$TI_synthesis_rate_RC)
      type  <- "syn"
      textOut <- paste0("\\ce{",
                        arrow,
                        "[{", rc, "}]",
                        "[{", type, "}]",
                        "}",
                        var
      )
    }
    
    # if (input$eqn_syn_law == "rate") {
    #   arrow <- "->"
    #   var   <- Var2MathJ(input$eqn_syn_rate_var)
    #   rc    <- Var2MathJ(input$eqn_syn_rate_RC)
    #   type  <- "syn"
    #   textOut <- paste0("\\ce{",
    #                     arrow,
    #                     "[{", rc, "}]",
    #                     "[{", type, "}]",
    #                     "}",
    #                     var
    #                     )
    # } else if (input$eqn_syn_law == "byFactor") {
    #   arrow  <- "->"
    #   var    <- Var2MathJ(input$eqn_syn_sby_var)
    #   rc     <- Var2MathJ(input$eqn_syn_sby_RC)
    #   factor <- Var2MathJ(input$eqn_syn_sby_factor)
    #   type   <- "syn"
    #   textOut <- paste0(factor,
    #                     "\\ce{",
    #                     arrow,
    #                     "[{", rc, "}]",
    #                     "[{", type, "}]",
    #                     "}",
    #                     var
    #   )
    # }
  } 
  else if (input$eqnCreate_reaction_law == "degradation_rate") {
    # Get products if they exist
    if (input$CB_degradation_rate_toProducts) {
      num.deg.products <- as.numeric(input$NI_degradation_rate_num_products)
      product <- ""
      for (i in seq(num.deg.products)) {
        prod <- eval(parse(text = paste0("input$PI_degradation_rate_product_", 
                                         as.character(i))))
        if (i == num.deg.products) {
          product <- paste0(product, Var2MathJ(prod))
        } else {
          product <- paste0(product, Var2MathJ(prod), " + ")
        }
      }
    } else {
      product <- "\\bigotimes"
    }
    
    # Build Equations
    arrow <- "->"
    var   <- Var2MathJ(input$PI_degradation_rate_species)
    rc    <- Var2MathJ(input$TI_degradation_rate_RC)
    type  <- "deg"
    textOut <- paste0(var,
                      "\\ce{",
                      arrow,
                      "[{", rc, "}]",
                      "[{", type, "}]",
                      "}",
                      product
    )
  }
  else if (input$eqnCreate_reaction_law == "degradation_by_enzyme") {
    # Get products if they exist
    if (input$CB_degradation_enzyme_toProducts) {
      num.deg.products <- as.numeric(input$NI_degradation_enzyme_num_products)
      product <- ""
      for (i in seq(num.deg.products)) {
        prod <- eval(parse(text = paste0("input$PI_degradation_enzyme_product_", 
                                         as.character(i))))
        if (i == num.deg.products) {
          product <- paste0(product, Var2MathJ(prod))
        } else {
          product <- paste0(product, Var2MathJ(prod), " + ")
        }
      }
    } else {
      product <- "\\bigotimes"
    }
    
    # Build Equations
    arrow <- "->"
    var   <- Var2MathJ(input$PI_degradation_enzyme_species)
    Km    <- Var2MathJ(input$TI_degradation_enzyme_Km)
    type  <- "deg"
    
    if (input$CB_degradation_enzyme_useVmax) {
      Vmax <- Var2MathJ(input$TI_degradation_enzyme_Vmax)
      textOut <- paste0(var,
                        "\\ce{",
                        arrow,
                        "[{", Km, ",\\ ", Vmax, "}]",
                        "[{", type, "}]",
                        "}",
                        product
      )
    } else {
      enz  <- Var2MathJ(input$PI_degradation_enzyme_enzyme)
      kcat <- Var2MathJ(input$TI_degradation_enzyme_kcat)
      textOut <- paste0(var,
                        "\\ce{",
                        arrow,
                        "[{", Km, ",\\ ", kcat, ",\\ ", enz, "}]",
                        "[{", type, "}]",
                        "}",
                        product
      )
    }
  }
  else if (input$eqnCreate_reaction_law == "michaelis_menten") {
    substrate <- Var2MathJ(input$PI_michaelis_menten_substrate)
    product   <- Var2MathJ(input$PI_michaelis_menten_product)
    arrow     <- "->"
    enzyme    <- Var2MathJ(input$PI_michaelis_menten_enzyme)
    Km        <- Var2MathJ(input$TI_michaelis_menten_Km)
    
    if (!input$CB_michaelis_menten_useVmax) {
      kcat    <- Var2MathJ(input$TI_michaelis_menten_kcat)
      textOut <- paste0(substrate,
                        " + ",
                        enzyme,
                        "\\ce{",
                        arrow,
                        "[{", Km ,"}]",
                        "[{", kcat, "}]",
                        "}",
                        product)
    }
    else if (input$CB_michaelis_menten_useVmax) {
      Vmax = Var2MathJ(input$TI_michaelis_menten_vmax)
      textOut <- paste0(substrate, 
                        "\\ce{",
                        arrow,
                        "[{", Vmax, "}]",
                        "[{", Km, "}]",
                        "}",
                        product
      )
    }
  }
  else if (input$eqnCreate_type_of_equation == "rate_eqn") {
    rate_left <- input$eqnCreate_custom_eqn_lhs
    rate_right <- input$eqnCreate_custom_eqn_rhs
    textOut <- paste0(rate_left, " = ", rate_right)
  }
  else if (input$eqnCreate_type_of_equation == "time_dependent") {
    TD_left <- input$eqnCreate_time_dependent_firstvar
    TD_right <- input$eqnCreate_time_dependent_equation
    textOut <- paste0(TD_left, "=", TD_right)
  }
  else{textOut <- "ERROR"}
  textOut <- paste0("$$", textOut, "$$")
  return(textOut)
})

equationBuilder_edit_mathJax <- reactive({
  if (input$eqnCreate_type_of_equation_edit == "chem_rxn") {
    number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS_edit)
    number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS_edit)
    number_forward_regulators = 
      as.numeric(input$eqn_options_chem_num_forward_regulators_edit)
    number_reverse_regulators = 
      as.numeric(input$eqn_options_chem_num_reverse_regulators_edit)
    
    eqn_LHS <- ""
    for (i in seq(number_LHS_equations)) {
      coef <-
        eval(parse(text = paste0(
          "input$LHS_Coeff_edit", as.character(i)
        )))
      var <-
        eval(parse(text = paste0(
          "input$LHS_Var_edit", as.character(i)
        )))
      if (coef != "1") {
        eqn_LHS <- paste0(eqn_LHS, coef, "*")
      }
      if (i == as.numeric(number_LHS_equations)) {
        eqn_LHS <- paste0(eqn_LHS, Var2MathJ(var))
      }
      else{
        eqn_LHS <- paste0(eqn_LHS, Var2MathJ(var), " + ")
      }
    }
    
    eqn_RHS <- ""
    for (i in seq(number_RHS_equations)) {
      coef <-
        eval(parse(text = paste0(
          "input$RHS_Coeff_edit", as.character(i)
        )))
      var <-
        eval(parse(text = paste0(
          "input$RHS_Var_edit", as.character(i)
        )))
      if (coef != "1") {
        eqn_RHS <- paste0(eqn_RHS, coef, "*")
      }
      if (i == as.numeric(number_RHS_equations)) {
        eqn_RHS <- paste0(eqn_RHS, Var2MathJ(var))
      }
      else{
        eqn_RHS <- paste0(eqn_RHS, Var2MathJ(var), " + ")
      }
    }
    
    if (input$eqn_chem_forward_or_both_edit == "both_directions") {
      arrow <- "<->"
      if (input$eqn_options_chem_modifier_forward_edit && 
          input$eqn_options_chem_modifier_reverse_edit) {
        #find regulators and add them together in form ([regulator/constant, regulator2/constant2, etc...])
        forwardModifiers <- c()
        for (i in seq(number_forward_regulators)) {
          regulator <-
            eval(parse(text = paste0(
              "input$eqn_forward_regulator_edit", as.character(i)
            )))
          rateConstant <-
            eval(parse(
              text = paste0("input$eqn_forward_rateConstant_edit", as.character(i))
            ))
          modifierExpression <- paste0("(",
                                       Var2MathJ(regulator),
                                       ":",
                                       Var2MathJ(rateConstant),
                                       ")")
          forwardModifiers <-
            c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ", ")
        
        reverseModifiers <- c()
        for (i in seq(number_reverse_regulators)) {
          regulator <-
            eval(parse(text = paste0(
              "input$eqn_reverse_regulator_edit", as.character(i)
            )))
          rateConstant <-
            eval(parse(
              text = paste0("input$eqn_reverse_rateConstant_edit", as.character(i))
            ))
          modifierExpression <- paste0("(",
                                       Var2MathJ(regulator),
                                       ":",
                                       Var2MathJ(rateConstant),
                                       ")")
          reverseModifiers <-
            c(reverseModifiers, modifierExpression)
        }
        reverseModifiers <- paste(reverseModifiers, collapse = ", ")
        
        arrow <- paste0("\\ce{",
                        arrow, 
                        "[{",
                        forwardModifiers,
                        "}]", 
                        "[{", 
                        reverseModifiers, 
                        "}]",
                        "}")
      }
      else if (input$eqn_options_chem_modifier_forward_edit 
               && !input$eqn_options_chem_modifier_reverse_edit) {
        forwardModifiers <- c()
        for (i in seq(number_forward_regulators)) {
          regulator <-
            eval(parse(text = paste0(
              "input$eqn_forward_regulator_edit", as.character(i)
            )))
          rateConstant <-
            eval(parse(
              text = paste0("input$eqn_forward_rateConstant_edit", as.character(i))
            ))
          modifierExpression <- paste0("(",
                                       Var2MathJ(regulator),
                                       ":",
                                       Var2MathJ(rateConstant),
                                       ")")
          forwardModifiers <-
            c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ", ")
        
        arrow <- paste0(
          "\\ce{",
          arrow,
          "[{",
          forwardModifiers,
          "}]",
          "[{",
          Var2MathJ(input$eqn_chem_back_k_edit),
          "}]",
          "}"
        )
      }
      else if (!input$eqn_options_chem_modifier_forward_edit &&
               input$eqn_options_chem_modifier_reverse_edit) {
        reverseModifiers <- c()
        for (i in seq(number_reverse_regulators)) {
          regulator <-
            eval(parse(
              text = paste0("input$eqn_reverse_regulator_edit", as.character(i))
            ))
          rateConstant <-
            eval(parse(
              text = paste0("input$eqn_reverse_rateConstant_edit", as.character(i))
            ))
          modifierExpression <- paste0("(",
                                       Var2MathJ(regulator),
                                       ":",
                                       Var2MathJ(rateConstant),
                                       ")")
          reverseModifiers <-
            c(reverseModifiers, modifierExpression)
        }
        reverseModifiers <- paste(reverseModifiers, collapse = ",")
        arrow <- paste0( "\\ce{",
                         arrow, 
                         "[{", 
                         Var2MathJ(input$eqn_chem_forward_k_edit),
                         "}]",
                         "[{", 
                         reverseModifiers, 
                         "}]",
                         "}"
        )
      } else {
        arrow <- paste0("\\ce{",
                        arrow, 
                        "[{", 
                        Var2MathJ(input$eqn_chem_forward_k_edit), 
                        "}]", 
                        "[{", 
                        Var2MathJ(input$eqn_chem_back_k_edit), 
                        "}]",
                        "}")
      }
    }
    else if (input$eqn_chem_forward_or_both_edit == "forward_only") {
      arrow = "->"
      if (input$eqn_options_chem_modifier_forward_edit) {
        forwardModifiers <- c()
        for (i in seq(number_forward_regulators)) {
          regulator <-
            eval(parse(text = paste0(
              "input$eqn_forward_regulator_edit", as.character(i)
            )))
          rateConstant <-
            eval(parse(
              text = paste0("input$eqn_forward_rateConstant_edit", as.character(i))
            ))
          modifierExpression <- paste0("(",
                                       Var2MathJ(regulator),
                                       ":",
                                       Var2MathJ(rateConstant),
                                       ")")
          forwardModifiers <-
            c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ",")
        arrow <- paste0("\\ce{",
                        arrow,
                        "[{",
                        forwardModifiers,
                        "}]",
                        "}")
      }
      else
      {
        arrow <- paste0("\\ce{",
                        arrow, 
                        "[{", 
                        Var2MathJ(input$eqn_chem_forward_k_edit), 
                        "}]",
                        "}")
      }
    }
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
  }
  else if (input$eqnCreate_type_of_equation_edit == "enzyme_rxn") {
    substrate <- Var2MathJ(input$eqn_enzyme_substrate_edit)
    product   <- Var2MathJ(input$eqn_enzyme_product_edit)
    arrow     <- "->"
    enzyme    <- Var2MathJ(input$eqn_enzyme_enzyme_edit)
    Km        <- Var2MathJ(input$eqn_enzyme_Km_edit)
    
    if (!input$eqn_options_enzyme_useVmax_edit) {
      kcat    <- Var2MathJ(input$eqn_enzyme_kcat_edit)
      textOut <- paste0(substrate,
                        " + ",
                        enzyme,
                        "\\ce{",
                        arrow,
                        "[{", Km ,"}]",
                        "[{", kcat, "}]",
                        "}",
                        product)
    }
    else if (input$eqn_options_enzyme_useVmax_edit) {
      Vmax = input$eqn_enzyme_Vmax_edit
      textOut <- paste0(substrate, 
                        "\\ce{",
                        arrow,
                        "[{", Vmax, "}]",
                        "[{", Km, "}]",
                        "}",
                        product
      )
    }
  }
  else if (input$eqnCreate_type_of_equation_edit == "syn") {
    
    if (input$eqn_syn_law_edit == "rate") {
      arrow <- "->"
      var   <- Var2MathJ(input$eqn_syn_rate_var_edit)
      rc    <- Var2MathJ(input$eqn_syn_rate_RC_edit)
      type  <- "syn"
      textOut <- paste0("\\ce{",
                        arrow,
                        "[{", rc, "}]",
                        "[{", type, "}]",
                        "}",
                        var
      )
    } else if (input$eqn_syn_law_edit == "byFactor") {
      arrow  <- "->"
      var    <- Var2MathJ(input$eqn_syn_sby_var_edit)
      rc     <- Var2MathJ(input$eqn_syn_sby_RC_edit)
      factor <- Var2MathJ(input$eqn_syn_sby_factor_edit)
      type   <- "syn"
      textOut <- paste0(factor,
                        "\\ce{",
                        arrow,
                        "[{", rc, "}]",
                        "[{", type, "}]",
                        "}",
                        var
      )
    }
  } 
  else if (input$eqnCreate_type_of_equation_edit == "deg") {
    # Get products if they exist
    if (input$eqn_deg_to_products_edit) {
      num.deg.products <- as.numeric(input$eqn_deg_num_products_edit)
      product <- ""
      for (i in seq(num.deg.products)) {
        prod <- eval(parse(text = paste0("input$eqn_deg_product_edit", as.character(i))))
        if (i == num.deg.products) {
          product <- paste0(product, Var2MathJ(prod))
        } else {
          product <- paste0(product, Var2MathJ(prod), " + ")
        }
      }
    } else {
      product <- "\\bigotimes"
    }
    
    # Build Equations
    if (input$eqn_deg_law_edit == "rate") {
      arrow <- "->"
      var   <- Var2MathJ(input$eqn_deg_var_edit)
      rc    <- Var2MathJ(input$eqn_deg_rate_RC_edit)
      type  <- "deg"
      textOut <- paste0(var,
                        "\\ce{",
                        arrow,
                        "[{", rc, "}]",
                        "[{", type, "}]",
                        "}",
                        product
      )
      
      
    } else if (input$eqn_deg_law_edit == "byEnzyme") {
      arrow <- "->"
      var   <- Var2MathJ(input$eqn_deg_var_edit)
      Km    <- Var2MathJ(input$eqn_deg_Km_edit)
      type  <- "deg"
      
      if (input$eqn_deg_use_Vmax_edit) {
        Vmax <- Var2MathJ(input$eqn_deg_Vmax_edit)
        textOut <- paste0(var,
                          "\\ce{",
                          arrow,
                          "[{", Km, ",\\ ", Vmax, "}]",
                          "[{", type, "}]",
                          "}",
                          product
        )
      } else {
        enz  <- Var2MathJ(input$eqn_deg_enzyme_edit)
        kcat <- Var2MathJ(input$eqn_deg_kcat_edit)
        textOut <- paste0(var,
                          "\\ce{",
                          arrow,
                          "[{", Km, ",\\ ", kcat, ",\\ ", enz, "}]",
                          "[{", type, "}]",
                          "}",
                          product
        )
      }
    }
  }
  # else if (input$eqnCreate_type_of_equation_edit == "rate_eqn")
  # {
  #   rate_left <- input$eqnCreate_rate_firstvar_edit
  #   rate_right <- input$eqnCreate_rate_equation_edit
  #   textOut <- paste0(rate_left, " = ", rate_right)
  # }
  # else if (input$eqnCreate_type_of_equation_edit == "time_dependent")
  # {
  #   TD_left <- input$eqnCreate_time_dependent_firstvar_edit
  #   TD_right <- input$eqnCreate_time_dependent_equation_edit
  #   textOut <- paste0(TD_left, "=", TD_right)
  # }
  else{textOut <- "ERROR"}
  textOut <- paste0("$$", textOut, "$$")
  return(textOut)
  
})

equationLatexBuilder <- reactive({
  
  if (input$eqnCreate_reaction_law == "mass_action") {
    number.reactants <- as.numeric(input$NI_mass_action_num_reactants)
    number.products  <- as.numeric(input$NI_mass_action_num_products)
    
    eqn_LHS <- ""
    for (i in seq(number.reactants)) {
      coef <- eval(parse(text = paste0("input$NI_MA_r_stoichiometry_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MA_reactant_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_LHS <- paste0(eqn_LHS, coef, "*")
        }
      } else {
        eqn_LHS <- ""
      }
      
      if (i == as.numeric(number.reactants)) {
        eqn_LHS <- paste0(eqn_LHS, Var2Latex(var))
      } else {
        eqn_LHS <- paste0(eqn_LHS, Var2Latex(var), " + ")
      }
    }
    
    eqn_RHS <- ""
    for (i in seq(number.products)) {
      coef <- eval(parse(text = paste0("input$NI_MA_p_stoichiometry_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MA_product_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_RHS <- paste0(eqn_RHS, coef, "*")
        }
      } else {
        eqn_RHS <- ""
      }
      
      if (i == as.numeric(number.products)) {
        eqn_RHS <- paste0(eqn_RHS, Var2Latex(var))
      }
      else{
        eqn_RHS <- paste0(eqn_RHS, Var2Latex(var), " + ")
      }
    }
    
    if (input$PI_mass_action_reverisble_option == "both_directions") {
      arrow <- "\\xrightleftharpoons"
      
      arrow <- paste0(arrow, 
                      "[", 
                      Var2Latex(input$TI_mass_action_forward_k), 
                      "]", 
                      "{", 
                      Var2Latex(input$TI_mass_action_reverse_k), 
                      "}")
    }
    else if (input$PI_mass_action_reverisble_option == "forward_only") {
      arrow <- "\\xrightarrow"
      
      arrow <- paste0(arrow, 
                      "[", 
                      Var2Latex(input$TI_mass_action_forward_k), 
                      "]")
    }
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
  
  }
  else if (input$eqnCreate_reaction_law == "mass_action_w_reg") {
    arrow <- "\\xrightarrow"
    
    number.reactants <- as.numeric(input$NI_mass_action_wReg_num_reactants)
    number.products  <- as.numeric(input$NI_mass_action_wReg_num_products)
    
    has.f.reg <- input$CB_MAwR_chem_modifier_forward
    has.r.reg <- input$CB_MAwR_chem_modifier_reverse
    
    number_forward_regulators = as.numeric(input$NI_MAwR_n_forward_regulators)
    number_reverse_regulators = as.numeric(input$NI_MAwR_n_reverse_regulators)
    
    reversible <- input$PI_mass_action_reverisble_option
    
    # Build Reactant Equation Side
    eqn_LHS <- ""
    for (i in seq(number.reactants)) {
      coef <- eval(parse(text = paste0("input$NI_MAwR_r_stoichiometry_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MAwR_reactant_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_LHS <- paste0(eqn_LHS, coef, "*")
        }
      } else {
        eqn_LHS <- ""
      }
      
      if (i == as.numeric(number.reactants)) {
        eqn_LHS <- paste0(eqn_LHS, Var2Latex(var))
      } else {
        eqn_LHS <- paste0(eqn_LHS, Var2Latex(var), " + ")
      }
    }
    
    # Build Product Equation Side
    eqn_RHS <- ""
    for (i in seq(number.products)) {
      coef <- eval(parse(text = paste0("input$NI_MAwR_p_stoichiometry_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MAwR_product_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_RHS <- paste0(eqn_RHS, coef, "*")
        }
      } else {
        eqn_RHS <- ""
      }
      
      if (i == as.numeric(number.products)) {
        eqn_RHS <- paste0(eqn_RHS, Var2Latex(var))
      }
      else{
        eqn_RHS <- paste0(eqn_RHS, Var2Latex(var), " + ")
      }
    }
    
    # Check For Forward Regulators
    
    if (has.f.reg) {
      #find regulators and add them together in form ([regulator/constant, 
      #regulator2/constant2, etc...])
      forwardModifiers <- c()
      for (i in seq(number_forward_regulators)) {
        regulator <-
          eval(parse(text = paste0(
            "input$PI_MAwR_forward_regulator_", as.character(i)
          )))
        rateConstant <-
          eval(parse(text = paste0(
            "input$TI_MAwR_forward_regulator_RC_", as.character(i)
          )))
        modifierExpression <- paste0("(",
                                     Var2Latex(regulator),
                                     ":",
                                     Var2Latex(rateConstant),
                                     ")")
        forwardModifiers <-
          c(forwardModifiers, modifierExpression)
      }
      forwardModifiers <- paste(forwardModifiers, collapse = ", ")
    } 
    else {
      # If no forward regulators, use kf
      forwardModifiers <- Var2Latex(input$TI_MAwR_forward_k)
    }
    forwardModifiers <- paste0("[",
                               forwardModifiers,
                               "]")
    # Check If Reaction Is Reversible
    if (reversible == "both_directions") {
      arrow <- "\\xrightleftharpoons"
      # Check if Reverse Regulator is used
      if (has.r.reg) {
        reverseModifiers <- c()
        for (i in seq(number_reverse_regulators)) {
          regulator <-
            eval(parse(text = paste0(
              "input$PI_MAwR_reverse_regulator_", as.character(i)
            )))
          rateConstant <-
            eval(parse(text = paste0(
              "input$TI_MAwR_reverse_regulator_RC_", as.character(i)
            )))
          modifierExpression <- paste0("(",
                                       Var2Latex(regulator),
                                       ":",
                                       Var2Latex(rateConstant),
                                       ")")
          reverseModifiers <-
            c(reverseModifiers, modifierExpression)
        }
        reverseModifiers <- paste(reverseModifiers, collapse = ", ")
      }
      else {
        # If no regulators, use kr
        reverseModifiers <- Var2Latex(input$TI_MAwR_reverse_k)
      }
      reverseModifiers <- paste0("{", 
                                 reverseModifiers, 
                                 "}")
    } 
    else {
      reverseModifiers <- ""
    }
    
    arrow <- paste0(arrow,
                    forwardModifiers,
                    reverseModifiers
                    )
    
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
    
  }
  else if (input$eqnCreate_reaction_law == "synthesis") {
    if (input$CB_synthesis_factor_checkbox) {
      arrow  <- "\\xrightarrow"
      var    <- Var2Latex(input$PI_synthesis_byFactor_var)
      rc     <- Var2Latex(input$TI_synthesis_byFactor_RC)
      factor <- Var2Latex(input$PI_synthesis_byFactor_factor)
      type   <- "syn"
      textOut <- paste0(factor,
                        arrow,
                        "[", rc, "]",
                        "{", type, "}",
                        var
      )
    } else {
      arrow  <- "\\xrightarrow"
      var   <- Var2Latex(input$PI_synthesis_rate_var)
      rc    <- Var2Latex(input$TI_synthesis_rate_RC)
      type  <- "syn"
      textOut <- paste0(arrow,
                        "[", rc, "]",
                        "{", type, "}",
                        var
      )
    }
  } 
  else if (input$eqnCreate_reaction_law == "degradation_rate") {
    # Get products if they exist
    if (input$CB_degradation_rate_toProducts) {
      num.deg.products <- as.numeric(input$NI_degradation_rate_num_products)
      product <- ""
      for (i in seq(num.deg.products)) {
        prod <- eval(parse(text = paste0("input$PI_degradation_rate_product_", 
                                         as.character(i))))
        if (i == num.deg.products) {
          product <- paste0(product, Var2Latex(prod))
        } else {
          product <- paste0(product, Var2Latex(prod), " + ")
        }
      }
    } else {
      product <- "\\bigotimes"
    }
    
    # Build Equations
    arrow  <- "\\xrightarrow"
    var   <- Var2Latex(input$PI_degradation_rate_species)
    rc    <- Var2Latex(input$TI_degradation_rate_RC)
    type  <- "deg"
    textOut <- paste0(var,
                      arrow,
                      "[", rc, "]",
                      "{", type, "}",
                      product
    )
  } 
  else if (input$eqnCreate_reaction_law == "degradation_by_enzyme") {
    # Get products if they exist
    if (input$CB_degradation_enzyme_toProducts) {
      num.deg.products <- as.numeric(input$NI_degradation_enzyme_num_products)
      product <- ""
      for (i in seq(num.deg.products)) {
        prod <- eval(parse(text = paste0("input$PI_degradation_enzyme_product_", 
                                         as.character(i))))
        if (i == num.deg.products) {
          product <- paste0(product, Var2Latex(prod))
        } else {
          product <- paste0(product, Var2Latex(prod), " + ")
        }
      }
    } else {
      product <- "\\bigotimes"
    }
    
    # Build Equations
    arrow  <- "\\xrightarrow"
    var   <- Var2Latex(input$PI_degradation_enzyme_species)
    Km    <- Var2Latex(input$TI_degradation_enzyme_Km)
    type  <- "deg"
    
    if (input$CB_degradation_enzyme_useVmax) {
      Vmax <- Var2Latex(input$TI_degradation_enzyme_Vmax)
      textOut <- paste0(var,
                        arrow,
                        "[", Km, ", ", Vmax, "]",
                        "{", type, "} ",
                        product
      )
    } else {
      enz  <- Var2Latex(input$PI_degradation_enzyme_enzyme)
      kcat <- Var2Latex(input$TI_degradation_enzyme_kcat)
      textOut <- paste0(var,
                        arrow,
                        "[", Km, ", ", kcat, ", ", enz, "]",
                        "{", type, "}",
                        product
      )
    }
  } 
  else if (input$eqnCreate_reaction_law == "michaelis_menten") {
    substrate <- Var2Latex(input$PI_michaelis_menten_substrate)
    product   <- Var2Latex(input$PI_michaelis_menten_product)
    arrow     <- "\\xrightarrow"
    enzyme    <- Var2Latex(input$PI_michaelis_menten_enzyme)
    Km        <- Var2Latex(input$TI_michaelis_menten_Km)
    
    if (!input$CB_michaelis_menten_useVmax) {
      kcat    <- Var2Latex(input$TI_michaelis_menten_kcat)
      textOut <- paste0(substrate,
                        " + ",
                        enzyme, " ",
                        arrow,
                        "[", Km ,"]",
                        "{", kcat, "} ",
                        product)
    }
    else if (input$CB_michaelis_menten_useVmax) {
      Vmax <- Var2Latex(input$TI_michaelis_menten_vmax)
      textOut <- paste0(substrate, 
                        arrow,
                        "[", Vmax, "]",
                        "{", Km, "}",
                        product
      )
    }
  }
  else {
    textOut <- "ERROR"
  }
 
  return(textOut)
})



