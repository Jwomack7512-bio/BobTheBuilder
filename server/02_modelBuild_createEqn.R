CheckParametersForErrors <- function(paramsToCheck, allParamVariables, allSpecies) {
  # takes input of all parameters inputs for chem, enyzme, etc..only some will be active
  passed.test = TRUE #set true by default and change if error found
  for (var in paramsToCheck) {
    varCheck <- variableCheck(var, allSpecies, allParamVariables)
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
    
    #add unique id
    ids <- GenerateId(id$id.var.seed, "parameter")
    unique.id <- ids[[2]]
    id$id.var.seed <- ids[[1]]
    idx.to.add <- nrow(id$id.parameters) + 1
    id$id.parameters[idx.to.add, ] <- c(unique.id, parameterToAdd)
    
    #add parameter to parameter table
    row.to.add <- c(parameterToAdd, 0, "")
    if (nrow(params$param.table) == 0) {
      params$param.table[1,] <- row.to.add
    } else {
      params$param.table <- rbind(params$param.table, row.to.add)
    }
    loop$parameters <- params$param.table
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
    
    ids <- GenerateId(id$id.seed, "parameter")
    unique.id <- ids[[2]]
    id$id.seed <- ids[[1]]
    idx.to.add <- nrow(id$id.parameters) + 1
    id$id.parameters[idx.to.add, ] <- c(unique.id, parameterToAdd)
  }
  
  #add parameter to parameter table
  row.to.add <- c(parameterToAdd, 0, "")
  if (nrow(params$param.table) == 0) {
    params$param.table[1,] <- row.to.add
  } else {
    params$param.table <- rbind(params$param.table, row.to.add)
  }
  loop$parameters <- params$param.table
}

build_db_row <- function(eqn_type, RHS_coef, RHS_var, LHS_coef, LHS_var,arrow_type, kf, kr, description){
  row_out <- c(eqn_type, RHS_coef, RHS_var, LHS_coef, LHS_var,arrow_type, kf, kr, description)
}

BuildEquationSide <- function(coefUI, varUI, n) {
  # coefUI - strings of coef ui used to build equations ("2", "1" from input$LHS_coef)
  # varUI - strings of var used to build equations ("E2F", from input$LHS_Var_)
  # n - number of inputs on this side of the equation
  coefs <- vector()
  vars <- vector()
  
  for (i in seq(n)) { #find all coefficients and variables on left hand side of equation and add them to vectors
    coef <- eval(parse(text = paste0(coefUI, as.character(i))))
    var <- eval(parse(text = paste0(varUI, as.character(i))))
    coefs <- append(coefs, coef)
    vars <- append(vars, var)
  }
  coefs <- paste(coefs, collapse = " ") #paste vectors into space separated variables (ex k1 k2 k3)
  vars <- paste(vars, collapse = " ") #paste vectors into space separated variables
  
  out <- list("coefs" = coefs, "vars" = vars)
  return(out)
}

BuildRegulatorSide <- function(regUI, RC.UI, n) {
  # coefUI - strings of coef ui used to build equations ("2", "1" from input$LHS_coef)
  # varUI - strings of var used to build equations ("E2F", from input$LHS_Var_)
  # n - number of inputs on this side of the equation
  regs  <- vector()
  RCs   <- vector()
  p.add <- vector()
  
  for (i in seq(n)) { #find all coefficients and variables on left hand side of equation and add them to vectors
    reg   <- eval(parse(text = paste0(regUI, as.character(i))))
    rc    <- eval(parse(text = paste0(RC.UI, as.character(i))))
    regs  <- append(regs, reg)
    RCs   <- append(RCs, rc)
    p.add <- c(p.add, rc)
  }
  regs <- paste(regs, collapse = " ") #paste vectors into space separated variables (ex k1 k2 k3)
  RCs  <- paste(RCs, collapse = " ") #paste vectors into space separated variables
  
  out <- list("regulators" = regs, "rateConstants" = RCs, "P.to.add" = p.add)
  return(out)
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
  eqn_type           <- input$eqnCreate_type_of_equation
  p.add              <- c()
  passed.error.check <- TRUE
  var.in.eqns        <- c()
  par.in.eqns        <- c()
  
  if (eqn_type == "chem_rxn") {
    #this will hold all the functions for chemical reactions:
    # Currently holds: Mass Action, Regulated Mass Action
    jPrint("chem_rxn")
    compartment = 1 #placeholder for compartments to be added in future
    # Set all non-chem rxn parameters to NA
    kcat = NA
    Vmax = NA 
    Km = NA 
    enzyme = NA
    
    n.RHS = as.numeric(input$eqnCreate_num_of_eqn_RHS) #number of variables on RHS of equation
    n.LHS = as.numeric(input$eqnCreate_num_of_eqn_LHS) #number of variables on LHS of equation
    
    if (input$eqn_chem_law == "MA") { # Mass Action
      jPrint("Mass Action")
      law = "MassAction"
      # Set regulators to null
      FM.bool <- FALSE
      FMs     <- NA
      FM.RC   <- NA
      RM.bool <- FALSE
      RMs     <- NA
      RM.RC   <- NA
      # Build left hand side of equation
      left     <- BuildEquationSide("input$LHS_Coeff_", "input$LHS_Var_", n.LHS)
      coef.LHS <- left["coefs"]
      var.LHS  <- left["vars"]
      
      # Build right hand side equation
      right    <- BuildEquationSide("input$RHS_Coeff_","input$RHS_Var_", n.RHS)
      coef.RHS <- right["coefs"]
      var.RHS  <- right["vars"]
      
      arrow <- input$eqn_chem_forward_or_both
      if (arrow == "both_directions") {
        jPrint("both directions")
          # Rate Constants
          kf    <- input$eqn_chem_forward_k
          kr    <- input$eqn_chem_back_k
          p.add <- c(p.add, kf, kr)

      } else if (arrow == "forward_only") {
          kf    <- input$eqn_chem_forward_k
          kr    <- NA
          p.add <- c(p.add, kf)
      }
      eqn.description <- ""
      var.in.eqns <- paste(var.LHS, var.RHS)
      
    } else if (input$eqn_chem_law == 'MAwR') { # Mass Action w/ Regulation
      law = "RegulatedMA"
      n.f.reg = as.numeric(input$eqn_options_chem_num_forward_regulators) #number of regulators for forward reaction
      n.r.reg = as.numeric(input$eqn_options_chem_num_reverse_regulators) #number of regulators for reverse reaction
      
      # Build left hand side of equation
      left     <- BuildEquationSide("input$LHS_Coeff_", "input$LHS_Var_", n.LHS)
      coef.LHS <- left["coefs"]
      var.LHS  <- left["vars"]
      
      # Build right hand side equation
      right    <- BuildEquationSide("input$RHS_Coeff_","input$RHS_Var_", n.RHS)
      coef.RHS <- right["coefs"]
      var.RHS  <- right["vars"]
      
      arrow <- input$eqn_chem_forward_or_both
      if (arrow == "both_directions") {
        if (input$eqn_options_chem_modifier_forward) {
          kf      <- NA
          FM.bool <- TRUE

          f.regs <- BuildRegulatorSide("input$eqn_forward_regulator_", "input$eqn_forward_rateConstant_", n.f.reg)
          FMs <- f.regs["regulators"]
          FM.RC <- f.regs["rateConstants"]
          p.add <- c(p.add, f.regs["P.to.add"])
          FMs <- paste(FMs, collapse = " ")
          FM.RC <- paste(FM.RC, collapse = " ")
        } else {
          kf <- input$eqn_chem_forward_k
          p.add <- c(p.add, kf)
          FM.bool <- FALSE
          FMs <- NA
          FM.RC <- NA
        }
        # Checks if regulator was used in reverse reaction, hence removing kr 
        # and updating the appropriate values for the regulator 
        if (input$eqn_options_chem_modifier_reverse) {
          kr      <- NA
          RM.bool <- TRUE 
   
          r.regs <- BuildRegulatorSide("input$eqn_reverse_regulator_", "input$eqn_reverse_rateConstant_", n.r.reg)
          RMs    <- r.regs["regulators"]
          RM.RC  <- r.regs["rateConstants"]
          p.add  <- c(p.add, r.regs["P.to.add"])
          RMs <- paste(RMs, collapse = " ")
          RM.RC <- paste(RM.RC, collapse = " ")
        }
        else{
          kr      <- input$eqn_chem_back_k
          RM.bool <- FALSE
          RMs     <- NA
          RM.RC   <- NA
          p.add   <- c(p.add, kr)
        }
      } else if (arrow == "forward_only") {
        
        # Set reverse regulator variables to NA
        kr <- NA
        RM.bool <- FALSE
        RMs <- NA
        RM.RC <- NA
        
        if (input$eqn_options_chem_modifier_forward) {
          kf      <- NA
          FM.bool <- TRUE
          
          f.regs <- BuildRegulatorSide("input$eqn_forward_regulator_", "input$eqn_forward_rateConstant_", n.f.reg)
          FMs <- f.regs["regulators"]
          FM.RC <- f.regs["rateConstants"]
          p.add <- c(p.add, f.regs["P.to.add"])
          FMs <- paste(FMs, collapse = " ")
          FM.RC <- paste(FM.RC, collapse = " ")
        } else {
          kf <- input$eqn_chem_forward_k
          p.add <- c(p.add, kf)
          FM.bool <- FALSE
          FMs <- NA
          FM.RC <- NA
        }
      }

      eqn.description = ""
      to.add <- c(var.LHS, var.RHS)
      to.add <- to.add[!is.na(to.add)]
      var.in.eqns <- paste(to.add, collapse = " ")
    }
      
    # Add equation to df
    passed.error.check <- CheckParametersForErrors(p.add, 
                                                   vars$species, 
                                                   params$vars.all)
    
    if (passed.error.check) {
      jPrint("Error Check")
      jPrint(p.add)
      # Store parameters to parameter vector
      for (var in p.add) {
        StoreParamsEqn(var)
      }
      jPrint("stored parameter")
      # Store up params and variables in equation
      
      # Generate eqn ID
      jPrint(id$id.eqn.seed)
      ID.gen <- GenerateId(id$id.eqn.seed, "eqn")
      jPrint("generatedID")
      id$id.eqn.seed <- id$id.eqn.seed + 1
      jPrint("extracted Seed")
      ID <- ID.gen["id"]
      jPrint("Generated Id")
      #Build up Dataframe rows
      row.to.df.chem <- c(ID,
                          law,
                          coef.LHS, 
                          var.LHS, 
                          coef.RHS, 
                          var.RHS, 
                          arrow,
                          kf, 
                          kr,
                          FM.bool, 
                          FMs, 
                          FM.RC,
                          RM.bool, 
                          RMs, 
                          RM.RC
      )
      row.to.df.info <- c(ID,
                          eqn_type,
                          law,
                          var.in.eqns,
                          paste0(p.add, collapse = " "),
                          compartment,
                          eqn.description)
      
      jPrint("Adding rows to df")
      jPrint(eqns$n.eqns)
      jPrint(eqns$n.eqns.chem)
      eqns$eqn.info[eqns$n.eqns+1, ]      <- row.to.df.info
      eqns$eqn.chem[eqns$n.eqns.chem+1, ] <- row.to.df.chem
      jPrint("added")
      #increment equation numbering
      eqns$n.eqns      <- eqns$n.eqns + 1
      eqns$n.eqns.chem <- eqns$n.eqns.chem + 1
    }
  }
  else if (eqn_type == "enzyme_rxn") {
    
    if (input$eqn_enzyme_law == "MM") {
      
      eqn.description <- ""
      compartment     <- 1
      law             <- "Michaelis Menten"
      
      substrate  <- input$eqn_enzyme_substrate
      product    <- input$eqn_enzyme_product
      Km         <- input$eqn_enzyme_Km
      arrow      <- "forward_only"
      p.add      <- c(Km)
      var.add    <- c(substrate, product)

      if (!input$eqn_options_enzyme_useVmax) {
        kcat    <- input$eqn_enzyme_kcat
        enzyme  <-  input$eqn_enzyme_enzyme
        Vmax    <-  NA
        p.add   <- c(p.add, kcat)
      } else if (input$eqn_options_enzyme_useVmax) {
        Vmax   <- input$eqn_enzyme_Vmax
        kcat   <- NA
        enzyme <- NA
        p.add  <- c(p.add, Vmax)
      }
      
      for (var in p.add) {
        StoreParamsEqn(var)
      }

      # Generate eqn ID
      ID.gen <- GenerateId(id$id.eqn.seed, "eqn")
      id$id.eqn.seed <- id$id.eqn.seed + 1
      ID <- ID.gen["id"]
      
      row.to.df.info <- c(ID,
                          eqn_type,
                          law,
                          paste0(var.add, collapse = " "),
                          paste0(p.add, collapse = " "),
                          compartment,
                          eqn.description)
      
      row.to.df.enzyme <- c(ID,
                            law,
                            substrate,
                            product, 
                            enzyme,
                            kcat,
                            Km, 
                            Vmax
                            )
      
      eqns$eqn.info[eqns$n.eqns+1, ]       <- row.to.df.info
      eqns$eqn.enzyme[eqns$n.eqns.enz+1, ] <- row.to.df.enzyme
      
      #increment equation numbering
      eqns$n.eqns      <- eqns$n.eqns + 1
      eqns$n.eqns.enz  <- eqns$n.eqns.enz + 1
    }
  } 
  else if (eqn_type == "simp_diff") {
    coef.LHS <- 1
    coef.RHS <- 1
    var.LHS = input$simp_diff_var1
    var.RHS = input$simp_diff_var2
    diff_coef <- input$simp_diff_PS_Var
    if (input$simp_diff_wayOfDiffusion) {
      arrow <- "forward_only"
      kf = diff_coef
      kr = NA
    }else{
      arrow <- "both_directions"
      kf = diff_coef
      kr = diff_coef
    }
    kcat = NA
    Vmax = NA 
    Km = NA 
    enzyme = NA
    FM.bool <- FALSE
    f_regulators_coef <- NA
    f_regulators_rateConstants <- NA
    RM.bool <- FALSE
    RMs <- NA
    RM.RC <- NA
    row_to_df <- c(eqn_type, coef.LHS, var.LHS, coef.RHS, var.RHS, arrow, kf, kr, 
                   kcat, Vmax, Km, enzyme,
                   FM.bool, f_regulators_coef, f_regulators_rateConstants,
                   RM.bool, RMs, RM.RC)    

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

  if (passed.error.check) {
    if (eqn_type != "rate_eqn" && eqn_type != "time_dependent") {
      eqns$main <- append(eqns$main, equationBuilder())   #store selected variable to list of variables
      # ids <- GenerateId(id$id.seed, "eqn")
      # unique.id <- ids[[2]]
      # id$id.seed <- ids[[1]]
      # idx.to.add <- nrow(id$id.equations) + 1
      # id$id.equations[idx.to.add, ] <- c(unique.id, equationBuilder())
      # eqns$eqn.descriptions <- c(eqns$eqn.descriptions, "")
    }
  }
  #solveForDiffEqs()
})

#-------------------------------------------------------------------------------

# Build Text Equation for User to See

#-------------------------------------------------------------------------------
equationBuilder <- reactive({
  if (input$eqnCreate_type_of_equation == "chem_rxn") {
    n.RHS = as.numeric(input$eqnCreate_num_of_eqn_RHS)
    n.LHS = as.numeric(input$eqnCreate_num_of_eqn_LHS)
    n.f.reg = as.numeric(input$eqn_options_chem_num_forward_regulators)
    n.r.reg = as.numeric(input$eqn_options_chem_num_reverse_regulators)

    eqn_LHS <- ""
    for (i in seq(n.LHS)) {
      coef <- eval(parse(text = paste0("input$LHS_Coeff_", as.character(i))))
      var <- eval(parse(text = paste0("input$LHS_Var_", as.character(i))))
      if (coef != "1") {eqn_LHS <- paste0(eqn_LHS, coef, "*")}
      if (i == as.numeric(n.LHS)) {eqn_LHS <- paste0(eqn_LHS, var)}
      else{eqn_LHS <- paste0(eqn_LHS, var, " + ")}
    }

    eqn_RHS <- ""
    for (i in seq(n.RHS)) {
      coef <- eval(parse(text = paste0("input$RHS_Coeff_", as.character(i))))
      var <- eval(parse(text = paste0("input$RHS_Var_", as.character(i))))
      if (coef != "1") {eqn_RHS <- paste0(eqn_RHS, coef, "*")}
      if (i == as.numeric(n.RHS)) {eqn_RHS <- paste0(eqn_RHS, var)}
      else{eqn_RHS <- paste0(eqn_RHS, var, " + ")}
    }

    if (input$eqn_chem_forward_or_both == "both_directions") {
      arrow <- "<-->"
      if (input$eqn_options_chem_modifier_forward && input$eqn_options_chem_modifier_reverse) {
        #find regulators and add them together in form ([regulator/constant, regulator2/constant2, etc...])
        forwardModifiers <- c()
        for (i in seq(n.f.reg)) {
          regulator <- eval(parse(text = paste0("input$eqn_forward_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_forward_rateConstant_", as.character(i))))
          modifierExpression <- paste0(regulator, "/", rateConstant)
          forwardModifiers <- c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ",")

        reverseModifiers <- c()
        for (i in seq(n.r.reg)) {
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
        for (i in seq(n.f.reg)) {
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
        for (i in seq(n.r.reg)) {
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
        for (i in seq(n.f.reg)) {
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

    if (input$eqn_options_enzyme_useVmax) {
      kcat = input$eqn_enzyme_kcat
      textOut <- paste0(substrate," + ", enzyme,  " (", kcat, ")", arrow, "(", Km, ") ", product)
    }
    else if (!input$eqn_options_enzyme_useVmax) {
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
  n.RHS = as.numeric(input$eqnCreate_num_of_eqn_RHS)
  n.LHS = as.numeric(input$eqnCreate_num_of_eqn_LHS)
  nums <- c(n.RHS, n.LHS)
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

output$eqnCreate_showEquationBuilding <- renderUI({
  withMathJax(
    equationBuilder_MathJax()
  )
  })
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
  updateSelectInput(session
                    ,"eqnCreate_delete_equation"
                    ,choices = as.character(seq(eqns$n.eqns)))
})


observeEvent(input$createEqn_delete_equation_button, {
  #delete associated parameters used in this equation if they aren't used elsewhere
  eqn_to_delete <- as.numeric(input$eqnCreate_delete_equation)
  #find parameters used in this equation
  eqn.row <- eqns$eqn.info[eqn_to_delete, 1:ncol(eqns$eqn.info)]
 #extract all possible parameters from eqn.info 
  kf <- eqn.row[7]
  kr <- eqn.row[8]
  kcat <- eqn.row[9]
  Vmax <- eqn.row[10]
  Km <- eqn.row[11]
  fr <- eqn.row[15]
  rr <- eqn.row[18]
  p <- c(kf, kr, kcat, Vmax, Km, fr, rr)
  #replace string NA with actual NA
  p <- dplyr::na_if(p, "NA")

  #remove equation from all sections
  eqns$eqn.info <- eqns$eqn.info[-eqn_to_delete, 1:ncol(eqns$eqn.info)] #delete equation from dataframe
  eqns$main <- eqns$main[-eqn_to_delete] #removes equation from equation list
  eqns$n.eqns <- eqns$n.eqns - 1
  eqns$eqn.descriptions <- eqns$eqn.descriptions[-eqn_to_delete]
  
  #check to see if that parameter is used elsewhere and save it if it is
  p.remove <- c()
  p.save <- c()
  # Search eqns and IO for parameter
  for (param in p) {
    check1 <- ParameterSearchDF(param, eqns$eqn.info)
    check2 <- ParameterSearchDF(param, IO$input.info)
    check3 <- ParameterSearchDF(param, IO$output.info)
    if (check1 | check2 | check3) {
      p.save <- c(p.save, param)
    } else {
      p.remove <- c(p.remove, param)
    }
  }
  #if not, remove it
  for (var in p.remove) {
    DeleteParameters(var)
  } 
  #if so, store in message of variables not removed
  if (length(p.save) > 0) {
    message.out <- paste0("The following parameter(s) were not deleted because they are used elsewhere: ", 
                          paste0(p.save, collapse=", ")
    )
    session$sendCustomMessage(type = 'testmessage',
                              message = message.out)
  }
  my.choices <- paste0(seq(eqns$n.eqns), ") ", eqns$main)
  updatePickerInput(session,
                    "eqnCreate_selectEqnForDescription",
                    choices = my.choices)
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
  n.RHS = as.numeric(input$eqnCreate_num_of_eqn_RHS_edit)
  n.LHS = as.numeric(input$eqnCreate_num_of_eqn_LHS_edit)
  eqn_number = input$eqnCreate_edit_select_equation #eqn number to edit
  eqn_to_edit <- eqns$eqn.info[eqn_number, 1:ncol(eqns$eqn.info)] #extract equation
  
  fluidRow(column(width = 2
                  ,lapply(seq(n.LHS), function(i){
                    numericInput(inputId = paste0("LHS_Coeff_edit_", as.character(i))
                                 ,label = "Coefficient"
                                 ,value = str_split(eqn_to_edit[2], " ")[[1]][i]
                                 ,min = 1
                                 ,step = 1)
                  })
  )#end Column
  ,column(width = 2
          ,lapply(seq(n.LHS), function(i){
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
          ,lapply(seq(n.RHS), function(i){
            numericInput(inputId = paste0("RHS_Coeff_edit_", as.character(i))
                         ,label = "Coefficient"
                         ,value = str_split(eqn_to_edit[4], " ")[[1]][i]
                         ,min = 1
                         ,step = 1)
          })
  )#end Column
  ,column(width = 2
          ,lapply(seq(n.RHS), function(i){
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
                    ,conditionalPanel(condition = "input.eqn_options_enzyme_useVmax"
                                      ,pickerInput(inputId = "eqn_enzyme_enzyme_edit"
                                                   ,label = "Enzyme"
                                                   ,choices = sort(vars$species)
                                                   ,selected = eqn_to_edit[12]))
    )
    ,column(width = 3
            ,offset = 1
            ,conditionalPanel(condition = "!input.eqn_options_enzyme_useVmax"
                              ,textInput(inputId = "eqn_enzyme_Vmax_edit"
                                         ,label = "Vmax"
                                         ,value = eqn_to_edit[10]))
            ,conditionalPanel(condition = "input.eqn_options_enzyme_useVmax"
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
    n.RHS = as.numeric(input$eqnCreate_num_of_eqn_RHS_edit)
    n.LHS = as.numeric(input$eqnCreate_num_of_eqn_LHS_edit)
    
    eqn_LHS <- ""
    for (i in seq(n.LHS)) {
      coef <- eval(parse(text = paste0("input$LHS_Coeff_edit_", as.character(i))))
      var <- eval(parse(text = paste0("input$LHS_Var_edit_", as.character(i))))
      if (coef != "1") {eqn_LHS <- paste0(eqn_LHS, coef, "*")}
      if (i == is.numeric(n.LHS)) {eqn_LHS <- paste0(eqn_LHS, var)}
      else{eqn_LHS <- paste0(eqn_LHS, var, " + ")}
    }
    
    eqn_RHS <- ""
    for (i in seq(n.RHS)) {
      coef <- eval(parse(text = paste0("input$RHS_Coeff_edit_", as.character(i))))
      var <- eval(parse(text = paste0("input$RHS_Var_edit_", as.character(i))))
      if (coef != "1") {eqn_RHS <- paste0(eqn_RHS, coef, "*")}
      if (i == as.numeric(n.RHS)) {eqn_RHS <- paste0(eqn_RHS, var)}
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
    
    if (input$eqn_options_enzyme_useVmax) {
      kcat = input$eqn_enzyme_kcat_edit
      textOut <- paste0(substrate," + ", enzyme,  " (", kcat, ")", arrow, "(", Km, ") ", product)
    } else if (!input$eqn_options_enzyme_useVmax) {
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
    n.RHS = as.numeric(input$eqnCreate_num_of_eqn_RHS_edit)
    n.LHS = as.numeric(input$eqnCreate_num_of_eqn_LHS_edit)
    
    coef.LHS <- vector()
    var.LHS <- vector()
    for (i in seq(n.LHS)) {
      coef <- eval(parse(text = paste0("input$LHS_Coeff_edit_", as.character(i))))
      var <- eval(parse(text = paste0("input$LHS_Var_edit_", as.character(i))))
      coef.LHS <- append(coef.LHS, coef)
      var.LHS <- append(var.LHS, var)
    }
    coef.LHS <- paste(coef.LHS, collapse = " ")
    var.LHS <- paste(var.LHS, collapse = " ")
    
    coef.RHS <- vector()
    var.RHS <- vector()
    for (i in seq(n.RHS)) {
      coef <- eval(parse(text = paste0("input$RHS_Coeff_edit_", as.character(i))))
      var <- eval(parse(text = paste0("input$RHS_Var_edit_", as.character(i))))
      coef.RHS <- append(coef.RHS, coef)
      var.RHS <- append(var.RHS, var)
    }
    coef.RHS <- paste(coef.RHS, collapse = " ")
    var.RHS <- paste(var.RHS, collapse = " ")
    
    arrow <- input$eqn_chem_forward_or_both_edit
    if (arrow == "both_directions") {
      kf <- input$eqn_chem_forward_k_edit
      kr <- input$eqn_chem_back_k_edit
      # params$eqns.vars <- append(params$eqns.vars, kf)
      # params$eqns.vars <- append(params$eqns.vars, kr)
      StoreParamsEqn(kf)
      StoreParamsEqn(kr)
    }
    else if (arrow == "forward_only") {
      kf <- input$eqn_chem_forward_k_edit
      kr <- NA
      #params$eqns.vars <- append(params$eqns.vars, kf)
      StoreParamsEqn(kf)
    }
    kcat = NA
    Vmax = NA 
    Km = NA 
    enzyme = NA
    FM.bool <- FALSE
    f_regulators_coef <- NA
    f_regulators_rateConstants <- NA
    RM.bool <- FALSE
    RMs <- NA
    RM.RC <- NA
    row_to_df <- c(eqn_type, coef.LHS, var.LHS, coef.RHS, var.RHS, arrow, kf, kr, 
                   kcat, Vmax, Km, enzyme,
                   FM.bool, f_regulators_coef, f_regulators_rateConstants,
                   RM.bool, RMs, RM.RC)
    
  }#end if chem_rxn
  else if (eqn_type == "enzyme_rxn") {
    coef.LHS <- 1
    coef.RHS <- 1
    var.LHS = input$eqn_enzyme_substrate_edit
    var.RHS = input$eqn_enzyme_product_edit
    arrow <- "forward_only"
    Km = input$eqn_enzyme_Km_edit
    #params$eqns.vars <- append(params$eqns.vars, Km)
    StoreParamsEqn(Km)
    
    if (input$eqn_options_enzyme_useVmax) {
      kcat = input$eqn_enzyme_kcat_edit
      enzyme = input$eqn_enzyme_enzyme_edit
      Vmax = NA
      # params$eqns.vars <- append(params$eqns.vars, kcat)
      # params$eqns.vars <- append(params$eqns.vars, Km)
      StoreParamsEqn(kcat)
      StoreParamsEqn(Km)
    } else if (!input$eqn_options_enzyme_useVmax) {
      Vmax = input$eqn_enzyme_Vmax_edit
      kcat = NA
      enzyme = NA
      #params$eqns.vars <- append(params$eqns.vars, Vmax)
      StoreParamsEqn(Vmax)
    }
    
    kf = NA
    kr = NA
    FM.bool <- FALSE
    f_regulators_coef <- NA
    f_regulators_rateConstants <- NA
    RM.bool <- FALSE
    RMs <- NA
    RM.RC <- NA
    row_to_df <- c(eqn_type, coef.LHS, var.LHS, coef.RHS, var.RHS, arrow, kf, kr, 
                   kcat, Vmax, Km, enzyme,
                   FM.bool, f_regulators_coef, f_regulators_rateConstants,
                   RM.bool, RMs, RM.RC)
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

observeEvent(input$view_eqns_debug, {
  jPrint(eqns$eqn.info)
  jPrint(eqns$eqn.chem)
  jPrint(eqns$eqn.enzyme)
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

