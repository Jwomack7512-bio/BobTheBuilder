
waiter.eqns <- Waiter$new(id = "eqnCreate_showEquations",
                          html =  tagList(
                            div(
                              style = "color:black",
                              spin_whirly(),
                              hr(),
                              h4("Storing Equation...")
                            )
                          ),
                          color = transparent(0.7)
                          )

w.test <- Waiter$new(
  html =  tagList(
    div(
      style = "color:black",
      spin_whirly(),
      hr(),
      h4("Storing Equation...")
    )
  ),
  color = transparent(0.7)
)

CheckParametersForErrors <- function(paramsToCheck, 
                                     allSpeciesVar,
                                     allParamVariables, 
                                     onEdit = FALSE) {
  # Inputs: 
  #  @paramsToCheck - variable to be checked for conflicts
  #  @allParamVariables  - vector of parameter names
  #  @allSpeciesVar - vector of variable names
  #  @onEdit - boolean telling if this is an check on an equation edit
  # Outputs:
  #  @passed.test - boolean if parameter is good and should be stored.
  
  
  #Error Codes:
  # 0 - No Error
  # 1 - Variable name found in variable name vector
  # 2 - Variable name starts with number
  # 3 - Variable name contains special characters
  # 4 - Variable name starts with punctuation
  # 5 - Variable name found in parameter names
  # 6 - Variable name entered was all white space (no entered var)
  
  # Variables pass if error code of 5 is found but not 1,2,3,4,6
  
  # takes input of all parameters inputs for chem, enyzme, etc..only some will be active
  passed.test = TRUE #set true by default and change if error found
  for (var in paramsToCheck) {
    varCheck      <- variableCheck(var, allSpeciesVar, allParamVariables)
    pass.check    <- varCheck[[1]]
    error.message <- varCheck[[2]]
    error.code    <- varCheck[[3]]
    if (!pass.check) {
      if (error.code == 1 || 
          error.code == 2 || 
          error.code == 3 || 
          error.code == 4 ||
          error.code == 6) {
        # sends error and returns boolean to not store
        # errors on if parameter name == variable name, wrong punctuation, starts with number
        #   or contains special characters
        passed.test = FALSE
        sendSweetAlert(
          session = session,
          title = "Error...",
          text = error.message,
          type = "error"
        )
        break
        # sends warning if parameter is already used, but returns store boolean
      } else if (error.code == 5) { 
        if (onEdit) {
          # Don't warning message on edit of equation
          # This is because often the parameters stay the same and its annoying
        } else {
          passed.test = FALSE
          sendSweetAlert(
            session = session,
            title = "Warning !!!",
            text = error.message,
            type = "warning"
          )
        }
      }
    }
  }
  print("PASSDED")
  print(passed.test)
  return(passed.test)
}



BuildParameters <- function(pToAdd,
                            pAll,
                            idSeed,
                            pValue = 0, 
                            pDescription = "", 
                            pUnit = "pH",
                            pUnitD = "num <div> time",
                            pBaseUnit = "BASE",
                            pBaseValue = 0,
                            pLocation = "reactionType",
                            pLocationNote = "") {
  
  if (!(pToAdd %in% pAll)) {
    # Generate Parameter ID
    ids <- GenerateId(idSeed, "parameter")
    id <- ids$id

    # Add Parameter to Parameter List
    nPar <- length(pAll)
    pAll[nPar + 1] <- pToAdd
    p.list.entry <- list(Name = pToAdd,
                         ID = id,
                         Value = pValue,
                         Unit = pUnit,
                         UnitD = pUnitD,
                         BaseUnit = pBaseUnit,
                         BaseValue = pBaseValue,
                         Description = pDescription,
                         Type = pLocation,
                         TypeNote = pLocationNote)
    
    # Assign List Name
    names(p.list.entry) <- pToAdd
    
    # Add Row to Parameter Table
    row.to.add <- c(pToAdd, 
                    pValue, 
                    pUnit, 
                    pDescription)
    # params$param.table[nPar + 1, ] <- row.to.add
    passed.check <- TRUE
  } else {
    passed.check <- FALSE
    p.list.entry <- NULL
    row.to.add <- NULL
  }
  
  out <- list(passed = passed.check,
              par.id = id,
              par.all = pAll,
              p.entry = p.list.entry,
              row.for.datatable = row.to.add)
}

StoreParameters <- function(BuildParmetersOutput) {
  
  # Unpack Output
  passed    <- BuildParmetersOutput$passed
  par.id    <- BuildParmetersOutput$par.id
  par.all   <- BuildParmetersOutput$par.all
  p.entry   <- BuildParmetersOutput$p.entry
  row.2.add <- BuildParmetersOutput$row.for.datatable
  
  nPar      <- length(par.all)

  names(p.entry) <- c("Name", 
                      "ID", 
                      "Value", 
                      "Unit",
                      "Unit.Description",
                      "Base.Unit",
                      "Base.Value",
                      "Description", 
                      "Type",
                      "Type.Note")
  # Store Parameter Name RV
  params$vars.all <- par.all
  print(params$vars.all)
  # Store Params to List
  params$params[[nPar]] <- p.entry
  names(params$params)[nPar] <- par.all[length(par.all)]
  # Add to Parameter Ids
  id$id.param.seed <- id$id.param.seed + 1
  id$id.df[nrow(id$id.df) + 1,] <- c(par.id, p.entry$Name)
  # Add to Parameter Tabl
  params$param.table[nPar, ] <- row.2.add
  # Rewrite the loop parameter table
  loop$parameters <- params$param.table
}

StoreParamsEqn <- function(pToAdd, 
                           pValue = 0, 
                           pDescription = "", 
                           pUnit = "pH",
                           pLocation = "reactionType") {
  
  if (!(pToAdd %in% params$vars.all)) {
    # Generate Parameter ID
    ids <- GenerateId(id$id.var.seed, "parameter")
    unique.id <- ids[[2]]
    id$id.var.seed <- ids[[1]]
    idx.to.add <- nrow(id$id.df) + 1
    id$id.df[idx.to.add, ] <- c(unique.id, pToAdd)
    browser()
    # Add Parameter to Parameter List
    nPar <- length(params$vars.all)
    params$vars.all[nPar + 1] <- pToAdd
    params$params[[nPar + 1]] <- list(Name = pToAdd,
                                      ID = ids[[1]],
                                      Value = pValue,
                                      Unit = pUnit,
                                      Description = pDescription,
                                      Type = pLocation)

    # Assign List Name
    names(params$params)[nPar + 1] <- pToAdd

    # Add Row to Parameter Table
    row.to.add <- c(pToAdd, 
                    pValue, 
                    pUnit, 
                    pDescription)
    params$param.table[nPar + 1, ] <- row.to.add

    # Rewrite the loop parameter table
    loop$parameters <- params$param.table
    
  }
  #NEED TO ADD CHECK IF PARAM ALREADY EXISTS
  if (!(pToAdd %in% params$vars.all) &&
        !(pToAdd %in% params$rate.params)) {
    params$eqns.vars <- append(params$eqns.vars, pToAdd)
    params$eqns.vals <- append(params$eqns.vals, 0)
    params$eqns.comments <- append(params$eqns.comments, pDescription)
    
    params$vars.all <- append(params$vars.all, pToAdd)
    params$vals.all <- append(params$vals.all, 0)
    params$comments.all <- append(params$comments.all, pDescription)
    params$par.units.all <- append(params$par.units.all, pUnit)
    

    
    #add parameter to parameter table

    # if (nrow(params$param.table) == 0) {
    #   params$param.table[1,] <- row.to.add
    # } else {
    #   params$param.table <- rbind(params$param.table, row.to.add)
    # }
    # Store Parameter in List
    
    print(params$params)
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
    idx.to.add <- nrow(id$id.df) + 1
    id$id.df[idx.to.add, ] <- c(unique.id, parameterToAdd)
  }
  
  #add parameter to parameter table
  row.to.add <- c(parameterToAdd, 0, "ph", "")
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
  ids <- vector()
  
  for (i in seq(n)) { #find all coefficients and variables on left hand side of equation and add them to vectors
    coef <- eval(parse(text = paste0(coefUI, as.character(i))))
    var <- eval(parse(text = paste0(varUI, as.character(i))))
    coefs <- append(coefs, coef)
    vars <- append(vars, var)
    ids <- append(ids, FindId(var))
  }
  coefs <- paste(coefs, collapse = " ") #paste vectors into space separated variables (ex k1 k2 k3)
  vars <- paste(vars, collapse = " ") #paste vectors into space separated variables
  ids   <- paste(ids, collapse = " ")
  
  out <- list("coefs" = coefs, 
              "vars" = vars,
              "ids" = ids)
  return(out)
}

BuildRegulatorSide <- function(regUI, 
                               RC.UI, 
                               n, 
                               LHS.var, 
                               RHS.var,
                               ForwardReg) {
  # regUI - strings of regulators ui used to build equations
  # RC.UI - strings of rate constants used to build equations
  # n - number of inputs on this side of the equation
  # LHS.var - variables on the left (used for parameter description)
  # RHS.var - variables on the right
  # ForwardReg - True if forward regulator (used for description)
  regs     <- vector()
  RCs      <- vector()
  p.add    <- vector()
  d.add    <- vector()
  rc.d.add <- vector()
  ids      <- vector()
  
  for (i in seq(n)) { #find all coefficients and variables on left hand side of equation and add them to vectors
    reg   <- eval(parse(text = paste0(regUI, as.character(i))))
    rc    <- eval(parse(text = paste0(RC.UI, as.character(i))))
    regs  <- append(regs, reg)
    RCs   <- append(RCs, rc)
    p.add <- c(p.add, rc)
    ids   <- c(ids, FindId(reg))
    
    if (ForwardReg) {
      rc.d  <- paste0("Rate constant for forward regulator, ",
                      reg,
                      ", on the reaction of ",
                      paste0(str_split(LHS.var, " ")[[1]], collapse = ", "),
                      " to ",
                      paste0(str_split(RHS.var, " ")[[1]], collapse = ", ")
                      )
    } else {
      rc.d  <- paste0("Rate constant for reverse regulator, ",
                      reg,
                      ", on the reaction of ",
                      paste0(str_split(LHS.var, " ")[[1]], collapse = ", "),
                      " to ",
                      paste0(str_split(RHS.var, " ")[[1]], collapse = ", ")
      )
    }
    rc.d.add <- append(rc.d.add, rc.d)
  }
  regs <- paste(regs, collapse = " ") #paste vectors into space separated variables (ex k1 k2 k3)
  RCs  <- paste(RCs, collapse = " ") #paste vectors into space separated variables
  ids  <- paste(ids, collapse = " ")
  
  
  out <- list("regulators"     = regs, 
              "rateConstants"  = RCs, 
              "P.to.add"       = p.add,
              "P.descriptions" = rc.d.add,
              "reg.ids"        = ids)
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


# Reactive Variable Filtering By Compartment -----------------------------------
# Info:
# Select compartment for creating equation
# Search df of var for mathcing compartments and take names

observeEvent({input$eqnCreate_active_compartment
              vars$compartments.info
              vars$var.info}, {
  req(!is_empty(vars$var.df))

  vars$df.by.compartment <- 
    vars$var.df %>% filter(Compartment == input$eqnCreate_active_compartment)
})


#-------------------------------------------------------------------------------

# Extract data and store equation elements into a df to solve ODEs from

#-------------------------------------------------------------------------------
observeEvent(input$eqnCreate_addEqnToVector, {
  #waiter.eqns$show()
  w.test$show()
  shinyjs::disable("eqnCreate_addEqnToVector")
  Sys.sleep(0.5)
  eqn_type           <- input$eqnCreate_type_of_equation
  p.add              <- c() # Parameter Variable Vector
  u.add              <- c() # parameter Unit Vector
  ud.add             <- c() # Parameter Unit Breakdown Vector
  d.add              <- c() # Parameter Description Vector
  b.unit             <- c() # Base Unit for calculations
  b.val              <- c() # Base Unit Values
  passed.error.check <- TRUE
  var.add            <- c() # Variables in model to add
  p.id               <- c()
  var.id             <- c()
  
  # browser()
  if (eqn_type == "chem_rxn") {
    #this will hold all the functions for chemical reactions:
    # Currently holds: Mass Action, Regulated Mass Action
    jPrint("chem_rxn")
    compartment <- input$eqnCreate_active_compartment
    comp.id     <- FindId(compartment)
    # browser()
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
      id.LHS   <- left["ids"]
      
      # Build right hand side equation
      right    <- BuildEquationSide("input$RHS_Coeff_","input$RHS_Var_", n.RHS)
      coef.RHS <- right["coefs"]
      var.RHS  <- right["vars"]
      id.RHS   <- right["ids"]
      
      arrow <- input$eqn_chem_forward_or_both
      if (arrow == "both_directions") {
        jPrint("both directions")
          # Rate Constants
          kf    <- input$eqn_chem_forward_k
          kr    <- input$eqn_chem_back_k
          kf.u <- DetermineRateConstantUnits(coef.LHS$coefs, 
                                             units$base.units$For.Var,
                                             units$base.units$Volume,
                                             units$base.units$Duration,
                                             units$selected.units$For.Var,
                                             units$selected.units$Volume,
                                             units$selected.units$Duration)
          kr.u <- DetermineRateConstantUnits(coef.RHS$coefs, 
                                             units$base.units$For.Var,
                                             units$base.units$Volume,
                                             units$base.units$Duration,
                                             units$selected.units$For.Var,
                                             units$selected.units$Volume,
                                             units$selected.units$Duration)
          kf.unit   <- kf.u$unit
          kr.unit   <- kr.u$unit
          kf.b.unit <- kr.u$unit.base
          kr.b.unit <- kr.u$unit.base
          kf.unit.d <- kf.u$unit.d
          kr.unit.d <- kr.u$unit.d
          
          kf.d <- paste0("Forward rate constant for the reaction of ",
                         paste0(str_split(var.LHS, " ")[[1]], collapse = ", "),
                         " to ",
                         paste0(str_split(var.RHS, " ")[[1]], collapse = ", "))
          kr.d <- paste0("Reverse rate constant for the reaction of ",
                         paste0(str_split(var.LHS, " ")[[1]], collapse = ", "),
                         " to ",
                         paste0(str_split(var.RHS, " ")[[1]], collapse = ", ")
          )
          
          p.add  <- c(p.add, kf, kr)
          u.add  <- c(u.add, kf.unit, kr.unit)
          ud.add <- c(ud.add, kf.unit.d, kr.unit.d)
          d.add  <- c(d.add, kf.d, kr.d)
          b.unit <- c(b.unit, kf.b.unit, kr.b.unit)
          b.val  <- c(b.val, 0, 0)

      } else if (arrow == "forward_only") {
          kf    <- input$eqn_chem_forward_k
          kr    <- NA
          
          kf.u <- DetermineRateConstantUnits(coef.LHS$coefs, 
                                             units$base.units$For.Var,
                                             units$base.units$Volume,
                                             units$base.units$Duration,
                                             units$selected.units$For.Var,
                                             units$selected.units$Volume,
                                             units$selected.units$Duration)
          
          kf.unit      <- kf.u$unit
          kf.unit.base <- kf.u$unit.base
          kf.unit.d    <- kf.u$unit.d
          
          kf.d <- paste0("Forward rate constant for the reaction of ",
                         paste0(str_split(var.LHS, " ")[[1]], collapse = ", "),
                         " to ",
                         paste0(str_split(var.RHS, " ")[[1]], collapse = ", "))
          
          p.add  <- c(p.add, kf)
          u.add  <- c(u.add, kf.unit)
          ud.add <- c(ud.add, kf.unit.d)
          d.add  <- c(d.add, kf.d)
          b.unit <- c(b.unit, kf.unit.base)
          b.val  <- c(b.val, 0)
      }
      eqn.description <- ""
      var.add <- paste(var.LHS, var.RHS)
      var.id <- paste(id.LHS, id.RHS)
      
    } else if (input$eqn_chem_law == 'MAwR') { # Mass Action w/ Regulation
      law = "RegulatedMA"
      n.f.reg = as.numeric(input$eqn_options_chem_num_forward_regulators) #number of regulators for forward reaction
      n.r.reg = as.numeric(input$eqn_options_chem_num_reverse_regulators) #number of regulators for reverse reaction
      
      # Build left hand side of equation
      left     <- BuildEquationSide("input$LHS_Coeff_", "input$LHS_Var_", n.LHS)
      coef.LHS <- left["coefs"]
      var.LHS  <- left["vars"]
      id.LHS   <- left["ids"]
      
      # Build right hand side equation
      right    <- BuildEquationSide("input$RHS_Coeff_","input$RHS_Var_", n.RHS)
      coef.RHS <- right["coefs"]
      var.RHS  <- right["vars"]
      id.RHS   <- right["ids"]
      
      arrow <- input$eqn_chem_forward_or_both
      if (arrow == "both_directions") {
        if (input$eqn_options_chem_modifier_forward) {
          kf      <- NA
          FM.bool <- TRUE

          f.regs <- BuildRegulatorSide("input$eqn_forward_regulator_", 
                                       "input$eqn_forward_rateConstant_", 
                                       n.f.reg,
                                       var.LHS,
                                       var.RHS,
                                       TRUE)
          FMs     <- f.regs["regulators"]
          FM.RC   <- f.regs["rateConstants"]
          FM.ids  <- f.regs["reg.ids"]
          
          p.add   <- c(p.add, f.regs["P.to.add"][[1]])
          d.add   <- c(d.add, f.regs["P.descriptions"][[1]])

          FMs     <- paste(FMs, collapse = " ")
          FM.RC   <- paste(FM.RC, collapse = " ")
          for (reg in strsplit(FM.RC, " ")[[1]]) {
            u <- DetermineRateConstantUnits("1",
                                            units$base.units$For.Var,
                                            units$base.units$Volume,
                                            units$base.units$Duration,
                                            units$selected.units$For.Var,
                                            units$selected.units$Volume,
                                            units$selected.units$Duration)
            u.add  <- c(u.add,  u$unit)
            b.unit <- c(b.unit, u$unit.base)
            b.val  <- c(b.val,  0)
            ud.add <- c(ud.add, u$unit.d)
          }
        } else {
          kf      <- input$eqn_chem_forward_k
          p.add   <- c(p.add, kf)
          FM.bool <- FALSE
          FMs     <- NA
          FM.RC   <- NA
          FM.ids  <- NA
          
          
          kf.unit <- DetermineRateConstantUnits(coef.LHS$coefs, 
                                                units$base.units$For.Var,
                                                units$base.units$Volume,
                                                units$base.units$Duration,
                                                units$selected.units$For.Var,
                                                units$selected.units$Volume,
                                                units$selected.units$Duration)
          
          kf.d <- paste0("Reverse rate constant for the reaction of ",
                         paste0(str_split(var.LHS, " ")[[1]], collapse = ", "),
                         " to ",
                         paste0(str_split(var.RHS, " ")[[1]], collapse = ", ")
                        )
          u.add  <- c(u.add,  kf.unit$unit)
          ud.add <- c(ud.add, kf.unit$unit.d)
          b.unit <- c(b.unit, kf.unit$base.unit)
          b.val  <- c(b.val,  0)
          d.add  <- c(d.add,  kf.d)
        }
        # Checks if regulator was used in reverse reaction, hence removing kr 
        # and updating the appropriate values for the regulator 
        if (input$eqn_options_chem_modifier_reverse) {
          kr      <- NA
          RM.bool <- TRUE 
   
          r.regs <- BuildRegulatorSide("input$eqn_reverse_regulator_", 
                                       "input$eqn_reverse_rateConstant_", 
                                       n.r.reg,
                                       var.LHS,
                                       var.RHS,
                                       FALSE)
          RMs     <- r.regs["regulators"]
          RM.RC   <- r.regs["rateConstants"]
          RM.ids  <- r.regs["reg.ids"]
          
          p.add   <- c(p.add, r.regs["P.to.add"][[1]])
          d.add   <- c(d.add, r.regs["P.descriptions"][[1]])
          RMs     <- paste(RMs, collapse = " ")
          RM.RC   <- paste(RM.RC, collapse = " ")
          RM.ids  <- paste(RM.ids, collapse = " ")
          
          for (reg in strsplit(RM.RC, " ")[[1]]) {
            u <- DetermineRateConstantUnits("1",
                                            units$base.units$For.Var,
                                            units$base.units$Volume,
                                            units$base.units$Duration,
                                            units$selected.units$For.Var,
                                            units$selected.units$Volume,
                                            units$selected.units$Duration)
            u.add  <- c(u.add,  u$unit)
            b.unit <- c(b.unit, u$unit.base)
            b.val  <- c(b.val,  0)
            ud.add <- c(ud.add, u$unit.d)
          }
        }
        else{
          kr      <- input$eqn_chem_back_k
          RM.bool <- FALSE
          RMs     <- NA
          RM.RC   <- NA
          RM.ids  <- NA
          
          kr.unit <- DetermineRateConstantUnits(coef.RHS$coefs, 
                                                units$base.units$For.Var,
                                                units$base.units$Volume,
                                                units$base.units$Duration,
                                                units$selected.units$For.Var,
                                                units$selected.units$Volume,
                                                units$selected.units$Duration)
          kr.d <- paste0("Reverse rate constant for the reaction of ",
                         paste0(str_split(var.LHS, " ")[[1]], collapse = ", "),
                         " to ",
                         paste0(str_split(var.RHS, " ")[[1]], collapse = ", ")
                        )
          
          p.add  <- c(p.add,  kr)
          u.add  <- c(u.add,  kr.unit$unit)
          ud.add <- c(ud.add, kr.unit$unit.d)
          b.unit <- c(b.unit, kr.unit$unit.base)
          b.val  <- c(b.val,  0)
          d.add  <- c(d.add,  kr.d)
        } 
      } else if (arrow == "forward_only") {
        
        # Set reverse regulator variables to NA
        kr      <- NA
        RM.bool <- FALSE
        RMs     <- NA
        RM.RC   <- NA
        RM.Ids  <- NA
        
        if (input$eqn_options_chem_modifier_forward) {
          kf      <- NA
          FM.bool <- TRUE
          
          f.regs <- BuildRegulatorSide("input$eqn_forward_regulator_", 
                                       "input$eqn_forward_rateConstant_", 
                                       n.f.reg,
                                       var.LHS,
                                       var.RHS,
                                       TRUE)
          FMs     <- f.regs["regulators"]
          FM.RC   <- f.regs["rateConstants"]
          FM.ids  <- f.regs["reg.ids"]
          
          p.add   <- c(p.add, f.regs["P.to.add"][[1]])
          d.add   <- c(d.add, f.regs["P.descriptions"][[1]])
          FMs     <- paste(FMs, collapse = " ")
          FM.RC   <- paste(FM.RC, collapse = " ")
          FM.ids  <- paste(FM.ids, collapse = " ")
          
          for (reg in strsplit(FM.RC, " ")[[1]]) {
            u <- DetermineRateConstantUnits("1",
                                            units$base.units$For.Var,
                                            units$base.units$Volume,
                                            units$base.units$Duration,
                                            units$selected.units$For.Var,
                                            units$selected.units$Volume,
                                            units$selected.units$Duration)
            u.add  <- c(u.add,  u$unit)
            b.unit <- c(b.unit, u$unit.base)
            b.val  <- c(b.val,  0)
            ud.add <- c(ud.add, u$unit.d)
          }
        } else {
          kf <- input$eqn_chem_forward_k
          kf.unit <- DetermineRateConstantUnits(coef.LHS$coefs, 
                                                units$base.units$For.Var,
                                                units$base.units$Volume,
                                                units$base.units$Duration,
                                                units$selected.units$For.Var,
                                                units$selected.units$Volume,
                                                units$selected.units$Duration)
          u.add  <- c(u.add, kf.unit$unit)
          ud.add <- c(ud.add, kf.unit$unit.d)
          b.unit <- c(b.unit, kf.unit$unit.base)
          b.val  <- c(b.val,  0)
          p.add <- c(p.add, kf)
          FM.bool <- FALSE
          FMs <- NA
          FM.RC <- NA
          FM.ids <- NA
        }
      }

      eqn.description = ""
      to.add  <- c(var.LHS, var.RHS)
      to.add  <- to.add[!is.na(to.add)]
      var.add <- paste(to.add, collapse = " ")
      
      #ids
      to.add <- c(id.LHS, id.RHS)
      to.add <- to.add[!is.na(to.add)]
      var.id <- paste(to.add, collapse = " ")
    }
      
    # Add equation to df
    passed.error.check <- CheckParametersForErrors(p.add, 
                                                   vars$species, 
                                                   params$vars.all)
    
    if (passed.error.check) {
      jPrint("passed error check")
      # Store parameters to parameter vector
      par.id.2.store <- c()
      for (i in seq(length(p.add))) {
        p.to.add <- p.add[i]
        print("Build Params")
        par.out <- BuildParameters(p.add[i],
                                   params$vars.all,
                                   id$id.param.seed,
                                   pUnit = u.add[i],
                                   pUnitD = ud.add[i],
                                   pBaseUnit = b.unit[i],
                                   pBaseValue = b.val[i],
                                   pDescription = d.add[i],
                                   pLocation = "Reaction",
                                   pLocationNote = eqn_type)
        StoreParameters(par.out)
        par.id.2.store <- c(par.id.2.store, par.out["par.id"])
        #Pull information
        # params$all
      }
      par.id.2.store <- paste(par.id.2.store, collapse = " ")
      jPrint("parameters stored")
      # Store up params and variables in equation
      
      # Generate eqn ID
      jPrint(id$id.eqn.seed)
      ID.gen <- GenerateId(id$id.eqn.seed, "eqn")
      jPrint(ID.gen)
      id$id.eqn.seed <- id$id.eqn.seed + 1
      ID <- ID.gen["id"]
      jPrint("ID Generated")
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
                          var.add,
                          paste0(p.add, collapse = " "),
                          compartment,
                          eqn.description,
                          var.id,
                          par.id.2.store,
                          comp.id)
      
      eqns$eqn.info[eqns$n.eqns+1, ]      <- row.to.df.info
      eqns$eqn.chem[eqns$n.eqns.chem+1, ] <- row.to.df.chem
      #increment equation numbering
      eqns$n.eqns        <- eqns$n.eqns + 1
      eqns$n.eqns.chem   <- eqns$n.eqns.chem + 1
      eqns$n.eqns.no.del <- eqns$n.eqns.no.del + 1
      jPrint("Finished passed check 1")
    }
  }
  else if (eqn_type == "enzyme_rxn") {
    if (input$eqn_enzyme_law == "MM") {
      eqn.description <- ""
      compartment     <- input$eqnCreate_active_compartment
      law             <- "Michaelis Menten"
      p.add           <- c()
      u.add           <- c()
      ud.add          <- c()
      var.add         <- c()
      b.unit          <- c()
      b.val           <- c()
      var.id          <- c()
      
      substrate  <- input$eqn_enzyme_substrate
      product    <- input$eqn_enzyme_product
      Km         <- input$eqn_enzyme_Km
      arrow      <- "forward_only"
      Km.unit    <- units$selected.units$For.Var
      Km.b.u     <- units$base.units$For.Var
      Km.unit.d  <- paste0("conc (",input$GO_species_unit_choice, ")")
      
      p.add      <- c(p.add, Km)
      var.add    <- c(var.add, substrate, product)
      u.add      <- c(u.add, Km.unit)
      ud.add     <- c(ud.add, Km.unit.d)
      b.unit     <- c(b.unit, Km.b.u)
      b.val      <- c(b.val, 0)
      var.id     <- c(var.id, FindId(substrate), FindId(product))
      
      Km.d <- paste0("Michaelis Menten constant for the enzymatic conversion of ",
                               substrate,
                               " to ",
                               product
                               )
      d.add <- c(Km.d)
      
      if (!input$eqn_options_enzyme_useVmax) {
        kcat      <- input$eqn_enzyme_kcat
        enzyme    <- input$eqn_enzyme_enzyme
        Vmax      <- NA
        kcat.unit <- paste0("1/", units$selected.units$Duration)
        kcat.b.u  <- paste0("1/", units$base.units$Duration)
        kcat.u.d  <- "num <div> time"
        kcat.d <- paste0("Rate constant for the enzymatic conversion of ",
                         substrate,
                         " to ",
                         product
                         )
        
        var.add <- c(var.add, enzyme)
        p.add  <- c(p.add, kcat)
        d.add  <- c(d.add, kcat.d)
        u.add  <- c(u.add, kcat.unit)
        ud.add <- c(ud.add, kcat.u.d)
        b.unit <- c(b.unit, kcat.b.u)
        b.val  <- c(b.val, 0)
        print(var.id)
        print(FindId(enzyme))
        var.id     <- c(var.id, FindId(enzyme))
        print(var.id)
      } else if (input$eqn_options_enzyme_useVmax) {
        Vmax   <- input$eqn_enzyme_Vmax
        kcat   <- NA
        enzyme <- NA
        p.add  <- c(p.add, Vmax)
        
        Vmax.unit <- paste0(units$selected.units$For.Var, "/",
                            units$selected.units$Duration)
        Vmax.b.u  <- paste0(units$base.units$For.Var, "/",
                            units$base.units$Duration)
        Vmax.u.d  <- paste0("conc (",
                            input$GO_species_unit_choice,
                            ") <div> time")
        Vmax.d <- paste0("Maximum velocity for the enzymatic conversion of ",
                         substrate,
                         " to ",
                         product
                         )
        d.add <- c(d.add, Vmax.d)
        u.add <- c(u.add, Vmax.unit)
        ud.add <- c(ud.add, Vmax.u.d)
        b.unit <- c(b.unit, Vmax.b.u)
        b.val  <- c(b.val, 0)
        
      }
      
      passed.error.check <- CheckParametersForErrors(p.add, 
                                                     vars$species, 
                                                     params$vars.all)
      
      if (passed.error.check) {
        par.id.2.store <- c()
        for (i in seq(length(p.add))) {
          par.out <- BuildParameters(p.add[i],
                                     params$vars.all,
                                     id$id.param.seed,
                                     pUnit = u.add[i],
                                     pUnitD = ud.add[i],
                                     pBaseUnit = b.unit[i],
                                     pBaseValue = b.val[i],
                                     pDescription = d.add[i],
                                     pLocation = "Reaction",
                                     pLocationNote = eqn_type)
          StoreParameters(par.out)
          par.id.2.store <- c(par.id.2.store, par.out["par.id"])

        }
        par.id.2.store <- paste(par.id.2.store, collapse = " ")
        
        
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
                            eqn.description,
                            paste0(var.id, collapse = " "),
                            par.id.2.store,
                            comp.id
                            )
        
        row.to.df.enzyme <- c(ID,
                              law,
                              substrate,
                              product, 
                              enzyme,
                              kcat,
                              Km, 
                              Vmax)
        
        eqns$eqn.info[eqns$n.eqns+1, ]       <- row.to.df.info
        eqns$eqn.enzyme[eqns$n.eqns.enz+1, ] <- row.to.df.enzyme
        
        #increment equation numbering
        eqns$n.eqns        <- eqns$n.eqns + 1
        eqns$n.eqns.enz    <- eqns$n.eqns.enz + 1
        eqns$n.eqns.no.del <- eqns$n.eqns.no.del + 1
      }
    }
  }
  else if (eqn_type == "syn") {
    compartment <- input$eqnCreate_active_compartment
    p.add       <- c()
    u.add       <- c()
    ud.add      <- c()
    d.add       <- c()
    var.add     <- c()
    b.unit      <- c()
    b.val       <- c()
    var.id      <- c()
    
    if (input$eqn_syn_law == "rate") {
      
      eqn.d   <- ""
      var     <- input$eqn_syn_rate_var
      rc      <- input$eqn_syn_rate_RC
      rc.b.u  <- paste0(units$base.units$For.Var, 
                        "/", 
                        units$base.units$Duration)
      rc.unit <- paste0(units$base.units$For.Var, 
                        "/", 
                        units$selected.units$Duration)
      rc.ud   <- paste0("conc (",
                        units$selected.units$For.Var,
                        ") <div> time")
      rc.b.v  <- 0
      rc.d    <- paste0("Synthesis rate constant for ", var)
      factor  <- NA
      
      p.add   <- c(p.add, rc)
      var.add <- c(var.add, var)
      d.add   <- c(d.add, rc.d)
      u.add   <- c(u.add, rc.unit)
      ud.add  <- c(ud.add, rc.ud)
      b.unit  <- c(b.unit, rc.b.u)
      b.val   <- c(b.val, rc.b.v)
      var.id  <- c(var.id, FindId(var))
      
    } else if (input$eqn_syn_law == "byFactor") {
      
      eqn.d   <- ""
      var     <- input$eqn_syn_sby_var
      rc      <- input$eqn_syn_sby_RC
      rc.b.u  <- paste0("1/", units$base.units$Duration)
      rc.unit <- paste0("1/", units$selected.units$Duration)
      rc.ud   <- "num <div> time"
      factor  <- input$eqn_syn_sby_factor
      rc.d    <- paste0("Synthesis rate constant of ", 
                        var, 
                        " by factor ", 
                        factor)
      
      d.add   <- c(d.add, rc.d)
      p.add   <- c(p.add, rc)
      var.add <- c(var.add, var)
      u.add   <- c(u.add, rc.unit)
      ud.add  <- c(ud.add, rc.ud)
      b.unit  <- c(b.unit, rc.b.u)
      b.val   <- c(b.val, 0)
      var.id  <- c(var.id, FindId(var), FindId(factor))
      
    }
    passed.error.check <- CheckParametersForErrors(p.add, 
                                                   vars$species, 
                                                   params$vars.all)
    
    if (passed.error.check) {
      
      # Store parameters to parameter vector
      par.id.2.store <- c()
      for (i in seq(length(p.add))) {
        par.out <- BuildParameters(p.add[i],
                                   params$vars.all,
                                   id$id.param.seed,
                                   pUnit = u.add[i],
                                   pUnitD = ud.add[i],
                                   pBaseUnit = b.unit[i],
                                   pBaseValue = b.val[i],
                                   pDescription = d.add[i],
                                   pLocation = "Reaction",
                                   pLocationNote = eqn_type)
        StoreParameters(par.out)
        par.id.2.store <- c(par.id.2.store, par.out["par.id"])
      }
      par.id.2.store <- paste(par.id.2.store, collapse = " ")
      
      # Generate eqn ID
      ID.gen <- GenerateId(id$id.eqn.seed, "eqn")
      id$id.eqn.seed <- id$id.eqn.seed + 1
      ID <- ID.gen["id"]
      
      #Build up Dataframe rows
      row.to.df <- c(ID,
                     input$eqn_syn_law,
                     var,
                     rc, 
                     factor)
      
      row.to.df.info <- c(ID,
                          eqn_type,
                          input$eqn_syn_law,
                          paste0(var.add, collapse = " "),
                          paste0(p.add, collapse = " "),
                          compartment,
                          eqn.d,
                          paste0(var.id, collapse = " "),
                          par.id.2.store,
                          comp.id)
      
      eqns$eqn.info[eqns$n.eqns+1, ]      <- row.to.df.info
      eqns$eqn.syn[eqns$n.eqns.syn+1, ]   <- row.to.df
      #increment equation numbering
      eqns$n.eqns        <- eqns$n.eqns + 1
      eqns$n.eqns.syn    <- eqns$n.eqns.syn + 1
      eqns$n.eqns.no.del <- eqns$n.eqns.no.del + 1
      
    }
  }
  else if (eqn_type == "deg") {
    
    compartment <- input$eqnCreate_active_compartment
    p.add       <- c()
    u.add       <- c()
    d.add       <- c()
    var.add     <- c()
    ud.add      <- c()
    b.unit      <- c()
    b.val       <- c()
    var.id      <- c()
    
    if (input$eqn_deg_to_products) {
      num.deg.products <- as.numeric(input$eqn_deg_num_products)
      product <- c()
      for (i in seq(num.deg.products)) {
        prod <- eval(parse(text = paste0("input$eqn_deg_product_", as.character(i))))
        product <- c(product, prod)
      }
      var.add <- c(var.add, product)
      product <- paste0(product, collapse = " ")
      for (spec in var.add) {var.id <- c(var.id, FindId(spec))}
    } else {
      product <- NA
    }
    
    if (input$eqn_deg_law == "rate") {
      
      eqn.d   <- ""
      var     <- input$eqn_deg_var
      rc      <- input$eqn_deg_rate_RC
      rc.b.u  <- paste0("1/", units$base.units$Duration)
      rc.unit <- paste0("1/", units$selected.units$Duration)
      rc.ud   <- "num <div> time"
      ConcDep <- input$eqn_deg_rate_conc_dependent
      rc.d    <- paste0("Degradation rate constant for ", var)
      
      d.add   <- c(d.add, rc.d)
      p.add   <- c(p.add, rc)
      var.add <- c(var.add, var)
      u.add   <- c(u.add, rc.unit)
      ud.add  <- c(ud.add, rc.ud)
      b.unit  <- c(b.unit, rc.b.u)
      b.val   <- c(b.val, 0)
      var.id  <- c(var.id, FindId(var))
      
      enz    <- NA
      Km     <- NA
      Vmax   <- NA
      kcat   <- NA
      
    } else if (input$eqn_deg_law == "byEnzyme") {
      
      eqn.d   <- ""
      ConcDep <- FALSE
      var     <- input$eqn_deg_var
      Km      <- input$eqn_deg_Km  
      Km.unit <- units$selected.units$For.Var
      Km.b.u  <- units$base.units$For.Var
      Km.ud   <- paste0("conc (",input$GO_species_unit_choice, ")")
      Km.d    <- paste0("Michelias Menten constant for degradation of ", var)

      var.add <- c(var.add, var)
      p.add   <- c(p.add, Km)
      u.add   <- c(u.add, Km.unit)
      d.add   <- c(d.add, Km.d)
      ud.add  <- c(ud.add, Km.ud)
      b.unit  <- c(b.unit, Km.b.u)
      b.val   <- c(b.val, 0)
      var.id  <- c(var.id, FindId(var))
      
      
      if (input$eqn_deg_use_Vmax) {
        Vmax      <- input$eqn_deg_Vmax
        Vmax.b.u  <- paste0(units$base.units$For.Var, "/",
                            units$base.units$Duration)
        Vmax.unit <- paste0(units$selected.units$For.Var, "/",
                            units$selected.units$Duration)
        Vmax.ud   <- paste0("conc (",
                            units$selected.units$For.Var,
                            ") <div> time")
        Vmax.d    <- paste0("Maximum Velocity for degradation of ", var)
        
        p.add  <- c(p.add, Vmax)
        u.add  <- c(u.add, Vmax.unit)
        ud.add <- c(ud.add, Vmax.ud)
        d.add  <- c(d.add, Vmax.d)
        b.unit  <- c(b.unit, Vmax.b.u)
        b.val   <- c(b.val, 0)
        
        rc    <- NA
        enz   <- NA
      } else {
        enz     <- input$eqn_deg_enzyme
        rc      <- input$eqn_deg_kcat
        rc.b.u  <- paste0("1/", units$base.units$Duration)
        rc.unit <- paste0("1/", units$selected.units$Duration)
        rc.ud   <- "num <div> time"
        kcat.d  <- paste0("Enzymatic degradation rate constant of ", 
                         var, 
                         " by  ", 
                         enz)
        
        p.add  <- c(p.add, rc)
        u.add  <- c(u.add, rc.unit)
        ud.add <- c(ud.add, rc.ud)
        d.add  <- c(d.add, kcat.d)
        b.unit <- c(b.unit, rc.b.u)
        b.val  <- c(b.val, 0)
        var.id  <- c(var.id, FindId(enz))
        
        Vmax <- NA
      }
    }
    passed.error.check <- CheckParametersForErrors(p.add, 
                                                   vars$species, 
                                                   params$vars.all)
    
    if (passed.error.check) {
      par.id.2.store <- c()
      # Store parameters to parameter vector
      for (i in seq(length(p.add))) {
        par.out <- BuildParameters(p.add[i],
                                   params$vars.all,
                                   id$id.param.seed,
                                   pUnit = u.add[i],
                                   pUnitD = ud.add[i],
                                   pBaseUnit = b.unit[i],
                                   pBaseValue = b.val[i],
                                   pDescription = d.add[i],
                                   pLocation = "Reaction",
                                   pLocationNote = eqn_type)
        StoreParameters(par.out)
        par.id.2.store <- c(par.id.2.store, par.out["par.id"])
      }
      par.id.2.store <- paste(par.id.2.store, collapse = " ")
      
      # Generate eqn ID
      ID.gen <- GenerateId(id$id.eqn.seed, "eqn")
      id$id.eqn.seed <- id$id.eqn.seed + 1
      ID <- ID.gen["id"]
      
      #Build up Dataframe rows
      row.to.df <- c(ID,
                     input$eqn_deg_law,
                     var,
                     ConcDep,
                     rc,
                     Km, 
                     enz,
                     Vmax,
                     product
                     )
      
      row.to.df.info <- c(ID,
                          eqn_type,
                          input$eqn_deg_law,
                          paste0(var.add, collapse = " "),
                          paste0(p.add, collapse = " "),
                          compartment,
                          eqn.d,
                          paste0(var.id, collapse = " "),
                          par.id.2.store,
                          comp.id
                          )
      
      eqns$eqn.info[eqns$n.eqns+1, ]      <- row.to.df.info
      eqns$eqn.deg[eqns$n.eqns.deg+1, ]   <- row.to.df
      #increment equation numbering
      eqns$n.eqns        <- eqns$n.eqns + 1
      eqns$n.eqns.deg    <- eqns$n.eqns.deg + 1
      eqns$n.eqns.no.del <- eqns$n.eqns.no.del + 1
      
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
    eqn.left   <- input$eqnCreate_custom_eqn_lhs
    eqn.right  <- input$eqnCreate_custom_eqn_rhs
    custom.eqn <- paste0(eqn.left, " = ", eqn.right)
    eqns$additional.eqns <- c(eqns$additional.eqns, custom.eqn)
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
    jPrint("Storing equations to vector")
    if (eqn_type != "rate_eqn" && eqn_type != "time_dependent") {
      eqns$main <- append(eqns$main, equationBuilder())   #store selected variable to list of variables
      eqns$eqn.main.latex <- append(eqns$eqn.main.latex, equationLatexBuilder())
      eqns$eqn.main.mathjax <- append(eqns$eqn.main.mathjax, equationBuilder_MathJax())
      # ids <- GenerateId(id$id.seed, "eqn")
      # unique.id <- ids[[2]]
      # id$id.seed <- ids[[1]]
      # idx.to.add <- nrow(id$id.equations) + 1
      # id$id.equations[idx.to.add, ] <- c(unique.id, equationBuilder())
      # eqns$eqn.descriptions <- c(eqns$eqn.descriptions, "")
    }
  }
  
  #waiter.eqns$hide()
  w.test$hide()
  
  shinyjs::enable("eqnCreate_addEqnToVector")
  
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

    if (!input$eqn_options_enzyme_useVmax) {
      kcat = input$eqn_enzyme_kcat
      textOut <- paste0(substrate," + ", enzyme,  " (", kcat, ")", arrow, "(", Km, ") ", product)
    }
    else if (input$eqn_options_enzyme_useVmax) {
      Vmax = input$eqn_enzyme_Vmax
      textOut <- paste0(substrate, " (", Vmax, ", Enzyme)", arrow, "(", Km, ") ", product)

    }
  }
  else if (input$eqnCreate_type_of_equation == "syn") {
    if (input$eqn_syn_law == "rate") {
      arrow <- "-->"
      var   <- input$eqn_syn_rate_var
      rc    <- input$eqn_syn_rate_RC
      type  <- "syn"
      textOut <- paste0(arrow,
                        "(", rc, ")",
                        var
      )
    } 
    else if (input$eqn_syn_law == "byFactor") {
      arrow  <- "-->"
      var    <- input$eqn_syn_sby_var
      rc     <- input$eqn_syn_sby_RC
      factor <- input$eqn_syn_sby_factor
      type   <- "syn"
      textOut <- paste0(factor,
                        arrow,
                        "(", rc, ")",
                        var
      )
    }
  }
  else if (input$eqnCreate_type_of_equation == "deg") {
    if (input$eqn_deg_to_products) {
      num.deg.products <- as.numeric(input$eqn_deg_num_products)
      product <- ""
      for (i in seq(num.deg.products)) {
        prod <- eval(parse(text = paste0("input$eqn_deg_product_", as.character(i))))
        if (i == num.deg.products) {
          product <- paste0(product, Var2MathJ(prod))
        } else {
          product <- paste0(product, Var2MathJ(prod), " + ")
        }
      }
    } else {
      product <- ""
    }
    if (input$eqn_deg_law == "rate") {
      arrow <- "->"
      var   <- input$eqn_deg_var
      rc    <- input$eqn_deg_rate_RC
      type  <- "deg"
      textOut <- paste0(var,
                        arrow,
                        "(", rc, ")",
                        product
      )
      
    } else if (input$eqn_deg_law == "byEnzyme") {
      arrow <- "->"
      var   <- input$eqn_deg_var
      Km    <- input$eqn_deg_Km
      type  <- "deg"
      
      if (input$eqn_deg_use_Vmax) {
        Vmax <- input$eqn_deg_Vmax
        textOut <- paste0(var,
                          arrow,
                          "(", Km, ", ", Vmax, ")",
                          product
        )
      } else {
        enz  <- input$eqn_deg_enzyme
        kcat <- input$eqn_deg_kcat
        textOut <- paste0(var,
                          arrow,
                          "(", Km, ", ", kcat, ", ", enz, ")",
                          product
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
  updatePickerInput(session,'eqnCreate_edit_select_equation_custom',choices = seq(length(eqns$additional.eqns)))
  updateCheckboxInput(session,"eqn_options_chem_modifier_forward",value = FALSE)
  updateNumericInput(session, "eqn_options_chem_num_forward_regulators", value = 1)
  updateCheckboxInput(session,"eqn_options_chem_modifier_reverse",value = FALSE)
  updateNumericInput(session, "eqn_options_chem_num_reverse_regulators", value = 1)
  #updatePickerInput(session, "eqnCreate_rate_firstvar", choices = params$vars.all)

})

#-------------------------------------------------------------------------------

# Equation Text outputs

#-------------------------------------------------------------------------------

output$eqnCreate_showEquationBuilding <- renderUI({
  withMathJax(
    equationBuilder_MathJax()
  )
  })
output$test_mathjax_equations <- renderUI({
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
  # Activates and deactivates button depending how many equations there are
  # Updates pickerInput with number of equations
  if (eqns$n.eqns > 0) {
    out <- seq(eqns$n.eqns)
    shinyjs::enable("createEqn_delete_equation_button")
  } else {
    out <- NULL
    shinyjs::disable("createEqn_delete_equation_button")
  }

  updatePickerInput(session
                    ,"eqnCreate_delete_equation"
                    ,choices = out )#as.character(seq(eqns$n.eqns)))
})

observeEvent(eqns$additional.eqns, {
  # Activates and deactivates button depending how many custom equations there are
  # Updates pickerInput with number of custom equations

  if (length(eqns$additional.eqns) > 0) {
    out <- seq(length(eqns$additional.eqns))
    shinyjs::enable("createEqn_delete_custom_equation_button")
    updatePickerInput(session
                      ,"eqnCreate_delete_equation_custom"
                      ,choices = out
    )
  } else {
    shinyjs::disable("createEqn_delete_custom_equation_button")
    updatePickerInput(session
                      ,"eqnCreate_delete_equation_custom"
                      ,choices = "No Custom Equations Built"
    )
  }
  
  # updatePickerInput(session
  #                   ,"eqnCreate_delete_equation_custom"
  #                   ,choices = out
  #                   )
})

observeEvent(input$eqnCreate_delete_eqn_type, {
  # When radio button options change to delete eqns or custom change viewing tab
  
  out <- "Equations" #catch if more tabs are added to avoid error
  if (input$eqnCreate_delete_eqn_type == "Equations") {
    out <- "Equation"
  } else if (input$eqnCreate_delete_eqn_type == "Custom") {
    out <- "Custom"
  }
  updateTabsetPanel(session,
                    "eqns_tabbox",
                    selected = out)
})

observeEvent(input$createEqn_delete_custom_equation_button, {
  # Delete custom equation from data was stated by user
  jPrint("Deleting Custom Eqn")
  jPrint(eqns$additional.eqns)
  # Get eqn row
  eqn_to_delete <- as.numeric(input$eqnCreate_delete_equation_custom)
  jPrint(eqn_to_delete)
  # Remove from custom equation vector
  eqns$additional.eqns <- eqns$additional.eqns[-eqn_to_delete]
  jPrint(eqn_to_delete)
})

observeEvent(input$createEqn_delete_equation_button, {
  # Delete associated parameters used in this equation if they aren't used elsewhere
  eqn_to_delete <- as.numeric(input$eqnCreate_delete_equation)
  # Find parameters used in this equation
  eqn.row <- eqns$eqn.info[eqn_to_delete, 1:ncol(eqns$eqn.info)]
  # Eqn ID
  eqn.id <- eqn.row$ID[1]
  eqn.type <- eqn.row$EqnType[1]
  eqn.param <- eqn.row$RateConstants[1]

  # Find Matching Id in possible tables
  if (eqn.type == "chem_rxn") {
    for (i in 1:nrow(eqns$eqn.chem)) {
      id <- eqns$eqn.chem$ID[i]
      if (eqn.id == id){
        idx <- i
        break
      }
    }
    eqns$eqn.chem <- eqns$eqn.chem[-idx, 1:ncol(eqns$eqn.chem)] 
  } else if (eqn.type == "enzyme_rxn") {
    for (i in 1:nrow(eqns$eqn.enzyme)) {
      id <- eqns$eqn.enzyme$ID[i]
      if (eqn.id == id){
        idx <- i
        break
      }
    }
    eqns$eqn.enzyme <- eqns$eqn.enzyme[-idx, 1:ncol(eqns$eqn.enzyme)]
  } else if (eqn.type == "syn") {
    for (i in 1:nrow(eqns$eqn.syn)) {
      id <- eqns$eqn.syn$ID[i]
      if (eqn.id == id){
        idx <- i
        break
      }
    }
    eqns$eqn.syn <- eqns$eqn.syn[-idx, 1:ncol(eqns$eqn.syn)]
  } else if (eqn.type == "deg") {
    for (i in 1:nrow(eqns$eqn.deg)) {
      id <- eqns$eqn.deg$ID[i]
      if (eqn.id == id){
        idx <- i
        break
      }
    }
    eqns$eqn.deg <- eqns$eqn.deg[-idx, 1:ncol(eqns$eqn.deg)]
  }
  
  #remove equation from all sections
  eqns$eqn.info <- eqns$eqn.info[-eqn_to_delete, 1:ncol(eqns$eqn.info)] #delete equation from dataframe
  eqns$main <- eqns$main[-eqn_to_delete] #removes equation from equation list
  eqns$n.eqns <- eqns$n.eqns - 1
  eqns$eqn.descriptions <- eqns$eqn.descriptions[-eqn_to_delete]
  
  # #check to see if that parameter is used elsewhere and save it if it is
  #extract all possible parameters from eqn.info 
  p <- strsplit(eqn.param, " ")[[1]]
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
  jPrint(eqns$eqn.syn)
  jPrint(eqns$eqn.deg)
  jPrint(eqns$eqn.main.latex)
  jPrint(eqns$additional.eqns)
})

observeEvent(input$refresh_text_eqns, {
  # Run new functions to rewrite RV storing text eqns
  jPrint(eqns$main)
  eqns$main <- ReCalcTextEqns(eqns$eqn.info,
                              eqns$eqn.chem,
                              eqns$eqn.enzyme,
                              eqns$eqn.syn,
                              eqns$eqn.deg,
                              eqns$main)
  jPrint("After Rewrite")
  jPrint(eqns$main)
})

#--------------------------Random----------------------------------------------

