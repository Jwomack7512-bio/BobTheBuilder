
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
                                     allowRepeatParams = FALSE,
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
  repeated.parameters <- TRUE
  for (var in paramsToCheck) {
    varCheck      <- variableCheck(var, 
                                   allSpeciesVar, 
                                   allParamVariables,
                                   allowRepeatParams)
    pass.check    <- varCheck[[1]]
    error.message <- varCheck[[2]]
    error.code    <- varCheck[[3]]
    repeat.param  <- varCheck[[4]]
    if (repeat.param) {repeated.parameters <- TRUE}
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
  out <- list(passed.test, repeated.parameters)
  return(out)
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
                      "UnitDescription",
                      "BaseUnit",
                      "BaseValue",
                      "Description", 
                      "Type",
                      "Type.Note")

  # Store Params to List
  rv.PARAMETERS$parameters[[nPar]] <- p.entry
  names(rv.PARAMETERS$parameters)[nPar] <- par.id
  # Add to Parameter Ids
  rv.ID$id.param.seed <- rv.ID$id.param.seed + 1
  rv.ID$id.df[nrow(rv.ID$id.df) + 1,] <- c(par.id, p.entry$Name)
  
  # Rewrite the loop parameter table
  # rv.PLOT.LOOP$loop.parameters <- rv.PARAMETERS$parameters.df %>% 
  #   select("Name", "Value", "Unit", "Description")
}

StoreParamsEqn <- function(pToAdd, 
                           pValue = 0, 
                           pDescription = "", 
                           pUnit = "pH",
                           pLocation = "reactionType") {
  
  if (!(pToAdd %in% names(rv.PARAMETERS$parameters))) {
    # Generate Parameter ID
    ids <- GenerateId(rv.ID$id.var.seed, "parameter")
    unique.id <- ids[[2]]
    rv.ID$id.var.seed <- ids[[1]]
    idx.to.add <- nrow(rv.ID$id.df) + 1
    rv.ID$id.df[idx.to.add, ] <- c(unique.id, pToAdd)
    # Add Parameter to Parameter List
    nPar <- length(rv.PARAMETERS$parameters)
    rv.PARAMETERS$parameters[[nPar + 1]] <- list(Name = pToAdd,
                                      ID = ids[[1]],
                                      Value = pValue,
                                      Unit = pUnit,
                                      Description = pDescription,
                                      Type = pLocation)

    # Assign List Name
    names(rv.PARAMETERS$parameters)[nPar + 1] <- pToAdd

    # Rewrite the loop parameter table
    # rv.PLOT.LOOP$loop.parameters <- rv.PARAMETERS$parameters.df %>% 
    #   select("Name", "Value", "Unit", "Description")
    
  }
}

StoreParamsRate <- function(parameterToAdd) {
  
  if (!rv.PARAMETERS$first.rate.eqn.stored) rv.PARAMETERS$first.rate.eqn.stored = TRUE

}

build_db_row <- function(eqn_type, RHS_coef, RHS_var, LHS_coef,
                         LHS_var,arrow_type, kf, kr, description){
  row_out <- c(eqn_type, RHS_coef, RHS_var, LHS_coef, LHS_var,arrow_type,
               kf, kr, description)
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
  updatePickerInput(session, "eqnCreate_recep", choices = sort(rv.SPECIES$species.names))
  updatePickerInput(session, "eqnCreate_lig", choices = sort(rv.SPECIES$species.names))

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

observeEvent({input$eqnCreate_active_compartment
              rv.COMPARTMENTS$compartments
              rv.SPECIES$species}, {
  req(!is_empty(rv.SPECIES$species.df))

  rv.SPECIES$df.by.compartment <- 
    rv.SPECIES$species.df %>% filter(Compartment == input$eqnCreate_active_compartment)
})



# Add Equation Event -----------------------------------------------------------
observeEvent(input$eqnCreate_addEqnToVector, {
  #waiter.rv.REACTIONS$show()
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
  p.id               <- c() # Parameter Ids
  var.id             <- c() # Variable Ids
  
  # browser()
  if (eqn_type == "chem_rxn") {
    #this will hold all the functions for chemical reactions:
    # Currently holds: Mass Action, Regulated Mass Action
    compartment <- input$eqnCreate_active_compartment
    comp.id     <- FindId(compartment)
    
    # Number of variables on RHS/LHS of equation
    n.RHS = as.numeric(input$eqnCreate_num_of_eqn_RHS) 
    n.LHS = as.numeric(input$eqnCreate_num_of_eqn_LHS) 
    
    if (input$eqn_chem_law == "MA") { # Mass Action
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
          # Rate Constants
          kf    <- input$eqn_chem_forward_k
          kr    <- input$eqn_chem_back_k
          kf.u <- DetermineRateConstantUnits(coef.LHS$coefs, 
                                             rv.UNITS$units.base$For.Var,
                                             rv.UNITS$units.base$Volume,
                                             rv.UNITS$units.base$Duration,
                                             rv.UNITS$units.selected$For.Var,
                                             rv.UNITS$units.selected$Volume,
                                             rv.UNITS$units.selected$Duration)
          kr.u <- DetermineRateConstantUnits(coef.RHS$coefs, 
                                             rv.UNITS$units.base$For.Var,
                                             rv.UNITS$units.base$Volume,
                                             rv.UNITS$units.base$Duration,
                                             rv.UNITS$units.selected$For.Var,
                                             rv.UNITS$units.selected$Volume,
                                             rv.UNITS$units.selected$Duration)
          kf.unit   <- kf.u$unit
          kr.unit   <- kr.u$unit
          kf.b.unit <- kf.u$unit.base
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
                                             rv.UNITS$units.base$For.Var,
                                             rv.UNITS$units.base$Volume,
                                             rv.UNITS$units.base$Duration,
                                             rv.UNITS$units.selected$For.Var,
                                             rv.UNITS$units.selected$Volume,
                                             rv.UNITS$units.selected$Duration)
          
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
      
      # Number of regulators for forward/reverse reactions
      n.f.reg = as.numeric(input$eqn_options_chem_num_forward_regulators) 
      n.r.reg = as.numeric(input$eqn_options_chem_num_reverse_regulators) 
      
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
                                            rv.UNITS$units.base$For.Var,
                                            rv.UNITS$units.base$Volume,
                                            rv.UNITS$units.base$Duration,
                                            rv.UNITS$units.selected$For.Var,
                                            rv.UNITS$units.selected$Volume,
                                            rv.UNITS$units.selected$Duration)
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
                                                rv.UNITS$units.base$For.Var,
                                                rv.UNITS$units.base$Volume,
                                                rv.UNITS$units.base$Duration,
                                                rv.UNITS$units.selected$For.Var,
                                                rv.UNITS$units.selected$Volume,
                                                rv.UNITS$units.selected$Duration)
          
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
                                            rv.UNITS$units.base$For.Var,
                                            rv.UNITS$units.base$Volume,
                                            rv.UNITS$units.base$Duration,
                                            rv.UNITS$units.selected$For.Var,
                                            rv.UNITS$units.selected$Volume,
                                            rv.UNITS$units.selected$Duration)
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
                                                rv.UNITS$units.base$For.Var,
                                                rv.UNITS$units.base$Volume,
                                                rv.UNITS$units.base$Duration,
                                                rv.UNITS$units.selected$For.Var,
                                                rv.UNITS$units.selected$Volume,
                                                rv.UNITS$units.selected$Duration)
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
                                            rv.UNITS$units.base$For.Var,
                                            rv.UNITS$units.base$Volume,
                                            rv.UNITS$units.base$Duration,
                                            rv.UNITS$units.selected$For.Var,
                                            rv.UNITS$units.selected$Volume,
                                            rv.UNITS$units.selected$Duration)
            u.add  <- c(u.add,  u$unit)
            b.unit <- c(b.unit, u$unit.base)
            b.val  <- c(b.val,  0)
            ud.add <- c(ud.add, u$unit.d)
          }
        } else {
          kf <- input$eqn_chem_forward_k
          kf.unit <- DetermineRateConstantUnits(coef.LHS$coefs, 
                                                rv.UNITS$units.base$For.Var,
                                                rv.UNITS$units.base$Volume,
                                                rv.UNITS$units.base$Duration,
                                                rv.UNITS$units.selected$For.Var,
                                                rv.UNITS$units.selected$Volume,
                                                rv.UNITS$units.selected$Duration)
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

    # Add equation to DF
    error.check <- CheckParametersForErrors(p.add, 
                                            rv.SPECIES$species.names,
                                            names(rv.PARAMETERS$parameters))
    passed.error.check <- error.check[[1]]
    
    if (passed.error.check) {
      # Store parameters to parameter vector
      par.id.2.store <- c()
      for (i in seq(length(p.add))) {
        p.to.add <- p.add[i]
        par.out <- BuildParameters(p.add[i],
                                   names(rv.PARAMETERS$parameters),
                                   rv.ID$id.param.seed,
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
        # rv.PARAMETERS$all
      }
      par.id.2.store <- paste(par.id.2.store, collapse = " ")

      # Generate eqn ID
      ID.gen <- GenerateId(rv.ID$id.eqn.seed, "eqn")
      rv.ID$id.eqn.seed <- rv.ID$id.eqn.seed + 1
      ID.to.add <- ID.gen[["id"]]

      # Add overall data to eqn list data structure
      eqn.list.entry <- list(ID = ID.to.add,
                             Eqn.Type = eqn_type,
                             Law = law,
                             Species = var.add,
                             Rate.Constants = paste0(p.add, collapse = " "),
                             Compartment = compartment,
                             Description = eqn.description,
                             Species.Id = var.id,
                             Parameters.Id = par.id.2.store,
                             Compartment.Id = comp.id,
                             Equation.Text = equationBuilder(),
                             Equation.Latex = equationLatexBuilder(),
                             Equation.MathJax = equationMathJaxBuilder())
      

          
      n.eqns <- length(rv.REACTIONS$reactions)
      rv.REACTIONS$reactions[[n.eqns + 1]] <- eqn.list.entry
      names(rv.REACTIONS$reactions)[n.eqns+1] <- ID.to.add
      
      eqn.chem.entry <- list(ID = ID.to.add,
                             Law = law,
                             LHS.coef = coef.LHS, 
                             LHS.var = var.LHS, 
                             RHS.coef = coef.RHS, 
                             RHS.var = var.RHS, 
                             arrow = arrow,
                             kf = kf, 
                             kr = kr,
                             FM.bool = FM.bool, 
                             FMs = FMs, 
                             FM.rateC = FM.RC,
                             RM.bool = RM.bool, 
                             RMs = RMs, 
                             RM.rateC = RM.RC)
      
      n.chem <- length(rv.REACTIONS$massAction)
      rv.REACTIONS$massAction[[n.chem + 1]] <- eqn.chem.entry
      names(rv.REACTIONS$massAction)[n.chem + 1] <- ID.to.add
    }
  }
  else if (eqn_type == "enzyme_rxn") {
    if (input$eqn_enzyme_law == "MM") {
      eqn.description <- ""
      compartment     <- input$eqnCreate_active_compartment
      comp.id         <- FindId(compartment)
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
      Km.unit    <- rv.UNITS$units.selected$For.Var
      Km.b.u     <- rv.UNITS$units.base$For.Var
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
        kcat.unit <- paste0("1/", rv.UNITS$units.selected$Duration)
        kcat.b.u  <- paste0("1/", rv.UNITS$units.base$Duration)
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
        var.id     <- c(var.id, FindId(enzyme))
      } else if (input$eqn_options_enzyme_useVmax) {
        Vmax   <- input$eqn_enzyme_Vmax
        kcat   <- NA
        enzyme <- NA
        p.add  <- c(p.add, Vmax)
        
        Vmax.unit <- paste0(rv.UNITS$units.selected$For.Var, "/",
                            rv.UNITS$units.selected$Duration)
        Vmax.b.u  <- paste0(rv.UNITS$units.base$For.Var, "/",
                            rv.UNITS$units.base$Duration)
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
      
      error.check <- CheckParametersForErrors(p.add, 
                                              rv.SPECIES$species.names,
                                              names(rv.PARAMETERS$parameters))
      passed.error.check <- error.check[[1]]
      
      if (passed.error.check) {
        par.id.2.store <- c()
        for (i in seq(length(p.add))) {
          par.out <- BuildParameters(p.add[i],
                                     names(rv.PARAMETERS$parameters),
                                     rv.ID$id.param.seed,
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
        ID.gen <- GenerateId(rv.ID$id.eqn.seed, "eqn")
        rv.ID$id.eqn.seed <- rv.ID$id.eqn.seed + 1
        ID.to.add <- ID.gen[["id"]]
        
        # Add overall data to eqn list data structure
        eqn.list.entry <- list(ID = ID.to.add,
                               Eqn.Type = eqn_type,
                               Law = law,
                               Species = paste0(var.add, collapse = " "),
                               Rate.Constants = paste0(p.add, collapse = " "),
                               Compartment = compartment,
                               Description = eqn.description,
                               Species.Id = paste0(var.id, collapse = " "),
                               Parameters.Id = par.id.2.store,
                               Compartment.Id = comp.id,
                               Equation.Text = equationBuilder(),
                               Equation.Latex = equationLatexBuilder(),
                               Equation.MathJax = equationMathJaxBuilder())
        
        n.eqns <- length(rv.REACTIONS$reactions)
        rv.REACTIONS$reactions[[n.eqns + 1]] <- eqn.list.entry
        names(rv.REACTIONS$reactions)[n.eqns+1] <- ID.to.add
        
        eqn.enz.entry  <- list(ID = ID.to.add,
                               Law = law,
                               Substrate = substrate, 
                               Product = product, 
                               Enzyme = enzyme, 
                               kcat = kcat,
                               Km = Km, 
                               Vmax = Vmax)
        
        n <- length(rv.REACTIONS$michaelisMenten)
        rv.REACTIONS$michaelisMenten[[n + 1]] <- eqn.enz.entry
        names(rv.REACTIONS$michaelisMenten)[n + 1] <- ID.to.add
      }
    }
  }
  else if (eqn_type == "syn") {
    compartment <- input$eqnCreate_active_compartment
    comp.id     <- FindId(compartment)
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
      rc.b.u  <- paste0(rv.UNITS$units.base$For.Var, 
                        "/", 
                        rv.UNITS$units.base$Duration)
      rc.unit <- paste0(rv.UNITS$units.base$For.Var, 
                        "/", 
                        rv.UNITS$units.selected$Duration)
      rc.ud   <- paste0("conc (",
                        rv.UNITS$units.selected$For.Var,
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
      rc.b.u  <- paste0("1/", rv.UNITS$units.base$Duration)
      rc.unit <- paste0("1/", rv.UNITS$units.selected$Duration)
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
    error.check <- CheckParametersForErrors(p.add, 
                                            rv.SPECIES$species.names,
                                            names(rv.PARAMETERS$parameters))
    passed.error.check <- error.check[[1]]
    
    if (passed.error.check) {
      
      # Store parameters to parameter vector
      par.id.2.store <- c()
      for (i in seq(length(p.add))) {
        par.out <- BuildParameters(p.add[i],
                                   names(rv.PARAMETERS$parameters),
                                   rv.ID$id.param.seed,
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
      ID.gen <- GenerateId(rv.ID$id.eqn.seed, "eqn")
      rv.ID$id.eqn.seed <- rv.ID$id.eqn.seed + 1
      ID.to.add <- ID.gen[["id"]]
      
      # Add overall data to eqn list data structure
      eqn.list.entry <- list(ID = ID.to.add,
                             Eqn.Type = eqn_type,
                             Law = input$eqn_syn_law,
                             Species = paste0(var.add, collapse = " "),
                             Rate.Constants = paste0(p.add, collapse = " "),
                             Compartment = compartment,
                             Description = eqn.d,
                             Species.Id = paste0(var.id, collapse = " "),
                             Parameters.Id = par.id.2.store,
                             Compartment.Id = comp.id,
                             Equation.Text = equationBuilder(),
                             Equation.Latex = equationLatexBuilder(),
                             Equation.MathJax = equationMathJaxBuilder())
      
      n.eqns <- length(rv.REACTIONS$reactions)
      rv.REACTIONS$reactions[[n.eqns + 1]] <- eqn.list.entry
      names(rv.REACTIONS$reactions)[n.eqns+1] <- ID.to.add
      
      eqn.syn.entry  <- list(ID = ID.to.add,
                             Law = input$eqn_syn_law,
                             VarSyn = var, 
                             RC = rc, 
                             Factor = factor)
      
      n <- length(rv.REACTIONS$synthesis)
      rv.REACTIONS$synthesis[[n + 1]] <- eqn.syn.entry
      names(rv.REACTIONS$synthesis)[n + 1] <- ID.to.add
    }
  }
  else if (eqn_type == "deg") {
    
    compartment <- input$eqnCreate_active_compartment
    comp.id     <- FindId(compartment)
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
      rc.b.u  <- paste0("1/", rv.UNITS$units.base$Duration)
      rc.unit <- paste0("1/", rv.UNITS$units.selected$Duration)
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
      Km.unit <- rv.UNITS$units.selected$For.Var
      Km.b.u  <- rv.UNITS$units.base$For.Var
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
        Vmax.b.u  <- paste0(rv.UNITS$units.base$For.Var, "/",
                            rv.UNITS$units.base$Duration)
        Vmax.unit <- paste0(rv.UNITS$units.selected$For.Var, "/",
                            rv.UNITS$units.selected$Duration)
        Vmax.ud   <- paste0("conc (",
                            rv.UNITS$units.selected$For.Var,
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
        rc.b.u  <- paste0("1/", rv.UNITS$units.base$Duration)
        rc.unit <- paste0("1/", rv.UNITS$units.selected$Duration)
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
    
    error.check <- CheckParametersForErrors(p.add, 
                                            rv.SPECIES$species.names,
                                            names(rv.PARAMETERS$parameters))
    passed.error.check <- error.check[[1]]
    
    if (passed.error.check) {
      par.id.2.store <- c()
      # Store parameters to parameter vector
      for (i in seq(length(p.add))) {
        par.out <- BuildParameters(p.add[i],
                                   names(rv.PARAMETERS$parameters),
                                   rv.ID$id.param.seed,
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
      ID.gen <- GenerateId(rv.ID$id.eqn.seed, "eqn")
      rv.ID$id.eqn.seed <- rv.ID$id.eqn.seed + 1
      ID.to.add <- ID.gen[["id"]]
      
      # Add overall data to eqn list data structure
      eqn.list.entry <- list(ID = ID.to.add,
                             Eqn.Type = eqn_type,
                             Law = input$eqn_deg_law,
                             Species = paste0(var.add, collapse = " "),
                             Rate.Constants = paste0(p.add, collapse = " "),
                             Compartment = compartment,
                             Description = eqn.d,
                             Species.Id = paste0(var.id, collapse = " "),
                             Parameters.Id = par.id.2.store,
                             Compartment.Id = comp.id,
                             Equation.Text = equationBuilder(),
                             Equation.Latex = equationLatexBuilder(),
                             Equation.MathJax = equationMathJaxBuilder())
      
      n.eqns <- length(rv.REACTIONS$reactions)
      rv.REACTIONS$reactions[[n.eqns + 1]] <- eqn.list.entry
      names(rv.REACTIONS$reactions)[n.eqns+1] <- ID.to.add
      
      eqn.deg.entry  <- list(ID = ID.to.add,
                             Law = input$eqn_deg_law,
                             VarDeg = var,
                             ConcDep = ConcDep,
                             RC = rc,
                             Km = Km, 
                             Enz = enz,
                             Vmax = Vmax,
                             Prods = product
                             )
      
      n <- length(rv.REACTIONS$degradation)
      rv.REACTIONS$degradation[[n + 1]] <- eqn.deg.entry
      names(rv.REACTIONS$degradation)[n + 1] <- ID.to.add
    }
  }
  else if (eqn_type == "rate_eqn") {
    eqn.left   <- input$eqnCreate_custom_eqn_lhs
    eqn.right  <- input$eqnCreate_custom_eqn_rhs
    custom.eqn <- paste0(eqn.left, " = ", eqn.right)
    rv.REACTIONS$additional.eqns <- c(rv.REACTIONS$additional.eqns, custom.eqn)
  }
  else if (eqn_type == "time_dependent") {
    TD_left <- input$eqnCreate_time_dependent_firstvar
    TD_right <- input$eqnCreate_time_dependent_equation
    TD_eqn <- paste0(TD_left, "=", TD_right)
    rv.REACTIONS$additional.eqns <- c(rv.REACTIONS$additional.eqns, TD_eqn)
    rv.PARAMETERS$parameters.based.on.other.values <- TD_left
  }
  
  # Tracks subscripts of eqns
  rv.REACTIONS$reaction.id.counter <- rv.REACTIONS$reaction.id.counter + 1
  
  #waiter.rv.REACTIONS$hide()
  w.test$hide()
  
  shinyjs::enable("eqnCreate_addEqnToVector")
  
  if (input$checkbox_modal_keep_active_add_eqn) {
    toggleModal(session,
                "modal_create_equations",
                toggle = "close")
  }

  
  #solveForDiffEqs()
})

# Equation Main Table Render ---------------------------------------------------
output$main_eqns_table <- renderRHandsontable({
  override <- rv.REFRESH$refresh.eqn.table
  
  if (nrow(rv.REACTIONS$reactions.df) == 0) {
    temp <- data.frame(c("Press addition button below to add equations
                       to compartment."))
    temp <- transpose(temp)
    colnames(temp) <- c("Instructions")
    rhandsontable(temp,
                  rowHeaders = NULL,
                  overflow = "visible",
                  colHeaderWidth = 100,
                  stretchH = "all",
                  readOnly = TRUE
    ) %>%
      hot_cols(manualColumnMove = FALSE,
               manualColumnResize = FALSE,
               halign = "htCenter",
               valign = "htMiddle",
               renderer = "
         function (instance, td, row, col, prop, value, cellProperties) {
           Handsontable.renderers.NumericRenderer.apply(this, arguments);
           if (row % 2 == 0) {
            td.style.background = '#f9f9f9';
           } else {
            td.style.background = 'white';
           };
         }") %>%
      hot_rows(rowHeights = 30) %>%
      hot_context_menu(allowRowEdit = FALSE,
                       allowColEdit = FALSE
      )
    } else {
    df.to.show <- select(rv.REACTIONS$reactions.df,
                         "Equation.Text",
                         "Eqn.Type",
                         "Law",
                         "Compartment")
    
    df.to.show <- as.data.frame(df.to.show)
    colnames(df.to.show) <- c("Equation", 
                              "Type", 
                              "Law", 
                              "Compartment")
    
    hot <- rhandsontable(df.to.show,
                  overflow = "visible",
                  readOnly = TRUE,
                  selectCallback = TRUE,
                  colHeaderWidth = 100,
                  stretchH = "all",
                  fillHandle = FALSE
    ) %>%
      hot_cols(
        colWidth = c(60, 20, 20, 20),
        manualColumnMove = FALSE,
        manualColumnResize = TRUE,
        halign = "htCenter",
        valign = "htMiddle",
        renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (row % 2 == 0) {
              td.style.background = '#f9f9f9';
              td.style.color = 'black';
             } else {
              td.style.background = 'white';
              td.style.color = 'black';
             };
           }") %>%
      #hot_col("Variable Name", readOnly = TRUE) %>%
      hot_rows(rowHeights = 30) %>%
      hot_context_menu(
        allowRowEdit = FALSE,
        allowColEdit = FALSE
      )
    csv = list(
      name = "Download",
      callback  = htmlwidgets::JS(
        "function (key, options) {
           var csv = csvString(this, sep=',', dec='.');
           var link = document.createElement('a');
           link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
             encodeURIComponent(csv));
           link.setAttribute('download', 'equations.csv');
           document.body.appendChild(link);
           link.click();
           document.body.removeChild(link);
         }"
      )
    )
    
    # context menu callback has 3 inputs, key, selection, clickevent
    eqnEdit = list(
      name = "Edit",
      callback = htmlwidgets::JS(
        "function(key, options) {
                Shiny.setInputValue('edit_equation_menu_item', options);
              }"
      )
    )
    
    eqnAdd = list(
      name = "Add",
      callback = htmlwidgets::JS(
        "function(key, options) {
                Shiny.setInputValue('add_equation_menu_item', options);
              }"
      )
    )
    
    eqnDel = list(
      name = "Delete",
      callback = htmlwidgets::JS(
        "function(key, options) {
                Shiny.setInputValue('delete_equation_menu_item', options);
              }"
      )
    )
    
    hot$x$contextMenu <- list(items = list(eqnAdd, 
                                           eqnEdit,
                                           eqnDel,
                                           csv))
    hot
  }
})

observeEvent(input$add_equation_menu_item, {
  toggleModal(
    session = session,
    modalId = "modal_create_equations",
    toggle = "open"
  )
})

observeEvent(input$delete_equation_menu_item, {
  toggleModal(
    session = session,
    modalId = "modal_delete_equations",
    toggle = "open"
  )
})

observeEvent(input$edit_equation_menu_item, {
  print("Edit option")
  
  start.row <- input$edit_equation_menu_item[[1]]
  start.col <- input$edit_equation_menu_item[[2]]
  end.row   <- input$edit_equation_menu_item[[3]]
  end.col   <- input$edit_equation_menu_item[[4]]
  
  # Update equation number with row from edit
  updatePickerInput(
    session = session, 
    inputId = "eqnCreate_edit_select_equation",
    selected = as.character(start.row+1)
  )
  
  # Open Edit Equation Modal
  toggleModal(
    session = session,
    modalId = "modal_edit_equations",
    toggle = "open"
  )
  
  
})

# Build Text Equation for UI viewer --------------------------------------------
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
  else if (input$eqnCreate_type_of_equation == "rate_eqn") {
    rate_left <- input$eqnCreate_rate_firstvar
    rate_right <- input$eqnCreate_rate_equation
    textOut <- paste0(rate_left, " = ", rate_right)
  }
  else if (input$eqnCreate_type_of_equation == "time_dependent") {
    TD_left <- input$eqnCreate_time_dependent_firstvar
    TD_right <- input$eqnCreate_time_dependent_equation
    textOut <- paste0(TD_left, "=", TD_right)
  }
  else {textOut <- "ERROR"}
  return(textOut)
})

# Rate Equation Store Parameter/Time Dependent ---------------------------------
observeEvent(input$eqnCreate_time_dependent_store_new_parameter, {
  new_parameter <- input$eqnCreate_time_dependent_parameters
  rv.PARAMETERS$time.dep.vars <- append(rv.PARAMETERS$time.dep.vars, new_parameter)
  updateTextInput(session,
                  "eqnCreate_time_dependent_parameters",
                  value = "")
})

# When Equation Add button pressed, store vars to respective places
observeEvent(input$eqnCreate_addEqnToVector, {
  eqn_type <- input$eqnCreate_type_of_equation
  
  #Add additional parameters in rate equation to proper rv
  if (eqn_type == "rate_eqn") {
    if (isTruthy(input$eqnCreate_rate_new_parameter)) { #if new parameters were entered (var1,var2,var3)
      num.param.to.add <- length(str_split(input$eqnCreate_rate_new_parameter, ","))
      parameters.to.add <- str_split(input$eqnCreate_rate_new_parameter, ",")
      for (i in seq(num.param.to.add)) {
        new.parameter <- gsub(" ", "", parameters.to.add[[i]], fixed = TRUE)
        phrase <- paste0("Added Param ", new.parameter)
        #rv.PARAMETERS$rate.eqn.vars <- append(rv.PARAMETERS$rate.eqn.vars, new.parameter)
        StoreParamsRate(new.parameter)
      }
      
      #remove parameter and value and comment from paramter vectors 
      param.to.remove = input$eqnCreate_rate_firstvar
      rv.PARAMETERS$rate.params <- append(rv.PARAMETERS$rate.params, param.to.remove)
      #search named vector for this parameter and remove
      if (param.to.remove %in% rv.PARAMETERS$eqns.vars) {
        # idx.of.param = which(rv.PARAMETERS$eqns.vars == param.to.remove)
        # rv.PARAMETERS$eqns.vars = rv.PARAMETERS$eqns.vars[-idx.of.param]
        # rv.PARAMETERS$eqns.vals = rv.PARAMETERS$eqns.vals[-idx.of.param]
        # rv.PARAMETERS$eqns.comments = rv.PARAMETERS$eqns.comments[-idx.of.param]
        # if (length(rv.PARAMETERS$eqns.vars) == 0) {
        #    rv.PARAMETERS$first.param.eqn.stored = FALSE
        # }
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

  updateNumericInput(session, 
                     "eqnCreate_num_of_eqn_LHS", 
                     value = 1)
  updateNumericInput(session, 
                     "eqnCreate_num_of_eqn_RHS", 
                     value = 1)
  updatePickerInput(session,
                    'eqnCreate_edit_select_equation',
                    choices = seq(length(rv.REACTIONS$reactions)))
  updatePickerInput(session,
                    'eqnCreate_delete_select_equation',
                    choices = seq(length(rv.REACTIONS$reactions)))
  updatePickerInput(session,
                    'eqnCreate_edit_select_equation_custom',
                    choices = seq(length(rv.REACTIONS$additional.eqns)))
  updateCheckboxInput(session,
                      "eqn_options_chem_modifier_forward",
                      value = FALSE)
  updateNumericInput(session, 
                     "eqn_options_chem_num_forward_regulators", 
                     value = 1)
  updateCheckboxInput(session,
                      "eqn_options_chem_modifier_reverse",
                      value = FALSE)
  updateNumericInput(session, 
                     "eqn_options_chem_num_reverse_regulators", 
                     value = 1)

})

# Equation Text outputs --------------------------------------------------------

output$eqnCreate_showEquationBuilding <- renderUI({
  withMathJax(
    equationMathJaxBuilder()
  )
})

output$test_mathjax_equations <- renderUI({
  if (length(rv.REACTIONS$main) == 0) {
    paste("No equations entered")
  } else {
    n_eqns = seq(length(rv.REACTIONS$main))
    eqns_to_display <- c()
    for (i in n_eqns) {
      new_eqn <- paste0("(",i, ") ", rv.REACTIONS$main[i])
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse = "<br>")
  }
})


output$eqnCreate_showAdditionalEquations <- renderText({
  if (length(rv.REACTIONS$additional.eqns) == 0) {
    "No additional equations entered"
  } else{
    eqns_to_display <- c()
    n_eqns = seq(length(rv.REACTIONS$additional.eqns))

    for (i in n_eqns) {
      new_eqn <- paste0("(",n_eqns[i], ") ", rv.REACTIONS$additional.eqns[i])
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse = "<br>")
  }
})

# Delete Equations -------------------------------------------------------------
output$deleteEquations_table_viewer <- renderRHandsontable({
  
  eqn.num <- as.numeric(input$eqnCreate_delete_select_equation)
  myindex = eqn.num - 1
  
  df.to.show <- select(rv.REACTIONS$reactions.df,
                       "Equation.Text",
                       "Eqn.Type",
                       "Law",
                       "Compartment")
  
  df.to.show <- as.data.frame(df.to.show)
  colnames(df.to.show) <- c("Equation", 
                            "Type", 
                            "Law", 
                            "Compartment")
  rhandsontable(df.to.show,
                myindex = myindex) %>%
    hot_cols(renderer = 
     "function(instance, td, row, col, prop, value, cellProperties) {
       Handsontable.renderers.TextRenderer.apply(this, arguments);
       if (instance.params) {
       mhrows = instance.params.myindex;
       mhrows = mhrows instanceof Array ? mhrows : [mhrows];
       }
       if (instance.params && mhrows.includes(row)) td.style.background = '#FFCCCB';
      }"
    )
})

observeEvent(input$modal_delete_eqn_button, {
  # browser()
  eqns.to.delete <- as.numeric(input$eqnCreate_delete_select_equation)
  eqn.ids <- rv.REACTIONS$reactions.df$ID[eqns.to.delete]
  
  # Extract parameter ids used in removed equations
  parameter.ids <- rv.REACTIONS$reactions.df$Parameters.Id[eqns.to.delete]
  
  # Delete Equations from Reactive Variables
  for (i in eqn.ids) {
    rv.REACTIONS$reactions[[i]] <- NULL
  }
  
  # Reform eqn df
  rv.REACTIONS$reactions.df <- bind_rows(rv.REACTIONS$reactions)
  
  # Remove Parameters from model if they are not located elsewhere
  pars.to.check <- c()
  for (par.ids in parameter.ids) {
    pars.to.check <- c(pars.to.check, strsplit(par.ids, " ")[[1]])
  }

  # Gather params from equations
  pars.in.eqns <- c()
  par.extraction <- rv.REACTIONS$reactions.df$Parameters.Id
  for (par.ids in par.extraction) {
    pars.in.eqns <- c(pars.in.eqns, strsplit(par.ids, " ")[[1]])
  }

  # Gather params from Input/Outputs
  pars.in.IO <- c()
  par.extraction <- rv.IO$IO.df$parameter.id
  for (par.ids in par.extraction) {
    pars.in.IO <- c(pars.in.IO, strsplit(par.ids, " ")[[1]])
  }

  # Join par vectors
  pars.in.model <- c(pars.in.eqns, pars.in.IO)

  # Check IO for parameters and other equations
  pars.to.remove <- c()
  for (i in pars.to.check) {
    # Check other equations
    if (!(i %in% pars.in.model)) {
      pars.to.remove <- c(pars.to.remove, i)
    }
  }

  # Remove Parameters
  for (p in pars.to.remove) {
   rv.PARAMETERS$parameters[[p]] <- NULL 
  }
  
  if (input$checkbox_modal_delete_keep_modal_active) {
    toggleModal(session,
                "modal_delete_equations",
                toggle = "close")
  }
})


# Equation Event Updates -------------------------------------------------------

observeEvent(rv.REACTIONS$reactions, {
  rv.REACTIONS$reactions.df <- bind_rows(rv.REACTIONS$reactions)
  
  #Update Number Counters on Equation Modals
  updatePickerInput(session,
                    'eqnCreate_edit_select_equation',
                    choices = seq(length(rv.REACTIONS$reactions)))
  
  updatePickerInput(session,
                    'eqnCreate_delete_select_equation',
                    choices = seq(length(rv.REACTIONS$reactions)))
})

observeEvent(rv.REACTIONS$massAction, {
  rv.REACTIONS$massAction.df <- bind_rows(rv.REACTIONS$massAction)
  print(rv.REACTIONS$massAction.df)
})

observeEvent(rv.REACTIONS$michaelisMenten, {
  rv.REACTIONS$michaelisMenten.df <- bind_rows(rv.REACTIONS$michaelisMenten)
  print(rv.REACTIONS$michaelisMenten.df)
})

observeEvent(rv.REACTIONS$synthesis, {
  rv.REACTIONS$synthesis.df <- bind_rows(rv.REACTIONS$synthesis)
  print(rv.REACTIONS$synthesis.df)
})

observeEvent(rv.REACTIONS$degradation, {
  rv.REACTIONS$degradation.df <- bind_rows(rv.REACTIONS$degradation)
  print(rv.REACTIONS$degradation.df)
})

#--------------------------Random----------------------------------------------

