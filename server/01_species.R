# Helper Functions -------------------------------------------------------------
variableCheck <- function(variable, 
                          currentVarList, 
                          parameterList,
                          allowRepeatParam = FALSE
) {
  #function checks if variable is good to use for model
  # Inputs: 
  #  @variable - variable to be checked for conflicts
  #  @currentVarList - vector of variable names
  #  @parameterList  - vector of parameter names
  # Outputs:
  #  @var.pass - boolean, true if no conflicts, false if conflicts
  #  @error.message - message describing conflict
  #  @error.code - numeric code referring to type of conflict
  
  
  #Checks for: 
  # 1. Repeat Var Name
  # 2. Var starting with number
  # 3. Var containing punctuation that is not "_" or "."
  # 4. Check that variable does not start with punctuation
  #Returns:
  # 1. True if variable is okay, False if variable is not
  # 2. Error code of the problem
  # 3. Int value relating to error messages
  
  #Error Codes:
  # 0 - No Error
  # 1 - Variable name found in variable name vector
  # 2 - Variable name starts with number
  # 3 - Variable name contains special characters
  # 4 - Variable name starts with punctuation
  # 5 - Variable name found in parameter names
  #browser()
  var.pass <- TRUE
  error.message <- "None"
  error.code = 0 
  first.letter.of.var <- substr(variable, 1, 1)
  
  print(variable)
  print(parameterList)
  print(variable %in% parameterList)
  #regrex expression checks if values contains alpha numeric char, _, and .
  ex <- "[^[:alnum:]_.]" 
  repeat.param <- FALSE
  
  #check for repeat var
  if (variable %in% currentVarList) {
    var.pass <- FALSE
    error.message <- paste0(variable, ": Variable is already used")
    error.code <- 1
  }
  #checks if the first letter of the variable is a number
  else if (grepl("^([0-9])", first.letter.of.var)) {
    var.pass <- FALSE
    error.message <- paste0(variable, ": Variables cannot start with number")
    error.code <- 2
  }
  #checks if variable contains punctuation other than . or _
  else if (grepl(ex, variable)) {
    var.pass <- FALSE
    error.message <- paste0(variable, ": Contains special characters")
    error.code <- 3
  }
  #check to see if variable starts with punctuation
  else if (grepl("^([[:punct:]])", variable)) {
    var.pass <- FALSE
    error.message <- paste0(variable, ": starts with punctuation")
    error.code <- 4
  }
  else if (variable %in% parameterList) {
    
    if (!allowRepeatParam) {
      var.pass <- FALSE
      error.message <- paste0(variable, ": Variable is already used in parameters")
      error.code <- 5
    }
    else {
      var.pass <- TRUE
      repeat.param <- TRUE
    }
    
  }
  #check to see if variable is blank space
  else if (grepl("^\\s*$", variable)) {
    var.pass <- FALSE
    error.message <- "Variable is missing..."
    error.code <- 6
  }
  
  out <- list(var.pass, error.message, error.code, repeat.param)
  return(out)
}

FindId <- function(varName) {
  # Searches Id database to find ID corresponding to name
  idx <- which(id$id.df[,2] %in% varName)
  var.id <- id$id.df[idx, 1]
  return(var.id)
}
# Variables --------------------------------------------------------------------
## Add Variable Button ---------------------------------------------------------
observeEvent(input$createVar_add_variable_button, {
  
  # Find Base Naming Variables
  current.n <- length(vars$var.info) + 1
  base = "var"
  name.to.add <- paste0(base, "_", current.n)
  
  # Generate ID
  ids <- GenerateId(id$id.var.seed, "var")
  unique.id <- ids[[2]]
  id$id.var.seed <- ids[[1]]
  idx.to.add <- nrow(id$id.df) + 1
  id$id.df[idx.to.add, ] <- c(unique.id, paste0(base, "_", current.n))
  
  # Create List Entry
  to.add <- list(Name = paste0(base, "_", current.n),
                 ID = unique.id,
                 Value = 0,
                 Unit = units$selected.units$For.Var,
                 UnitDescription = paste0("conc (",
                                          units$selected.units$For.Var, 
                                          ")"),
                 BaseUnit = units$selected.units$For.Var,
                 BaseValue = 0,
                 Description = "",
                 Compartment = input$createVar_active_compartment,
                 Compartment.id = FindId(input$createVar_active_compartment))
  
  # Add Entry To RV
  vars$var.info[[current.n]] <- to.add
  names(vars$var.info)[current.n] <- name.to.add
  
})

observeEvent(input$createVar_add_variable_to_all_button, {
  toggleModal(session,
              "modal_create_variable",
              toggle = "open")
})

observeEvent(input$createVar_add_to_all_compartments, {
  if (input$createVar_add_to_all_compartments) {
    shinyjs::hide("createVar_add_variable_button")
    shinyjs::show("createVar_add_variable_to_all_button")
  } else {
    shinyjs::hide("createVar_add_variable_to_all_button")
    shinyjs::show("createVar_add_variable_button")
  }
})

# Add variable button in the modal pressed
observeEvent(input$modal_createVariable_add_button, {
  
  for (i in seq_along(vars$compartments.info)) {
    comp.name <- vars$compartments.info[[i]]$Name
    current.n <- length(vars$var.info) + 1
    base      <- input$modal_variable_name
    
    if (input$modal_variable_name_subset == "COMPNAME") {
      name.to.add <- paste0(base, "_", comp.name)
    } else if (input$modal_variable_name_subset == "COMPNUMBER") {
      name.to.add <- paste0(base, "_", i)
    }
    
    
    # Generate ID
    ids <- GenerateId(id$id.var.seed, "var")
    unique.id <- ids[[2]]
    id$id.var.seed <- ids[[1]]
    idx.to.add <- nrow(id$id.df) + 1
    id$id.df[idx.to.add, ] <- c(unique.id, paste0(base, "_", current.n))
    
    # Create List Entry
    to.add <- list(Name = name.to.add,
                   ID = unique.id,
                   Value = 0,
                   Unit = units$selected.units$For.Var,
                   UnitDescription = paste0("conc (",
                                            units$selected.units$For.Var, 
                                            ")"),
                   BaseUnit = units$selected.units$For.Var,
                   BaseValue = 1,
                   Description = "",
                   Compartment = comp.name,
                   Compartment.id = FindId(comp.name))
    
    # Add Entry To RV
    vars$var.info[[current.n]] <- to.add
    names(vars$var.info)[current.n] <- name.to.add
  }
  
  toggleModal(session,
              "modal_create_variable",
              toggle = "close")
})

# Cancel button in the variable modal is pressed
observeEvent(input$modal_createVariable_cancel_button, {
  toggleModal(session,
              "modal_create_variable",
              toggle = "close")
})

# Event: Add Var ---------------------------------------------------------------
observeEvent(input$createVar_addVarToList, {
  if (input$createVar_varInput == "")
  {
    #nothing happens if a blank space is added
  }
  # else if (input$createVar_varInput %in% vars$species) #var already exists in
  # model, let user know
  # {
  #   session$sendCustomMessage(type = 'testmessage',
  #                             message = 'This variable is already used')
  #   updateTextInput(session = session
  #                   ,'createVar_varInput'
  #                   ,value = "")
  # }
  else {
    #split input
    vector.of.vars <- strsplits(input$createVar_varInput, c(",", " "))
    # Cycle through vector inputs
    for (i in seq(length(vector.of.vars))) {
      var <- vector.of.vars[i]
      # Check for errors
      check.vars <- variableCheck(var, 
                                  vars$species, 
                                  names(params$params))
      passed.check <- check.vars[[1]]
      error.message <- check.vars[[2]]
      # Add Variable To Model
      if (passed.check) {
        # Generate Variable ID
        ids <- GenerateId(id$id.var.seed, "variable")
        unique.id <- ids[[2]]
        id$id.var.seed <- ids[[1]]
        idx.to.add <- nrow(id$id.df) + 1
        id$id.df[idx.to.add, ] <- c(unique.id, vector.of.vars[i])
        
        # Compartment
        active.compartment <- input$createVar_active_compartment
        # Append Variable in Variable List
        nVar <- length(vars$var.info)
        unit.d <- paste0("conc (", units$base.units$For.Var, ")")
        p.entry <- list(Name = vector.of.vars[i],
                        ID = unique.id,
                        IV = 0,
                        Unit = units$selected.units$For.Var,
                        UnitDescription = unit.d,
                        BaseUnit = units$base.units$For.Var,
                        BaseValue = 0,
                        Description = "",
                        Compartment = active.compartment)
        vars$var.info[[nVar+1]] <- p.entry
        names(vars$var.info)[[nVar+1]] <- vector.of.vars[i]
        
        # Append Variable to proper RVs
        vars$species <- append(vars$species, vector.of.vars[i])
        vars$descriptions <- append(vars$descriptions, "")
        
        #add variable to variable table
        if (nrow(vars$table) == 0) {
          vars$table[1,] <- c(var, "")
        } else {
          row.to.df <- c(var, "")
          vars$table <- rbind(vars$table, row.to.df)
        }
        #add variable to ICs table
        var.to.add <- var
        val.to.add <- 0
        unit.to.add <- units$base.units$For.Var
        description.to.add <- paste0("Initial Concentration of ", var)
        if (nrow(ICs$ICs.table) == 0) {
          
          ICs$ICs.table[1,] <- c(var.to.add,
                                 val.to.add,
                                 unit.to.add,
                                 description.to.add)
        } else {
          row.to.df <- c(var.to.add,
                         val.to.add,
                         unit.to.add,
                         description.to.add)
          ICs$ICs.table <- rbind(ICs$ICs.table, row.to.df)
        }
        ICs$vals     <- c(ICs$vals, val.to.add)
        ICs$units    <- c(ICs$units, unit.to.add)
        ICs$comments <- c(ICs$comments, description.to.add)
        loop$ICs     <- ICs$ICs.table
      }
      else{
        # session$sendCustomMessage(type = 'testmessage',
        #                           message = error.message)
        sendSweetAlert(
          session = session,
          title = "Error...",
          text = error.message,
          type = "error"
        )
      }
      
    }
    #store selected variable to list of variables
    #vars$species <- append(vars$species, input$createVar_varInput)
    #reset text input to blank when variable entered
    updateTextInput(session = session
                    ,'createVar_varInput'
                    ,value = "")
    
    updatePickerInput(session = session
                      ,"createVar_deleteVarPicker"
                      ,choices = vars$species)
  }
})

# Event: Confirm Delete from Modal----------------------------------------------
observeEvent(input$confirmDelete, {
  #find location of variable in var list (match or which function)
  value.to.find <- input$createVar_deleteVarPicker
  idx.of.value <- match(value.to.find, vars$species)
  #remove that location from species, description, and table
  vars$species <- vars$species[-idx.of.value]
  vars$descriptions <- vars$descriptions[-idx.of.value]
  vars$table <- vars$table[-idx.of.value, ]
  
  #move that location from all IC values 
  ICs$vals <- ICs$vals[-idx.of.value]
  ICs$comments <- ICs$comments[-idx.of.value]
  ICs$units    <- ICs$units[-idx.of.value]
  ICs$ICs.table <- ICs$ICs.table[-idx.of.value, ]
  
  # Remove from variable list
  vars$var.info[[value.to.find]] <- NULL
  removeModal()
  #reset pickerinputs for variables
  updatePickerInput(session
                    ,"InOut_selectVar"
                    ,choices = vars$species)
  updatePickerInput(session = session
                    ,"createVar_deleteVarPicker"
                    ,choices = vars$species)
})

# Event: Delete Var, Create Modal-----------------------------------------------
observeEvent(input$createVar_deleteVarButton, {
  val.to.delete <- input$createVar_deleteVarPicker
  
  showModal(modalDialog(
    
    title = paste0("Are you sure you want to delete variable: ", 
                   val.to.delete,
                   "? Removing variables that are used in equations could be detremential to the program"),
    footer = tagList(actionButton("confirmDelete", "Delete"),
                     modalButton("Cancel")
    )
  ))
  
  #add check to see if variable is used in model.  Add warning.
  
  
})

# Table Render for Variables ---------------------------------------------------
output$myVariables_DT <- renderRHandsontable({
  if (length(vars$var.info) == 0) {
    temp <- data.frame(c("Press addition button below to add species
                         to compartment."))
    temp <- transpose(temp)
    colnames(temp) <- c("Instructions")
    rhandsontable(temp,
                  rowHeaders = NULL,
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
    
    if (input$createVar_show_active_compartment_only) {
      #Extract variables of active compartment
      my.compartment <- input$createVar_active_compartment
      df.by.comp <- filter(vars$var.df, Compartment == my.compartment)
      df.by.comp <- select(df.by.comp, 
                           Name, 
                           Value, 
                           Unit, 
                           Compartment, 
                           Description)
    } else {
      df.by.comp <- select(vars$var.df, 
                           Name, 
                           Value, 
                           Unit, 
                           Compartment, 
                           Description)
    }
    df.by.comp <- as.data.frame(df.by.comp)
    colnames(df.by.comp) <- c("Name",
                              "Value",
                              "Unit",
                              "Compartment",
                              "Description"
    )
    vars$plotted.var.table <- df.by.comp
    
    rhandsontable(df.by.comp,
                  overflow = "visible",
                  rowHeaders = NULL,
                  selectCallback = TRUE,
                  colHeaderWidth = 100,
                  stretchH = "all"
    ) %>%
      hot_cols(
        colWidth = c(30, 20, 20, 30, 60),
        manualColumnMove = FALSE,
        manualColumnResize = TRUE,
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
      #hot_col("Variable Name", readOnly = TRUE) %>%
      hot_rows(rowHeights = 30) %>%
      hot_context_menu(allowRowEdit = FALSE,
                       allowColEdit = FALSE
      )
    
    # rhandsontable(vars$table,
    #               selectCallback = TRUE,
    #               readOnly = TRUE)
  }
})

# Variable Input Rhandsontable: cell Change ------------------------------------
observeEvent(input$myVariables_DT$changes$changes, {
  xi = input$myVariables_DT$changes$changes[[1]][[1]]
  yi = input$myVariables_DT$changes$changes[[1]][[2]]
  old = input$myVariables_DT$changes$changes[[1]][[3]]
  new = input$myVariables_DT$changes$changes[[1]][[4]]
  
  # Find which variable is being changed
  var.name <- vars$plotted.var.table[xi+1, 1]
  
  # If Name changed
  if (yi == 0) {
    # Check if name change is a valid new name
    
    # Find id of variable name 
    # Find variable id and change corresponding name 
    idx.for.id <- which(id$id.df[, 2] %in% old)
    var.id <- id$id.df[idx.for.id, 1]
    id$id.df[idx.for.id, 2] <- new
    
    # Search Other Areas Affected by Var Name Change
    # Steps: 
    #  Search eqn df for id.
    
    if (nrow(eqns$eqn.info) != 0) {
      for (i in seq(nrow(eqns$eqn.info))) {
        row <- eqns$eqn.info[i,]$Species.ID
        ids.in.eqn <- strsplit(row, " ")[[1]]
        if (var.id %in% ids.in.eqn) {
          print("CHANGING VARIABLE IN EQUATIONS")
          # Find which idx and eqn type
          eqn.type <- eqns$eqn.info[i,]$EqnType
          idx.in.split <- which(ids.in.eqn %in% var.id)
          switch(eqn.type,
                 "chem_rxn" = {
                   eqns$eqn.chem <- RenameVarInDF(old,
                                                  new,
                                                  eqns$eqn.chem)
                 }, 
                 "enzyme_rxn" = {
                   eqns$eqn.enzyme <- RenameVarInDF(old,
                                                    new,
                                                    eqns$eqn.enzyme)
                 },
                 "syn" = {
                   eqns$eqn.syn <- RenameVarInDF(old,
                                                 new,
                                                 eqns$eqn.syn)
                 },
                 "deg" = {
                   eqns$eqn.deg <- RenameVarInDF(old,
                                                 new,
                                                 eqns$eqn.deg)
                 }
          )
          eqns$eqn.info[i,] <- RenameVarInVector(old,
                                                 new,
                                                 eqns$eqn.info[i,])
        }
      }
    }
    
    # If it is, change the variable name everywhere. 
    vars$var.info[[old]]$Name <- new
    idx <- which(names(vars$var.info) %in% old)
    names(vars$var.info)[idx] <- new
    
  } else if (yi == 1) {
    # Change Species Value
    vars$var.info[[var.name]]$Value <- new
    
    # Change the base value of the value if needed.
    select.unit <- vars$var.info[[var.name]]$Unit
    base.unit   <- vars$var.info[[var.name]]$BaseUnit
    if (select.unit != base.unit) {
      # Perform Unit Conversion
      descriptor <- vars$var.info[[var.name]]$UnitDescription
      converted.value <- UnitConversion(descriptor,
                                        select.unit,
                                        base.unit,
                                        as.numeric(new))
      vars$var.info[[var.name]]$BaseValue <- converted.value
    } else {
      # Simply Overwrite BaseValue
      vars$var.info[[var.name]]$BaseValue <- new
    }
  } else if (yi == 2) {
    # Change species Unit
    descriptor <- vars$var.info[[var.name]]$UnitDescription
    
    comparison <- UnitCompare(descriptor,
                              new,
                              units$possible.units$For.Var,
                              units$possible.units$Duration)
    
    if (comparison$is.match) {
      # Perform Unit Conversion
      new.value <- UnitConversion(descriptor,
                                  old, 
                                  new,
                                  as.numeric(vars$var.info[[var.name]]$Value))
      vars$var.info[[var.name]]$Value <- new.value
      vars$var.info[[var.name]]$Unit  <- new
      
    } else {
      vars$var.info[[var.name]]$Unit  <- old
    }
    
  } else if (yi == 3) {
    vars$var.info[[var.name]]$Compartment <- new
  } else if (yi == 4) {
    vars$var.info[[var.name]]$Description <- new
  }
  
  vars$var.df <- bind_rows(vars$var.info)
  # Overwrite save to dataframe since this doesn't seem to pop event
  # vars$var.df <- bind_rows(vars$var.info)
  
})

observeEvent(input$myVariables_DT_select$select$r, {
  req(length(vars$species > 0))
  cat("Selected Row", input$myVariables_DT_select$select$r)
  cat('\nSelected Column:',input$myVariables_DT_select$select$c)
})

# Events that change on variable change ----------------------------------------
observeEvent(vars$var.info, {
  vars$var.df <- bind_rows(vars$var.info)
  print(vars$var.df)
})
