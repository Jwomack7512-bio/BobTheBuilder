
# Functions --------------------------------------------------------------------
strsplits <- function(x, splits, ...)
#splits string on multiple inputs
#used strsplits(a, c(",", " ")) for space and comma splits of c
#returns vector of split variables
#https://stackoverflow.com/questions/10738729/r-strsplit-with-multiple-unordered-split-arguments
{
  for (split in splits)
  {
    x <- unlist(strsplit(x, split, ...))
  }
  return(x[!x == ""]) # Remove empty values
}

variableCheck <- function(variable, 
                          currentVarList, 
                          parameterList
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
  
  var.pass <- TRUE
  error.message <- "None"
  error.code = 0 
  first.letter.of.var <- substr(variable, 1, 1)
  ex <- "[^[:alnum:]_.]" #regrex expression checks if values contains alpha numeric char, _, and .
  
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
    var.pass <- FALSE
    error.message <- paste0(variable, ": Variable is already used in parameters")
    error.code <- 5
  }
  #check to see if variable is blank space
  else if (grepl("^\\s*$", variable)) {
    var.pass <- FALSE
    error.message <- "Variable is missing..."
    error.code <- 6
  }
  
  out <- list(var.pass, error.message, error.code)
  return(out)
}

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
      check.vars <- variableCheck(var, vars$species, params$vars.all)
      passed.check <- check.vars[[1]]
      error.message <- check.vars[[2]]
      # Add Variable To Model
      if (passed.check) {
        # Generate Variable ID
        ids <- GenerateId(id$id.var.seed, "variable")
        unique.id <- ids[[2]]
        id$id.var.seed <- ids[[1]]
        idx.to.add <- nrow(id$id.variables) + 1
        id$id.variables[idx.to.add, ] <- c(unique.id, vector.of.vars[i])
        
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
  colnames(vars$table) <- c("Variable Name", "Description")
  if (nrow(vars$table) == 0) {
    temp <- data.frame(c("<- Add Variable(s) to begin", " "))
    temp <- transpose(temp)
    colnames(temp) <- c("Variable Name", "Description")
    rhandsontable(temp,
                  rowHeaders = NULL,
                  selectCallback = TRUE,
                  colHeaderWidth = 100,
                  stretchH = "all",
                  readOnly = TRUE
    ) %>%
      hot_cols(colWidth = c(90, 30),
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
      hot_rows(rowHeights = 40) %>%
      hot_context_menu(allowRowEdit = FALSE,
                       allowColEdit = FALSE
      )
  } else {
    rhandsontable(vars$table,
                  rowHeaders = NULL,
                  selectCallback = TRUE,
                  colHeaderWidth = 100,
                  stretchH = "all"
    ) %>%
      hot_cols(colWidth = c(30, 90),
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
      hot_col("Variable Name", readOnly = TRUE) %>%
      hot_rows(rowHeights = 40) %>%
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
  
  # Add check in here for variable changing name
  
  #copying table to dataframe
  vars$table[xi+1, yi+1]  <- new
  #vars$species[xi+1]      <- vars$table[xi+1, 1]
  vars$descriptions[xi+1] <- vars$table[xi+1, 2]
  
  if (yi == 1) {
    var.name <- vars$table[xi+1, 1]
    vars$var.info[[var.name]]$Description <- new
  }
})

observeEvent(input$myVariables_DT_select$select$r, {
  req(length(vars$species > 0))
  print("KKKKKK")
  cat("Selected Row", input$myVariables_DT_select$select$r)
  cat('\nSelected Column:',input$myVariables_DT_select$select$c)
})


# Propery Editor UI ------------------------------------------------------------
output$createVar_PE_variables <- renderUI({
  #Find selected element and information to fill
  row <- input$myVariables_DT_select$select$r
  col <- input$myVariables_DT_select$select$c
  if (is.null(row) | is.null(col)) {
    div(
      "Click variable names in table to open property editor"
    )
  } else {
    var.name <- vars$table[row,1]
    
    isolate({
      var.unit <- vars$var.info[[var.name]]$Unit
      var.val  <- vars$var.info[[var.name]]$IV
      var.des  <- vars$var.info[[var.name]]$Description
      var.comp <- vars$var.info[[var.name]]$Compartment
    })
    div(
      tags$table(
        class = "PE_variable_UI_table",
        tags$tr(
          width = "100%",
          tags$td(
            width = "30%",
            div(
              style = "font-size: 16px;",
              tags$b("Value")
            )
          ),
          tags$td(
            width = "70%",
            textInput(
              inputId = "PE_variable_IC",
              label = '',
              value = var.val
            )
          )
        ),
        tags$tr(
          width = "100%",
          tags$td(
            width = "30%",
            div(
              style = "font-size: 16px;",
              tags$b("Unit")
            )
          ),
          tags$td(
            width = "70%",
            pickerInput(
              inputId = "PE_variable_unit",
              label = NULL,
              choices = units$possible.units$For.Var,
              selected = var.unit,
              width = "100%"
            )
          )
        ),
          tags$tr(
            width = "100%",
            tags$td(
              width = "30%",
              div(
                style = "font-size: 16px;",
                tags$b("Compartment")
              )
            ),
            tags$td(
              width = "70%",
              pickerInput(
                inputId = "PE_variable_compartment",
                label = NULL,
                choices =vars$compartments,
                selected = var.comp,
                width = "207.2px"
              )
            )
          )
        ),
      # Variable Description
      textAreaInput(
        inputId = "PE_variable_description",
        label = "Description",
        value = var.des,
        width = NULL,
        height = "200px"
      )
    )
  }

})

output$createVar_PE_box_title <- renderText({
  row <- input$myVariables_DT_select$select$r
  
  var.name <- vars$table[row,1]
  paste0("Property Editor: ", var.name)
})

# ObserveEvent: Property Editors -----------------------------------------------
observeEvent(input$PE_variable_IC, {
  row <- input$myVariables_DT_select$select$r
  var.name <- vars$table[row, 1]
  idx <- which(ICs$ICs.table[,1] %in% var.name)

  vars$var.info[[var.name]]$IV <- input$PE_variable_IC
  ICs$ICs.table[idx, 2] <- input$PE_variable_IC
})

observeEvent(input$PE_variable_unit, {
  row <- input$myVariables_DT_select$select$r
  var.name <- vars$table[row, 1]
  idx <- which(ICs$ICs.table[,1] %in% var.name)
  
  vars$var.info[[var.name]]$Unit <- input$PE_variable_unit
  ICs$ICs.table[idx, 3] <- input$PE_variable_unit
})

observeEvent(input$PE_variable_description, {
  row <- input$myVariables_DT_select$select$r
  var.name <- vars$table[row, 1]
  idx <- which(ICs$ICs.table[,1] %in% var.name)
  
  vars$var.info[[var.name]]$Description <- input$PE_variable_description
  ICs$ICs.table[idx, 4] <- input$PE_variable_description
})


# Compartments -----------------------------------------------------------------

## Add -------------------------------------------------------------------------
observeEvent(input$createVar_add_compartment, {
  req(input$createVar_compartment_input != "")
  
  var.name <- input$createVar_compartment_input
  
  #split input
  vec.of.comps <- strsplits(input$createVar_compartment_input, c(",", " "))
  #browser()
  # Cycle through vector inputs
  for (i in seq(length(vec.of.comps))) {
    comp.to.add <- vec.of.comps[i]
    # Check for errors
    check.vars <- variableCheck(comp.to.add, vars$species, params$vars.all)
    passed.check <- check.vars[[1]]
    error.message <- check.vars[[2]]
    # Add Variable To Model
    if (passed.check) {
      # Generate Variable ID
      ids <- GenerateId(id$id.comp.seed, "compartment")
      id$id.comp.seed <- ids[[1]]
      unique.id <- ids[[2]]
      idx.to.add <- nrow(id$id.compartments) + 1
      id$id.compartments[idx.to.add, ] <- c(unique.id, vec.of.comps[i])
      
      # Append Compartment to List
      nVar <- length(vars$compartments.info)
      p.entry <- list(Name = vec.of.comps[i],
                      ID = unique.id,
                      IV = 1,
                      Unit = units$selected.units$Volume,
                      UnitDescription = "vol",
                      BaseUnit = units$base.units$Volume,
                      BaseValue = 0, 
                      Description = "")
      
      vars$compartments.info[[nVar+1]] <- p.entry
      names(vars$compartments.info)[[nVar+1]] <- vec.of.comps[i]
      
      vars$compartments <- c(vars$compartments, 
                             vec.of.comps[i])
      
    } else {
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = error.message,
        type = "error"
      )
    }
    
  }

  
  updateTextInput(session = session,
                  inputId = "createVar_compartment_input",
                  value = "")
})

observeEvent(vars$compartments.info, {
  compartment.names <- names(vars$compartments.info)
  updatePickerInput(session,
                    "createVar_active_compartment",
                    choices = compartment.names)
})

# Debug ------------------------------------------------------------------------


