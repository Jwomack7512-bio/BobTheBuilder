

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

variableCheck <- function(variable, currentVarList) {
  #function checks if variable is good to use for model
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
  # 1 - Variable is already used
  # 2 - Variable name starts with number
  # 3 - Variable name contains special characters
  # 4 - Variable name starts with punctuation
  
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
  
  out <- list(var.pass, error.message, error.code)
  return(out)
}

# Add Variable Button Action
observeEvent(input$createVar_addVarToList, {
  if (input$createVar_varInput == "")
  {
    #nothing happens if a blank space is added
  }
  # else if (input$createVar_varInput %in% vars$species) #var already exists in model, let user know
  # {
  #   session$sendCustomMessage(type = 'testmessage',
  #                             message = 'This variable is already used')
  #   updateTextInput(session = session
  #                   ,'createVar_varInput'
  #                   ,value = "")
  # }
  else
  {
    #split input
    vector.of.vars <- strsplits(input$createVar_varInput, c(",", " "))
    for (i in seq(length(vector.of.vars))) {
      var <- vector.of.vars[i]
      check.vars <- variableCheck(var, vars$species)
      passed.check <- check.vars[[1]]
      error.message <- check.vars[[2]]
      if (passed.check) {
        vars$species <- append(vars$species, vector.of.vars[i])
        vars$descriptions <- append(vars$descriptions, "")
        #update table values for for infor
        observe({print(vars$table)})
        observe(print(var))
        #add variable to variable table
        if (nrow(vars$table) == 0) {
          vars$table[1,] <- c(var, "")
        } else {
          row.to.df <- c(var, "")
          vars$table <- rbind(vars$table, row.to.df)
        }
        #add variable to ICs table
        if (nrow(ICs$ICs.table) == 0) {
          ICs$ICs.table[1,] <- c(var, 0, paste0("Initial Concentration of ", var))
          ICs$vals <- c(ICs$vals, 0)
          ICs$comments <- c(ICs$comments, paste0("Initial Concentration of ", var))
        } else {
          row.to.df <- c(var, 0, paste0("Initial Concentration of ", var))
          ICs$ICs.table <- rbind(ICs$ICs.table, row.to.df)
          ICs$vals <- c(ICs$vals, 0)
          ICs$comments <- c(ICs$comments, paste0("Initial Concentration of ", var))
        }
        
        observe({print(vars$table)})
      }
      else{
        session$sendCustomMessage(type = 'testmessage',
                                  message = error.message)
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
  ICs$ICs.table <- ICs$ICs.table[-idx.of.value, ]
  
  
  removeModal()
  #reset pickerinputs for variables
  updatePickerInput(session
                    ,"InOut_selectVar"
                    ,choices = vars$species)
  updatePickerInput(session = session
                    ,"createVar_deleteVarPicker"
                    ,choices = vars$species)
})
#delete variable button action
observeEvent(input$createVar_deleteVarButton, {
  val.to.delete <- input$createVar_deleteVarPicker
  
  showModal(modalDialog(
    
    title = paste0("Are you sure you want to delete variable: ", val.to.delete),
    footer = tagList(actionButton("confirmDelete", "Delete"),
                     modalButton("Cancel")
    )
  ))
  
  #add check to see if variable is used in model.  Add warning.
  
  
})


output$createVar_displayVars <- renderText({
  if (length(vars$species > 0)) {
    paste(vars$species, collapse = "<br>")
  } else {
    paste("Added Variables will appear here")
  }
  
})

################################################################################
#Server Section that controls editable table of variables
# needs to create table that is editable and changes the respectable RVs.
# should control the vars: name, description, table

output$myVariables_DT <- renderDT({
  DT::datatable(vars$table
                ,editable = list(target = "column", disable = list(columns = 0))
                #,extensions = 'Buttons'
                ,options = list(autoWidth = TRUE
                                ,ordering = FALSE
                                ,pageLength = -1
                                ,columnDefs = list(list(width = "85%", targets = 2))
                                ,dom = 't'
                                ,initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#007bff', 'color': 'white'});",
                                  "}")
                                # ,buttons = list("copy"
                                #                 ,list(extend = "csv", filename = "Variables")
                                #                 ,list(extend = "excel", filename = "Variables")
                                #                 ,list(extend = "pdf", filename = "Variables")
                                #                 ,"print"
                                #                 )
                                )
                )

})


proxy_var_table = dataTableProxy("vars_DT")

observeEvent(input$myVariables_DT_cell_edit, {
  info = input$myVariables_DT_cell_edit
  vars$table <- editData(vars$table, info)
  replaceData(proxy_var_table, vars$table, resetPaging = FALSE)

  #Reset the parameter data to match the table values by pulling table values
  # to match parameter vectors
  vars$species <- vars$table[, 1] #will need to add a check here in teh future to change this value in all equations.
  vars$descriptions <- vars$table[, 2]
})





