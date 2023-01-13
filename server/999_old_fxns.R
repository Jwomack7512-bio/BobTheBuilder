# ObserveEvent: Property Editors -----------------------------------------------
observeEvent(input$PE_variable_IC, {
  row <- input$myVariables_DT_select$select$r
  var.name <- vars$table[row, 1]
  idx <- which(ICs$ICs.table[,1] %in% var.name)
  
  vars$var.info[[var.name]]$IV <- as.numeric(input$PE_variable_IC)
  print(input$PE_variable_IC)
  print(ICs$ICs.table)
  print(idx)
  ICs$ICs.table[idx, 2] <- as.numeric(input$PE_variable_IC)
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


# Propery Editor UI ------------------------------------------------------------
output$createVar_PE_variables <- renderUI({
  #   #Find selected element and information to fill
  row <- input$myVariables_DT_select$select$r
  col <- input$myVariables_DT_select$select$c
  if (is.null(row) | is.null(col)) {
    div(
      "Click variable names in table to open property editor"
    )
  } else {
    # browser()
    var.name <- vars$table[row,1]
    
    isolate({
      var.unit <- vars$var.info[[var.name]]$Unit
      var.val  <- vars$var.info[[var.name]]$IV
      var.des  <- vars$var.info[[var.name]]$Description
      var.comp <- vars$var.info[[var.name]]$Compartment
      print(var.val)
    })
    div(tags$table(
      class = "PE_variable_UI_table",
      tags$tr(
        width = "100%",
        tags$td(width = "30%",
                div(style = "font-size: 16px;",
                    tags$b("Value"))),
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
        tags$td(width = "30%",
                div(style = "font-size: 16px;",
                    tags$b("Unit"))),
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
        tags$td(width = "30%",
                div(style = "font-size: 16px;",
                    tags$b("Compartment"))),
        tags$td(
          width = "70%",
          pickerInput(
            inputId = "PE_variable_compartment",
            label = NULL,
            choices = vars$compartments,
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

observeEvent(input$myVariables_DT_select$select$r, {
  req(length(vars$species > 0))
  cat("Selected Row", input$myVariables_DT_select$select$r)
  cat('\nSelected Column:',input$myVariables_DT_select$select$c)
})


## Add -------------------------------------------------------------------------
# observeEvent(input$createVar_add_compartment, {
#   req(input$createVar_compartment_input != "")
#   
#   var.name <- input$createVar_compartment_input
#   
#   #split input
#   vec.of.comps <- strsplits(input$createVar_compartment_input, c(",", " "))
#   #browser()
#   # Cycle through vector inputs
#   for (i in seq(length(vec.of.comps))) {
#     comp.to.add <- vec.of.comps[i]
#     # Check for errors
#     check.vars <- variableCheck(comp.to.add, vars$species, params$vars.all)
#     passed.check <- check.vars[[1]]
#     error.message <- check.vars[[2]]
#     # Add Variable To Model
#     if (passed.check) {
#       # Generate Variable ID
#       ids <- GenerateId(id$id.comp.seed, "compartment")
#       id$id.comp.seed <- ids[[1]]
#       unique.id <- ids[[2]]
#       idx.to.add <- nrow(id$id.compartments) + 1
#       id$id.compartments[idx.to.add, ] <- c(unique.id, vec.of.comps[i])
#       
#       # Append Compartment to List
#       nVar <- length(vars$compartments.info)
#       p.entry <- list(Name = vec.of.comps[i],
#                       ID = unique.id,
#                       IV = 1,
#                       Unit = units$selected.units$Volume,
#                       UnitDescription = "vol",
#                       BaseUnit = units$base.units$Volume,
#                       BaseValue = 0, 
#                       Description = "")
#       
#       vars$compartments.info[[nVar+1]] <- p.entry
#       names(vars$compartments.info)[[nVar+1]] <- vec.of.comps[i]
#       
#       vars$compartments <- c(vars$compartments, 
#                              vec.of.comps[i])
#       
#     } else {
#       sendSweetAlert(
#         session = session,
#         title = "Error...",
#         text = error.message,
#         type = "error"
#       )
#     }
#     
#   }
# 
#   
#   updateTextInput(session = session,
#                   inputId = "createVar_compartment_input",
#                   value = "")
# })

# observeEvent(input$createVar_active_compartment, {
#   
#   if (input$createVar_active_compartment != "All") {
#     updatePickerInput(session,
#                       "createVar_table_filter",
#                       choices = c(names(vars$compartments.info), "All"),
#                       selected = input$createVar_active_compartment)
#   }
# })
# updatePickerInput(session,
#                   "createVar_table_filter",
#                   choices = c(compartment.names, "All"))