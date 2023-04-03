# ObserveEvent: Property Editors -----------------------------------------------
observeEvent(input$PE_variable_IC, {
  row <- input$myVariables_DT_select$select$r
  var.name <- vars$table[row, 1]
  idx <- which(ICs$ICs.table[,1] %in% var.name)
  
  rv.SPECIES$species[[var.name]]$Value <- as.numeric(input$PE_variable_IC)
  ICs$ICs.table[idx, 2] <- as.numeric(input$PE_variable_IC)
})

observeEvent(input$PE_variable_unit, {
  row <- input$myVariables_DT_select$select$r
  var.name <- vars$table[row, 1]
  idx <- which(ICs$ICs.table[,1] %in% var.name)
  
  rv.SPECIES$species[[var.name]]$Unit <- input$PE_variable_unit
  ICs$ICs.table[idx, 3] <- input$PE_variable_unit
})

observeEvent(input$PE_variable_description, {
  row <- input$myVariables_DT_select$select$r
  var.name <- vars$table[row, 1]
  idx <- which(ICs$ICs.table[,1] %in% var.name)
  
  rv.SPECIES$species[[var.name]]$Description <- input$PE_variable_description
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
      var.unit <- rv.SPECIES$species[[var.name]]$Unit
      var.val  <- rv.SPECIES$species[[var.name]]$Value
      var.des  <- rv.SPECIES$species[[var.name]]$Description
      var.comp <- rv.SPECIES$species[[var.name]]$Compartment
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
  req(length(rv.SPECIES$species.names > 0))
  cat("Selected Row", input$myVariables_DT_select$select$r)
  cat('\nSelected Column:',input$myVariables_DT_select$select$c)
})

DeleteParameters <- function(paramToDelete) {
  # Delete Parameter From Storage List
  rv.PARAMETERS$parameters[[paramToDelete]] <- NULL

  # Delete Parameter From Param Vector
  rv.PARAMETERS$vars.all <- RemoveFromVector(paramToDelete, rv.PARAMETERS$vars.all)
  
  # Delete Parameter From Param Dataframe
  idx <- match(paramToDelete, rv.PARAMETERS$param.table[,1])
  rv.PARAMETERS$param.table <- rv.PARAMETERS$param.table[-idx, ]
  
  updatePickerInput(session, "parameters_filter_type", selected = "Eqns")
  updatePickerInput(session, "parameters_filter_type", selected = "All")
}

# Modal for creating parameter -------------------------------------------------
observeEvent(input$modal_create_param_button, {
  #create row for parameter df
  var <- input$modal_param_param_name
  check.vars <- variableCheck(var, rv.SPECIES$species.names, rv.PARAMETERS$vars.all)
  passed.check <- check.vars[[1]]
  error.message <- check.vars[[2]]
  error.code <- check.vars[[3]]
  
  if (passed.check) {
    # Generate Param Id
    ids <- GenerateId(id$id.var.seed, "parameter")
    id <- ids$id
    
    # Create Parameter Entry For List Entry
    p.list.entry <- list(Name = input$modal_param_param_name,
                         ID = id,
                         Value = input$modal_param_value,
                         Unit = input$model_param_unit,
                         Description = input$modal_param_description,
                         Type = "Custom Added",
                         TypeNote = "")
    nPars <- length(rv.PARAMETERS$parameters)
    rv.PARAMETERS$parameters[[nPars+1]] <- p.list.entry
    names(rv.PARAMETERS$parameters)[[nPars+1]] <- input$modal_param_param_name
    # Add Param to Param Table
    row.to.add <- c(input$modal_param_param_name,
                    input$modal_param_value,
                    input$model_param_unit,
                    input$modal_param_description)
    
    rv.PARAMETERS$param.table[nrow(rv.PARAMETERS$param.table)+1,] <- row.to.add
    updatePickerInput(session, "parameters_filter_type", selected = "Eqns")
    updatePickerInput(session, "parameters_filter_type", selected = "All")
    
    rv.PARAMETERS$vars.all <- c(rv.PARAMETERS$vars.all, input$modal_param_param_name) 
    
    
    toggleModal(session, "modal_create_parameter", toggle =  "close")
  } else {
    session$sendCustomMessage(type = 'testmessage',
                              message = error.message)
  }
})
observeEvent(rv.PARAMETERS$vars.all, {
  updatePickerInput(session, "modal_params_to_delete", choices = rv.PARAMETERS$vars.all)
})

# Parameter Filters ------------------------------------------------------------
observeEvent(input$parameters_filter_type, {
  if (input$parameters_filter_type == "All") {
    my.table <- rv.PARAMETERS$param.table
  } else if (input$parameters_filter_type == "Eqns") {
    #subset table based on param eqn vars
    my.table <- 
      rv.PARAMETERS$param.table[rv.PARAMETERS$param.table[,1] %in% rv.PARAMETERS$eqns.vars,]
  } else if (input$parameters_filter_type == "Inputs") {
    my.table <- 
      rv.PARAMETERS$param.table[rv.PARAMETERS$param.table[,1] %in% rv.PARAMETERS$inputs.vars,]
  } else if (input$parameters_filter_type == "Outputs") {
    my.table <- 
      rv.PARAMETERS$param.table[rv.PARAMETERS$param.table[,1] %in% rv.PARAMETERS$outputs.vars,]
  }
  parameter_table_values$table <- my.table
  parameter_table_values$table.copy <- my.table
}) 

# Modal for deleting parameter--------------------------------------------------
observeEvent(input$modal_delete_param_button, {
  var.to.delete <- input$modal_params_to_delete
  DeleteParameters(var.to.delete)
  toggleModal(session, "modal_delete_param", toggle =  "close")
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
#     check.vars <- variableCheck(comp.to.add, rv.SPECIES$species.names, rv.PARAMETERS$vars.all)
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
#       nVar <- length(rv.COMPARTMENTS$compartments)
#       p.entry <- list(Name = vec.of.comps[i],
#                       ID = unique.id,
#                       IV = 1,
#                       Unit = units$selected.units$Volume,
#                       UnitDescription = "vol",
#                       BaseUnit = units$base.units$Volume,
#                       BaseValue = 0, 
#                       Description = "")
#       
#       rv.COMPARTMENTS$compartments[[nVar+1]] <- p.entry
#       names(rv.COMPARTMENTS$compartments)[[nVar+1]] <- vec.of.comps[i]
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
#                       choices = c(names(rv.COMPARTMENTS$compartments), "All"),
#                       selected = input$createVar_active_compartment)
#   }
# })
# updatePickerInput(session,
#                   "createVar_table_filter",
#                   choices = c(compartment.names, "All"))

# remove_rate_parameters_from_vectors <- function(parameter_to_remove)
# {
#     #search all parameters lists for parameter and remove it from each. (input, output, eqn, total)
#     if (parameter_to_remove %in% rv.PARAMETERS$inputs.vars) {
#         rv.PARAMETERS$inputs.vars <-
#             rv.PARAMETERS$inputs.vars[!rv.PARAMETERS$inputs.vars %in% parameter_to_remove]
#     }
#     if (parameter_to_remove %in% rv.PARAMETERS$outputs.vars) {
#         rv.PARAMETERS$outputs.vars <-
#             rv.PARAMETERS$outputs.vars[!rv.PARAMETERS$outputs.vars %in% parameter_to_remove]
#     }
#     if (parameter_to_remove %in% rv.PARAMETERS$eqns.vars) {
#         rv.PARAMETERS$eqns.vars <-
#             rv.PARAMETERS$eqns.vars[!rv.PARAMETERS$eqns.vars %in% parameter_to_remove]
#     }
#     if (parameter_to_remove %in% rv.PARAMETERS$vars.all) {
#         rv.PARAMETERS$vars.all <-
#             rv.PARAMETERS$vars.all[!rv.PARAMETERS$vars.all %in% parameter_to_remove]
#     }
#     #remove all excess variables from created lists if they exist (ie. we generated ui for parameter values and comments.  Will need to remove those)
# }


# From Execute -----------------------------------------------------------------

# output$table_output_test1 <- renderTable({
#   req(results$model.has.been.solved)
#   m <- results$model.final
#   rounded.model <- round(m[1:nrow(m), 1:ncol(m)], digits = 3)
#   time.w.units <- paste0("time (", results$time.units, ")")
#   colnames(rounded.model)[1] <- time.w.units
#   rounded.model
# },
# striped = TRUE,
# hover = TRUE, 
# bordered = TRUE,
# width = "100%",
# align = "l")

#hook up table to result of event reactive above
# output$execute_table_for_model <- renderDT({
#   
#   rhandsontable(model_output(),
#                 readOnly = TRUE, 
#                 contextMenu = FALSE,
#                 maxRoxs = 10)
# })


# model_output <- eventReactive(input$execute_run_model, {
#   # Error Checks for button
#   
#   w_execute$show()
#   #set up time for solver
#   
#   error.found <- FALSE
#   error.message <- "Model failed to solve. "
#   error.time <- FALSE
#   
#   time_in <- as.numeric(input$execute_time_start)
#   time_out <- as.numeric(input$execute_time_end)
#   time_step <- as.numeric(input$execute_time_step)
#   
#   #-----------------------------------------------------------------------------
#   # Error Checking for time values
#   #-----------------------------------------------------------------------------
#   if (is.na(time_in) | is.na(time_out) | is.na(time_step)) {
#     error.found <- TRUE
#     error.time <- TRUE
#     error.message <- paste0(error.message, 
#                             "Time values are not numerical values. ")
#   } else if (time_out < time_in) {
#     error.found <- TRUE
#     error.time <- TRUE
#     error.message <- paste0(error.message, 
#                             "Time out is a lower value than time in. ")
#   }
#   
#   if (!error.time) {
#     times <- seq(time_in, time_out, by = time_step)
#     if (length(times) < 10) {
#       error.found <- TRUE
#       error.message <- paste0(error.message, 
#                               "Step size not small enough. 
#                               Must have at least 10 units of time. ")
#     }
#   }
#   
#   #-----------------------------------------------------------------------------
#   # Error Checking for  missing values from Equation information
#   #-----------------------------------------------------------------------------
#   #for error checking for parameters and variables we neeed to check that all
#   # existing values in the equations exist in var$species and l
#   # vars.in.eqns <- c()
#   # p <- c()
#   # eqn.df <- rv.REACTIONS$eqn.info
#   # chem.df <- rv.REACTIONS$eqn.chem
#   # enz.df <- rv.REACTIONS$eqn.enzyme
#   # syn.df <- rv.REACTIONS$eqn.syn
#   # 
#   # for (row in 1:nrow(eqn.df)) {
#   #   
#   #   #Gather all variables from eqns
#   #   LHS_var <-  str_split(eqn.df[row,3], " ")[[1]]
#   #   RHS_var <-  str_split(eqn.df[row,5], " ")[[1]]
#   #   enzyme  <-  eqn.df[row,12]
#   #   FR      <-  eqn.df[row,14]
#   #   RR      <-  eqn.df[row,17]
#   #   vars.in.eqns <- c(vars.in.eqns, LHS_var, RHS_var, enzyme, FR, RR)
#   #   
#   #   #find parameters in eqns
#   #   kf   <- enz.df[row,7]
#   #   kr   <- enz.df[row,8]
#   #   kcat <- enz.df[row,9]
#   #   Vmax <- enz.df[row,10]
#   #   Km   <- enz.df[row,11]
#   #   fr   <- eqn.df[row,15]
#   #   rr   <- eqn.df[row,18]
#   #   p    <- c(p, kf, kr, kcat, Vmax, Km, fr, rr)
#   # }
#   # # Remove all duplicates in vectors    
#   # vars.in.eqns <- unique(vars.in.eqns)
#   # p <- unique(p)
#   # # Replace string NA with actual NA
#   # vars.in.eqns <- dplyr::na_if(vars.in.eqns, "NA")
#   # p <- dplyr::na_if(p, "NA")
#   # # Remove NA from vectors
#   # vars.in.eqns <- vars.in.eqns[!is.na(vars.in.eqns)]
#   # p <- p[!is.na(p)]
#   # # check to see if differences exist in lists
#   # diff.var <- setdiff(vars.in.eqns, rv.SPECIES$species.names)
#   # diff.p <- setdiff(p, rv.PARAMETERS$vars.all)
#   # #Throw error if there are differences
#   # if (length(diff.var) != 0) {
#   #   error.found <- TRUE
#   #   error.message <- paste0(error.message, "The following variables were found to 
#   #                           be used in equations but not found in the 
#   #                           species list: ", 
#   #                           paste0(diff.var, collapse = ","),
#   #                           ". ")
#   # }
#   # if (length(diff.p) != 0) {
#   #   error.found <- TRUE
#   #   error.message <- paste0(error.message, "The following parameters were found to 
#   #                           be used in equations but not found in the parameter
#   #                           list: ", 
#   #                           paste0(diff.p, collapse = ","),
#   #                           ". ")
#   # }
#   
#   #-----------------------------------------------------------------------------
#   # Error Checking for  missing values from IO
#   #-----------------------------------------------------------------------------
#   # vars.r <- c()
#   # p    <- c()
#   # I.df <- rv.IO$input.info
#   # O.df <- rv.IO$output.info
#   # #Search Input Dataframe
#   # for (row in 1:nrow(I.df)) {
#   #   species <-  I.df[row,2]
#   #   enz     <-  I.df[row,7]
#   #   vars.r  <- c(vars.r, species, enz)
#   # 
#   #   #find parameters in eqns
#   #   RC     <- I.df[row,3]
#   #   Vmax   <- I.df[row,5]
#   #   kcat   <- I.df[row,6]
#   #   p      <- c(p, RC, Vmax, kcat)
#   # }
#   # # Search Output Dataframe
#   # for (row in 1:nrow(O.df)) {
#   #   species <-  O.df[row,2]
#   #   enz     <-  O.df[row,7]
#   #   vars.r  <- c(vars.r, species, enz)
#   # 
#   #   #find parameters in eqns
#   #   RC     <- O.df[row,3]
#   #   Vmax   <- O.df[row,5]
#   #   kcat   <- O.df[row,6]
#   #   p      <- c(p, RC, Vmax, kcat)
#   # }
#   # vars.r <- dplyr::na_if(unique(vars.r), "NA")
#   # p <- dplyr::na_if(unique(p), "NA")
#   # diff.var <- setdiff(vars.r[!is.na(vars.r)], rv.SPECIES$species.names)
#   # diff.p <- setdiff(p[!is.na(p)], rv.PARAMETERS$vars.all)
#   # #Throw error if there are differences
#   # if (length(diff.var) != 0) {
#   #   error.found <- TRUE
#   #   error.message <- paste0(error.message, "The following variables were found to
#   #                           be used in Inputs/Outputs but not found in species list: ",
#   #                           paste0(diff.var, collapse = ","),
#   #                           ". ")
#   # }
#   # if (length(diff.p) != 0) {
#   #   error.found <- TRUE
#   #   error.message <- paste0(error.message, "The following parameters were found
#   #                           to be used in Inputs/Outputs but not found in
#   #                           parameter list: ",
#   #                           paste0(diff.p, collapse = ","),
#   #                           ". ")
#   # }
#   
#   #-----------------------------------------------------------------------------
#   # Solving model using ODE solver
#   #-----------------------------------------------------------------------------
#   #initialize parameters
#   parameters <- output_param_for_ode_solver(rv.PARAMETERS$parameters)
#   
#   #initialize initial conditions
#   state <- output_ICs_for_ode_solver(rv.SPECIES$species.names ,ICs$vals)
#   
#   #set up differential equations input string form
#   diff_eqns <- diffeq_to_text(DE$eqns, rv.SPECIES$species.names)
#   
#   d_of_var <- output_var_for_ode_solver(rv.SPECIES$species.names)
#   
#   rate_eqns <- rateEqns_to_text(rv.REACTIONS$additional.eqns)
#   
#   if (input$execute_turnOn_time_scale_var) {
#     d_of_var = paste0(input$execute_time_scale_var, "*", d_of_var)
#   }
#   
#   Lorenz <- function(t, state, parameters){
#     with(as.list(c(state, parameters)), {
#       eval(parse(text = rate_eqns))
#       eval(parse(text = diff_eqns))
#       list(eval(parse(text = d_of_var)))
#     })
#   }
#   jPrint("Before ode solver")
#   #out <- ode(y=state, times=times, func=model, parms=parameters)
#   
#   # if (error.found) {
#   #   out <- data.frame()
#   #   cat(error.message)
#   #   sendSweetAlert(session,
#   #                  "Error...",
#   #                  text = error.message,
#   #                  type = "error")
#   #   # session$sendCustomMessage(type = 'testmessage',
#   #   #                           message = HTML(paste(error.message, collapse="<br>")))
#   # } else {
#   # out <- ode(y = state, 
#   #            times = times, 
#   #            func = Lorenz, 
#   #            parms = parameters
#   #            #,method = input$execute_ode_solver_type
#   # )
#   # 
#   # jPrint("After ode solver")
#   # 
#   # 
#   # results$model <- out #store model to reactive var
#   # results$model.has.been.solved <- TRUE
#   # 
#   # # Initialize other plotting modes with this model
#   # loop$model.results <- out
#   # compareModel$model.1 <- out
#   # compareModel$model.2 <- out
#   # compareModel$model.3 <- out
#   # compareModel$model.4 <- out
#   # 
#   # #this is meant to prepare a previous version of save file that didn't have
#   # #these properly done
#   # if (is.null(results$is.pp)) results$is.pp = FALSE
#   # if (is.null(results$pp.eqns)) results$pp.eqns = vector()
#   # if (is.null(results$pp.vars)) results$pp.vars = vector()
#   # if (is.null(results$pp.model)) results$pp.model = data.frame()
#   # if (is.null(results$pp.eqns.col)) results$pp.eqns.col = vector()
#   # jPrint("All this if statements")
#   # jPrint(head(out))
#   # }
#   out <- ode(y = state, 
#              times = times, 
#              func = Lorenz, 
#              parms = parameters
#              #,method = input$execute_ode_solver_type
#   )
#   
#   jPrint("After ode solver")
#   
#   
#   results$model <- out #store model to reactive var
#   results$model.has.been.solved <- TRUE
#   # Initialize other plotting modes with this model
#   loop$model.results <- out
#   compareModel$model.1 <- out
#   compareModel$model.2 <- out
#   compareModel$model.3 <- out
#   compareModel$model.4 <- out
#   #this is meant to prepare a previous version of save file that didn't have
#   #these properly done
#   if (is.null(results$is.pp)) results$is.pp = FALSE
#   if (is.null(results$pp.eqns)) results$pp.eqns = vector()
#   if (is.null(results$pp.vars)) results$pp.vars = vector()
#   if (is.null(results$pp.model)) results$pp.model = data.frame()
#   if (is.null(results$pp.eqns.col)) results$pp.eqns.col = vector()
#   w_execute$hide()
#   return(out)
# })
