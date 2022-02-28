
checkForLoadedValue <- function(loadedValue, initValue) {
  #this function is meant to perform checks on if loaded value is null
  #between versions there are often things that get added that save as null.  
  #these checks are meant to stop breakdown between old models and new app versions
  # Inputs:
  #  @loadedValue - the value loaded from the model
  #  @initi value - what the initialzied value should be if it is null
  if (is.null(loadedValue)) {
    out <- initValue
  } else {
    out <- loadedValue
  }
  return(out)
}
################################ Load Server #################################

#when load model button is pressed, the .rds file is loaded in and its components are broken apart and added to the model
#some of these loads for reactive variables use a "is.null" check to make sure they exist.  These variables were added
# after specific models were made and this adds functionality to those models that would otherwise have issues.
observeEvent(input$load_model, {
  model.load <- readRDS(input$load_model$datapath)
  
  #-----------------------------------------------------------------------------
  
  # Load Variable Section
  
  #-----------------------------------------------------------------------------
  vars$species <- model.load$species
  ifelse(!is.null(model.load$descriptions), 
         vars$descriptions <- model.load$descriptions, 
         vars$descriptions <- vector())
  if (!is.null(model.load$table)) {
    vars$table <- model.load$table
  } else {
    vars$table <- data.frame(vars$species, vars$descriptions)
    colnames(vars$table) <- c("Variable Name", "Description")
  }
  #vars$table <- ifelse(exists(model.load$table), model.load$table, )
  #-----------------------------------------------------------------------------
  
  # Load Equations Section
  
  #-----------------------------------------------------------------------------
  eqns$main <- model.load$main
  if (!is.null(model.load$eqn.descriptions)) {
    eqns$eqn.descriptions <- model.load$eqn.descriptions
  } else {
    eqns$eqn.descriptions <- rep("", each = model.load$n.eqns)
  }
  eqns$n.eqns <- length(model.load$main) #number of equations in model (not including rates)
  
  eqns$rate.eqns <- model.load$rate.eqns #load rate equations
  eqns$time.dep.eqns = model.load$time.dep.eqns #load all time dependent eqns
  eqns$additional.eqns = model.load$additional.eqns #load all additional eqns -time, rate, etc...
  eqns$first.run <- model.load$first.run
  eqns$eqn.info <- model.load$eqn.info
  
  #-----------------------------------------------------------------------------
  
  # Load Parameter Section
  
  #-----------------------------------------------------------------------------
  #load total parameters from eqns, inputs, outputs (sum of vectors)
  params$vars.all <- model.load$vars.all
  params$vals.all <- model.load$vals.all
  params$comments.all <- model.load$comments.all

  if (!is.null(model.load$param.table)) {
    params$param.table <- model.load$param.table
  } else {
    params$param.table <- data.frame(params$vars.all, params$vals.all, params$comments.all)
    colnames(params$param.table) <- c("Parameter", "Value", "Description")
  }

  #load parameters from equations
  params$eqns.vars = model.load$eqns.vars
  params$eqns.vals = model.load$eqns.vals
  params$eqns.comments = model.load$eqns.comments
  params$first.param.eqn.stored = model.load$first.param.eqn.stored
  #load parameters for input variables
  params$inputs.vars = model.load$inputs.vars
  params$inputs.vals = model.load$inputs.vals
  params$inputs.comments = model.load$inputs.comments
  params$first.inputs.stored = model.load$first.inputs.stored
  #load parameters for output variables
  params$outputs.vars = model.load$outputs.vars
  params$outputs.vals = model.load$outputs.vals
  params$outputs.comments = model.load$outputs.comments
  params$first.outputs.stored = model.load$first.outputs.stored
  #load parameters from rate variables
  params$rate.eqn.vars = model.load$rate.eqn.vars
  params$rate.eqn.vals = model.load$rate.eqn.vals
  params$rate.eqn.comments = model.load$rate.eqn.comments
  params$first.rate.eqn.stored = model.load$first.rate.eqn.stored
  params$rate.params = model.load$rate.params
  #load parameterts from time dependent equations
  params$time.dep.vars = model.load$time.dep.vars
  params$time.dep.values = model.load$time.dep.values
  params$time.dep.comments = model.load$time.dep.comments
  params$first.time.dep.stored = model.load$first.time.dep.stored
  params$parameters.based.on.other.values <- model.load$parameters.based.on.other.values #list of parameters used in rate equations on LHS
  
  #-----------------------------------------------------------------------------
  
  # Load Initial Condition Section
  
  #-----------------------------------------------------------------------------
  #load initial condition variables
  ICs$vals <- model.load$vals
  ICs$comments <- model.load$comments
  if (!is.null(model.load$ICs.table)) {
    ICs$ICs.table <- data.frame(vars$species, ICs$vals, ICs$comments)
    colnames(ICs$ICs.table) <- c("Variable", "Value", "Description")
  } else {
    ICs$ICs.table <- data.frame(vars$species, ICs$vals, ICs$comments)
    colnames(ICs$ICs.table) <- c("Variable", "Value", "Description")
  }
  ICs$first.IC.stored <- model.load$first.IC.stored
  #load other items
  
  #-----------------------------------------------------------------------------
  
  # Load Differential Equations Section
  
  #-----------------------------------------------------------------------------
  DE$eqns <- checkForLoadedValue(model.load$eqns, vector()) 
  DE$eqn.in.latex <- checkForLoadedValue(model.load$eqn.in.latex, vector())
  
  #-----------------------------------------------------------------------------
  
  # Load Input/Outputs Section
  
  #-----------------------------------------------------------------------------
  IO$n.IO <- model.load$n.IO
  IO$bool.IO.exists <- model.load$bool.IO.exists
  IO$bool.IO.added <- model.load$bool.IO.added #boolean to tell differential solver to look for input outputs
  IO$IO.info <- model.load$IO.info
  IO$n.inputs = checkForLoadedValue(model.load$n.inputs, 0)
  IO$n.outputs = checkForLoadedValue(model.load$n.outputs, 0)
  IO$bool.input.exists = checkForLoadedValue(model.load$bool.input.exists, TRUE)
  IO$bool.output.exists = checkForLoadedValue(model.load$bool.output.exists, TRUE)
  IO$bool.output.exists = checkForLoadedValue(model.load$bool.output.exists, FALSE)
  IO$bool.output.exists = checkForLoadedValue(model.load$bool.output.exists, FALSE)
  IO$input.info = checkForLoadedValue(model.load$input.info, data.frame(matrix(ncol = 7, nrow = 0,
                                                                               dimnames = list(NULL, c("Type", 
                                                                                                       "Species", 
                                                                                                       "RateConstant",
                                                                                                       "RateBySpecies", 
                                                                                                       "Vmax", 
                                                                                                       "Kcat", 
                                                                                                       "Enzyme")))))
  IO$output.info = checkForLoadedValue(model.load$output.info, data.frame(matrix(ncol = 7, nrow = 0,
                                                                               dimnames = list(NULL, c("Type", 
                                                                                                       "Species", 
                                                                                                       "RateConstant",
                                                                                                       "RateBySpecies", 
                                                                                                       "Vmax", 
                                                                                                       "Kcat", 
                                                                                                       "Enzyme")))))
  
  # if (!is.null(model.load$n.inputs)) {IO$n.inputs = model.load$n.inputs} else {IO$n.inputs = 0}
  # if (!is.null(model.load$n.outputs)) {IO$n.outputs = model.load$n.outputs} else {IO$n.outputs = 0}
  # if (!is.null(model.load$bool.input.exists)) {IO$bool.input.exists = model.load$bool.input.exists} else {IO$bool.input.exists = TRUE}
  # if (!is.null(model.load$bool.output.exists)) {IO$bool.output.exists = model.load$bool.output.exists} else {IO$bool.output.exists = TRUE}
  # if (!is.null(model.load$bool.input.added)) {IO$bool.input.added = model.load$bool.input.added} else {IO$bool.input.added = FALSE}  
  # if (!is.null(model.load$bool.output.added)) {IO$bool.output.added = model.load$bool.output.added} else {IO$bool.output.added = FALSE}
  # if (!is.null(model.load$input.info)) {IO$input.info = model.load$input.info} else{input.info = data.frame(matrix(ncol = 7, nrow = 0,
  #                                                                                                          dimnames = list(NULL, c("Type", 
  #                                                                                                                                  "Species", 
  #                                                                                                                                  "RateConstant",
  #                                                                                                                                  "RateBySpecies", 
  #                                                                                                                                  "Vmax", 
  #                                                                                                                                  "Kcat", 
  #                                                                                                                                  "Enzyme"))))}
  # if (!is.null(model.load$output.info)) {IO$output.info = model.load$output.info} else{output.info = data.frame(matrix(ncol = 7, nrow = 0,
  #                                                                                                          dimnames = list(NULL, c("Type", 
  #                                                                                                                                  "Species", 
  #                                                                                                                                  "RateConstant",
  #                                                                                                                                  "RateBySpecies", 
  #                                                                                                                                  "Vmax", 
  #                                                                                                                                  "Kcat", 
  #                                                                                                                                  "Enzyme"))))}

  
  #-----------------------------------------------------------------------------
  
  # Load Options Section
  
  #-----------------------------------------------------------------------------
  options$time.start <- model.load$time.start
  options$time.end <- model.load$time.end
  options$time.step <- model.load$time.step
  options$time.scale.bool <- model.load$time.scale.bool
  options$time.scale.value <- model.load$time.scale.value
  options$ode.solver.type <- model.load$ode.solver.type
  
  #-----------------------------------------------------------------------------
  
  # Load Results Section
  
  #-----------------------------------------------------------------------------
  results$model <- model.load$model
  results$is.pp <- model.load$is.pp
  results$pp.eqns <- model.load$pp.eqns
  results$pp.eqns.col <- model.load$pp.eqns.col
  results$pp.vars <- model.load$pp.vars
  results$pp.model <- model.load$pp.model

  #-----------------------------------------------------------------------------
  
  # Load Logs Section
  
  #-----------------------------------------------------------------------------
  logs$IO.logs <- checkForLoadedValue(model.load$IO.logs, vector())
  logs$input.logs <- checkForLoadedValue(model.load$input.logs, vector())
  logs$output.logs <- checkForLoadedValue(model.load$output.logs, vector())
  
  #-----------------------------------------------------------------------------
  
  # Update all UI that need values from load
  
  #-----------------------------------------------------------------------------
  my.choices <- paste0(seq(eqns$n.eqns), ") ", eqns$main)
  
  # updatePickerInput(session, 
  #                   "compare_models_select_vars",
  #                   choices = params$vars.all)
  
  updatePickerInput(session,
                    "eqnCreate_selectEqnForDescription",
                    choices = my.choices)
  
  updatePickerInput(session = session
                    ,"createVar_deleteVarPicker"
                    ,choices = sort(vars$species))
  
  updatePickerInput(session, 
                    "eqnCreate_rate_firstvar",
                    choices = params$vars.all)
  
  updatePickerInput(session
                    ,"InOut_selectVar"
                    ,choices = sort(vars$species))
  
  updatePickerInput(session
                    ,"Inout_delete_IO_eqn"
                    ,choices = seq(IO$n.IO))
  
  updatePickerInput(session,
                    'eqnCreate_edit_select_equation'
                    ,choices = seq(length(eqns$main)))
  
  updatePickerInput(session
                    ,"enzyme_deg_enzyme"#updates output enzyme choices for enzyme degradation
                    ,choices = sort(vars$species))
  
  updatePickerInput(session,
                    "MA_species"
                    ,choices = sort(vars$species))
  
  updatePickerInput(session #updates output substrate choices for enzyme degradation
                    ,"enzyme_deg_substrate"
                    ,choices = sort(vars$species))
  
  # Update Model Options -------------------------------------------------------
  updateTextInput(session,
                  "execute_time_start",
                  value = options$time.start)
  updateTextInput(session,
                  "execute_time_end",
                  value = options$time.end)
  updateTextInput(session,
                  "execute_time_step",
                  value = options$time.step)
  updateCheckboxInput(session,
                      "execute_turnOn_time_scale_var",
                      value = options$time.scale.bool)
  updateTextInput(session,
                  "execute_time_scale_var",
                  value = options$time.scale.value)
  updatePickerInput(session,
                    "execute_ode_solver_type",
                    selected = options$ode.solver.type)

  
  #------------------------------------------------
  #Parameters Rendered from Equations
  #------------------------------------------------
  output$parameters_eqns_header <- renderUI({
    h4("Parameters From Equations")
  })
  
  output$parameters_eqns <- renderUI({
    number_parameters = length(params$eqns.vars)
    
    fluidRow(column(width=2
                    ,lapply(seq(number_parameters), function(i){
                      textInput(inputId=paste0("parameter_", as.character(i))
                                ,label=params$eqns.vars[i]
                                ,value = ifelse(params$first.param.eqn.stored, params$eqns.vals[i], "0"))
                    }))
             ,column(width=8
                     ,lapply(seq(number_parameters), function(i){
                       textInput(inputId=paste0("parameter_description_", as.character(i))
                                 ,label="Parameter Description"
                                 ,value =ifelse(params$first.param.eqn.stored, params$eqns.comments[i], ""))
                     }))
    ) #end fluidRow
  })
  
  #------------------------------------------------
  #Parameters Rendered from Input values
  #------------------------------------------------
  if(length(params$inputs.vars)>0){
    output$parameters_inputs_header <- renderUI({
      h4("Parameters From Inputs")
    })
    
    output$parameters_inputs <- renderUI({
      number_parameters = length(params$inputs.vars) #find number of parameters in inputs
      observe({print(paste("Lenght out:", number_parameters))
        print(paste("parame value:", params$param.input[1]))
      })
      #generate labels with paramters name to put value into
      #generate text input next to it to put comment for variable into
      #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
      fluidRow(column(width=2
                      ,lapply(seq(number_parameters), function(i){
                        textInput(inputId=paste0("parameter_input_", as.character(i))
                                  ,label=params$inputs.vars[i]
                                  ,value = ifelse(params$first.inputs.stored, params$inputs.vals[i], "0"))
                      }))
               ,column(width=8
                       ,lapply(seq(number_parameters), function(i){
                         textInput(inputId=paste0("parameter_description_input_", as.character(i))
                                   ,label="Parameter Description"
                                   ,value =ifelse(params$first.inputs.stored, params$inputs.comments[i], ""))
                       }))
      ) #end fluidRow
    })
  }
  
  #------------------------------------------------
  #Parameters Rendered from Output values
  #------------------------------------------------
  if(length(params$outputs.vars)>0){
    output$parameters_outputs_header <- renderUI({
      h4("Parameters From Output")
    })
    
    output$parameters_outputs <- renderUI({
      number_parameters = length(params$outputs.vars) #find number of parameters in inputs
      observe({print(paste("Lenght out:", number_parameters))
        print(paste("parame value:", params$param.output[1]))
      })
      
      #generate labels with paramters name to put value into
      #generate text input next to it to put comment for variable into
      #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
      fluidRow(column(width=2
                      ,lapply(seq(number_parameters), function(i){
                        textInput(inputId=paste0("parameter_output_", as.character(i))
                                  ,label=params$outputs.vars[i]
                                  ,value = ifelse(params$first.outputs.stored, params$outputs.vals[i], "0"))
                      }))
               ,column(width=8
                       ,lapply(seq(number_parameters), function(i){
                         textInput(inputId=paste0("parameter_description_output_", as.character(i))
                                   ,label="Parameter Description"
                                   ,value =ifelse(params$first.outputs.stored, params$outputs.comments[i], ""))
                       }))
      ) #end fluidRow
    })
  }
  
  #------------------------------------------------
  #Parameters Rendered from rateEqn values
  #------------------------------------------------
  if(length(params$rate.eqn.vars)>0){
    output$parameters_rateEqns_header <- renderUI({
      h4("Parameters From Rate Equation")
    })
    
    output$parameters_rateEqns <- renderUI({
      number_parameters = length(params$rate.eqn.vars) #find number of parameters in inputs
      observe({print(paste("Lenght out:", number_parameters))
        print(paste("parame value:", params$rate.eqn.vars[1]))
      })
      
      #generate labels with paramters name to put value into
      #generate text input next to it to put comment for variable into
      #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
      fluidRow(column(width=2
                      ,lapply(seq(number_parameters), function(i){
                        textInput(inputId=paste0("parameter_rateEqn_", as.character(i))
                                  ,label=params$rate.eqn.vars[i]
                                  ,value = ifelse(params$first.rate.eqn.stored, params$rate.eqn.vals[i], "0"))
                      }))
               ,column(width=8
                       ,lapply(seq(number_parameters), function(i){
                         textInput(inputId=paste0("parameter_description_rateEqn_", as.character(i))
                                   ,label="Parameter Description"
                                   ,value =ifelse(params$first.rate.eqn.stored, params$rate.eqn.comments[i], ""))
                       }))
      ) #end fluidRow
    })
  }
  
  #------------------------------------------------
  #Parameters Rendered from TimeDependent values
  #------------------------------------------------
  if(length(params$time.dep.vars)>0)
  {
    output$parameters_TD_eqns_header <- renderUI({
      h4("Parameters From Time Dependent Equations")
    })
    
    output$parameters_TD_eqns <- renderUI({
      number_parameters = length(params$time.dep.vars) #find number of parameters in inputs
      
      #generate labels with paramters name to put value into
      #generate text input next to it to put comment for variable into
      #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
      fluidRow(column(width = 2
                      ,lapply(seq(number_parameters), function(i){
                        textInput(inputId = paste0("parameter_TD_", as.character(i))
                                  ,label = params$time.dep.vars[i]
                                  ,value = ifelse(params$first.time.dep.stored, params$time.dep.vars_values[i], "0"))
                      }))
               ,column(width = 8
                       ,lapply(seq(number_parameters), function(i){
                         textInput(inputId = paste0("parameter_description_TD_", as.character(i))
                                   ,label = "Parameter Description"
                                   ,value = ifelse(params$first.time.dep.stored, params$time.dep.vars_comments[i], ""))
                       }))
      ) #end fluidRow
    })
  }
  
  
  #------------------------------------------------
  # ICs rendered from stored data
  #------------------------------------------------
  output$ICs_UI <- renderUI({
    number_var = length(vars$species)
    
    fluidRow(column(width=4
                    ,lapply(seq(number_var), function(i){
                      textInput(inputId=paste0("IC_", as.character(i))
                                ,label=paste(vars$species[i], "initial value:")
                                ,value = ICs$vals[i])
                    }))
             # ,column(width=1
             #         ,lapply(seq(number_parameters), function(i){
             #           checkboxInput(inputId=paste0("parameter_check_unknown_", as.character(i))
             #                     ,label="Value Unknown"
             #                     ,value = FALSE)
             #         }))
             ,column(width=6
                     ,lapply(seq(number_var), function(i){
                       textInput(inputId=paste0("ICs_description_", as.character(i))
                                 ,label="Comment"
                                 ,value = ICs$comments[i])
                     })
             )
    ) #end fluidRown
  })
  
  #------------------------------------------------
  # ICs rendered from stored data
  #------------------------------------------------
  # output$ICs_UI <- renderUI({
  #   number_var = length(vars$species)
  #   
  #   fluidRow(column(width=2
  #                   ,lapply(seq(number_var), function(i){
  #                     textInput(inputId=paste0("IC_", as.character(i))
  #                               ,label=paste(vars$species[i], "initial value:")
  #                               ,value = ICs$vals[i])
  #                   }))
  #            # ,column(width=1
  #            #         ,lapply(seq(number_parameters), function(i){
  #            #           checkboxInput(inputId=paste0("parameter_check_unknown_", as.character(i))
  #            #                     ,label="Value Unknown"
  #            #                     ,value = FALSE)
  #            #         }))
  #            ,column(width=8
  #                    ,lapply(seq(number_var), function(i){
  #                      textInput(inputId=paste0("ICs_description_", as.character(i))
  #                                ,label="Comment"
  #                                ,value = ICs$comments[i])
  #                    })
  #            )
  #   ) #end fluidRown
  # })
  
  #------------------------ L O O P  M O D E L --------------------------------#
  
################################################################################

#Load initial Conditions in

################################################################################
  #------------------------------------------------
  #Parameters Rendered from Equations
  #------------------------------------------------
  output$loop_parameters_eqns_header <- renderUI({
    #req(input$eqnCreate_addEqnToVector)
    h4("Parameters From Equations")
  })
  
  output$loop_parameters_eqns <- renderUI({
    #req(input$eqnCreate_addEqnToVector)
    number_parameters = length(params$eqns.vars)
    
    fluidRow(column(width=2
                    ,lapply(seq(number_parameters), function(i){
                      textInput(inputId=paste0("loop_parameter_", as.character(i))
                                ,label=params$eqns.vars[i]
                                ,value = ifelse(params$first.param.eqn.stored, params$eqns.vals[i], "0"))
                    }))
             ,column(width=8
                     ,lapply(seq(number_parameters), function(i){
                       textInput(inputId=paste0("loop_parameter_description_", as.character(i))
                                 ,label="Parameter Description"
                                 ,value =ifelse(params$first.param.eqn.stored, params$eqns.comments[i], ""))
                     }))
    ) #end fluidRow
  })
  
  #------------------------------------------------
  #Parameters Rendered from Input values
  #------------------------------------------------
  output$loop_parameters_inputs_header <- renderUI({
    #req(input$Inout_addInVarToDf)
    h4("Parameters From Inputs")
  })
  
  output$loop_parameters_inputs <- renderUI({
    req(input$Inout_addInVarToDf)
    number_parameters = length(params$inputs.vars) #find number of parameters in inputs
    
    #generate labels with paramters name to put value into
    #generate text input next to it to put comment for variable into
    #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
    fluidRow(column(width=2
                    ,lapply(seq(number_parameters), function(i){
                      textInput(inputId=paste0("loop_parameter_input_", as.character(i))
                                ,label=params$inputs.vars[i]
                                ,value = ifelse(params$first.inputs.stored, params$inputs.vals[i], "0"))
                    }))
             ,column(width=8
                     ,lapply(seq(number_parameters), function(i){
                       textInput(inputId=paste0("loop_parameter_description_input_", as.character(i))
                                 ,label="Parameter Description"
                                 ,value =ifelse(params$first.inputs.stored, params$inputs.comments[i], ""))
                     }))
    ) #end fluidRow
  })
  
  #------------------------------------------------
  #Parameters Rendered from Output values
  #------------------------------------------------
  output$loop_parameters_outputs_header <- renderUI({
    #req(input$Inout_addOutVarToDf)
    h4("Parameters From Output")
  })
  
  output$loop_parameters_outputs <- renderUI({
    req(input$Inout_addOutVarToDf)
    number_parameters = length(params$outputs.vars) #find number of parameters in inputs
    
    #generate labels with paramters name to put value into
    #generate text input next to it to put comment for variable into
    #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
    fluidRow(column(width = 2
                    ,lapply(seq(number_parameters), function(i){
                      textInput(inputId = paste0("loop_parameter_output_", as.character(i))
                                ,label = params$outputs.vars[i]
                                ,value = ifelse(params$first.outputs.stored, params$outputs.vals[i], "0"))
                    }))
             ,column(width = 8
                     ,lapply(seq(number_parameters), function(i){
                       textInput(inputId = paste0("loop_parameter_description_output_", as.character(i))
                                 ,label = "Parameter Description"
                                 ,value = ifelse(params$first.outputs.stored, params$outputs.comments[i], ""))
                     }))
    ) #end fluidRow
  })
  
  output$loop_ICs_UI <- renderUI({
    #req(input$Button_load_model)
    number_var = length(vars$species)
    
    
    fluidRow(column(width = 2
                    ,lapply(seq(number_var), function(i){
                      textInput(inputId = paste0("loop_IC_", as.character(i))
                                ,label = paste(vars$species[i], "initial value:")
                                ,value = ifelse(ICs$first.IC.stored, ICs$vals[i], "0"))
                    }))
             # ,column(width=1
             #         ,lapply(seq(number_parameters), function(i){
             #           checkboxInput(inputId=paste0("parameter_check_unknown_", as.character(i))
             #                     ,label="Value Unknown"
             #                     ,value = FALSE)
             #         }))
             # ,column(width=8
             #         ,lapply(seq(number_var), function(i){
             #           textInput(inputId=paste0("loop_ICs_description_", as.character(i))
             #                     ,label="Comment"
             #                     ,value = ifelse(ICs$first.IC.stored, ICs$comments[i], ""))
             #         })
             # )
    ) #end fluidRown
    
  })
  
})

# rv <- reactiveValues(vars_in_model = vector() #stores model variable
#                      ,eqns_in_model = vector() #stores eqn type in model
#                      ,parameters_in_model = vector() #store parameter variable
#                      ,parameter_values = vector() #store parameter value
#                      ,IC_values = vector() #store initial condition value
#                      ,diffEQs = vector() #store differential equations
#                      ,number_of_equations = 0 #stores number of total equations in model (used to autofill names of some var)
#                      ,inputOutputs_df = data.frame(matrix(ncol=6, nrow=0,
#                                                           dimnames=list(NULL, c("In_or_Out", "Type", "Species", "VarName", "VarValue", "VarComment"))))
#                      ,first_inOut = TRUE #determines if In/out input has been given yet.  Avoids adding to df error
#                      ,In_out_added = FALSE
#                      ,first_run = TRUE) #determine if first equation is added yet or not
# 
