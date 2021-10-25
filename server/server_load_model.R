
################################ Load Server #################################

#when load model button is pressed, the .rds file is loaded in and its components are broken apart and added to the model
observeEvent(input$load_model, {
  model.load <- readRDS(input$load_model$datapath)
  rv$vars_in_model <- model.load$vars_in_model
  rv$eqns_in_model <- model.load$eqns_in_model
  #load total parameters from eqns, inputs, outputs (sum of vectors)
  rv$parameters_in_model <- model.load$parameters_in_model
  rv$parameter_values <- model.load$parameter_values
  rv$parameter_descriptions <- model.load$parameter_descriptions
  #load parameters from equations
  rv$param_eqns = model.load$param_eqns
  rv$param_eqns_values = model.load$param_eqns_values
  rv$param_eqns_comments = model.load$param_eqns_comments
  rv$first_param_eqn_stored = model.load$first_param_eqn_stored
  #load parameters for input variables
  rv$param_inputs = model.load$param_inputs
  rv$param_inputs_values = model.load$param_inputs_values
  rv$param_inputs_comments = model.load$param_inputs_comments
  rv$first_param_inputs_stored = model.load$first_param_inputs_stored
  #load parameters for output variables
  rv$param_outputs = model.load$param_outputs
  rv$param_outputs_values = model.load$param_outputs_values
  rv$param_outputs_comments = model.load$param_outputs_comments
  rv$first_param_outputs_stored = model.load$first_param_outputs_stored
  #load parameters from rate variables
  rv$param_rateEqn = model.load$param_rateEqn
  rv$param_rateEqn_values = model.load$param_rateEqn_values
  rv$param_rateEqn_comments = model.load$param_rateEqn_comments
  rv$first_param_rateEqn_stored = model.load$first_param_rateEqn_stored
  #load parameterts from time dependent equations
  rv$param_timeDependentEqn = model.load$param_timeDependentEqn
  rv$param_timeDependentEqn_values = model.load$param_timeDependentEqn_values
  rv$param_timeDependentEqn_comments = model.load$param_timeDependentEqn_comments
  rv$first_param_timeDependentEqn_stored = model.load$first_param_timeDependentEqn_stored
  #load initial condition variables
  rv$IC_values <- model.load$IC_values
  rv$IC_descriptions <- model.load$IC_descriptions
  #load other items
  rv$diffEQs <- model.load$diffEQs #differential equations
  rv$number_of_equations <- model.load$number_of_equations #number of equations in model (not including rates)
  rv$number_of_IO <- model.load$number_of_IO
  rv$rate_eqns <- model.load$rate_eqns #load rate equations
  rv$time_dependent_eqns = model.load$time_dependent_eqns #load all time dependent eqns
  rv$additional_eqns = model.load$additional_eqns #load all additional eqns -time, rate, etc...
  rv$parameters_based_on_other_values <- model.load$parameters_based_on_other_values #list of parameters used in rate equations on LHS
  rv$inputOutputs_df <- model.load$inputOutputs_df #dataframe containing all the info for input output data
  rv$first_inOut <- model.load$first_inOut
  rv$In_out_added <- model.load$In_out_added #boolean to tell differential solver to look for input outputs
  rv$first_IC_stored <- model.load$first_IC_stored
  rv$first_run <- model.load$first_run
  
  #load options
  model.options$time.start <- model.load$time.start
  model.options$time.end <- model.load$time.end
  model.options$time.step <- model.load$time.step
  model.options$time.scale.bool <- model.load$time.scale.bool
  model.options$time.scale.value <- model.load$time.scale.value
  model.options$ode.solver.type <- model.load$ode.solver.type
  
  #load model results
  model.results$model <- model.load$model
  model.results$is.pp <- model.load$is.pp
  model.results$pp.eqns <- model.load$pp.eqns
  model.results$pp.eqns.col <- model.load$pp.eqns.col
  model.results$pp.vars <- model.load$pp.vars
  model.results$pp.model <- model.load$pp.model
  observe({print("IS POST PROCESSED")})
  observe({print(model.results$is.pp)})
  observe({print("THE MODEL DATAFRAME LOADED:")})
  observe({print(head(model.results$model))})
  observe({print("POST PROCESSED MODEL")})
  observe({print(head(model.results$pp.model))})
  
  data$eqn_info <- model.load$eqn_info
  
  logs$IO_logs <- model.load$IO_logs
  
  updatePickerInput(session, 
                    "eqnCreate_rate_firstvar",
                    choices = rv$parameters_in_model)
  
  updatePickerInput(session
                    ,"InOut_selectVar"
                    ,choices = sort(rv$vars_in_model))
  
  updatePickerInput(session
                    ,"Inout_delete_IO_eqn"
                    ,choices = seq(rv$number_of_IO))
  
  updatePickerInput(session,
                    'eqnCreate_edit_select_equation'
                    ,choices = seq(length(rv$eqns_in_model)))
  
  updatePickerInput(session
                    ,"enzyme_deg_enzyme"#updates output enzyme choices for enzyme degradation
                    ,choices = sort(rv$vars_in_model))
  
  updatePickerInput(session,
                    "MA_species"
                    ,choices = sort(rv$vars_in_model))
  
  updatePickerInput(session #updates output substrate choices for enzyme degradation
                    ,"enzyme_deg_substrate"
                    ,choices = sort(rv$vars_in_model))
  
  # Update Model Options -------------------------------------------------------
  updateTextInput(session,
                  "execute_time_start",
                  value = model.options$time.start)
  updateTextInput(session,
                  "execute_time_end",
                  value = model.options$time.end)
  updateTextInput(session,
                  "execute_time_step",
                  value = model.options$time.step)
  updateCheckboxInput(session,
                      "execute_turnOn_time_scale_var",
                      value = model.options$time.scale.bool)
  updateTextInput(session,
                  "execute_time_scale_var",
                  value = model.options$time.scale.value)
  updatePickerInput(session,
                    "execute_ode_solver_type",
                    selected = model.options$ode.solver.type)

  
  #------------------------------------------------
  #Parameters Rendered from Equations
  #------------------------------------------------
  output$parameters_eqns_header <- renderUI({
    h4("Parameters From Equations")
  })
  
  output$parameters_eqns <- renderUI({
    number_parameters = length(rv$param_eqns)
    
    fluidRow(column(width=2
                    ,lapply(seq(number_parameters), function(i){
                      textInput(inputId=paste0("parameter_", as.character(i))
                                ,label=rv$param_eqns[i]
                                ,value = ifelse(rv$first_param_eqn_stored, rv$param_eqns_values[i], "0"))
                    }))
             ,column(width=8
                     ,lapply(seq(number_parameters), function(i){
                       textInput(inputId=paste0("parameter_description_", as.character(i))
                                 ,label="Parameter Description"
                                 ,value =ifelse(rv$first_param_eqn_stored, rv$param_eqns_comments[i], ""))
                     }))
    ) #end fluidRow
  })
  
  #------------------------------------------------
  #Parameters Rendered from Input values
  #------------------------------------------------
  if(length(rv$param_inputs)>0){
    output$parameters_inputs_header <- renderUI({
      h4("Parameters From Inputs")
    })
    
    output$parameters_inputs <- renderUI({
      number_parameters = length(rv$param_inputs) #find number of parameters in inputs
      observe({print(paste("Lenght out:", number_parameters))
        print(paste("parame value:", rv$param_input[1]))
      })
      #generate labels with paramters name to put value into
      #generate text input next to it to put comment for variable into
      #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
      fluidRow(column(width=2
                      ,lapply(seq(number_parameters), function(i){
                        textInput(inputId=paste0("parameter_input_", as.character(i))
                                  ,label=rv$param_inputs[i]
                                  ,value = ifelse(rv$first_param_inputs_stored, rv$param_inputs_values[i], "0"))
                      }))
               ,column(width=8
                       ,lapply(seq(number_parameters), function(i){
                         textInput(inputId=paste0("parameter_description_input_", as.character(i))
                                   ,label="Parameter Description"
                                   ,value =ifelse(rv$first_param_inputs_stored, rv$param_inputs_comments[i], ""))
                       }))
      ) #end fluidRow
    })
  }
  
  #------------------------------------------------
  #Parameters Rendered from Output values
  #------------------------------------------------
  if(length(rv$param_outputs)>0){
    output$parameters_outputs_header <- renderUI({
      h4("Parameters From Output")
    })
    
    output$parameters_outputs <- renderUI({
      number_parameters = length(rv$param_outputs) #find number of parameters in inputs
      observe({print(paste("Lenght out:", number_parameters))
        print(paste("parame value:", rv$param_output[1]))
      })
      
      #generate labels with paramters name to put value into
      #generate text input next to it to put comment for variable into
      #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
      fluidRow(column(width=2
                      ,lapply(seq(number_parameters), function(i){
                        textInput(inputId=paste0("parameter_output_", as.character(i))
                                  ,label=rv$param_outputs[i]
                                  ,value = ifelse(rv$first_param_outputs_stored, rv$param_outputs_values[i], "0"))
                      }))
               ,column(width=8
                       ,lapply(seq(number_parameters), function(i){
                         textInput(inputId=paste0("parameter_description_output_", as.character(i))
                                   ,label="Parameter Description"
                                   ,value =ifelse(rv$first_param_outputs_stored, rv$param_outputs_comments[i], ""))
                       }))
      ) #end fluidRow
    })
  }
  
  #------------------------------------------------
  #Parameters Rendered from rateEqn values
  #------------------------------------------------
  if(length(rv$param_rateEqn)>0){
    output$parameters_rateEqns_header <- renderUI({
      h4("Parameters From Rate Equation")
    })
    
    output$parameters_rateEqns <- renderUI({
      number_parameters = length(rv$param_rateEqn) #find number of parameters in inputs
      observe({print(paste("Lenght out:", number_parameters))
        print(paste("parame value:", rv$param_rateEqn[1]))
      })
      
      #generate labels with paramters name to put value into
      #generate text input next to it to put comment for variable into
      #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
      fluidRow(column(width=2
                      ,lapply(seq(number_parameters), function(i){
                        textInput(inputId=paste0("parameter_rateEqn_", as.character(i))
                                  ,label=rv$param_rateEqn[i]
                                  ,value = ifelse(rv$first_param_rateEqn_stored, rv$param_rateEqn_values[i], "0"))
                      }))
               ,column(width=8
                       ,lapply(seq(number_parameters), function(i){
                         textInput(inputId=paste0("parameter_description_rateEqn_", as.character(i))
                                   ,label="Parameter Description"
                                   ,value =ifelse(rv$first_param_rateEqn_stored, rv$param_rateEqn_comments[i], ""))
                       }))
      ) #end fluidRow
    })
  }
  
  #------------------------------------------------
  #Parameters Rendered from TimeDependent values
  #------------------------------------------------
  if(length(rv$param_timeDependentEqn)>0)
  {
    output$parameters_TD_eqns_header <- renderUI({
      h4("Parameters From Time Dependent Equations")
    })
    
    output$parameters_TD_eqns <- renderUI({
      number_parameters = length(rv$param_timeDependentEqn) #find number of parameters in inputs
      
      #generate labels with paramters name to put value into
      #generate text input next to it to put comment for variable into
      #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
      fluidRow(column(width=2
                      ,lapply(seq(number_parameters), function(i){
                        textInput(inputId=paste0("parameter_TD_", as.character(i))
                                  ,label=rv$param_timeDependentEqn[i]
                                  ,value = ifelse(rv$first_param_timeDependentEqn_stored, rv$param_timeDependentEqn_values[i], "0"))
                      }))
               ,column(width=8
                       ,lapply(seq(number_parameters), function(i){
                         textInput(inputId=paste0("parameter_description_TD_", as.character(i))
                                   ,label="Parameter Description"
                                   ,value =ifelse(rv$first_param_timeDependentEqn_stored, rv$param_timeDependentEqn_comments[i], ""))
                       }))
      ) #end fluidRow
    })
  }
  
  
  #------------------------------------------------
  # ICs rendered from stored data
  #------------------------------------------------
  output$ICs_UI <- renderUI({
    number_var = length(rv$vars_in_model)
    
    fluidRow(column(width=4
                    ,lapply(seq(number_var), function(i){
                      textInput(inputId=paste0("IC_", as.character(i))
                                ,label=paste(rv$vars_in_model[i], "initial value:")
                                ,value = rv$IC_values[i])
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
                                 ,value = rv$IC_descriptions[i])
                     })
             )
    ) #end fluidRown
  })
  
  #------------------------------------------------
  # ICs rendered from stored data
  #------------------------------------------------
  # output$ICs_UI <- renderUI({
  #   number_var = length(rv$vars_in_model)
  #   
  #   fluidRow(column(width=2
  #                   ,lapply(seq(number_var), function(i){
  #                     textInput(inputId=paste0("IC_", as.character(i))
  #                               ,label=paste(rv$vars_in_model[i], "initial value:")
  #                               ,value = rv$IC_values[i])
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
  #                                ,value = rv$IC_descriptions[i])
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
    number_parameters = length(rv$param_eqns)
    
    fluidRow(column(width=2
                    ,lapply(seq(number_parameters), function(i){
                      textInput(inputId=paste0("loop_parameter_", as.character(i))
                                ,label=rv$param_eqns[i]
                                ,value = ifelse(rv$first_param_eqn_stored, rv$param_eqns_values[i], "0"))
                    }))
             ,column(width=8
                     ,lapply(seq(number_parameters), function(i){
                       textInput(inputId=paste0("loop_parameter_description_", as.character(i))
                                 ,label="Parameter Description"
                                 ,value =ifelse(rv$first_param_eqn_stored, rv$param_eqns_comments[i], ""))
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
    number_parameters = length(rv$param_inputs) #find number of parameters in inputs
    
    #generate labels with paramters name to put value into
    #generate text input next to it to put comment for variable into
    #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
    fluidRow(column(width=2
                    ,lapply(seq(number_parameters), function(i){
                      textInput(inputId=paste0("loop_parameter_input_", as.character(i))
                                ,label=rv$param_inputs[i]
                                ,value = ifelse(rv$first_param_inputs_stored, rv$param_inputs_values[i], "0"))
                    }))
             ,column(width=8
                     ,lapply(seq(number_parameters), function(i){
                       textInput(inputId=paste0("loop_parameter_description_input_", as.character(i))
                                 ,label="Parameter Description"
                                 ,value =ifelse(rv$first_param_inputs_stored, rv$param_inputs_comments[i], ""))
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
    number_parameters = length(rv$param_outputs) #find number of parameters in inputs
    
    #generate labels with paramters name to put value into
    #generate text input next to it to put comment for variable into
    #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
    fluidRow(column(width=2
                    ,lapply(seq(number_parameters), function(i){
                      textInput(inputId=paste0("loop_parameter_output_", as.character(i))
                                ,label=rv$param_outputs[i]
                                ,value = ifelse(rv$first_param_outputs_stored, rv$param_outputs_values[i], "0"))
                    }))
             ,column(width=8
                     ,lapply(seq(number_parameters), function(i){
                       textInput(inputId=paste0("loop_parameter_description_output_", as.character(i))
                                 ,label="Parameter Description"
                                 ,value =ifelse(rv$first_param_outputs_stored, rv$param_outputs_comments[i], ""))
                     }))
    ) #end fluidRow
  })
  
  output$loop_ICs_UI <- renderUI({
    #req(input$Button_load_model)
    number_var = length(rv$vars_in_model)
    
    
    fluidRow(column(width=2
                    ,lapply(seq(number_var), function(i){
                      textInput(inputId=paste0("loop_IC_", as.character(i))
                                ,label=paste(rv$vars_in_model[i], "initial value:")
                                ,value = ifelse(rv$first_IC_stored, rv$IC_values[i], "0"))
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
             #                     ,value = ifelse(rv$first_IC_stored, rv$IC_descriptions[i], ""))
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
