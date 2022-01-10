##############################################################################

#LooP

##############################################################################

#---------------------------- P A R A M E T E R S ------------------------------


#------------------------------------------------
#Parameters Rendered from Equations
#------------------------------------------------
output$loop_parameters_eqns_header <- renderUI({
  req(input$eqnCreate_addEqnToVector)
  h4("Parameters From Equations")
})

output$loop_parameters_eqns <- renderUI({
  req(input$eqnCreate_addEqnToVector)
  number_parameters = length(params$eqns.vars)
  
  fluidRow(column(width = 2
                  ,lapply(seq(number_parameters), function(i){
                    textInput(inputId = paste0("loop_parameter_", as.character(i))
                              ,label = params$eqns.vars[i]
                              ,value = ifelse(params$first.param.eqn.stored, params$eqns.vals[i], "0"))
                  }))
           ,column(width = 8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId = paste0("loop_parameter_description_", as.character(i))
                               ,label = "Parameter Description"
                               ,value = ifelse(params$first.param.eqn.stored, params$eqns.comments[i], ""))
                   }))
  ) #end fluidRow
})

#------------------------------------------------
#Parameters Rendered from Input values
#------------------------------------------------
output$loop_parameters_inputs_header <- renderUI({
  req(input$Inout_addInVarToDf)
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
  req(input$Inout_addOutVarToDf)
  h4("Parameters From Output")
})

output$loop_parameters_outputs <- renderUI({
  req(input$Inout_addOutVarToDf)
  number_parameters = length(params$outputs.vars) #find number of parameters in inputs
  
  #generate labels with paramters name to put value into
  #generate text input next to it to put comment for variable into
  #ifelse in value is used to put the current value into the text input if it exists otherwise a 0 or ""
  fluidRow(column(width=2
                  ,lapply(seq(number_parameters), function(i){
                    textInput(inputId=paste0("loop_parameter_output_", as.character(i))
                              ,label=params$outputs.vars[i]
                              ,value = ifelse(params$first.outputs.stored, params$outputs.vals[i], "0"))
                  }))
           ,column(width=8
                   ,lapply(seq(number_parameters), function(i){
                     textInput(inputId=paste0("loop_parameter_description_output_", as.character(i))
                               ,label="Parameter Description"
                               ,value =ifelse(params$first.outputs.stored, params$outputs.comments[i], ""))
                   }))
  ) #end fluidRow
})

################################################################################

#Load initial Conditions in

################################################################################
output$loop_ICs_UI <- renderUI({
  req(input$createVar_addVarToList)
  number_var = length(vars$species)
  
  
  fluidRow(column(width=2
                  ,lapply(seq(number_var), function(i){
                    textInput(inputId=paste0("loop_IC_", as.character(i))
                              ,label=paste(vars$species[i], "initial value:")
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

loop_model_output <- eventReactive(input$loop_execute_run_model, {
  num_ICs<- length(vars$species)
  
  IC_values <- vector()
  IC_comments <- vector()
  for(i in seq(num_ICs)){
    single_value <- eval(parse(text=paste0("input$loop_IC_", as.character(i))))
    IC_values <- append(IC_values, single_value)
    
    single_comment <- eval(parse(text=paste0("input$loop_ICs_description_", as.character(i)))) #evaluate value in textinput
    IC_comments <- append(IC_comments, single_comment) #append comments to vector
  }
  IC_values <- paste(IC_values, sep=" ")
  
  ICs$vals <- as.numeric(IC_values)
  ICs$comments <- IC_comments
  
  #-------------------------------store params
  #store equation parameters
  if(length(params$eqns.vars != 0)){
    params$first.param.eqn.stored <- TRUE #set boolean so parameter values refil when UI is rendered
    num_params <- length(params$eqns.vars)
    param_values <- vector() #create vector to store parameter values
    param_comments <- vector() #create vector to store parameter commemts
    
    for(i in seq(num_params)){
      single_value <- eval(parse(text=paste0("input$loop_parameter_", as.character(i)))) #evaluate value in textinput
      param_values <- append(param_values, single_value) #add value from textinput to vector
      
      single_comment <- eval(parse(text=paste0("input$loop_parameter_description_", as.character(i)))) #evaluate value in textinput
      param_comments <- append(param_comments, single_comment) #append comments to vector
      param_values <- paste(param_values, sep = " ") #drop vector to a single string separated by spaces
      params$eqns.vals <- as.numeric(param_values) #store parameter values to reactive value
      params$eqns.comments <- param_comments # store paramter comments to reactive value
    }
  }
  
  #store input parameters
  if(length(params$inputs.vars != 0)){
    params$first.inputs.stored <- TRUE #set boolean so parameter values refil when UI is rendered
    num_params <- length(params$inputs.vars)
    param_values <- vector() #create vector to store parameter values
    param_comments <- vector() #create vector to store parameter commemts
    
    for(i in seq(num_params)){
      single_value <- eval(parse(text=paste0("input$loop_parameter_input_", as.character(i)))) #evaluate value in textinput
      param_values <- append(param_values, single_value) #add value from textinput to vector
      
      single_comment <- eval(parse(text=paste0("input$loop_parameter_description_input_", as.character(i)))) #evaluate value in textinput
      param_comments <- append(param_comments, single_comment) #append comments to vector
      param_values <- paste(param_values, sep = " ") #drop vector to a single string separated by spaces
      params$inputs.vals <- as.numeric(param_values) #store parameter values to reactive value
      params$inputs.comments <- param_comments # store paramter comments to reactive value
    }
  }
  
  #store output parameters
  if(length(params$outputs.vars != 0)){
    params$first.outputs.stored <- TRUE #set boolean so parameter values refil when UI is rendered
    num_params <- length(params$outputs.vars)
    param_values <- vector() #create vector to store parameter values
    param_comments <- vector() #create vector to store parameter commemts
    
    for(i in seq(num_params)){
      single_value <- eval(parse(text=paste0("input$loop_parameter_output_", as.character(i)))) #evaluate value in textinput
      param_values <- append(param_values, single_value) #add value from textinput to vector
      
      single_comment <- eval(parse(text=paste0("input$loop_parameter_description_output_", as.character(i)))) #evaluate value in textinput
      param_comments <- append(param_comments, single_comment) #append comments to vector
      param_values <- paste(param_values, sep = " ") #drop vector to a single string separated by spaces
      params$outputs.vals <- as.numeric(param_values) #store parameter values to reactive value
      params$outputs.comments <- param_comments # store paramter comments to reactive value
    }
  }
  
  #Store all Paramters to overall vector
  params$vars.all = c(params$eqns.vars, params$inputs.vars, params$outputs.vars)
  params$vals.all = c(params$eqns.vals, params$inputs.vals, params$outputs.vals)
  params$comments.all = c(params$eqns.comments, params$inputs.comments, params$outputs.comments)
  
  #run the model 
  #set up time for solver
  time_in <- as.numeric(input$loop_execute_time_start)
  time_out <- as.numeric(input$loop_execute_time_end)
  time_step <- as.numeric(input$loop_execute_time_step)
  times <- seq(time_in, time_out, by=time_step)
  
  #initialize parameters
  parameters <- output_param_for_ode_solver(params$vars.all, params$vals.all)
  
  #initialize initial conditions
  state <- output_ICs_for_ode_solver(vars$species ,ICs$vals)
  
  #set up differential equations input string form
  diff_eqns <- diffeq_to_text(DE$eqns, vars$species)
  d_of_var <- output_var_for_ode_solver(vars$species)
  
  Lorenz <- function(t, state, parameters){
    with(as.list(c(state, parameters)), {
      eval(parse(text=diff_eqns))
      list(eval(parse(text=d_of_var)))
    })
  }
  
  #out <- ode(y=state, times=times, func=model, parms=parameters)
  out <- ode(y=state, times=times, func = Lorenz, parms = parameters)
  
  return(out)
})

#updates filter_2 variable choices based on items selected in checkbox selct boxes
observeEvent(input$loop_execute_run_model, {
  updateSelectInput(session
                    ,"lineplot_xvar"
                    ,choices = colnames(loop_model_output())[1])
})

#updates filter_2 variable choices based on items selected in checkbox selct boxes
observeEvent(input$loop_execute_run_model, {
  updatePickerInput(session,
                    "lineplot_yvar"
                    ,choices  = colnames(loop_model_output())[2:ncol(loop_model_output())]
                    ,selected = colnames(loop_model_output())[2:ncol(loop_model_output())]
  )
})
