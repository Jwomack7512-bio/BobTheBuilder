#server for model_createVars
rv <- reactiveValues(vars_in_model = vector() #stores model variable
                     ,eqns_in_model = vector() #stores eqn type in model
                     
                     #store total parameters from eqns, inputs, outputs (sum of vectors)
                     ,parameters_in_model = vector() #store parameter variable
                     ,parameter_values = vector() #store parameter value
                     ,parameter_descriptions = vector() #store comments of parameters
                     #store parameters from equations
                     ,param_eqns = vector() #param variable
                     ,param_eqns_values = vector() #param variable values
                     ,param_eqns_comments = vector() #param comments
                     ,first_param_eqn_stored = FALSE #if parameter stored button hit then this will update parameter values based on those stored and not reset them all to zero
                     #store parameters for input variables
                     ,param_inputs = vector()
                     ,param_inputs_values = vector()
                     ,param_inputs_comments = vector()
                     ,first_param_inputs_stored = FALSE
                     #store parameters for output variables
                     ,param_outputs = vector()
                     ,param_outputs_values = vector()
                     ,param_outputs_comments = vector()
                     ,first_param_outputs_stored = FALSE
                     #store parameters from rate variables
                     ,param_rateEqn = vector()
                     ,param_rateEqn_values = vector()
                     ,param_rateEqn_comments = vector()
                     ,first_param_rateEqn_stored = FALSE
                     ,rate_params = vector()
                     #store parameters from rate variables
                     ,param_timeDependentEqn = vector()
                     ,param_timeDependentEqn_values = vector()
                     ,param_timeDependentEqn_comments = vector()
                     ,first_param_timeDependentEqn_stored = FALSE
                     #Store initial condition variables
                     ,IC_values = vector() #store initial condition value
                     ,IC_descriptions = vector() #store comments for ICs
                     ,diffEQs = vector() #store differential equations
                     ,number_of_equations = 0 #stores number of total equations in model (used to autofill names of some var)
                     ,number_of_IO = 0 #stores the number of total Input and Outputs
                     ,rate_eqns = vector() #stores all the elements of the rate equations to be added to the model
                     ,time_dependent_eqns = vector() #stores all time dependent eqns
                     ,additional_eqns = vector() #stores all additional eqns -time, rate, etc...
                     ,lr_eqns = vector() #stores all rate eqns
                     ,parameters_based_on_other_values = vector() #stores all vectors that are not based on other values and not given a hard value (ie k1 = 5*k2+k3 not simply k1 = 5)
                     ,inputOutputs_df = data.frame(matrix(ncol = 8, nrow = 0,
                                                          dimnames = list(NULL, c("In_or_Out", "Type", "Species", "RateConstant","RateBySpecies", "Vmax", "Kcat", "Enzyme"))))
                                                          #(1) in_or_out = value to tell if this column is an input or output: "input" or "output"
                                                          #(2) Type = gets the type of the input (rate, diffusion, synthesis, etc)
                                                          #(3) Species = actual name of the species going in or out
                                                          #(4) RateConstant = if type rate, name of the rate constant 
                                                          #(5) Vmax = if type enzyme, Vmax of enzyme reaction
                                                          #(6) Kcat = f type enzyme and Vmax not used, kcat of reaction (note Vmax = kcat*enzyme)
                                                          #(7) Enzyme = if type enzyme and Vmax not used, enzyme concentration of reaction
                                                          #(8) RateBySpecies = if rate equation, boolean to tell user to multiply the rate by the concentration of the rate species 
                     ,first_inOut = TRUE #determines if In/out input has been given yet.  Avoids adding to df error
                     ,In_out_added = FALSE
                     ,first_IC_stored = FALSE #if IC stored, this parameter is used to render values
                     ,first_run = TRUE #determine if first equation is added yet or not
                     ) 

model.options <- reactiveValues(time.start = 0 
                                ,time.end = 100
                                ,time.step = 1
                                ,time.scale.bool = FALSE
                                ,time.scale.value = 0
                                ,ode.solver.type = "lsoda"
                                )

model.results <- reactiveValues(model = data.frame()
                               ,is.pp = FALSE #lets system know if post processing has occured
                               ,pp.eqns = vector() # keeeps tack of equations in text print form.
                               ,pp.eqns.col = vector() # keeps track of equation in processing form
                               ,pp.vars = vector() #vars to add
                               ,pp.model = data.frame() #new model with post processing
                               )

#stores data frame of information to be parsed at a later time.  this keeps its structure which should make it easier to parse than the above RV (list)
data = reactiveValues(eqn_info = data.frame(matrix(ncol = 18, nrow = 0, 
                                                   dimnames = list(NULL, c("eqn_type", "LHS_coef", "LHS_var", "RHS_coef", "RHS_var","arrow_type", "kf", "kr",  
                                                                         "kcat","Vmax", "Km", "Enzyme",
                                                                         "FM_bool", "FMs", "FM_rateC",
                                                                         "RM_bool", "RMs", "RM_rateC")))))
                                                  #(1)  eqn_type = type of equation (chem, diffusion, enzyme, etc)    
                                                  #(2)  LHS_coef = Coefficients on LHS of equation (the 3 in 3A -> 2B)
                                                  #(3)  LHS_var = variables on LHS of equation (the A in 3A -> 2B)
                                                  #(4)  RHS_coef = Coefficients on RHS of equation (the 2 in 3A -> 2B) 
                                                  #(5)  RHS_var = variables on RHS of equation (the B in 3A -> 2B)
                                                  #(6)  arrow_type = tells if the reaction is reversible or not
                                                  #(7)  kf = reaction forward coefficient
                                                  #(8)  kr = reaction reverse coefficient
                                                  #(9)  kcat = catalytic coefficient for enzyme reactions
                                                  #(10) Vmax = maximum velocity for enzyme reactions
                                                  #(11) Km = Michelis Menton coefficient for enzyme reactions
                                                  #(12) Enzyme = The enzyme of the reaction
                                                  #(13) FM_bool = boolean if modifiers are used on forward equation (chem rxn)
                                                  #(14) FMs = variables that are used in the modification of reaction (chem rxn) (ex think Wee1)
                                                  #(15) FM_rateC = rate constants associated with the modifying variables
                                                  #(16) RM_bool = boolean if modifiers are used on forward equation (chem rxn)
                                                  #(17) RMs = variables that are used in the modification of reaction (chem rxn) (ex think Wee1)
                                                  #(18) RM_rateC = rate constants associated with the modifying variables

logs <- reactiveValues(IO_logs = vector() #record the log for which inputs are added or not
                       )

observeEvent(input$createVar_addVarToList, {
  if (input$createVar_varInput == "")
  {
    #nothing happens if a blank space is added
  }
  else if (input$createVar_varInput %in% rv$vars_in_model) #var already exists in model, let user know
  {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'This variable is already used')
    updateTextInput(session = session
                    ,'createVar_varInput'
                    ,value = "")
  }
  else
  {
    #store selected variable to list of variables
    rv$vars_in_model <- append(rv$vars_in_model, input$createVar_varInput)
    #reset text input to blank when variable entered
    updateTextInput(session = session
                    ,'createVar_varInput'
                    ,value = "")
  }

  
  # updateOrderInput(session, "foo",
  #                  items = rv$vars_in_model,
  #                  item_class = "success")
})

observeEvent(input$createVar_removeVarFromList, {
  rv$vars_in_model <- rv$vars_in_model[-length(rv$vars_in_model)]
  
  updatePickerInput(session
                    ,"InOut_selectVar"
                    ,choices = rv$vars_in_model)
})

output$createVar_displayVars <- renderText({
  paste(rv$vars_in_model, collapse = "<br>")
})

# observe({print(rv$vars_in_model)})
# observe({print(rv$eqns_in_model)})