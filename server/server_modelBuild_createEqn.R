source("./server/helper_createEqns.R") #load functions to solve differential equations


build_db_row <- function(eqn_type, RHS_coef, RHS_var, LHS_coef, LHS_var,arrow_type, kf, kr, description){
  row_out <- c(eqn_type, RHS_coef, RHS_var, LHS_coef, LHS_var,arrow_type, kf, kr, description)
}

#-------------------------------------------------------------------------------

# Extract data and store equation elements into a df to solve ODEs from

#-------------------------------------------------------------------------------
observeEvent(input$eqnCreate_addEqnToVector, {
  rv$number_of_equations <- rv$number_of_equations + 1
  eqn_type <- input$eqnCreate_type_of_equation
  
  if(eqn_type=="chem_rxn"){
    number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS)
    number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS)
    
    coef_LHS <- vector()
    var_LHS <- vector()
    for(i in seq(number_LHS_equations)){
      coef <- eval(parse(text=paste0("input$LHS_Coeff_", as.character(i))))
      var <- eval(parse(text=paste0("input$LHS_Var_", as.character(i))))
      coef_LHS <- append(coef_LHS, coef)
      var_LHS <- append(var_LHS, var)
    }
    coef_LHS <- paste(coef_LHS, collapse=" ")
    var_LHS <- paste(var_LHS, collapse=" ")
    
    coef_RHS <- vector()
    var_RHS <- vector()
    for(i in seq(number_RHS_equations)){
      coef <- eval(parse(text=paste0("input$RHS_Coeff_", as.character(i))))
      var <- eval(parse(text=paste0("input$RHS_Var_", as.character(i))))
      coef_RHS <- append(coef_RHS, coef)
      var_RHS <- append(var_RHS, var)
    }
    coef_RHS <- paste(coef_RHS, collapse=" ")
    var_RHS <- paste(var_RHS, collapse=" ")
    
    arrow_direction <- input$eqn_chem_forward_or_both
    if(arrow_direction=="both_directions"){
      kf <- input$eqn_chem_forward_k
      kr <- input$eqn_chem_back_k
      rv$param_eqns <- append(rv$param_eqns, kf)
      rv$param_eqns <- append(rv$param_eqns, kr)
    }
    else if(arrow_direction=="forward_only"){
      kf <- input$eqn_chem_forward_k
      kr <- NA
      rv$param_eqns <- append(rv$param_eqns, kf)
    }
    kcat = NA
    Vmax = NA 
    Km = NA 
    enzyme = NA
    row_to_df <- c(eqn_type, coef_LHS, var_LHS, coef_RHS, var_RHS, arrow_direction, kf, kr, kcat, Vmax, Km, enzyme)
    # if(rv$first_run){
    #   rv$first_run <- FALSE
    #   data$eqn_info[1,] <- row_to_df
    # }else{
    #   data$eqn_info  <- rbind(data$eqn_info , row_to_df)
    # }
    
  }#end if chem_rxn
  else if(eqn_type == "enzyme_rxn")
  {
    LHS_coef <- 1
    RHS_coef <- 1
    LHS_var = input$eqn_enzyme_substrate
    RHS_var = input$eqn_enzyme_product
    arrow_type <- "forward_only"
    Km = input$eqn_enzyme_Km
    rv$param_eqns <- append(rv$param_eqns, Km)
    
    if(input$eqn_options_enzyme_noVmax)
    {
      kcat = input$eqn_enzyme_kcat
      enzyme = input$eqn_enzyme_enzyme
      Vmax = NA
      rv$param_eqns <- append(rv$param_eqns, kcat)
      rv$param_eqns <- append(rv$param_eqns, Km)
    }
    else if(!input$eqn_options_enzyme_noVmax)
    {
      Vmax = input$eqn_enzyme_Vmax
      kcat = NA
      enzyme = NA
      rv$param_eqns <- append(rv$param_eqns, Vmax)
    }
    
    kf = NA
    kr = NA
    row_to_df <- c(eqn_type, LHS_coef, LHS_var, RHS_coef, RHS_var,arrow_type, kf, kr, kcat, Vmax, Km, enzyme)
  }
  else if(eqn_type=="simp_diff"){
    LHS_coef <- 1
    RHS_coef <- 1
    LHS_var = input$simp_diff_var1
    RHS_var = input$simp_diff_var2
    diff_coef <- input$simp_diff_PS_Var
    if(input$simp_diff_wayOfDiffusion){
      arrow_type <- "forward_only"
      kf = diff_coef
      kr = NA
    }else{
      arrow_type <- "both_directions"
      kf = diff_coef
      kr = diff_coef
    }
    kcat = NA
    Vmax = NA 
    Km = NA 
    enzyme = NA
    row_to_df <- c(eqn_type, coef_LHS, var_LHS, coef_RHS, var_RHS, arrow_direction, kf, kr, kcat, Vmax, Km, enzyme)
    # if(rv$first_run){
    #   rv$first_run <- FALSE
    #   data$eqn_info[1,] <- row_to_df
    # }else{
    #   data$eqn_info  <- rbind(data$eqn_info , row_to_df)
    #}
    #store parameter value
    rv$param_eqns <- append(rv$param_eqns, kf)
  }
  else if(eqn_type == "rate_eqn")
  {
    rate_left <- input$eqnCreate_rate_firstvar
    rate_right <- input$eqnCreate_rate_equation
    rate_eqn <- paste0(rate_left, " = ", rate_right)
    rv$rate_eqns <- c(rv$rate_eqns, rate_eqn)
    rv$parameters_based_on_other_values <- rate_left
    #remove rate_left from parameters-----------------------------------------------------------------------------------------------------------------------
    #split_rate_to_components()
    #search all parameters lists for parameter and remove it from each. (input, output, eqn, total)
    parameter_to_remove <- rate_left
    if(parameter_to_remove %in% rv$param_inputs)
    {
      rv$param_inputs <- rv$param_inputs[!rv$param_inputs %in% parameter_to_remove]
    }
    if(parameter_to_remove %in% rv$param_outputs)
    {
      rv$param_outputs <- rv$param_outputs[!rv$param_outputs %in% parameter_to_remove]
    }
    if(parameter_to_remove %in% rv$param_eqns)
    {
      rv$param_eqns <- rv$param_eqns[!rv$param_eqns %in% parameter_to_remove]
    }
    if(parameter_to_remove %in% rv$parameters_in_model)
    {
      rv$parameters_in_model <- rv$parameters_in_model[!rv$parameters_in_model %in% parameter_to_remove]
    }
    #remove all excess variables from created lists if they exist (ie. we generated ui for parameter values and comments.  Will need to remove those)
  }
  if(eqn_type != "rate_eqn")
  {
    if(rv$first_run)
    {
      rv$first_run <- FALSE
      data$eqn_info[1,] <- row_to_df
    }
    else
    {
      data$eqn_info  <- rbind(data$eqn_info , row_to_df)
    }
  }

})
observe(print(data$eqn_info))

#-------------------------------------------------------------------------------

# Build Text Equation for User to See

#-------------------------------------------------------------------------------
equationBuilder <- reactive({
  if(input$eqnCreate_type_of_equation=="chem_rxn"){
    number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS)
    number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS)
    
    eqn_LHS <- ""
    for(i in seq(number_LHS_equations)){
      coef <- eval(parse(text=paste0("input$LHS_Coeff_", as.character(i))))
      var <- eval(parse(text=paste0("input$LHS_Var_", as.character(i))))
      if(coef!="1"){eqn_LHS <- paste0(eqn_LHS, coef, "*")}
      if(i==as.numeric(number_LHS_equations)){eqn_LHS <- paste0(eqn_LHS, var)}
      else{eqn_LHS <- paste0(eqn_LHS, var, " + ")}
    }
    
    eqn_RHS <- ""
    for(i in seq(number_RHS_equations)){
      coef <- eval(parse(text=paste0("input$RHS_Coeff_", as.character(i))))
      var <- eval(parse(text=paste0("input$RHS_Var_", as.character(i))))
      if(coef!="1"){eqn_RHS <- paste0(eqn_RHS, coef, "*")}
      if(i==as.numeric(number_RHS_equations)){eqn_RHS <- paste0(eqn_RHS, var)}
      else{eqn_RHS <- paste0(eqn_RHS, var, " + ")}
    }
    
    if(input$eqn_chem_forward_or_both=="both_directions"){
      arrow <- "<-->"
      arrow <- paste0("(", input$eqn_chem_back_k, ")", arrow, "(", input$eqn_chem_forward_k, ")")
    }
    else if(input$eqn_chem_forward_or_both=="forward_only"){
      arrow = "--->"
      arrow <- paste0(arrow, "(", input$eqn_chem_forward_k, ")")
    }
    
    textOut <-paste(eqn_LHS, arrow, eqn_RHS)
  }
  
  else if(input$eqnCreate_type_of_equation=="enzyme_rxn")
  {
    substrate = input$eqn_enzyme_substrate
    product = input$eqn_enzyme_product
    arrow = "-->"
    enzyme = input$eqn_enzyme_enzyme
    Km = input$eqn_enzyme_Km
    
    if(input$eqn_options_enzyme_noVmax)
    {
      kcat = input$eqn_enzyme_kcat
      textOut <- paste0(substrate," + ", enzyme,  " (", kcat, ")", arrow, "(", Km, ") ", product)
    }
    else if(!input$eqn_options_enzyme_noVmax)
    {
      Vmax = input$eqn_enzyme_Vmax
      textOut <- paste0(substrate, " (", Vmax, ", Enzyme)", arrow, "(", Km, ") ", product)
      
    }
  }
  else if(input$eqnCreate_type_of_equation=="simp_diff"){
    var_left = input$simp_diff_var1
    var_right = input$simp_diff_var2
    diff_coef <- input$simp_diff_PS_Var
    ifelse(input$simp_diff_wayOfDiffusion, symbol <- "-->", symbol <- "<-->")
    
    textOut <- paste0(var_left, " ", symbol, "(", diff_coef, ") ", var_right)
  }
  else if(input$eqnCreate_type_of_equation=="rate_eqn")
  {
    rate_left <- input$eqnCreate_rate_firstvar
    rate_right <- input$eqnCreate_rate_equation
    textOut <- paste0(rate_left, " = ", rate_right)
  }
  else if(input$eqnCreate_type_of_equation=="mass_bal"){
    textOut <- "MASS BAL"
  }
  else{textOut <- "ERROR"}
  return(textOut)
})

#-------------------------------------------------------------------------------

# Equation Building UI

#-------------------------------------------------------------------------------
output$eqnCreate_equationBuilder_chem <- renderUI({
  number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS)
  number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS)
  
  fluidRow(column(width=2
                  ,lapply(seq(number_LHS_equations), function(i){
                    numericInput(inputId=paste0("LHS_Coeff_", as.character(i))
                                 ,label="Coefficient"
                                 ,value = 1
                                 ,min = 1
                                 ,step=1)
                  })
  )#end Column
  ,column(width=2
          ,lapply(seq(number_LHS_equations), function(i){
            pickerInput(inputId=paste0("LHS_Var_", as.character(i))
                        ,label="Choose Var"
                        ,choices=rv$vars_in_model)
          })
  )#end column
  ,column(width=3
          #,offset=1
          ,pickerInput(inputId="eqn_chem_forward_or_both"
                       ,label="Reaction Direction"
                       ,choices=c("Forward" = 'forward_only'
                                  ,"Both" = "both_directions"))
          ,textInput(inputId="eqn_chem_forward_k"
                     ,label="Forward Rate Constant"
                     ,value=paste0("kf", as.character(rv$number_of_equations+1)))
          ,conditionalPanel(condition="input.eqn_chem_forward_or_both=='both_directions'"
                            ,textInput(inputId = "eqn_chem_back_k"
                                       ,label="Reverse Rate Constant"
                                       ,value=paste0("kr", as.character(rv$number_of_equations+1))))
  )#end column
  ,column(width=2
          #,offset=1
          ,lapply(seq(number_RHS_equations), function(i){
            numericInput(inputId=paste0("RHS_Coeff_", as.character(i))
                         ,label="Coefficient"
                         ,value = 1
                         ,min = 1
                         ,step=1)
          })
  )#end Column
  ,column(width=2
          ,lapply(seq(number_RHS_equations), function(i){
            pickerInput(inputId=paste0("RHS_Var_", as.character(i))
                        ,label="Choose Var"
                        ,choices=rv$vars_in_model)
          })
  )#end column
  )#end fluidRow
  
})

output$eqnCreate_equationBuilder_enzyme <- renderUI({

  div(
    fluidRow(column(width=3
                    ,pickerInput(inputId="eqn_enzyme_substrate"
                                 ,label="Substrate"
                                 ,choices=rv$vars_in_model)
                    ,conditionalPanel(condition="input.eqn_options_enzyme_noVmax"
                                      ,pickerInput(inputId="eqn_enzyme_enzyme"
                                                   ,label="Enzyme"
                                                   ,choices=rv$vars_in_model))
                    )
             ,column(width=3
                     ,offset = 1
                     ,conditionalPanel(condition="!input.eqn_options_enzyme_noVmax"
                                       ,textInput(inputId="eqn_enzyme_Vmax"
                                                  ,label = "Vmax"
                                                  ,value = paste0("Vmax_", as.character(rv$number_of_equations+1))))
                     ,conditionalPanel(condition="input.eqn_options_enzyme_noVmax"
                                       ,textInput(inputId="eqn_enzyme_kcat"
                                                  ,label = "kcat"
                                                  ,value = paste0("kcat_", as.character(rv$number_of_equations+1))))
                     
                     ,textInput(inputId="eqn_enzyme_Km"
                                ,label = "Km"
                                ,value = paste0("Km_", as.character(rv$number_of_equations+1)))
                     )
             ,column(width=3
                     ,offset=1
                     ,pickerInput(inputId="eqn_enzyme_product"
                                  ,label="Product"
                                  ,choices=rv$vars_in_model))
    )#end fluidRow
  )#end div
})

output$eqnCreate_equationBuilder_simp_diff <- renderUI({
  #number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS)
  #number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS)
  
  div(
    fluidRow(column(width=3
                    ,pickerInput(inputId="simp_diff_var1"
                                 ,label="Var1"
                                 ,choices=rv$vars_in_model))
             ,column(width=3
                     ,textInput(inputId="simp_diff_PS_Var"
                                ,label = "Diffusion Constant"
                                ,value = paste0("PS", as.character(rv$number_of_equations+1))))
             ,column(width=3
                     ,pickerInput(inputId="simp_diff_var2"
                                  ,label="Var2"
                                  ,choices=rv$vars_in_model))
    )#end fluidRow
    ,fluidRow(column(width=4,
                     checkboxInput(inputId="simp_diff_wayOfDiffusion"
                                   ,label="This diffusion is one way"
                                   ,value = FALSE)
    ))
  )#end div
})

# output$eqnCreate_equationBuilder_rate <- renderUI({
#   div(
#     orderInput("foo", "foo",
#                items = NULL,
#                item_class = 'info'
#                ,connect = "test_eqn")
#     ,orderInput("build_eqn_symbols"
#                ,"Drag Symbol"
#                ,items = c("+", "-", "*", "/")
#                ,as_source = TRUE
#                ,connect = "test_eqn")
#     ,orderInput("test_eqn"
#                ,"Eqn"
#                ,items = NULL
#                ,placeholder = "Drag Here"
#                ,connect = "foo")
#   )
# 
# })
#-------------------------------------------------------------------------------

# Rate Equation Store Parameter

#-------------------------------------------------------------------------------

observeEvent(input$eqnCreate_rate_store_new_parameter, {
  new_parameter <- input$eqnCreate_rate_new_parameter
  rv$param_rateEqn <- append(rv$param_rateEqn, new_parameter)
  updateTextInput(session
                  ,"eqnCreate_rate_new_parameter"
                  ,value = "")
})

#-------------------------------------------------------------------------------

# Options UI

#-------------------------------------------------------------------------------
output$eqnCreate_Options <- renderUI({
  conditionalPanel(condition="input.eqnCreate_type_of_equation=='enzyme_rxn'"
                  ,p("Enzyme Reaction")
                  ,checkboxInput(inputId = "eqn_options_enzyme_noVmax"
                                 ,label = "Split Vmax to kcat and enzyme"
                                 ,value = FALSE))
})

#-------------------------------------------------------------------------------

# When Equation Add button pressed, store vars to respective places

#-------------------------------------------------------------------------------
observeEvent(input$eqnCreate_addEqnToVector, {
  eqn_type <- input$eqnCreate_type_of_equation
  
  if(eqn_type!="rate_eqn")
  {
  rv$eqns_in_model <- append(rv$eqns_in_model, equationBuilder())   #store selected variable to list of variables
  }
  #rate equation added in different part of code
  
  #reset text input to blank when variable entered
  eqn_type <- input$eqnCreate_type_of_equation
  number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS)
  number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS)
  nums <- c(number_RHS_equations, number_LHS_equations)
  out_list <- list(eqn_type, nums)
  #rv$eqn_info <- append(rv$eqn_info, out_list)
  updateNumericInput(session, "eqnCreate_num_of_eqn_LHS", value = 1)
  updateNumericInput(session, "eqnCreate_num_of_eqn_RHS", value = 1)
  
  updatePickerInput(session,
                    'eqnCreate_edit_select_equation'
                    ,choices = seq(length(rv$eqns_in_model)))
  
})

#-------------------------------------------------------------------------------

# Equation Text outputs

#-------------------------------------------------------------------------------

output$eqnCreate_showEquationBuilding <- renderText({equationBuilder()})
#output$eqnCreate_showEquations <- renderPrint({data$eqn_info})
output$eqnCreate_showEquations <- renderText({
  if(length(rv$eqns_in_model)==0)
  {
    paste("No equations entered")
  }
  else
  {
    n_eqns = seq(length(rv$eqns_in_model))
    eqns_to_display <- c()
    for(i in n_eqns)
    {
      new_eqn <- paste0("(",i, ") ", rv$eqns_in_model[i])
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse="<br>")
  }

})

output$eqnCreate_showRateEquations <- renderText({
  if(length(rv$rate_eqns)==0)
  {
    "No rate equations entered"
  }
  else
  {
    n_eqns = seq(length(rv$rate_eqns))
    eqns_to_display <- c()
    for(i in n_eqns)
    {
      new_eqn <- paste0("(",n_eqns[i], ") ", rv$rate_eqns[i])
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse="<br>")
  }
})



#-------------------------------------------------------------------------------

# Removing last Equation from list

#-------------------------------------------------------------------------------
#when back button is pressed
observeEvent(input$createEqn_removeEqnFromList, {
  rv$eqns_in_model <- rv$eqns_in_model[-length(rv$eqns_in_model)] #removes equanation from equation list
  
  #need to remove parameters
  param1 = data$eqn_info[nrow(data$eqn_info), 7] #kf
  param2 = data$eqn_info[nrow(data$eqn_info), 8] #kr
  if(!is.na(param1)){
    rv$param_eqns <- rv$param_eqns[-length(rv$param_eqns)]
    rv$param_eqns_values <- rv$param_eqns_values[-length(rv$param_eqns_values)]
    rv$param_eqns_comments <- rv$param_eqns_comments[-length(rv$param_eqns_comments)]
  }
  if(!is.na(param2)){
    rv$param_eqns <- rv$param_eqns[-length(rv$param_eqns)]
    rv$param_eqns_values <- rv$param_eqns_values[-length(rv$param_eqns_values)]
    rv$param_eqns_comments <- rv$param_eqns_comments[-length(rv$param_eqns_comments)]
  }
  
  #removes equation from its data matrix
  if(nrow(data$eqn_info)==1){ #if only row in matrix
    data$eqn_info <- data$eqn_info[-nrow(data$eqn_info), ] #remove equation info from data base
    rv$first_run = TRUE #reset to be no equations
  }else{
    data$eqn_info <- data$eqn_info[-nrow(data$eqn_info), ] #remove equation info from data base
  }
  rv$number_of_equations <- rv$number_of_equations - 1
})

observeEvent(input$createEqn_removeFirstRate, {
  rv$rate_eqns <- rv$rate_eqns[-1]
})

#-------------------------------------------------------------------------------

# Delete Equation from Model

#-------------------------------------------------------------------------------
observeEvent(input$eqnCreate_addEqnToVector, {
  updatePickerInput(session
                    ,"eqnCreate_delete_equation"
                    ,choices = seq(rv$number_of_equations))
})


observeEvent(input$createEqn_delete_equation_button, {
  #delete the number equation in the list
  number_of_eqn_to_delete <- as.numeric(input$eqnCreate_delete_equation)
  data$eqn_info <- data$eqn_info[-number_of_eqn_to_delete, 1:ncol(data$eqn_info)] #delete equation from dataframe
  rv$eqns_in_model <- rv$eqns_in_model[-number_of_eqn_to_delete] #removes equanation from equation list
  rv$number_of_equations <- rv$number_of_equations - 1
  
  updatePickerInput(session
                    ,"eqnCreate_delete_equation"
                    ,choices = seq(rv$number_of_equations))
})

#-------------------------------------------------------------------------------

# Edit Tab Controlling the editing of equations

#-------------------------------------------------------------------------------

#currently returns the type of equation the user selected
testernum<-eventReactive(input$createEqn_edit_equation_button , {
  eqn_number = input$eqnCreate_edit_select_equation #eqn number to edit
  eqn_to_edit <- data$eqn_info[eqn_number, 1:ncol(data$eqn_info)] #extract equation
  return(eqn_to_edit[1])
})

#prints the type of equation
output$build_equation_edit <- renderPrint({
  req(input$createEqn_edit_equation_button)
  equationBuilder_edit()
})

output$eqnCreate_renderingUIcomponents <- renderUI({
  req(input$createEqn_edit_equation_button)
  
  eqn_number = input$eqnCreate_edit_select_equation #eqn number to edit
  eqn_to_edit <- data$eqn_info[eqn_number, 1:ncol(data$eqn_info)] #extract equation
  
  div(
    fluidRow(column(width = 3,
                    pickerInput(inputId="eqnCreate_type_of_equation_edit"
                                ,label="Select Type"
                                ,choices=c("Chemical Rxn" = "chem_rxn"
                                           ,"Enzyme-Catalyzed Rxn" = "enzyme_rxn"
                                           ,"Simple Diffusion" = "simp_diff"
                                           ,"Rate Equation" = "rate_eqn")
                                ,selected = testernum()))
             ,conditionalPanel(condition="input.eqnCreate_type_of_equation_edit=='chem_rxn'"
                               ,column(width=3
                                       ,numericInput(inputId="eqnCreate_num_of_eqn_LHS_edit"
                                                     ,label="Number of Variable on LHS"
                                                     ,value=length(str_split(eqn_to_edit[2], " ")[[1]]) 
                                                     ,min=1
                                                     ,step=1))
                               ,column(width=3
                                       ,numericInput(inputId="eqnCreate_num_of_eqn_RHS_edit"
                                                     ,label="Number of Variable on RHS"
                                                     ,value=length(str_split(eqn_to_edit[4], " ")[[1]])
                                                     ,min=1
                                                     ,step=1))
             )#end conditional Panel on chem_rxn
    )
    ,conditionalPanel(condition = "input.eqnCreate_type_of_equation_edit =='chem_rxn'"
                      ,hr()
                      ,uiOutput("edit_chemical_reaction")
    )
    ,conditionalPanel(condition = "input.eqnCreate_type_of_equation_edit =='enzyme_rxn'"
                      ,hr()
                      ,uiOutput("edit_enzyme_reaction"))
    ,hr()
    ,fluidRow(column(width=12
                      ,actionButton(inputId="edit_save_changes_button"
                                     ,label="Save Changes"))
              ,align = "right")
  ) #end div
  

})

output$edit_chemical_reaction <- renderUI({
  req(input$createEqn_edit_equation_button)
  number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS_edit)
  number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS_edit)
  eqn_number = input$eqnCreate_edit_select_equation #eqn number to edit
  eqn_to_edit <- data$eqn_info[eqn_number, 1:ncol(data$eqn_info)] #extract equation
  
  fluidRow(column(width=2
                  ,lapply(seq(number_LHS_equations), function(i){
                    numericInput(inputId=paste0("LHS_Coeff_edit_", as.character(i))
                                 ,label="Coefficient"
                                 ,value = str_split(eqn_to_edit[2], " ")[[1]][i]
                                 ,min = 1
                                 ,step=1)
                  })
  )#end Column
  ,column(width=2
          ,lapply(seq(number_LHS_equations), function(i){
            pickerInput(inputId=paste0("LHS_Var_edit_", as.character(i))
                        ,label="Choose Var"
                        ,choices=rv$vars_in_model
                        ,selected = str_split(eqn_to_edit[3], " ")[[1]][i])
          })
  )#end column
  ,column(width=3
          #,offset=1
          ,pickerInput(inputId="eqn_chem_forward_or_both_edit"
                       ,label="Reaction Direction"
                       ,choices=c("Forward" = 'forward_only'
                                  ,"Both" = "both_directions")
                       ,selected = ifelse(eqn_to_edit[6] == "forward_only", "forward_only", "both_directions"))
          ,textInput(inputId="eqn_chem_forward_k_edit"
                     ,label="Forward Rate Constant"
                     ,value=eqn_to_edit[7])
          ,conditionalPanel(condition="input.eqn_chem_forward_or_both_edit=='both_directions'"
                            ,textInput(inputId = "eqn_chem_back_k_edit"
                                       ,label="Reverse Rate Constant"
                                       ,value=eqn_to_edit[8]))
  )#end column
  ,column(width=2
          #,offset=1
          ,lapply(seq(number_RHS_equations), function(i){
            numericInput(inputId=paste0("RHS_Coeff_edit_", as.character(i))
                         ,label="Coefficient"
                         ,value = str_split(eqn_to_edit[4], " ")[[1]][i]
                         ,min = 1
                         ,step=1)
          })
  )#end Column
  ,column(width=2
          ,lapply(seq(number_RHS_equations), function(i){
            pickerInput(inputId=paste0("RHS_Var_edit_", as.character(i))
                        ,label="Choose Var"
                        ,choices=rv$vars_in_model
                        ,selected = str_split(eqn_to_edit[5], " ")[[1]][i])
          })
  )#end column
  )#end fluidRow
  
})

output$edit_enzyme_reaction <- renderUI({
  req(input$createEqn_edit_equation_button)
  eqn_number = input$eqnCreate_edit_select_equation #eqn number to edit
  eqn_to_edit <- data$eqn_info[eqn_number, 1:ncol(data$eqn_info)] #extract equation
  div(
    fluidRow(column(width=3
                    ,pickerInput(inputId="eqn_enzyme_substrate_edit"
                                 ,label="Substrate"
                                 ,choices=rv$vars_in_model
                                 ,selected=eqn_to_edit[3])
                    ,conditionalPanel(condition="input.eqn_options_enzyme_noVmax"
                                      ,pickerInput(inputId="eqn_enzyme_enzyme_edit"
                                                   ,label="Enzyme"
                                                   ,choices=rv$vars_in_model
                                                   ,selected=eqn_to_edit[12]))
    )
    ,column(width=3
            ,offset = 1
            ,conditionalPanel(condition="!input.eqn_options_enzyme_noVmax"
                              ,textInput(inputId="eqn_enzyme_Vmax_edit"
                                         ,label = "Vmax"
                                         ,value = eqn_to_edit[10]))
            ,conditionalPanel(condition="input.eqn_options_enzyme_noVmax"
                              ,textInput(inputId="eqn_enzyme_kcat_edit"
                                         ,label = "kcat"
                                         ,value = eqn_to_edit[9]))
            
            ,textInput(inputId="eqn_enzyme_Km_edit"
                       ,label = "Km"
                       ,value = eqn_to_edit[11])
    )
    ,column(width=3
            ,offset=1
            ,pickerInput(inputId="eqn_enzyme_product_edit"
                         ,label="Product"
                         ,choices=rv$vars_in_model
                         ,selected=eqn_to_edit[5]))
    )#end fluidRow
  )#end div
})
# 
# ,conditionalPanel(condition="input.eqnCreate_type_of_equation=='chem_rxn'"
#                   ,uiOutput("eqnCreate_equationBuilder_chem"))

equationBuilder_edit <- reactive({
  if(input$eqnCreate_type_of_equation_edit=="chem_rxn"){
    number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS_edit)
    number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS_edit)
    
    eqn_LHS <- ""
    for(i in seq(number_LHS_equations)){
      coef <- eval(parse(text=paste0("input$LHS_Coeff_edit_", as.character(i))))
      var <- eval(parse(text=paste0("input$LHS_Var_edit_", as.character(i))))
      if(coef!="1"){eqn_LHS <- paste0(eqn_LHS, coef, "*")}
      if(i==as.numeric(number_LHS_equations)){eqn_LHS <- paste0(eqn_LHS, var)}
      else{eqn_LHS <- paste0(eqn_LHS, var, " + ")}
    }
    
    eqn_RHS <- ""
    for(i in seq(number_RHS_equations)){
      coef <- eval(parse(text=paste0("input$RHS_Coeff_edit_", as.character(i))))
      var <- eval(parse(text=paste0("input$RHS_Var_edit_", as.character(i))))
      if(coef!="1"){eqn_RHS <- paste0(eqn_RHS, coef, "*")}
      if(i==as.numeric(number_RHS_equations)){eqn_RHS <- paste0(eqn_RHS, var)}
      else{eqn_RHS <- paste0(eqn_RHS, var, " + ")}
    }
    
    if(input$eqn_chem_forward_or_both_edit=="both_directions"){
      arrow <- "<-->"
      arrow <- paste0("(", input$eqn_chem_back_k_edit, ")", arrow, "(", input$eqn_chem_forward_k_edit, ")")
    }
    else if(input$eqn_chem_forward_or_both_edit=="forward_only"){
      arrow = "--->"
      arrow <- paste0(arrow, "(", input$eqn_chem_forward_k_edit, ")")
    }
    
    textOut <-paste(eqn_LHS, arrow, eqn_RHS)
  }
  
  else if(input$eqnCreate_type_of_equation_edit=="enzyme_rxn")
  {
    substrate = input$eqn_enzyme_substrate_edit
    product = input$eqn_enzyme_product_edit
    arrow = "-->"
    enzyme = input$eqn_enzyme_enzyme_edit
    Km = input$eqn_enzyme_Km_edit
    
    if(input$eqn_options_enzyme_noVmax)
    {
      kcat = input$eqn_enzyme_kcat_edit
      textOut <- paste0(substrate," + ", enzyme,  " (", kcat, ")", arrow, "(", Km, ") ", product)
    }
    else if(!input$eqn_options_enzyme_noVmax)
    {
      Vmax = input$eqn_enzyme_Vmax_edit
      textOut <- paste0(substrate, " (", Vmax, ", Enzyme)", arrow, "(", Km, ") ", product)
      
    }
  }
  
  # else if(input$eqnCreate_type_of_equation_edit=="simp_diff"){
  #   var_left = input$simp_diff_var1
  #   var_right = input$simp_diff_var2
  #   diff_coef <- input$simp_diff_PS_Var
  #   ifelse(input$simp_diff_wayOfDiffusion, symbol <- "-->", symbol <- "<-->")
  #   
  #   textOut <- paste0(var_left, " ", symbol, "(", diff_coef, ") ", var_right)
  # }
  # else if(input$eqnCreate_type_of_equation=="mass_bal"){
  #   textOut <- "MASS BAL"
  # }
  else{textOut <- "ERROR"}
  return(textOut)
})
#-------------------------------------------------------------------------------

# Edit Tab rewriting of Equations from Equation UI

#-------------------------------------------------------------------------------

observeEvent(input$edit_save_changes_button, {
  #find which equation we are editing. 
  eqn_number = input$eqnCreate_edit_select_equation #eqn number to edit
  eqn_to_edit <- data$eqn_info[eqn_number, 1:ncol(data$eqn_info)] #extract equation
  
  #delete components
  
  eqn_type <- input$eqnCreate_type_of_equation_edit
  #add new components to equation sheet.
  
  if(eqn_type=="chem_rxn"){
    number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS_edit)
    number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS_edit)
    
    coef_LHS <- vector()
    var_LHS <- vector()
    for(i in seq(number_LHS_equations)){
      coef <- eval(parse(text=paste0("input$LHS_Coeff_edit_", as.character(i))))
      var <- eval(parse(text=paste0("input$LHS_Var_edit_", as.character(i))))
      coef_LHS <- append(coef_LHS, coef)
      var_LHS <- append(var_LHS, var)
    }
    coef_LHS <- paste(coef_LHS, collapse=" ")
    var_LHS <- paste(var_LHS, collapse=" ")
    
    coef_RHS <- vector()
    var_RHS <- vector()
    for(i in seq(number_RHS_equations)){
      coef <- eval(parse(text=paste0("input$RHS_Coeff_edit_", as.character(i))))
      var <- eval(parse(text=paste0("input$RHS_Var_edit_", as.character(i))))
      coef_RHS <- append(coef_RHS, coef)
      var_RHS <- append(var_RHS, var)
    }
    coef_RHS <- paste(coef_RHS, collapse=" ")
    var_RHS <- paste(var_RHS, collapse=" ")
    
    arrow_direction <- input$eqn_chem_forward_or_both_edit
    if(arrow_direction=="both_directions"){
      kf <- input$eqn_chem_forward_k_edit
      kr <- input$eqn_chem_back_k_edit
      rv$param_eqns <- append(rv$param_eqns, kf)
      rv$param_eqns <- append(rv$param_eqns, kr)
    }
    else if(arrow_direction=="forward_only"){
      kf <- input$eqn_chem_forward_k_edit
      kr <- NA
      rv$param_eqns <- append(rv$param_eqns, kf)
    }
    kcat = NA
    Vmax = NA 
    Km = NA 
    enzyme = NA
    row_to_df <- c(eqn_type, coef_LHS, var_LHS, coef_RHS, var_RHS, arrow_direction, kf, kr, kcat, Vmax, Km, enzyme)
  }#end if chem_rxn
  else if(eqn_type == "enzyme_rxn")
  {
    LHS_coef <- 1
    RHS_coef <- 1
    LHS_var = input$eqn_enzyme_substrate_edit
    RHS_var = input$eqn_enzyme_product_edit
    arrow_type <- "forward_only"
    Km = input$eqn_enzyme_Km_edit
    rv$param_eqns <- append(rv$param_eqns, Km)
    
    if(input$eqn_options_enzyme_noVmax)
    {
      kcat = input$eqn_enzyme_kcat_edit
      enzyme = input$eqn_enzyme_enzyme_edit
      Vmax = NA
      rv$param_eqns <- append(rv$param_eqns, kcat)
      rv$param_eqns <- append(rv$param_eqns, Km)
    }
    else if(!input$eqn_options_enzyme_noVmax)
    {
      Vmax = input$eqn_enzyme_Vmax_edit
      kcat = NA
      enzyme = NA
      rv$param_eqns <- append(rv$param_eqns, Vmax)
    }
    
    kf = NA
    kr = NA
    row_to_df <- c(eqn_type, LHS_coef, LHS_var, RHS_coef, RHS_var,arrow_type, kf, kr, kcat, Vmax, Km, enzyme)
  }
  data$eqn_info[eqn_number, 1:ncol(data$eqn_info)]  <- row_to_df
  rv$eqns_in_model[as.numeric(eqn_number)] <- equationBuilder_edit()
  # updatePickerInput(session,
  #                   'eqnCreate_edit_select_equation'
  #                   ,choices = seq(length(rv$eqns_in_model)))
})
observe({print(rv$eqns_in_model)})
#-------------------------------------------------------------------------------

# View Tab controlling the equations view

#-------------------------------------------------------------------------------

output$test_mathJax <- renderUI({
  withMathJax(
    # sprintf('The resulting enzyme kinetic reaction
    #            $$\\frac{d}{dt} = \\frac{V_{max}S}{K_{m} + S}
    #         = %s$$', input$eqn_enzyme_Vmax))
    tags$b("The resulting enzyme kinetic reaction:"),
    br(),
    paste0("$$\\frac{d}{dt} = \\frac{V_{max}S}{K_{m} + S} = $$ ", sprintf("$$\\frac{%s*%s}{%s + %s}$$",input$eqn_enzyme_Vmax, input$eqn_enzyme_substrate, input$eqn_enzyme_Km, input$eqn_enzyme_substrate)),
    br()
  )
})

# output$test_orderInputs <- renderUI({
#   orderInput("source1", "Vars", items =rv$vars_in_model, as_source = TRUE, connect = "test_eqn")
#   orderInput("test_eqn", "Eqn", items = NULL, placeholder = "Drag Here")
# })

