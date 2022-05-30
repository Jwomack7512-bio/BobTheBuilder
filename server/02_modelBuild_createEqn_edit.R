#-------------------------------------------------------------------------------

# Edit Tab Controlling the editing of equations

#-------------------------------------------------------------------------------

#currently returns the type of equation the user selected
# testernum <- eventReactive(input$createEqn_edit_equation_button , {
#   eqn_number = input$eqnCreate_edit_select_equation #eqn number to edit
#   eqn_to_edit <- eqns$eqn.info[eqn_number, 1:ncol(eqns$eqn.info)] #extract equation
#   return(eqn_to_edit[1])
# })
# 
# #prints the type of equation
# output$build_equation_edit <- renderPrint({
#   req(input$createEqn_edit_equation_button)
#   equationBuilder_edit()
# })

output$eqnCreate_renderingUIcomponents <- renderUI({
  
  eqn.num     <- as.numeric(input$eqnCreate_edit_select_equation)
  eqn.row     <- eqns$eqn.info[eqn.num, 1:ncol(eqns$eqn.info)]
  
  eqn.ID      <- eqn.row$ID
  eqn.type    <- eqn.row$EqnType
  eqn.law     <- eqn.row$Law
  eqn.species <- eqn.row$Species
  eqn.RC      <- eqn.row$RateConstants
  eqn.compart <- eqn.row$Compartment
  eqn.descrpt <- eqn.row$Description
  
  # Unpack the different kind of laws to fill out proper information
  if (eqn.type == "chem_rxn") {
    # Find Row with matching ID and extract
    row        <- match(eqn.ID, eqns$eqn.chem[1:nrow(eqns$eqn.chem), 1])
    chemInfo   <- eqns$eqn.chem[row, 1:ncol(eqns$eqn.chem)]
    jPrint(row)
    jPrint(chemInfo)
    jPrint("end chem render")
    ID         <- chemInfo$ID[1]
    law        <- chemInfo$Law[1]
    LHS.coef   <- str_split(chemInfo$LHS_coef[1], " ")[[1]]
    LHS.var    <- str_split(chemInfo$LHS_var[1],  " ")[[1]]
    RHS.coef   <- str_split(chemInfo$RHS_coef[1], " ")[[1]]
    RHS.var    <- str_split(chemInfo$RHS_var[1],  " ")[[1]] 
    arrow_type <- chemInfo$arrow_type[1]
    kf         <- chemInfo$kf[1]
    kr         <- chemInfo$kr[1]
    FR.bool    <- chemInfo$FM_bool[1] 
    FRs        <- chemInfo$FMs[1] 
    FR.RCs     <- chemInfo$FM_rateC[1] 
    RR.bool    <- chemInfo$RM_bool[1] 
    RRs        <- chemInfo$RMs[1] 
    RR.RCs     <- chemInfo$RM_rateC[1]
    jPrint(arrow_type)
  }
  
  
  div(
    pickerInput(
      inputId = "eqnCreate_type_of_equation_edit",
      label = "Equation Type",
      choices = c("Chemical Reaction" = "chem_rxn",
                  "Enzyme Based Reaction" = "enzyme_rxn",
                  "Synthesis" = "syn",
                  "Degradation" = "deg",
                  "Custom Rate Parameter" = "rate_eqn",
                  "Time Dependent Equation" = "time_dependent"
      ),
      selected = eqn.type
    ),
    conditionalPanel(
      condition = "input.eqnCreate_type_of_equation_edit == 'chem_rxn'",
      pickerInput(
        inputId = "eqn_chem_law_edit",
        label = "Law",
        choices = c("Mass Action" = "MA",
                    "Regulated Mass Action" = "MAwR"
        ),
        selected = eqn.law
      ),
      pickerInput(
        inputId = "eqn_chem_forward_or_both_edit"
        ,label = "Reaction Direction"
        ,choices = c("Reversible" = "both_directions",
                     "Forward" = 'forward_only')
        ,choicesOpt = list(icon = c("glyphicon glyphicon-resize-horizontal",
                                    "glyphicon glyphicon-arrow-right"
                                   )
        ),
        selected = arrow_type
      ),
      conditionalPanel(
        condition = "input.eqn_chem_law_edit == 'MAwR'",
        hr(),
        prettyCheckbox(
          inputId = "eqn_options_chem_modifier_forward_edit",
          label = "Add Forward Regulator(s)",
          value = FR.bool
        ),
        conditionalPanel(
          condition = "input.eqn_options_chem_modifier_forward_edit",
          numericInput(inputId = "eqn_options_chem_num_forward_regulators_edit"
                       ,label = "# of Forward Regulators"
                       ,value = 1
                       ,min = 1
                       ,step = 1)
        ),
        conditionalPanel(
          condition = "input.eqn_chem_forward_or_both_edit == 'both_directions'",
          prettyCheckbox(
            inputId = "eqn_options_chem_modifier_reverse_edit"
            ,label = "Add Reverse Regulator(s)"
            ,value = FALSE
          ),
          conditionalPanel(
            condition = "input.eqn_options_chem_modifier_reverse_edit",
            numericInput(inputId = "eqn_options_chem_num_reverse_regulators_edit"
                         ,label = "# of Reverse Regulators"
                         ,value = 1
                         ,min = 1
                         ,step = 1)
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input.eqnCreate_type_of_equation_edit == 'enzyme_rxn'",
      pickerInput(
        inputId = "eqn_enzyme_law_edit",
        label = "Law",
        choices = c("Michaelis Menten Kinetics" = "MM",
                    "Other" = "Other")
      ),
      hr(),
      prettyCheckbox(
        inputId = "eqn_options_enzyme_useVmax_edit"
        ,label = "Use Vmax"
        ,value = FALSE
      )
    ),
    conditionalPanel(
      condition = "input.eqnCreate_type_of_equation_edit == 'syn'",
      pickerInput(
        inputId = "eqn_syn_law_edit",
        label = "Law",
        choices = c("Rate" = "rate",
                    "By Factor" = "byFactor")
      )
    ),
    conditionalPanel(
      condition = "input.eqnCreate_type_of_equation_edit == 'deg'",
      pickerInput(
        inputId = "eqn_deg_law_edit",
        label = "Law",
        choices = c("Rate" = "rate",
                    "By Enzyme" = "byEnzyme")
      ),
      hr(),
      prettyCheckbox(
        inputId = "eqn_deg_to_products",
        label = "Degrades to species",
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.eqn_deg_to_products",
        numericInput(
          inputId = "eqn_deg_num_products",
          label = "Number of Species",
          value = 1,
          min = 1,
          step = 1
        )
      ),
      conditionalPanel(
        condition = "input.eqn_deg_law == 'byEnzyme'",
        hr(),
        prettyCheckbox(
          inputId = "eqn_deg_use_Vmax",
          label = "Use Vmax",
          value = FALSE
        ),
      )
    )
  )
})

# ---------------Editing Equations RenderUI-------------------------------------
output$edit_chemical_reaction <- renderUI({
  req(input$createEqn_edit_equation_button)
  n.RHS = as.numeric(input$eqnCreate_num_of_eqn_RHS_edit)
  n.LHS = as.numeric(input$eqnCreate_num_of_eqn_LHS_edit)
  eqn_number = input$eqnCreate_edit_select_equation #eqn number to edit
  eqn_to_edit <- eqns$eqn.info[eqn_number, 1:ncol(eqns$eqn.info)] #extract equation
  
  fluidRow(column(width = 2
                  ,lapply(seq(n.LHS), function(i){
                    numericInput(inputId = paste0("LHS_Coeff_edit_", as.character(i))
                                 ,label = "Coefficient"
                                 ,value = str_split(eqn_to_edit[2], " ")[[1]][i]
                                 ,min = 1
                                 ,step = 1)
                  })
  )#end Column
  ,column(width = 2
          ,lapply(seq(n.LHS), function(i){
            pickerInput(inputId = paste0("LHS_Var_edit_", as.character(i))
                        ,label = "Choose Var"
                        ,choices = sort(vars$species)
                        ,selected = str_split(eqn_to_edit[3], " ")[[1]][i])
          })
  )#end column
  ,column(width = 3
          #,offset=1
          ,pickerInput(inputId = "eqn_chem_forward_or_both_edit"
                       ,label = "Reaction Direction"
                       ,choices = c("Forward" = 'forward_only'
                                    ,"Both" = "both_directions")
                       ,selected = ifelse(eqn_to_edit[6] == "forward_only", "forward_only", "both_directions"))
          ,textInput(inputId = "eqn_chem_forward_k_edit"
                     ,label = "Forward Rate Constant"
                     ,value = eqn_to_edit[7])
          ,conditionalPanel(condition = "input.eqn_chem_forward_or_both_edit=='both_directions'"
                            ,textInput(inputId = "eqn_chem_back_k_edit"
                                       ,label = "Reverse Rate Constant"
                                       ,value = eqn_to_edit[8]))
  )#end column
  ,column(width = 2
          #,offset=1
          ,lapply(seq(n.RHS), function(i){
            numericInput(inputId = paste0("RHS_Coeff_edit_", as.character(i))
                         ,label = "Coefficient"
                         ,value = str_split(eqn_to_edit[4], " ")[[1]][i]
                         ,min = 1
                         ,step = 1)
          })
  )#end Column
  ,column(width = 2
          ,lapply(seq(n.RHS), function(i){
            pickerInput(inputId = paste0("RHS_Var_edit_", as.character(i))
                        ,label = "Choose Var"
                        ,choices = sort(vars$species)
                        ,selected = str_split(eqn_to_edit[5], " ")[[1]][i])
          })
  )#end column
  )#end fluidRow
  
})

output$edit_enzyme_reaction <- renderUI({
  req(input$createEqn_edit_equation_button)
  eqn_number = input$eqnCreate_edit_select_equation #eqn number to edit
  eqn_to_edit <- eqns$eqn.info[eqn_number, 1:ncol(eqns$eqn.info)] #extract equation
  div(
    fluidRow(column(width = 3
                    ,pickerInput(inputId = "eqn_enzyme_substrate_edit"
                                 ,label = "Substrate"
                                 ,choices = sort(vars$species)
                                 ,selected = eqn_to_edit[3])
                    ,conditionalPanel(condition = "input.eqn_options_enzyme_useVmax"
                                      ,pickerInput(inputId = "eqn_enzyme_enzyme_edit"
                                                   ,label = "Enzyme"
                                                   ,choices = sort(vars$species)
                                                   ,selected = eqn_to_edit[12]))
    )
    ,column(width = 3
            ,offset = 1
            ,conditionalPanel(condition = "!input.eqn_options_enzyme_useVmax"
                              ,textInput(inputId = "eqn_enzyme_Vmax_edit"
                                         ,label = "Vmax"
                                         ,value = eqn_to_edit[10]))
            ,conditionalPanel(condition = "input.eqn_options_enzyme_useVmax"
                              ,textInput(inputId = "eqn_enzyme_kcat_edit"
                                         ,label = "kcat"
                                         ,value = eqn_to_edit[9]))
            
            ,textInput(inputId = "eqn_enzyme_Km_edit"
                       ,label = "Km"
                       ,value = eqn_to_edit[11])
    )
    ,column(width = 3
            ,offset = 1
            ,pickerInput(inputId = "eqn_enzyme_product_edit"
                         ,label = "Product"
                         ,choices = sort(vars$species)
                         ,selected = eqn_to_edit[5]))
    )#end fluidRow
  )#end div
})
# 
# ,conditionalPanel(condition="input.eqnCreate_type_of_equation=='chem_rxn'"
#                   ,uiOutput("eqnCreate_equationBuilder_chem"))

equationBuilder_edit <- reactive({
  if (input$eqnCreate_type_of_equation_edit == "chem_rxn") {
    n.RHS = as.numeric(input$eqnCreate_num_of_eqn_RHS_edit)
    n.LHS = as.numeric(input$eqnCreate_num_of_eqn_LHS_edit)
    
    eqn_LHS <- ""
    for (i in seq(n.LHS)) {
      coef <- eval(parse(text = paste0("input$LHS_Coeff_edit_", as.character(i))))
      var <- eval(parse(text = paste0("input$LHS_Var_edit_", as.character(i))))
      if (coef != "1") {eqn_LHS <- paste0(eqn_LHS, coef, "*")}
      if (i == is.numeric(n.LHS)) {eqn_LHS <- paste0(eqn_LHS, var)}
      else{eqn_LHS <- paste0(eqn_LHS, var, " + ")}
    }
    
    eqn_RHS <- ""
    for (i in seq(n.RHS)) {
      coef <- eval(parse(text = paste0("input$RHS_Coeff_edit_", as.character(i))))
      var <- eval(parse(text = paste0("input$RHS_Var_edit_", as.character(i))))
      if (coef != "1") {eqn_RHS <- paste0(eqn_RHS, coef, "*")}
      if (i == as.numeric(n.RHS)) {eqn_RHS <- paste0(eqn_RHS, var)}
      else{eqn_RHS <- paste0(eqn_RHS, var, " + ")}
    }
    
    if (input$eqn_chem_forward_or_both_edit == "both_directions") {
      arrow <- "<-->"
      arrow <- paste0("(", input$eqn_chem_back_k_edit, ")", arrow, "(", input$eqn_chem_forward_k_edit, ")")
    }
    else if (input$eqn_chem_forward_or_both_edit == "forward_only") {
      arrow = "--->"
      arrow <- paste0(arrow, "(", input$eqn_chem_forward_k_edit, ")")
    }
    
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
  }
  
  else if (input$eqnCreate_type_of_equation_edit == "enzyme_rxn") {
    substrate = input$eqn_enzyme_substrate_edit
    product = input$eqn_enzyme_product_edit
    arrow = "-->"
    enzyme = input$eqn_enzyme_enzyme_edit
    Km = input$eqn_enzyme_Km_edit
    
    if (input$eqn_options_enzyme_useVmax) {
      kcat = input$eqn_enzyme_kcat_edit
      textOut <- paste0(substrate," + ", enzyme,  " (", kcat, ")", arrow, "(", Km, ") ", product)
    } else if (!input$eqn_options_enzyme_useVmax) {
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
  eqn_to_edit <- eqns$eqn.info[eqn_number, 1:ncol(eqns$eqn.info)] #extract equation
  
  #delete components
  
  eqn_type <- input$eqnCreate_type_of_equation_edit
  #add new components to equation sheet.
  
  if (eqn_type == "chem_rxn") {
    n.RHS = as.numeric(input$eqnCreate_num_of_eqn_RHS_edit)
    n.LHS = as.numeric(input$eqnCreate_num_of_eqn_LHS_edit)
    
    coef.LHS <- vector()
    var.LHS <- vector()
    for (i in seq(n.LHS)) {
      coef <- eval(parse(text = paste0("input$LHS_Coeff_edit_", as.character(i))))
      var <- eval(parse(text = paste0("input$LHS_Var_edit_", as.character(i))))
      coef.LHS <- append(coef.LHS, coef)
      var.LHS <- append(var.LHS, var)
    }
    coef.LHS <- paste(coef.LHS, collapse = " ")
    var.LHS <- paste(var.LHS, collapse = " ")
    
    coef.RHS <- vector()
    var.RHS <- vector()
    for (i in seq(n.RHS)) {
      coef <- eval(parse(text = paste0("input$RHS_Coeff_edit_", as.character(i))))
      var <- eval(parse(text = paste0("input$RHS_Var_edit_", as.character(i))))
      coef.RHS <- append(coef.RHS, coef)
      var.RHS <- append(var.RHS, var)
    }
    coef.RHS <- paste(coef.RHS, collapse = " ")
    var.RHS <- paste(var.RHS, collapse = " ")
    
    arrow <- input$eqn_chem_forward_or_both_edit
    if (arrow == "both_directions") {
      kf <- input$eqn_chem_forward_k_edit
      kr <- input$eqn_chem_back_k_edit
      # params$eqns.vars <- append(params$eqns.vars, kf)
      # params$eqns.vars <- append(params$eqns.vars, kr)
      StoreParamsEqn(kf)
      StoreParamsEqn(kr)
    }
    else if (arrow == "forward_only") {
      kf <- input$eqn_chem_forward_k_edit
      kr <- NA
      #params$eqns.vars <- append(params$eqns.vars, kf)
      StoreParamsEqn(kf)
    }
    kcat = NA
    Vmax = NA 
    Km = NA 
    enzyme = NA
    FM.bool <- FALSE
    f_regulators_coef <- NA
    f_regulators_rateConstants <- NA
    RM.bool <- FALSE
    RMs <- NA
    RM.RC <- NA
    row_to_df <- c(eqn_type, coef.LHS, var.LHS, coef.RHS, var.RHS, arrow, kf, kr, 
                   kcat, Vmax, Km, enzyme,
                   FM.bool, f_regulators_coef, f_regulators_rateConstants,
                   RM.bool, RMs, RM.RC)
    
  }#end if chem_rxn
  else if (eqn_type == "enzyme_rxn") {
    coef.LHS <- 1
    coef.RHS <- 1
    var.LHS = input$eqn_enzyme_substrate_edit
    var.RHS = input$eqn_enzyme_product_edit
    arrow <- "forward_only"
    Km = input$eqn_enzyme_Km_edit
    #params$eqns.vars <- append(params$eqns.vars, Km)
    StoreParamsEqn(Km)
    
    if (input$eqn_options_enzyme_useVmax) {
      kcat = input$eqn_enzyme_kcat_edit
      enzyme = input$eqn_enzyme_enzyme_edit
      Vmax = NA
      # params$eqns.vars <- append(params$eqns.vars, kcat)
      # params$eqns.vars <- append(params$eqns.vars, Km)
      StoreParamsEqn(kcat)
      StoreParamsEqn(Km)
    } else if (!input$eqn_options_enzyme_useVmax) {
      Vmax = input$eqn_enzyme_Vmax_edit
      kcat = NA
      enzyme = NA
      #params$eqns.vars <- append(params$eqns.vars, Vmax)
      StoreParamsEqn(Vmax)
    }
    
    kf = NA
    kr = NA
    FM.bool <- FALSE
    f_regulators_coef <- NA
    f_regulators_rateConstants <- NA
    RM.bool <- FALSE
    RMs <- NA
    RM.RC <- NA
    row_to_df <- c(eqn_type, coef.LHS, var.LHS, coef.RHS, var.RHS, arrow, kf, kr, 
                   kcat, Vmax, Km, enzyme,
                   FM.bool, f_regulators_coef, f_regulators_rateConstants,
                   RM.bool, RMs, RM.RC)
  }
  eqns$eqn.info[eqn_number, 1:ncol(eqns$eqn.info)]  <- row_to_df
  eqns$main[as.numeric(eqn_number)] <- equationBuilder_edit()
  # updatePickerInput(session,
  #                   'eqnCreate_edit_select_equation'
  #                   ,choices = seq(length(eqns$main)))
})
observe({print(eqns$main)})