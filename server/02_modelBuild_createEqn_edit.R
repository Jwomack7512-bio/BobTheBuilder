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
#----------------Left Box with edit options for the equation--------------------
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

  arrow_type  <- NA
  FR.bool     <- FALSE
  RR.bool     <- FALSE
  num.FRs     <- 1
  num.RRs     <- 1
  use.Vmax    <- FALSE
  prod.exists <- FALSE
  num.prods   <- 1
  
  # Unpack the different kind of laws to fill out proper information
  if (eqn.type == "chem_rxn") {
    # Find Row with matching ID and extract
    row        <- match(eqn.ID, eqns$eqn.chem[1:nrow(eqns$eqn.chem), 1])
    chemInfo   <- eqns$eqn.chem[row, 1:ncol(eqns$eqn.chem)]
    
    ID         <- chemInfo$ID[1]
    Law        <- chemInfo$Law[1]
    LHS.coef   <- str_split(chemInfo$LHS_coef[1], " ")[[1]]
    LHS.var    <- str_split(chemInfo$LHS_var[1],  " ")[[1]]
    RHS.coef   <- str_split(chemInfo$RHS_coef[1], " ")[[1]]
    RHS.var    <- str_split(chemInfo$RHS_var[1],  " ")[[1]] 
    arrow_type <- chemInfo$arrow_type[1]
    kf         <- chemInfo$kf[1]
    kr         <- chemInfo$kr[1]
    FR.bool    <- chemInfo$FM_bool[1] 
    FRs        <- str_split(chemInfo$FMs[1], " ")[[1]] 
    FR.RCs     <- str_split(chemInfo$FM_rateC[1], " ")[[1]] 
    RR.bool    <- chemInfo$RM_bool[1] 
    RRs        <- str_split(chemInfo$RMs[1], " ")[[1]] 
    RR.RCs     <- str_split(chemInfo$RM_rateC[1], " ")[[1]]
    
    num.FRs    <- length(FRs)
    num.RRs    <- length(RRs)
    
  } else if (eqn.type == "enzyme_rxn") {
    row        <- match(eqn.ID, eqns$eqn.enzyme[1:nrow(eqns$eqn.enzyme), 1])
    enz.info   <- eqns$eqn.enzyme[row, 1:ncol(eqns$eqn.enzyme)]
    
    ID        <- enz.info$ID[1]
    Law       <- enz.info$Law[1]
    substrate <- enz.info$Substrate[1]
    product   <- enz.info$Product[1]
    enzyme    <- enz.info$Enzyme[1]
    kcat      <- enz.info$kcat[1]
    Km        <- enz.info$Km[1]
    Vmax      <- enz.info$Vmax[1]
    use.Vmax  <- ifelse(is.na(Vmax), FALSE, TRUE)
    
  } else if (eqn.type == "syn") {
    row        <- match(eqn.ID, eqns$eqn.syn[1:nrow(eqns$eqn.syn), 1])
    synInfo    <- eqns$eqn.syn[row, 1:ncol(eqns$eqn.syn)]
    
    ID     <- synInfo$ID[1]
    Law    <- synInfo$Law[1]
    VarSyn <- synInfo$VarSyn[1]
    RC     <- synInfo$RC[1]
    Factor <- synInfo$Factor[1]
    
  } else if (eqn.type == "deg") {
    row        <- match(eqn.ID, eqns$eqn.deg[1:nrow(eqns$eqn.deg), 1])
    degInfo    <- eqns$eqn.deg[row, 1:ncol(eqns$eqn.deg)]
    
    ID        <- degInfo$ID[1]
    Law       <- degInfo$Law[1]
    VarDeg    <- degInfo$VarDeg[1]
    ConcDep   <- degInfo$ConcDep[1]
    RC        <- degInfo$RC[1]
    Km        <- degInfo$Km[1]
    Enz       <- degInfo$Enz[1]
    Vmax      <- degInfo$Vmax[1]
    Product   <- degInfo$Prods[1]
    use.Vmax  <- ifelse(is.na(Vmax), FALSE, TRUE)
    
    prod.exists <- ifelse(is.na(Product), FALSE, TRUE)
    if (prod.exists) {
      num.prods <- length(strsplit(Product, " ")[[1]])
    }
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
        choices = c("Mass Action" = "MassAction",
                    "Regulated Mass Action" = "RegulatedMA"
        ),
        selected = Law
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
        condition = "input.eqn_chem_law_edit == 'RegulatedMA'",
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
                       ,value = num.FRs
                       ,min = 1
                       ,step = 1)
        ),
        conditionalPanel(
          condition = "input.eqn_chem_forward_or_both_edit == 'both_directions'",
          prettyCheckbox(
            inputId = "eqn_options_chem_modifier_reverse_edit"
            ,label = "Add Reverse Regulator(s)"
            ,value = RR.bool
          ),
          conditionalPanel(
            condition = "input.eqn_options_chem_modifier_reverse_edit",
            numericInput(inputId = "eqn_options_chem_num_reverse_regulators_edit"
                         ,label = "# of Reverse Regulators"
                         ,value = num.RRs
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
        choices = c("Michaelis Menten Kinetics" = "Michaelis Menten",
                    "Other" = "Other"),
        selected = Law
      ),
      hr(),
      prettyCheckbox(
        inputId = "eqn_options_enzyme_useVmax_edit"
        ,label = "Use Vmax"
        ,value = use.Vmax
      )
    ),
    conditionalPanel(
      condition = "input.eqnCreate_type_of_equation_edit == 'syn'",
      pickerInput(
        inputId = "eqn_syn_law_edit",
        label = "Law",
        choices = c("Rate" = "rate",
                    "By Factor" = "byFactor"),
        selected = Law
      )
    ),
    conditionalPanel(
      condition = "input.eqnCreate_type_of_equation_edit == 'deg'",
      pickerInput(
        inputId = "eqn_deg_law_edit",
        label = "Law",
        choices = c("Rate" = "rate",
                    "By Enzyme" = "byEnzyme"),
        selected = Law
      ),
      hr(),
      prettyCheckbox(
        inputId = "eqn_deg_to_products",
        label = "Degrades to species",
        value = prod.exists
      ),
      conditionalPanel(
        condition = "input.eqn_deg_to_products",
        numericInput(
          inputId = "eqn_deg_num_products",
          label = "Number of Species",
          value = num.prods,
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
          value = use.Vmax
        ),
      )
    )
  )
})

# ---------------Editing Equations RenderUI-------------------------------------
output$eqnCreate_equationBuilder_chem_edit <- renderUI({
  
  eqn.num     <- as.numeric(input$eqnCreate_edit_select_equation)
  eqn.row     <- eqns$eqn.info[eqn.num, 1:ncol(eqns$eqn.info)]
  
  eqn.ID      <- eqn.row$ID
  eqn.type    <- eqn.row$EqnType
  eqn.law     <- eqn.row$Law
  eqn.species <- eqn.row$Species
  eqn.RC      <- eqn.row$RateConstants
  eqn.compart <- eqn.row$Compartment
  eqn.descrpt <- eqn.row$Description
  
  row        <- match(eqn.ID, eqns$eqn.chem[1:nrow(eqns$eqn.chem), 1])
  chemInfo   <- eqns$eqn.chem[row, 1:ncol(eqns$eqn.chem)]
  
  ID         <- chemInfo$ID[1]
  Law        <- chemInfo$Law[1]
  LHS.coef   <- str_split(chemInfo$LHS_coef[1], " ")[[1]]
  LHS.var    <- str_split(chemInfo$LHS_var[1],  " ")[[1]]
  RHS.coef   <- str_split(chemInfo$RHS_coef[1], " ")[[1]]
  RHS.var    <- str_split(chemInfo$RHS_var[1],  " ")[[1]] 
  arrow_type <- chemInfo$arrow_type[1]
  kf         <- chemInfo$kf[1]
  kr         <- chemInfo$kr[1]
  FR.bool    <- chemInfo$FM_bool[1] 
  FRs        <- str_split(chemInfo$FMs[1], " ")[[1]] 
  FR.RCs     <- str_split(chemInfo$FM_rateC[1], " ")[[1]] 
  RR.bool    <- chemInfo$RM_bool[1] 
  RRs        <- str_split(chemInfo$RMs[1], " ")[[1]] 
  RR.RCs     <- str_split(chemInfo$RM_rateC[1], " ")[[1]]
  
  num.LHS    <- length(LHS.coef)
  num.RHS    <- length(RHS.coef)
  num.FRs    <- length(FRs)
  num.RRs    <- length(RRs)
  
  div(
    fluidRow(
      column(
        width = 3, 
        numericInput(inputId = "eqnCreate_num_of_eqn_LHS_edit",
                     label = "Number of Reactants",
                     value = num.LHS,
                     min = 1,
                     step = 1)
      ),
      column(
        width = 3,
        numericInput(inputId = "eqnCreate_num_of_eqn_RHS_edit",
                     label = "Number of Products",
                     value = num.RHS,
                     min = 1,
                     step = 1)
      )
    ),
    hr(),
    fluidRow(
      column(
        style = "border-right: 1px solid #e5e5e5; padding-right:20px",
        width = 4,
        lapply(seq(input$eqnCreate_num_of_eqn_LHS_edit), function(i){
          div(
            HTML(paste0("<b>Reactant ", as.character(i), "</b>")),
            splitLayout(
              numericInput(
                inputId = paste0("LHS_Coeff_edit", as.character(i)),
                label = NULL,
                value = as.numeric(LHS.coef[i]),
                min = 1,
                step = 1),
              pickerInput(
                inputId = paste0("LHS_Var_edit", as.character(i)),
                label = NULL,
                choices = sort(vars$species),
                selected = LHS.var[i],
                options = pickerOptions(liveSearch = TRUE
                                         ,liveSearchStyle = "startsWith"
                                         ,dropupAuto = FALSE)
                )
              ,cellWidths = c("25%", "75%")
            )
          )
        })
      ), #end Column
      column(
        style = "border-right: 1px solid #e5e5e5; 
               padding-right: 20px; 
               padding-left: 20px;",
        width = 4,
        lapply(seq(input$eqnCreate_num_of_eqn_RHS_edit), function(i){
          div(
            HTML(paste0("<b>Product ", as.character(i), "</b>")),
            splitLayout(
              numericInput(
                inputId = paste0("RHS_Coeff_edit", as.character(i)),
                label = NULL,
                value = as.numeric(RHS.coef[i]),
                min = 1,
                step = 1),
              pickerInput(
                inputId = paste0("RHS_Var_edit", as.character(i)),
                label = NULL,
                choices = sort(vars$species),
                selected = RHS.var[i],
                options = pickerOptions(liveSearch = TRUE
                                         ,liveSearchStyle = "startsWith"
                                         ,dropupAuto = FALSE)
                )
              ,cellWidths = c("25%", "75%")
            )
          )
        })
      ), #end Column
      column(
        style = "padding-left: 20px;",
        width = 4,
        conditionalPanel(
          condition = "!input.eqn_options_chem_modifier_forward_edit",
          textInput(
            inputId = "eqn_chem_forward_k_edit",
            label = "Forward Rate Constant",
            value = kf
          ),
          tags$head(tags$style("#eqn_chem_forward_k_edit {margin-top: -7px;}")),
        ),
        conditionalPanel(
          condition = "input.eqn_chem_forward_or_both_edit == 'both_directions' && 
                       !input.eqn_options_chem_modifier_reverse_edit",
          textInput(
            inputId = "eqn_chem_back_k_edit",
            label = "Reverse Rate Constant",
            value = kr
          )
        )
      )#end column
    ), #end fluidRow`
    conditionalPanel(
      condition = "input.eqn_options_chem_modifier_forward_edit || 
                   input.eqn_options_chem_modifier_reverse_edit",
      hr()
    ),
    fluidRow(
      column(
        width = 3,
        conditionalPanel(
          condition = "input.eqn_options_chem_modifier_forward_edit",
            lapply(seq(input$eqn_options_chem_num_forward_regulators_edit), function(i){
              pickerInput(
                inputId = paste0("eqn_forward_regulator_edit", as.character(i)),
                label = paste0("Forward Regulator ", as.character(i)),
                choices = sort(vars$species),
                selected = FRs[i],
                options = pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = "startsWith"))
            })
        )
      ),
      column(
        width = 3,
        conditionalPanel(
          condition = "input.eqn_options_chem_modifier_forward_edit",
          lapply(seq(input$eqn_options_chem_num_forward_regulators_edit), function(i){
            textInput(
              inputId = paste0("eqn_forward_rateConstant_edit", as.character(i)),
              label = paste0("Rate Constant ", as.character(i)),
              value = FR.RCs[i]
            )
          })
        )
      )
    ),
    fluidRow(
      column(
        width = 3,
        conditionalPanel(
          condition = "input.eqn_options_chem_modifier_reverse_edit",
          lapply(seq(input$eqn_options_chem_num_reverse_regulators_edit), function(i){
            pickerInput(
              inputId = paste0("eqn_reverse_regulator_edit", as.character(i)),
              label = paste0("Reverse Regulator ", as.character(i)),
              choices = sort(vars$species),
              selected = RRs[i],
              options = pickerOptions(liveSearch = TRUE
                                       ,liveSearchStyle = "startsWith")
              )
          })
        )
      ),
      column(
        width = 3,
        conditionalPanel(
          condition = "input.eqn_options_chem_modifier_reverse_edit",
          lapply(seq(input$eqn_options_chem_num_reverse_regulators_edit), function(i){
            textInput(
              inputId = paste0("eqn_reverse_rateConstant_edit", as.character(i)),
              label = "Rate Constant",
              value = RR.RCs[i]
              )
          })
        )
      )
    )
  )
})

output$eqnCreate_equationBuilder_enzyme_edit <- renderUI({
  
  eqn.num     <- as.numeric(input$eqnCreate_edit_select_equation)
  eqn.row     <- eqns$eqn.info[eqn.num, 1:ncol(eqns$eqn.info)]
  
  eqn.ID      <- eqn.row$ID
  eqn.type    <- eqn.row$EqnType
  eqn.law     <- eqn.row$Law
  eqn.species <- eqn.row$Species
  eqn.RC      <- eqn.row$RateConstants
  eqn.compart <- eqn.row$Compartment
  eqn.descrpt <- eqn.row$Description
  
  row        <- match(eqn.ID, eqns$eqn.enzyme[1:nrow(eqns$eqn.enzyme), 1])
  enz.info   <- eqns$eqn.enzyme[row, 1:ncol(eqns$eqn.enzyme)]
  
  ID        <- enz.info$ID[1]
  Law       <- enz.info$Law[1]
  Substrate <- enz.info$Substrate[1]
  Product   <- enz.info$Product[1]
  Enzyme    <- enz.info$Enzyme[1]
  kcat      <- enz.info$kcat[1]
  Km        <- enz.info$Km[1]
  Vmax      <- enz.info$Vmax[1]
  use.Vmax  <- ifelse(is.na(Vmax), FALSE, TRUE)
  
  div(
    conditionalPanel(
      condition = "input.eqn_enzyme_law_edit == 'Michaelis Menten'",
      fluidRow(
        column(
          width = 3,
          pickerInput(
            inputId = "eqn_enzyme_substrate_edit",
            label = "Substrate",
            choices = sort(vars$species),
            selected = Substrate,
            options = pickerOptions(
              liveSearch = TRUE,
              liveSearchStyle = "startsWith",
              dropupAuto = FALSE
            )
          ),
          conditionalPanel(
            condition = "!input.eqn_options_enzyme_useVmax_edit",
            pickerInput(
              inputId = "eqn_enzyme_enzyme_edit",
              label = "Enzyme",
              choices = sort(vars$species),
              selected = Enzyme,
              options = pickerOptions(liveSearch = TRUE,
                                      liveSearchStyle = "startsWith")
            )
          )
        ),
        column(
          width = 3,
          offset = 1,
          conditionalPanel(
            condition = "input.eqn_options_enzyme_useVmax_edit",
            textInput(
              inputId = "eqn_enzyme_Vmax_edit",
              label = "Vmax",
              value = Vmax
            )
          ),
          conditionalPanel(
            condition = "!input.eqn_options_enzyme_useVmax_edit",
            textInput(
              inputId = "eqn_enzyme_kcat_edit",
              label = "kcat",
              value = kcat
            )
          ),
          textInput(
            inputId = "eqn_enzyme_Km_edit",
            label = "Km",
            value = Km
          )
        ),
        column(
          width = 3,
          offset = 1,
          pickerInput(
            inputId = "eqn_enzyme_product_edit",
            label = "Product",
            choices = sort(vars$species),
            selected = Product,
            options = pickerOptions(
              liveSearch = TRUE,
              liveSearchStyle = "startsWith",
              dropupAuto = FALSE
            )
          )
        )
      )#end fluidRow
    ),
    conditionalPanel(
      condition = "input.eqn_enzyme_law_edit == 'Other'",
      "Other enzyme laws will be added in these tabs in the future"
    )
  )#end div
})

output$eqnCreate_equationBuilder_synthesis_edit <- renderUI({
  
  eqn.num     <- as.numeric(input$eqnCreate_edit_select_equation)
  eqn.row     <- eqns$eqn.info[eqn.num, 1:ncol(eqns$eqn.info)]
  
  eqn.ID      <- eqn.row$ID
  eqn.type    <- eqn.row$EqnType
  eqn.law     <- eqn.row$Law
  eqn.species <- eqn.row$Species
  eqn.RC      <- eqn.row$RateConstants
  eqn.compart <- eqn.row$Compartment
  eqn.descrpt <- eqn.row$Description
  
  row        <- match(eqn.ID, eqns$eqn.syn[1:nrow(eqns$eqn.syn), 1])
  synInfo    <- eqns$eqn.syn[row, 1:ncol(eqns$eqn.syn)]
  
  ID     <- synInfo$ID[1]
  Law    <- synInfo$Law[1]
  VarSyn <- synInfo$VarSyn[1]
  RC     <- synInfo$RC[1]
  Factor <- synInfo$Factor[1]
  
  div(
    fluidRow(
      column(
        width = 4,
        conditionalPanel(
          condition = "input.eqn_syn_law_edit == 'rate'",
          pickerInput(
            inputId  = "eqn_syn_rate_var_edit",
            label    = "Species to synthesize",
            choices  = sort(vars$species),
            selected = VarSyn,
            options  = pickerOptions(liveSearch = TRUE
                                    ,liveSearchStyle = "startsWith") 
          ),
          textInput(
            inputId = "eqn_syn_rate_RC_edit",
            label   = "Rate Constant",
            value   = RC
          )
        ),
        conditionalPanel(
          condition = "input.eqn_syn_law_edit == 'byFactor'",
          pickerInput(
            inputId  = "eqn_syn_sby_var_edit",
            label    = "Species to synthesize",
            choices  = sort(vars$species),
            selected = VarSyn,
            options  = pickerOptions(liveSearch = TRUE
                                    ,liveSearchStyle = "startsWith") 
          ),
          pickerInput(
            inputId  = "eqn_syn_sby_factor_edit",
            label    = "Factor causing synthesis",
            choices  = sort(vars$species),
            selected = Factor
          ),
          textInput(
            inputId = "eqn_syn_sby_RC_edit",
            label = "Rate Constant",
            value = RC
          )
        )
      )
    )
  )
})



# output$edit_chemical_reaction <- renderUI({
#   req(input$createEqn_edit_equation_button)
#   n.RHS = as.numeric(input$eqnCreate_num_of_eqn_RHS_edit)
#   n.LHS = as.numeric(input$eqnCreate_num_of_eqn_LHS_edit)
#   eqn_number = input$eqnCreate_edit_select_equation #eqn number to edit
#   eqn_to_edit <- eqns$eqn.info[eqn_number, 1:ncol(eqns$eqn.info)] #extract equation
#   
#   fluidRow(column(width = 2
#                   ,lapply(seq(n.LHS), function(i){
#                     numericInput(inputId = paste0("LHS_Coeff_edit_", as.character(i))
#                                  ,label = "Coefficient"
#                                  ,value = str_split(eqn_to_edit[2], " ")[[1]][i]
#                                  ,min = 1
#                                  ,step = 1)
#                   })
#   )#end Column
#   ,column(width = 2
#           ,lapply(seq(n.LHS), function(i){
#             pickerInput(inputId = paste0("LHS_Var_edit_", as.character(i))
#                         ,label = "Choose Var"
#                         ,choices = sort(vars$species)
#                         ,selected = str_split(eqn_to_edit[3], " ")[[1]][i])
#           })
#   )#end column
#   ,column(width = 3
#           #,offset=1
#           ,pickerInput(inputId = "eqn_chem_forward_or_both_edit"
#                        ,label = "Reaction Direction"
#                        ,choices = c("Forward" = 'forward_only'
#                                     ,"Both" = "both_directions")
#                        ,selected = ifelse(eqn_to_edit[6] == "forward_only", "forward_only", "both_directions"))
#           ,textInput(inputId = "eqn_chem_forward_k_edit"
#                      ,label = "Forward Rate Constant"
#                      ,value = eqn_to_edit[7])
#           ,conditionalPanel(condition = "input.eqn_chem_forward_or_both_edit=='both_directions'"
#                             ,textInput(inputId = "eqn_chem_back_k_edit"
#                                        ,label = "Reverse Rate Constant"
#                                        ,value = eqn_to_edit[8]))
#   )#end column
#   ,column(width = 2
#           #,offset=1
#           ,lapply(seq(n.RHS), function(i){
#             numericInput(inputId = paste0("RHS_Coeff_edit_", as.character(i))
#                          ,label = "Coefficient"
#                          ,value = str_split(eqn_to_edit[4], " ")[[1]][i]
#                          ,min = 1
#                          ,step = 1)
#           })
#   )#end Column
#   ,column(width = 2
#           ,lapply(seq(n.RHS), function(i){
#             pickerInput(inputId = paste0("RHS_Var_edit_", as.character(i))
#                         ,label = "Choose Var"
#                         ,choices = sort(vars$species)
#                         ,selected = str_split(eqn_to_edit[5], " ")[[1]][i])
#           })
#   )#end column
#   )#end fluidRow
#   
# })
# 
# output$edit_enzyme_reaction <- renderUI({
#   req(input$createEqn_edit_equation_button)
#   eqn_number = input$eqnCreate_edit_select_equation #eqn number to edit
#   eqn_to_edit <- eqns$eqn.info[eqn_number, 1:ncol(eqns$eqn.info)] #extract equation
#   div(
#     fluidRow(column(width = 3
#                     ,pickerInput(inputId = "eqn_enzyme_substrate_edit"
#                                  ,label = "Substrate"
#                                  ,choices = sort(vars$species)
#                                  ,selected = eqn_to_edit[3])
#                     ,conditionalPanel(condition = "input.eqn_options_enzyme_useVmax"
#                                       ,pickerInput(inputId = "eqn_enzyme_enzyme_edit"
#                                                    ,label = "Enzyme"
#                                                    ,choices = sort(vars$species)
#                                                    ,selected = eqn_to_edit[12]))
#     )
#     ,column(width = 3
#             ,offset = 1
#             ,conditionalPanel(condition = "!input.eqn_options_enzyme_useVmax"
#                               ,textInput(inputId = "eqn_enzyme_Vmax_edit"
#                                          ,label = "Vmax"
#                                          ,value = eqn_to_edit[10]))
#             ,conditionalPanel(condition = "input.eqn_options_enzyme_useVmax"
#                               ,textInput(inputId = "eqn_enzyme_kcat_edit"
#                                          ,label = "kcat"
#                                          ,value = eqn_to_edit[9]))
#             
#             ,textInput(inputId = "eqn_enzyme_Km_edit"
#                        ,label = "Km"
#                        ,value = eqn_to_edit[11])
#     )
#     ,column(width = 3
#             ,offset = 1
#             ,pickerInput(inputId = "eqn_enzyme_product_edit"
#                          ,label = "Product"
#                          ,choices = sort(vars$species)
#                          ,selected = eqn_to_edit[5]))
#     )#end fluidRow
#   )#end div
# })
# # 
# # ,conditionalPanel(condition="input.eqnCreate_type_of_equation=='chem_rxn'"
# #                   ,uiOutput("eqnCreate_equationBuilder_chem"))
# 
# equationBuilder_edit <- reactive({
#   if (input$eqnCreate_type_of_equation_edit == "chem_rxn") {
#     n.RHS = as.numeric(input$eqnCreate_num_of_eqn_RHS_edit)
#     n.LHS = as.numeric(input$eqnCreate_num_of_eqn_LHS_edit)
#     
#     eqn_LHS <- ""
#     for (i in seq(n.LHS)) {
#       coef <- eval(parse(text = paste0("input$LHS_Coeff_edit_", as.character(i))))
#       var <- eval(parse(text = paste0("input$LHS_Var_edit_", as.character(i))))
#       if (coef != "1") {eqn_LHS <- paste0(eqn_LHS, coef, "*")}
#       if (i == is.numeric(n.LHS)) {eqn_LHS <- paste0(eqn_LHS, var)}
#       else{eqn_LHS <- paste0(eqn_LHS, var, " + ")}
#     }
#     
#     eqn_RHS <- ""
#     for (i in seq(n.RHS)) {
#       coef <- eval(parse(text = paste0("input$RHS_Coeff_edit_", as.character(i))))
#       var <- eval(parse(text = paste0("input$RHS_Var_edit_", as.character(i))))
#       if (coef != "1") {eqn_RHS <- paste0(eqn_RHS, coef, "*")}
#       if (i == as.numeric(n.RHS)) {eqn_RHS <- paste0(eqn_RHS, var)}
#       else{eqn_RHS <- paste0(eqn_RHS, var, " + ")}
#     }
#     
#     if (input$eqn_chem_forward_or_both_edit == "both_directions") {
#       arrow <- "<-->"
#       arrow <- paste0("(", input$eqn_chem_back_k_edit, ")", arrow, "(", input$eqn_chem_forward_k_edit, ")")
#     }
#     else if (input$eqn_chem_forward_or_both_edit == "forward_only") {
#       arrow = "--->"
#       arrow <- paste0(arrow, "(", input$eqn_chem_forward_k_edit, ")")
#     }
#     
#     textOut <- paste(eqn_LHS, arrow, eqn_RHS)
#   }
#   
#   else if (input$eqnCreate_type_of_equation_edit == "enzyme_rxn") {
#     substrate = input$eqn_enzyme_substrate_edit
#     product = input$eqn_enzyme_product_edit
#     arrow = "-->"
#     enzyme = input$eqn_enzyme_enzyme_edit
#     Km = input$eqn_enzyme_Km_edit
#     
#     if (input$eqn_options_enzyme_useVmax) {
#       kcat = input$eqn_enzyme_kcat_edit
#       textOut <- paste0(substrate," + ", enzyme,  " (", kcat, ")", arrow, "(", Km, ") ", product)
#     } else if (!input$eqn_options_enzyme_useVmax) {
#       Vmax = input$eqn_enzyme_Vmax_edit
#       textOut <- paste0(substrate, " (", Vmax, ", Enzyme)", arrow, "(", Km, ") ", product)
#     }
#   }
#   
#   # else if(input$eqnCreate_type_of_equation_edit=="simp_diff"){
#   #   var_left = input$simp_diff_var1
#   #   var_right = input$simp_diff_var2
#   #   diff_coef <- input$simp_diff_PS_Var
#   #   ifelse(input$simp_diff_wayOfDiffusion, symbol <- "-->", symbol <- "<-->")
#   #   
#   #   textOut <- paste0(var_left, " ", symbol, "(", diff_coef, ") ", var_right)
#   # }
#   # else if(input$eqnCreate_type_of_equation=="mass_bal"){
#   #   textOut <- "MASS BAL"
#   # }
#   else{textOut <- "ERROR"}
#   return(textOut)
# })
#-------------------------------------------------------------------------------

# Edit Tab rewriting of Equations from Equation UI

#-------------------------------------------------------------------------------

# observeEvent(input$edit_save_changes_button, {
#   #find which equation we are editing. 
#   eqn_number = input$eqnCreate_edit_select_equation #eqn number to edit
#   eqn_to_edit <- eqns$eqn.info[eqn_number, 1:ncol(eqns$eqn.info)] #extract equation
#   
#   #delete components
#   
#   eqn_type <- input$eqnCreate_type_of_equation_edit
#   #add new components to equation sheet.
#   
#   if (eqn_type == "chem_rxn") {
#     n.RHS = as.numeric(input$eqnCreate_num_of_eqn_RHS_edit)
#     n.LHS = as.numeric(input$eqnCreate_num_of_eqn_LHS_edit)
#     
#     coef.LHS <- vector()
#     var.LHS <- vector()
#     for (i in seq(n.LHS)) {
#       coef <- eval(parse(text = paste0("input$LHS_Coeff_edit_", as.character(i))))
#       var <- eval(parse(text = paste0("input$LHS_Var_edit_", as.character(i))))
#       coef.LHS <- append(coef.LHS, coef)
#       var.LHS <- append(var.LHS, var)
#     }
#     coef.LHS <- paste(coef.LHS, collapse = " ")
#     var.LHS <- paste(var.LHS, collapse = " ")
#     
#     coef.RHS <- vector()
#     var.RHS <- vector()
#     for (i in seq(n.RHS)) {
#       coef <- eval(parse(text = paste0("input$RHS_Coeff_edit_", as.character(i))))
#       var <- eval(parse(text = paste0("input$RHS_Var_edit_", as.character(i))))
#       coef.RHS <- append(coef.RHS, coef)
#       var.RHS <- append(var.RHS, var)
#     }
#     coef.RHS <- paste(coef.RHS, collapse = " ")
#     var.RHS <- paste(var.RHS, collapse = " ")
#     
#     arrow <- input$eqn_chem_forward_or_both_edit
#     if (arrow == "both_directions") {
#       kf <- input$eqn_chem_forward_k_edit
#       kr <- input$eqn_chem_back_k_edit
#       # params$eqns.vars <- append(params$eqns.vars, kf)
#       # params$eqns.vars <- append(params$eqns.vars, kr)
#       StoreParamsEqn(kf)
#       StoreParamsEqn(kr)
#     }
#     else if (arrow == "forward_only") {
#       kf <- input$eqn_chem_forward_k_edit
#       kr <- NA
#       #params$eqns.vars <- append(params$eqns.vars, kf)
#       StoreParamsEqn(kf)
#     }
#     kcat = NA
#     Vmax = NA 
#     Km = NA 
#     enzyme = NA
#     FM.bool <- FALSE
#     f_regulators_coef <- NA
#     f_regulators_rateConstants <- NA
#     RM.bool <- FALSE
#     RMs <- NA
#     RM.RC <- NA
#     row_to_df <- c(eqn_type, coef.LHS, var.LHS, coef.RHS, var.RHS, arrow, kf, kr, 
#                    kcat, Vmax, Km, enzyme,
#                    FM.bool, f_regulators_coef, f_regulators_rateConstants,
#                    RM.bool, RMs, RM.RC)
#     
#   }#end if chem_rxn
#   else if (eqn_type == "enzyme_rxn") {
#     coef.LHS <- 1
#     coef.RHS <- 1
#     var.LHS = input$eqn_enzyme_substrate_edit
#     var.RHS = input$eqn_enzyme_product_edit
#     arrow <- "forward_only"
#     Km = input$eqn_enzyme_Km_edit
#     #params$eqns.vars <- append(params$eqns.vars, Km)
#     StoreParamsEqn(Km)
#     
#     if (input$eqn_options_enzyme_useVmax) {
#       kcat = input$eqn_enzyme_kcat_edit
#       enzyme = input$eqn_enzyme_enzyme_edit
#       Vmax = NA
#       # params$eqns.vars <- append(params$eqns.vars, kcat)
#       # params$eqns.vars <- append(params$eqns.vars, Km)
#       StoreParamsEqn(kcat)
#       StoreParamsEqn(Km)
#     } else if (!input$eqn_options_enzyme_useVmax) {
#       Vmax = input$eqn_enzyme_Vmax_edit
#       kcat = NA
#       enzyme = NA
#       #params$eqns.vars <- append(params$eqns.vars, Vmax)
#       StoreParamsEqn(Vmax)
#     }
#     
#     kf = NA
#     kr = NA
#     FM.bool <- FALSE
#     f_regulators_coef <- NA
#     f_regulators_rateConstants <- NA
#     RM.bool <- FALSE
#     RMs <- NA
#     RM.RC <- NA
#     row_to_df <- c(eqn_type, coef.LHS, var.LHS, coef.RHS, var.RHS, arrow, kf, kr, 
#                    kcat, Vmax, Km, enzyme,
#                    FM.bool, f_regulators_coef, f_regulators_rateConstants,
#                    RM.bool, RMs, RM.RC)
#   }
#   eqns$eqn.info[eqn_number, 1:ncol(eqns$eqn.info)]  <- row_to_df
#   eqns$main[as.numeric(eqn_number)] <- equationBuilder_edit()
#   # updatePickerInput(session,
#   #                   'eqnCreate_edit_select_equation'
#   #                   ,choices = seq(length(eqns$main)))
# })
# observe({print(eqns$main)})