# Edit Tab Controlling the editing of equations

# Left Box: Equation Edit Options ----------------------------------------------
output$eqnCreate_edit_rendering_sidebar <- renderUI({
# browser()
  # Find equation in data structure
  eqn.num     <- as.numeric(input$eqnCreate_edit_select_equation)
  eqn.row     <- rv.REACTIONS$reactions[[eqn.num]]
  
  # Unpack Equation Information
  eqn.ID               <- eqn.row$ID            
  eqn.display.type     <- eqn.row$Eqn.Display.Type 
  eqn.reaction.law     <- eqn.row$Reaction.Law    
  eqn.species          <- eqn.row$Species          
  eqn.reactants        <- eqn.row$Reactants        
  eqn.products         <- eqn.row$Products         
  eqn.Modifiers        <- eqn.row$Modifiers  
  eqn.parameters       <- eqn.row$Parameters       
  eqn.compartment      <- eqn.row$Compartment      
  eqn.description      <- eqn.row$Description      
  eqn.species.id       <- eqn.row$Species.id      
  eqn.reactants.id     <- eqn.row$Reactants.id     
  eqn.products.id      <- eqn.row$Products.id      
  eqn.modifiers.id     <- eqn.row$Modifiers.id     
  eqn.parameters.id    <- eqn.row$Parameters.id   
  eqn.compartment.id   <- eqn.row$Compartment.id   
  eqn.equation.text    <- eqn.row$Equation.Text    
  eqn.equation.latex   <- eqn.row$Equation.Latex   
  eqn.equation.mathjax <- eqn.row$Equation.MathJax 
  eqn.string.rate.law  <- eqn.row$String.Rate.Law  
  eqn.pretty.rate.law  <- eqn.row$Pretty.Rate.Law  
  eqn.latex.rate.law   <- eqn.row$Latex.Rate.Law   
  eqn.mathjax.rate.law <- eqn.row$MathJax.Rate.Law 
  eqn.mathml.rate.law  <- eqn.row$MathMl.Rate.Law 
  eqn.reversible       <- eqn.row$Reversible       
  
  # Initializing Vars (Need to check if I can remove this now)
  arrow_type  <- NA
  FR.bool     <- FALSE
  RR.bool     <- FALSE
  num.FRs     <- 1
  num.RRs     <- 1
  use.Vmax    <- FALSE
  prod.exists <- FALSE
  num.prods   <- 1

  # Unpack the different kind of laws to fill out proper information
  if (eqn.reaction.law == "mass_action") {
    # Extract reaction from chemical equation
    chemInfo <- rv.REACTIONS$massAction[[eqn.ID]]
    
    ID               <- chemInfo$ID
    law              <- chemInfo$Reaction.Law
    r.stoichiometry  <- str_split(chemInfo$r.stoichiometry, ", ")[[1]]
    Reactants        <- str_split(chemInfo$Reactants,  ", ")[[1]]
    p.stoichiometry  <- str_split(chemInfo$p.stoichiometry, ", ")[[1]]
    Products         <- str_split(chemInfo$Products,  ", ")[[1]] 
    Reactants.id     <- str_split(chemInfo$Reactants.id, ", ")[[1]]
    Products.id      <- str_split(chemInfo$Products.id, ", ")[[1]]
    arrow_type       <- chemInfo$Reversible
    kf               <- chemInfo$kf
    kr               <- chemInfo$kr
    kf.id            <- chemInfo$kf.id
    kr.id            <- chemInfo$kr.id
    
    
    div(
        pickerInput(
          inputId = "PI_mass_action_reverisble_option_edit",
          label = "Reversability?",
          choices = c("Reversible" = "both_directions",
                      "Irreversible" = "forward_only"),
          choicesOpt = list(icon = c("glyphicon glyphicon-resize-horizontal",
                                     "glyphicon glyphicon-arrow-right")),
          selected = arrow_type
        )
    )
  }
  else if (eqn.reaction.law == "mass_action_w_reg") {
    
    chemInfo <- rv.REACTIONS$massActionwReg[[eqn.ID]]
    
    ID               <- chemInfo$ID
    law              <- chemInfo$Reaction.Law
    r.stoichiometry  <- str_split(chemInfo$r.stoichiometry, ", ")[[1]]
    Reactants        <- str_split(chemInfo$Reactants,  ", ")[[1]]
    p.stoichiometry  <- str_split(chemInfo$p.stoichiometry, ", ")[[1]]
    Products         <- str_split(chemInfo$Products,  ", ")[[1]] 
    Reactants.id     <- str_split(chemInfo$Reactants.id, ", ")[[1]]
    Products.id      <- str_split(chemInfo$Products.id, ", ")[[1]]
    arrow_type       <- chemInfo$Reversible
    kf               <- chemInfo$kf
    kr               <- chemInfo$kr
    kf.id            <- chemInfo$kf.id
    kr.id            <- chemInfo$kr.id
    Use.Forward.Mod  <- chemInfo$Use.Forward.Mod
    Forward.Mods     <- str_split(chemInfo$Forward.Mods, ", ")[[1]]
    Forward.Mods.id  <- str_split(chemInfo$Forward.Mods.id, ", ")[[1]]
    Forward.Pars     <- str_split(chemInfo$Forward.Pars, ", ")[[1]]
    Forward.Pars.id  <- str_split(chemInfo$Forward.Pars.id, ", ")[[1]]
    Use.Reverse.Mod  <- chemInfo$Use.Reverse.Mod
    Reverse.Mods     <- str_split(chemInfo$Reverse.Mods, ", ")[[1]]
    Reverse.Mods.id  <- str_split(chemInfo$Reverse.Mods.id, ", ")[[1]]
    Reverse.Pars     <- str_split(chemInfo$Reverse.Pars, ", ")[[1]]
    Reverse.Pars.id  <- str_split(chemInfo$Reverse.Pars.id, ", ")[[1]]
    
    # Number of forward mods
    if (Use.Forward.Mod) {
      n.f.mods <- length(strsplit(Forward.Mods, ", ")[[1]])
    } else { 
      n.f.mods <- 1
    }
    
    # Number of reverse mods
    if (Use.Reverse.Mod) {
      n.r.mods <- length(strsplit(Reverse.Mods, ", ")[[1]])
    } else { 
      n.r.mods <- 1
    }
    
    div(
      pickerInput(
        inputId = "reaction_mass_action_wReg_reverisble_edit",
        label = "Reversability?",
        choices = c("Reversible" = "both_directions",
                    "Irreversible" = 'forward_only'),
        choicesOpt =
          list(icon = c(
            "glyphicon glyphicon-resize-horizontal",
            "glyphicon glyphicon-arrow-right"
          )),
        selected = arrow_type
      ),
      hr(),
      prettyCheckbox(inputId = "CB_MAwR_chem_modifier_forward_edit",
                     label = "Add Forward Regulator(s)",
                     value = Use.Forward.Mod),
      conditionalPanel(
        condition = "input.CB_MAwR_chem_modifier_forward_edit",
        numericInput(
          inputId = "NI_MAwR_n_forward_regulators_edit",
          label = "# of Forward Regulators",
          value = n.f.mods,
          min = 1,
          step = 1
        )
      ),
      conditionalPanel(
        condition = "input.reaction_mass_action_wReg_reverisble_edit ==
                                                            'both_directions'",
        prettyCheckbox(
          inputId = "CB_MAwR_chem_modifier_reverse_edit",
          label = "Add Reverse Regulator(s)",
          value = Use.Reverse.Mod
        ),
        conditionalPanel(
          condition =
            "input.CB_MAwR_chem_modifier_reverse_edit",
          numericInput(
            inputId =
              "NI_MAwR_n_reverse_regulators_edit",
            label = "# of Reverse Regulators",
            value = n.r.mods,
            min = 1,
            step = 1
          )
        )
      )
    )
    
  }
  else if (eqn.reaction.law == "synthesis") {
    syn <- rv.REACTIONS$synthesis[[eqn.ID]]
    
    ID               <- syn$ID
    law              <- syn$Reaction.Law
    VarSyn           <- syn$VarSyn
    VarSyn.id        <- syn$VarSyn.id
    Rate.Constant    <- syn$Rate.Constant
    Rate.Constant.id <- syn$Rate.Constant.id
    Factor           <- syn$Factor
    Factor.id        <- syn$Factor.id
    
    if (is.na(Factor)) {use.factor <- FALSE} else {use.factor <- TRUE}
    
    div(
      prettyCheckbox(
            inputId = "CB_synthesis_factor_checkbox_edit",
            label = "Factor Driving Synthesis?",
            value = use.factor
          )
    )
  }
  else if (eqn.reaction.law == "degradation_rate") {
    degInfo   <- rv.REACTIONS$degradation.by.rate[[eqn.ID]]
    
    ID         <- degInfo$ID
    law        <- degInfo$Reaction.Law
    VarDeg     <- degInfo$VarDeg
    VarDeg.id  <- degInfo$VarDeg.id
    ConcDep    <- degInfo$ConcDep
    RC         <- degInfo$Rate.Constant
    RC.id      <- degInfo$Rate.Constant.id
    Product    <- degInfo$Products
    Product.id <- degInfo$Products.id
    
    prod.exists <- ifelse(is.na(Product), FALSE, TRUE)
    if (prod.exists) {
      num.prods <- length(strsplit(Product, ", ")[[1]])
    }
    
    div(
      prettyCheckbox(
        inputId = "CB_degradation_rate_toProducts_edit",
        label = "Degrade Into Products?",
        value = prod.exists
      ),
      conditionalPanel(
        condition = "input.CB_degradation_rate_toProducts_edit",
        numericInput(
          inputId = "NI_degradation_rate_num_products_edit",
          label = "Number of Products",
          value = num.prods,
          min = 1,
          step = 1
        )
      )
    )
  }
  else if (eqn.reaction.law == "degradation_by_enzyme") {
    degInfo   <- rv.REACTIONS$degradation.by.enzyme[[eqn.ID]]
    
    ID         <- degInfo$ID
    law        <- degInfo$Reaction.Law
    VarDeg     <- degInfo$VarDeg
    VarDeg.id  <- degInfo$VarDeg.id
    RC         <- degInfo$Rate.Constant
    RC.id      <- degInfo$Rate.Constant.id
    UseVmax    <- degInfo$UseVmax
    Km         <- degInfo$Km
    Km.id      <- degInfo$Km.id
    Vmax       <- degInfo$Vmax
    Vmax.id    <- degInfo$Vmax.id
    Enzyme     <- degInfo$Enzyme
    Enzyme.id  <- degInfo$Enzyme.id
    kcat       <- degInfo$kcat
    kcat.id    <- degInfo$kcat.id
    Product    <- degInfo$Products
    Product.id <- degInfo$Products.id
    
    prod.exists <- ifelse(is.na(Product), FALSE, TRUE)
    if (prod.exists) {
      num.prods <- length(strsplit(Product, ", ")[[1]])
    }
    
    div(
      prettyCheckbox(
        inputId = "CB_degradation_enzyme_toProducts_edit",
        label = "Degrade Into Products?",
        value = prod.exists
      ),
      conditionalPanel(
        condition = "input.CB_degradation_enzyme_toProducts_edit",
        numericInput(
          inputId = "NI_degradation_enzyme_num_products",
          label = "Number of Products",
          value = num.prods,
          min = 1,
          step = 1
        )
      ),
      hr(),
      prettyCheckbox(inputId = "CB_degradation_enzyme_useVmax_edit",
                     label = "Use Vmax",
                     value = UseVmax)
    )

  } 
  else if (eqn.reaction.law == "michaelis_menten") {
    Info   <- rv.REACTIONS$michaelisMenten[[eqn.ID]]
    
    ID            <- Info$ID
    law           <- Info$Reaction.Law
    Substrate     <- Info$Substrate
    Substrate.id  <- Info$Substrate.id
    Product       <- Info$Products
    Product.id    <- Info$Products.id
    UseVmax       <- Info$UseVmax
    Km            <- Info$Km
    Km.id         <- Info$Km.id
    Vmax          <- Info$Vmax
    Vmax.id       <- Info$Vmax.id
    Enzyme        <- Info$Enzyme
    Enzyme.id     <- Info$Enzyme.id
    kcat          <- Info$kcat
    kcat.id       <- Info$kcat.id
    
    div (
      prettyCheckbox(
        inputId = "CB_michaelis_menten_useVmax",
        label = "Use Vmax",
        value = FALSE
      ) 
    )
  }
})


output$eqnCreate_edit_rending_mainbar <- renderUI({
  eqn.num     <- as.numeric(input$eqnCreate_edit_select_equation)
  eqn.row     <- rv.REACTIONS$reactions[[eqn.num]]
  
  # Unpack Equation Information
  eqn.ID               <- eqn.row$ID            
  eqn.display.type     <- eqn.row$Eqn.Display.Type 
  eqn.reaction.law     <- eqn.row$Reaction.Law    
  eqn.species          <- eqn.row$Species          
  eqn.reactants        <- eqn.row$Reactants        
  eqn.products         <- eqn.row$Products         
  eqn.Modifiers        <- eqn.row$Modifiers  
  eqn.parameters       <- eqn.row$Parameters       
  eqn.compartment      <- eqn.row$Compartment      
  eqn.description      <- eqn.row$Description      
  eqn.species.id       <- eqn.row$Species.id      
  eqn.reactants.id     <- eqn.row$Reactants.id     
  eqn.products.id      <- eqn.row$Products.id      
  eqn.modifiers.id     <- eqn.row$Modifiers.id     
  eqn.parameters.id    <- eqn.row$Parameters.id   
  eqn.compartment.id   <- eqn.row$Compartment.id   
  eqn.equation.text    <- eqn.row$Equation.Text    
  eqn.equation.latex   <- eqn.row$Equation.Latex   
  eqn.equation.mathjax <- eqn.row$Equation.MathJax 
  eqn.string.rate.law  <- eqn.row$String.Rate.Law  
  eqn.pretty.rate.law  <- eqn.row$Pretty.Rate.Law  
  eqn.latex.rate.law   <- eqn.row$Latex.Rate.Law   
  eqn.mathjax.rate.law <- eqn.row$MathJax.Rate.Law 
  eqn.mathml.rate.law  <- eqn.row$MathMl.Rate.Law 
  eqn.reversible       <- eqn.row$Reversible  
  
  if (eqn.reaction.law == "mass_action") {
    # Extract chem information
    chemInfo <- rv.REACTIONS$massAction[[eqn.ID]]
    
    ID               <- chemInfo$ID
    law              <- chemInfo$Reaction.Law
    r.stoichiometry  <- str_split(chemInfo$r.stoichiometry, ", ")[[1]]
    Reactants        <- str_split(chemInfo$Reactants,  ", ")[[1]]
    p.stoichiometry  <- str_split(chemInfo$p.stoichiometry, ", ")[[1]]
    Products         <- str_split(chemInfo$Products,  ", ")[[1]] 
    Reactants.id     <- str_split(chemInfo$Reactants.id, ", ")[[1]]
    Products.id      <- str_split(chemInfo$Products.id, ", ")[[1]]
    arrow_type       <- chemInfo$Reversible
    kf               <- chemInfo$kf
    kr               <- chemInfo$kr
    kf.id            <- chemInfo$kf.id
    kr.id            <- chemInfo$kr.id

    number.reactants <- length(Reactants)
    number.products  <- length(Products)
    
    # Get parameter values
    kf.value <- rv.PARAMETERS$parameters[[kf.id]]$Value
    if (!is.na(kr.id)) {
      kr.value <- rv.PARAMETERS$parameters[[kr.id]]$Value
    } else {
      kr.value <- 0
    }
    
    
    # Render Ui
    div(
      fluidRow(
        column(
          width = 3, 
          numericInput(
            inputId = "NI_mass_action_num_reactants_edit",
            label = "Number of Reactants",
            value = number.reactants,
            min = 1,
            step = 1)
        ), 
        column(
          width = 3,
          numericInput(
            inputId = "NI_mass_action_num_products_edit",
            label = "Number of Products",
            value = number.products,
            min = 1,
            step = 1
          )
        )
      ),
      fluidRow(
        column(
          style = "border-right: 1px solid #e5e5e5; padding-right:20px",
          width = 4,
          lapply(seq(number.reactants), function(i){
            div(
              HTML(paste0("<b>Reactant ", as.character(i), "</b>")),
              splitLayout(
                numericInput(
                  inputId = paste0("NI_MA_r_stoichiometry_edit_", 
                                   as.character(i)),
                  label = NULL,
                  value = as.numeric(r.stoichiometry[i]),
                  min = 1,
                  step = 1),
                pickerInput(
                  inputId = paste0("PI_MA_reactant_edit_", as.character(i)),
                  label = NULL,
                  choices = sort(rv.SPECIES$df.by.compartment$Name),
                  selected = Reactants[i],
                  options = pickerOptions(liveSearch = TRUE,
                                          liveSearchStyle = "startsWith",
                                          dropupAuto = FALSE)
                ),
                cellWidths = c("25%", "75%")
              )
            )
          })
        ), #end Column
        column(
          style = "border-right: 1px solid #e5e5e5; 
                 padding-right: 20px; 
                 padding-left: 20px;",
          width = 4,
          lapply(seq(number.products), function(i){
            div(
              HTML(paste0("<b>Product ", as.character(i), "</b>")),
              splitLayout(
                numericInput(
                  inputId = paste0("NI_MA_p_stoichiometry_edit", 
                                   as.character(i)),
                  label = NULL,
                  value = p.stoichiometry[i],
                  min = 1,
                  step = 1),
                pickerInput(
                  inputId = paste0("PI_MA_product_edit", 
                                   as.character(i)),
                  label = NULL,
                  choices = sort(rv.SPECIES$df.by.compartment$Name),
                  selected = Products[i],
                  options = pickerOptions(liveSearch = TRUE,
                                          liveSearchStyle = "startsWith",
                                          dropupAuto = FALSE)
                ),
                cellWidths = c("25%", "75%")
              )
            )
          })
        ), #end Column
        column(
          style = "padding-left: 20px; padding-right: 0px",
          width = 3,
          textInput(
            inputId = "TI_mass_action_forward_k_edit",
            label = "Forward Rate Constant",
            value = kf
          ),
          conditionalPanel(
            condition = 
              "input.PI_mass_action_reverisble_option_edit== 'both_directions'",
            textInput(
              inputId = "TI_mass_action_reverse_k_edit",
              label = "Reverse Rate Constant",
              value = kr
            )
          )
        ), #end column
        column(
          style = "padding-left: 0px",
          width = 1,
          textInput(
            inputId = "TI_mass_action_forward_k_value_edit",
            label = "Value",
            value = kf.value
          ),
          conditionalPanel(
            condition = 
              "input.PI_mass_action_reverisble_option_edit== 'both_directions'",
            textInput(
              inputId = "TI_mass_action_reverse_k_value_edit",
              label = "Value",
              value = kr.value)
          )
        ),
        tags$head(tags$style("#TI_mass_action_forward_k_edit
                             {margin-top: -7px;}")),
        tags$head(tags$style("#TI_mass_action_reverse_k_edit
                             {margin-top: -7px;}")),
        tags$head(
          tags$style("#TI_mass_action_forward_k_value_edit
                     {margin-top: -7px;}")),
        tags$head(
          tags$style("#TI_mass_action_reverse_k_value_edit
                     {margin-top: -7px;}"))
      )
      
    )
  }
  else if (eqn.reaction.law == "mass_action_w_reg") {
    
    chemInfo <- rv.REACTIONS$massActionwReg[[eqn.ID]]
    
    ID               <- chemInfo$ID
    law              <- chemInfo$Reaction.Law
    r.stoichiometry  <- str_split(chemInfo$r.stoichiometry, ", ")[[1]]
    Reactants        <- str_split(chemInfo$Reactants,  ", ")[[1]]
    p.stoichiometry  <- str_split(chemInfo$p.stoichiometry, ", ")[[1]]
    Products         <- str_split(chemInfo$Products,  ", ")[[1]] 
    Reactants.id     <- str_split(chemInfo$Reactants.id, ", ")[[1]]
    Products.id      <- str_split(chemInfo$Products.id, ", ")[[1]]
    arrow_type       <- chemInfo$Reversible
    kf               <- chemInfo$kf
    kr               <- chemInfo$kr
    kf.id            <- chemInfo$kf.id
    kr.id            <- chemInfo$kr.id
    Use.Forward.Mod  <- chemInfo$Use.Forward.Mod
    Forward.Mods     <- str_split(chemInfo$Forward.Mods, ", ")[[1]]
    Forward.Mods.id  <- str_split(chemInfo$Forward.Mods.id, ", ")[[1]]
    Forward.Pars     <- str_split(chemInfo$Forward.Pars, ", ")[[1]]
    Forward.Pars.id  <- str_split(chemInfo$Forward.Pars.id, ", ")[[1]]
    Use.Reverse.Mod  <- chemInfo$Use.Reverse.Mod
    Reverse.Mods     <- str_split(chemInfo$Reverse.Mods, ", ")[[1]]
    Reverse.Mods.id  <- str_split(chemInfo$Reverse.Mods.id, ", ")[[1]]
    Reverse.Pars     <- str_split(chemInfo$Reverse.Pars, ", ")[[1]]
    Reverse.Pars.id  <- str_split(chemInfo$Reverse.Pars.id, ", ")[[1]]
    
    # Number of forward mods
    if (Use.Forward.Mod) {
      n.f.mods <- length(strsplit(Forward.Mods, ", ")[[1]])
    } else { 
      n.f.mods <- 1
    }
    
    # Number of reverse mods
    if (Use.Reverse.Mod) {
      n.r.mods <- length(strsplit(Reverse.Mods, ", ")[[1]])
    } else { 
      n.r.mods <- 1
    }
    
    number.reactants <- length(Reactants)
    number.products  <- length(Products)
    
    # Get parameter values
    kf.value <- rv.PARAMETERS$parameters[[kf.id]]$Value
    if (!is.na(kr.id)) {
      kr.value <- rv.PARAMETERS$parameters[[kr.id]]$Value
    } else {
      kr.value <- 0
    }
    
    div(
      fluidRow(
        column(
          style = "border-right: 1px solid #e5e5e5; padding-right:20px",
          width = 4,
          lapply(seq(number.reactants), function(i){
            div(
              HTML(paste0("<b>Reactant ", as.character(i), "</b>")),
              splitLayout(
                numericInput(
                  inputId = paste0("NI_MAwR_r_stoichiometry_edit_", 
                                   as.character(i)),
                  label = NULL,
                  value = r.stoichiometry[i],
                  min = 1,
                  step = 1),
                pickerInput(
                  inputId = paste0("PI_MAwR_reactant_edit_", as.character(i)),
                  label = NULL,
                  choices = sort(rv.SPECIES$df.by.compartment$Name),
                  selected = Reactants[i],
                  options = pickerOptions(liveSearch = TRUE,
                                          liveSearchStyle = "startsWith",
                                          dropupAuto = FALSE)
                ),
                cellWidths = c("25%", "75%")
              )
            )
          })
        ), #end Column
        column(
          style = "border-right: 1px solid #e5e5e5; 
               padding-right: 20px; 
               padding-left: 20px;",
          width = 4,
          lapply(seq(number.products), function(i){
            div(
              HTML(paste0("<b>Product ", as.character(i), "</b>")),
              splitLayout(
                numericInput(
                  inputId = paste0("NI_MAwR_p_stoichiometry_edit_", 
                                   as.character(i)),
                  label = NULL,
                  value = p.stoichiometry[i],
                  min = 1,
                  step = 1),
                pickerInput(
                  inputId = paste0("PI_MAwR_product_edit_", as.character(i)),
                  label = NULL,
                  choices = sort(rv.SPECIES$df.by.compartment$Name),
                  selected = Products[i],
                  options = pickerOptions(liveSearch = TRUE,
                                          liveSearchStyle = "startsWith",
                                          dropupAuto = FALSE)
                ),
                cellWidths = c("25%", "75%")
              )
            )
          })
        ), #end Column
        column(
          style = "padding-left: 20px; padding-right: 0px",
          width = 3,
          conditionalPanel(
            condition = "!input.CB_MAwR_chem_modifier_forward_edit",
            textInput(
              inputId = "TI_MAwR_forward_k_edit",
              label = "Forward Rate Constant",
              value = kf
              )
          ),
          conditionalPanel(
            condition = 
             "input.reaction_mass_action_wReg_reverisble_edit == 
                                                           'both_directions' && 
             !input.CB_MAwR_chem_modifier_reverse_edit",
            textInput(
              inputId = "TI_MAwR_reverse_k_edit",
              label = "Reverse Rate Constant",
              value = kr
              )
          )
        ),
        column(
          style = "padding-left: 0px",
          width = 1,
          conditionalPanel(
            condition = "!input.CB_MAwR_chem_modifier_forward_edit",
            textInput(
              inputId = "TI_MAwR_forward_k_value_edit",
              label = "Value",
              value = kf.value
            )
          ),
          conditionalPanel(
            condition = 
              "input.reaction_mass_action_wReg_reverisble_edit == 
            'both_directions' && 
             !input.CB_MAwR_chem_modifier_reverse_edit",
            textInput(
              inputId = "TI_MAwR_reverse_k_value_edit",
              label = "Value",
              value = kr.value)
          )
        ),
        tags$head(
          tags$style("#TI_MAwR_forward_k_value_edit {margin-top: -7px;}")),
        tags$head(
          tags$style("#TI_MAwR_reverse_k_value_edit {margin-top: -7px;}")),
        tags$head(
          tags$style("#TI_MAwR_reverse_k_edit {margin-top: -7px;}")),
        tags$head(
          tags$style("#TI_MAwR_forward_k_edit {margin-top: -7px;}"))
      ), #end fluidRow`
      conditionalPanel(
        condition = "input.CB_MAwR_chem_modifier_forward_edit || 
                     input.CB_MAwR_chem_modifier_reverse_edit",
        hr()
      ),
      fluidRow(
        column(
          width = 3,
          conditionalPanel(
            condition = "input.CB_MAwR_chem_modifier_forward_edit",
            lapply(seq(n.f.mods), function(i){
              pickerInput(
                inputId = paste0("PI_MAwR_forward_regulator_edit_", 
                                 as.character(i)),
                label = paste0("Forward Regulator ", as.character(i)),
                choices = sort(rv.SPECIES$df.by.compartment$Name),
                selected = Forward.Mods[i],
                options = pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = "startsWith"))
            })
          )
        ),
        column(
          width = 3,
          conditionalPanel(
            condition = "input.CB_MAwR_chem_modifier_forward_edit",
            lapply(seq(n.f.mods), function(i){
              textInput(
                inputId = paste0("TI_MAwR_forward_regulator_RC_edit_", 
                                 as.character(i)),
                label = "Rate Constant",
                value = Forward.Pars[i]
              )
            })
          )
        ),
        column(
          width = 3,
          conditionalPanel(
            condition = "input.CB_MAwR_chem_modifier_forward_edit",
            lapply(seq(n.f.mods), function(i){
              textInput(
                inputId = paste0("TI_MAwR_forward_regulator_RC_value_edit_",
                                 as.character(i)),
                label = "Value",
                value = rv.PARAMETERS$parameters[[Forward.Pars.id[i]]]$Value
              )
            })
          )
        )
      ),
      fluidRow(
        column(
          width = 3,
          conditionalPanel(
            condition = "input.CB_MAwR_chem_modifier_reverse_edit",
            lapply(seq(n.r.mods), function(i){
              pickerInput(
                inputId = paste0("PI_MAwR_reverse_regulator_edit_", 
                                 as.character(i)),
                label = paste0("Reverse Regulator ", as.character(i)),
                choices = sort(rv.SPECIES$df.by.compartment$Name),
                selected = Reverse.Mods[i],
                options = pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = "startsWith")
              )
            })
          )
        ),
        column(
          width = 3,
          conditionalPanel(
            condition = "input.CB_MAwR_chem_modifier_reverse",
            lapply(seq(n.r.mods), function(i){
              textInput(
                inputId = paste0("TI_MAwR_reverse_regulator_RC_", 
                                 as.character(i)),
                label = "Rate Constant",
                value = Reverse.Pars[i]
              )
            })
          )
        ),
        column(
          width = 3,
          conditionalPanel(
            condition = "input.CB_MAwR_chem_modifier_reverse",
            lapply(seq(n.r.mods), function(i){
              textInput(
                inputId = paste0("TI_MAwR_reverse_regulator_RC_value_",
                                 as.character(i)),
                label = "Value",
                value = rv.PARAMETERS$parameters[[Reverse.Pars.id[i]]]$Value
              )
            })
          )
        )
      )
    )
  }
  else if (eqn.reaction.law == "synthesis") {
    
    syn <- rv.REACTIONS$synthesis[[eqn.ID]]
    
    ID               <- syn$ID
    law              <- syn$Reaction.Law
    VarSyn           <- syn$VarSyn
    VarSyn.id        <- syn$VarSyn.id
    Rate.Constant    <- syn$Rate.Constant
    Rate.Constant.id <- syn$Rate.Constant.id
    Factor           <- syn$Factor
    Factor.id        <- syn$Factor.id
    
    if (is.na(Factor)) {use.factor <- FALSE} else {use.factor <- TRUE}
    
    div(
      conditionalPanel(
        condition = "!input.CB_synthesis_factor_checkbox_edit",
        fluidRow(
          column(
            width = 3,
            pickerInput(
              inputId = "PI_synthesis_rate_var_edit",
              label   = "Species to synthesize",
              choices = sort(rv.SPECIES$df.by.compartment$Name),
              selected = VarSyn,
              options = pickerOptions(liveSearch = TRUE,
                                      liveSearchStyle = "startsWith") 
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            textInput(
              inputId = "TI_synthesis_rate_RC_edit",
              label = "Rate Constant",
              value = Rate.Constant
              
            )
          ),
          column(
            width = 3, 
            textInput(
              inputId = "TI_synthesis_rate_RC_value_edit",
              label = "Value",
              value = rv.PARAMETERS$parameters[[Rate.Constant.id]]$Value
            )
          )
        )
      ), 
      conditionalPanel(
        condition = "input.CB_synthesis_factor_checkbox_edit",
        fluidRow(
          column(
            width = 3,
            pickerInput(
              inputId = "PI_synthesis_byFactor_var_edit",
              label   = "Species to synthesize",
              choices = sort(rv.SPECIES$df.by.compartment$Name),
              selected = VarSyn,
              options = pickerOptions(liveSearch = TRUE,
                                      liveSearchStyle = "startsWith") 
            )
          ),
          column(
            width = 3, 
            pickerInput(
              inputId = "PI_synthesis_byFactor_factor_edit",
              label = "Factor causing synthesis",
              choices = sort(rv.SPECIES$df.by.compartment$Name),
              selected = Factor
            )
          )
        ),
        fluidRow(
          column(
            width = 3, 
            textInput(
              inputId = "TI_synthesis_byFactor_RC",
              label = "Rate Constant",
              value = Rate.Constant
            )
          ),
          column(
            width = 3, 
            textInput(
              inputId = "TI_synthesis_byFactor_RC_value",
              label = "Value",
              value = rv.PARAMETERS$parameters[[Rate.Constant.id]]$Value
            )
          )
        )
      )
    )
  }
  else if (eqn.reaction.law == "degradation_rate") {
    degInfo   <- rv.REACTIONS$degradation.by.rate[[eqn.ID]]
    
    ID         <- degInfo$ID
    law        <- degInfo$Reaction.Law
    VarDeg     <- degInfo$VarDeg
    VarDeg.id  <- degInfo$VarDeg.id
    ConcDep    <- degInfo$ConcDep
    RC         <- degInfo$Rate.Constant
    RC.id      <- degInfo$Rate.Constant.id
    Product    <- degInfo$Products
    Product.id <- degInfo$Products.id
    
    prod.exists <- ifelse(is.na(Product), FALSE, TRUE)
    if (prod.exists) {
      num.prods <- length(strsplit(Product, ", ")[[1]])
    }
    
    div(
      fluidRow(
        column(
          width = 4,
          pickerInput(
            inputId = "PI_degradation_rate_species_edit",
            label   = "Species to degrade",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = VarDeg,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith") 
          )
        ),
        column(
          width = 4,
          conditionalPanel(
            condition = "input.CB_degradation_rate_toProducts_edit",
            lapply(
              seq(input$NI_degradation_rate_num_products_edit), function(i){
                pickerInput(
                  inputId = paste0("PI_degradation_rate_product_edit_", 
                                   as.character(i)),
                  label = paste0("Product ", as.character(i)),
                  choices = sort(rv.SPECIES$df.by.compartment$Name),
                  selected = Product[i],
                  options = pickerOptions(liveSearch = TRUE,
                                          liveSearchStyle = "startsWith"))
              }
            )
          )
        )
      ),
      hr(),
      fluidRow(
        column(
          width = 8,
          splitLayout(
            textInput(
              inputId = "TI_degradation_rate_RC_edit",
              label = "Rate Constant",
              value = RC
            ),
            textInput(
              inputId = "TI_degradation_rate_RC_value_edit",
              label = "Value",
              value = rv.PARAMETERS$parameters[[RC.id]]$Value
            ),
            div(
              style = "padding-top:38px; padding-left:15px;",
              checkboxInput(
                inputId = "CB_degradation_rate_conc_dependent_edit",
                label = "Concentration Dependent",
                value = ConcDep)
            )
          )
        )  
      )
    )
  }
  else if (eqn.reaction.law == "degradation_by_enzyme") {
    degInfo   <- rv.REACTIONS$degradation.by.enzyme[[eqn.ID]]
    
    ID         <- degInfo$ID
    law        <- degInfo$Reaction.Law
    VarDeg     <- degInfo$VarDeg
    VarDeg.id  <- degInfo$VarDeg.id
    RC         <- degInfo$Rate.Constant
    RC.id      <- degInfo$Rate.Constant.id
    UseVmax    <- degInfo$UseVmax
    Km         <- degInfo$Km
    Km.id      <- degInfo$Km.id
    Vmax       <- degInfo$Vmax
    Vmax.id    <- degInfo$Vmax.id
    Enzyme     <- degInfo$Enzyme
    Enzyme.id  <- degInfo$Enzyme.id
    kcat       <- degInfo$kcat
    kcat.id    <- degInfo$kcat.id
    Product    <- degInfo$Products
    Product.id <- degInfo$Products.id
    
    prod.exists <- ifelse(is.na(Product), FALSE, TRUE)
    if (prod.exists) {
      num.prods <- length(strsplit(Product, ", ")[[1]])
    }
    
    div(
      fluidRow(
        column(
          width = 3,
          pickerInput(
            inputId = "PI_degradation_enzyme_species_edit",
            label   = "Species to degrade",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = VarDeg,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith") 
          ),
          conditionalPanel(
            condition = "!input.CB_degradation_enzyme_useVmax_edit",
            pickerInput(
              inputId = "PI_degradation_enzyme_enzyme_edit",
              label = "Enzyme",
              choices = sort(rv.SPECIES$df.by.compartment$Name),
              selected = Enzyme
            )
          )
        ),
        column(
          width = 3,
          offset = 1,
          conditionalPanel(
            condition = "input.CB_degradation_enzyme_toProducts_edit",
            lapply(
              seq(input$NI_degradation_enzyme_num_products_edit), function(i){
                pickerInput(
                  inputId = paste0("PI_degradation_enzyme_product_edit_", 
                                   as.character(i)),
                  label = paste0("Product ", as.character(i)),
                  choices = sort(rv.SPECIES$df.by.compartment$Name),
                  selected = Product[i],
                  options = pickerOptions(liveSearch = TRUE,
                                          liveSearchStyle = "startsWith"))
              }
            )
          )
        )
      ),
      hr(),
      conditionalPanel(
        condition = "!input.CB_degradation_enzyme_useVmax_edit",
        fluidRow(
          column(
            style = "padding-right: 0px;",
            width = 3,
            textInput(
              inputId = "TI_degradation_enzyme_kcat_edit",
              label = "kcat",
              value = kcat
            )
          ),
          column(
            style = "padding-left: 0px;",
            width = 3,
            textInput(
              inputId = "TI_degradation_enzyme_kcat_value_edit",
              label = "Value",
              value = rv.PARAMETERS$parameters[[kcat.id]]$Value
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.CB_degradation_enzyme_useVmax_edit",
        fluidRow(
          column(
            style = "padding-right: 0px;",
            width = 3,
            textInput(
              inputId = "TI_degradation_enzyme_Vmax_edit",
              label = "Vmax",
              value = Vmax
            )
          ),
          column(
            style = "padding-left: 0px;",
            width = 3,
            textInput(
              inputId = "TI_degradation_enzyme_Vmax_value_edit",
              label = "Value",
              value = rv.PARAMETERS$parameters[[Vmax.id]]$Value
            )
          )
        )
      ),
      fluidRow(
        column(
          style = "padding-right: 0px;",
          width = 3,
          textInput(
            inputId = "TI_degradation_enzyme_Km_edit",
            label = "Km",
            value = Km
          )
        ),
        column(
          style = "padding-left: 0px;",
          width = 3,
          textInput(
            inputId = "TI_degradation_enzyme_Km_value_edit",
            label = "Value",
            value = rv.PARAMETERS$parameters[[Km.id]]$Value
          )
        )
      )
    )
  }
  else if (eqn.reaction.law == "michaelis_menten") {
    
    Info   <- rv.REACTIONS$michaelisMenten[[eqn.ID]]
    
    ID            <- Info$ID
    law           <- Info$Reaction.Law
    Substrate     <- Info$Substrate
    Substrate.id  <- Info$Substrate.id
    Product       <- Info$Products
    Product.id    <- Info$Products.id
    UseVmax       <- Info$UseVmax
    Km            <- Info$Km
    Km.id         <- Info$Km.id
    Vmax          <- Info$Vmax
    Vmax.id       <- Info$Vmax.id
    Enzyme        <- Info$Enzyme
    Enzyme.id     <- Info$Enzyme.id
    kcat          <- Info$kcat
    kcat.id       <- Info$kcat.id
    
    div(
      fluidRow(
        column(
          width = 3,
          pickerInput(
            inputId = "PI_michaelis_menten_substrate_edit",
            label = "Substrate",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = Substrate,
            options = pickerOptions(
              liveSearch = TRUE,
              liveSearchStyle = "startsWith",
              dropupAuto = FALSE
            )
          )
        ),
        column(
          width = 3,
          offset = 1,
          pickerInput(
            inputId = "PI_michaelis_menten_product_edit",
            label = "Product",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = Product,
            options = pickerOptions(
              liveSearch = TRUE,
              liveSearchStyle = "startsWith",
              dropupAuto = FALSE
            )
          )
        ),
        column(
          width = 3, 
          offset = 1,
          conditionalPanel(
            condition = "!input.CB_michaelis_menten_useVmax_edit",
            pickerInput(
              inputId = "PI_michaelis_menten_enzyme_edit",
              label = "Enzyme",
              choices = sort(rv.SPECIES$df.by.compartment$Name),
              selected = Enzyme,
              options = pickerOptions(liveSearch = TRUE,
                                      liveSearchStyle = "startsWith")
            )
          )
        )
      ),
      hr(),
      fluidRow(
        column(
          style = "padding-right: 0px",
          width = 3,
          textInput(
            inputId = "TI_michaelis_menten_Km_edit",
            label = "Km",
            value = Km
          )
        ),
        column(
          style = "padding-left: 0px",
          width = 3,
          textInput(
            inputId = "TI_michaelis_menten_Km_value_edit",
            label = "Value",
            value = rv.PARAMETERS$parameters[[Km.id]]$Value
          )
        )
      ),
      fluidRow(
        column(
          width = 3,
          style = "padding-right: 0px",
          conditionalPanel(
            condition = "input.CB_michaelis_menten_useVmax_edit",
            textInput(
              inputId = "TI_michaelis_menten_vmax_edit",
              label = "Vmax",
              value = Vmax
            )
          ),
          conditionalPanel(
            condition = "!input.CB_michaelis_menten_useVmax_edit",
            textInput(
              inputId = "TI_michaelis_menten_kcat_edit",
              label = "kcat",
              value = kcat
            )
          )
        ),
        column(
          width = 3,
          style = "padding-left: 0px",
          conditionalPanel(
            condition = "input.CB_michaelis_menten_useVmax_edit",
            textInput(
              inputId = "TI_michaelis_menten_vmax_value_edit",
              label = "Value",
              value = rv.PARAMETERS$parameters[[Vmax.id]]$Value
            )
          ),
          conditionalPanel(
            condition = "!input.CB_michaelis_menten_useVmax_edit",
            textInput(
              inputId = "TI_michaelis_menten_kcat_value_edit",
              label = "Value",
              value = rv.PARAMETERS$parameters[[kcat.id]]$Value
            )
          )
        )
      )
    )
  }
})
# Editing Equations RenderUI ---------------------------------------------------

## Mass Action -----------------------------------------------------------------
output$eqnCreate_equationBuilder_mass_action_edit <- renderUI({
  eqn.num     <- as.numeric(input$eqnCreate_edit_select_equation)
  eqn.row     <- rv.REACTIONS$reactions[[eqn.num]]
  
  # Unpack Equation Information
  eqn.ID               <- eqn.row$ID            
  eqn.display.type     <- eqn.row$Eqn.Display.Type 
  eqn.reaction.law     <- eqn.row$Reaction.Law    
  eqn.species          <- eqn.row$Species          
  eqn.reactants        <- eqn.row$Reactants        
  eqn.products         <- eqn.row$Products         
  eqn.Modifiers        <- eqn.row$Modifiers  
  eqn.parameters       <- eqn.row$Parameters       
  eqn.compartment      <- eqn.row$Compartment      
  eqn.description      <- eqn.row$Description      
  eqn.species.id       <- eqn.row$Species.id      
  eqn.reactants.id     <- eqn.row$Reactants.id     
  eqn.products.id      <- eqn.row$Products.id      
  eqn.modifiers.id     <- eqn.row$Modifiers.id     
  eqn.parameters.id    <- eqn.row$Parameters.id   
  eqn.compartment.id   <- eqn.row$Compartment.id   
  eqn.equation.text    <- eqn.row$Equation.Text    
  eqn.equation.latex   <- eqn.row$Equation.Latex   
  eqn.equation.mathjax <- eqn.row$Equation.MathJax 
  eqn.string.rate.law  <- eqn.row$String.Rate.Law  
  eqn.pretty.rate.law  <- eqn.row$Pretty.Rate.Law  
  eqn.latex.rate.law   <- eqn.row$Latex.Rate.Law   
  eqn.mathjax.rate.law <- eqn.row$MathJax.Rate.Law 
  eqn.mathml.rate.law  <- eqn.row$MathMl.Rate.Law 
  eqn.reversible       <- eqn.row$Reversible  
  
  # Extract chem information
  chemInfo <- rv.REACTIONS$massAction[[eqn.ID]]
  
  ID               <- chemInfo$ID
  law              <- chemInfo$Reaction.Law
  r.stoichiometry  <- str_split(chemInfo$r.stoichiometry, ", ")[[1]]
  Reactants        <- str_split(chemInfo$Reactants,  ", ")[[1]]
  p.stoichiometry  <- str_split(chemInfo$p.stoichiometry, ", ")[[1]]
  Products         <- str_split(chemInfo$Products,  ", ")[[1]] 
  Reactants.id     <- str_split(chemInfo$Reactants.id, ", ")[[1]]
  Products.id      <- str_split(chemInfo$Products.id, ", ")[[1]]
  arrow_type       <- chemInfo$Reversible
  kf               <- chemInfo$kf
  kr               <- chemInfo$kr
  kf.id            <- chemInfo$kf.id
  kr.id            <- chemInfo$kr.id
  
  
  number.reactants <- as.numeric(input$NI_mass_action_num_reactants)
  number.products  <- as.numeric(input$NI_mass_action_num_products)
  
  div(
    fluidRow(
      column(
        style = "border-right: 1px solid #e5e5e5; padding-right:20px",
        width = 4,
        lapply(seq(number.reactants), function(i){
          div(
            HTML(paste0("<b>Reactant ", as.character(i), "</b>")),
            splitLayout(
              numericInput(
                inputId = paste0("NI_MA_r_stoichiometry_edit_", as.character(i)),
                label = NULL,
                value = 1,
                min = 1,
                step = 1),
              pickerInput(
                inputId = paste0("PI_MA_reactant_edit_", as.character(i)),
                label = NULL,
                choices = sort(rv.SPECIES$df.by.compartment$Name),
                options = pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = "startsWith",
                                        dropupAuto = FALSE)
              ),
              cellWidths = c("25%", "75%")
            )
          )
        })
      ), #end Column
      column(
        style = "border-right: 1px solid #e5e5e5; 
                 padding-right: 20px; 
                 padding-left: 20px;",
        width = 4,
        lapply(seq(number.products), function(i){
          div(
            HTML(paste0("<b>Product ", as.character(i), "</b>")),
            splitLayout(
              numericInput(
                inputId = paste0("NI_MA_p_stoichiometry_edit_", as.character(i)),
                label = NULL,
                value = 1,
                min = 1,
                step = 1),
              pickerInput(
                inputId = paste0("PI_MA_product_edit_", as.character(i)),
                label = NULL,
                choices = sort(rv.SPECIES$df.by.compartment$Name),
                options = pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = "startsWith",
                                        dropupAuto = FALSE)
              ),
              cellWidths = c("25%", "75%")
            )
          )
        })
      ), #end Column
      column(
        style = "padding-left: 20px; padding-right: 0px",
        width = 3,
        textInput(
          inputId = "TI_mass_action_forward_k_edit_",
          label = "Forward Rate Constant",
          value = paste0("k_f", 
                         as.character(rv.REACTIONS$reaction.id.counter + 1))
        ),
        tags$head(
          tags$style("#TI_mass_action_forward_k_edit {margin-top: -7px;}")),
        conditionalPanel(
          condition = 
            "input.PI_mass_action_reverisble_option_edit == 'both_directions'",
          textInput(
            inputId = "TI_mass_action_reverse_k_edit_",
            label = "Reverse Rate Constant",
            value = paste0("k_r", 
                           as.character(rv.REACTIONS$reaction.id.counter + 1))
          ),
          tags$head(
            tags$style("#TI_mass_action_reverse_k_edit_ {margin-top: -7px;}"))
        )
      ), #end column
      column(
        style = "padding-left: 0px",
        width = 1,
        textInput(
          inputId = "TI_mass_action_forward_k_value_edit",
          label = "Value",
          value = 0
        ),
        conditionalPanel(
          condition = 
            "input.PI_mass_action_reverisble_option_edit == 'both_directions'",
          textInput(
            inputId = "TI_mass_action_reverse_k_value_edit",
            label = "Value",
            value = 0)
        )
      ),
      tags$head(
        tags$style("#TI_mass_action_forward_k_value_edit {margin-top: -7px;}")),
      tags$head(
        tags$style("#TI_mass_action_reverse_k_value_edit {margin-top: -7px;}"))
    ) #end fluidRow`
  )
  
})
## Mass Action w Regulation ----------------------------------------------------
output$equationBuilder_mass_action_w_regulation_edit <- renderUI({
  
})

## Synthesis -------------------------------------------------------------------
output$equationBuilder_synthesis_edit <- renderUI({
  
})

## Degradation by Rate ---------------------------------------------------------
output$equationBuilder_degradation_rate_edit <- renderUI({
  
})

## Degradation by Enzyme -------------------------------------------------------
output$equationBuilder_degradation_by_enzyme_edit <- renderUI({
  
})

## Michealis Menten ------------------------------------------------------------
output$equationBuilder_michaelis_menten_edit <- renderUI({
  
})




# Equation Text UI Show ---------------------------------------------------------
# output$build_equation_edit <- renderUI({
#   withMathJax(equationBuilder_edit_mathJax())
# })

# Equation Builder Text Builder ------------------------------------------------
# equationBuilder_edit <- reactive({
#   if (input$eqnCreate_type_of_equation_edit == "chem_rxn") {
#     n.RHS = as.numeric(input$eqnCreate_num_of_eqn_RHS_edit)
#     n.LHS = as.numeric(input$eqnCreate_num_of_eqn_LHS_edit)
#     n.f.reg = as.numeric(input$eqn_options_chem_num_forward_regulators_edit)
#     n.r.reg = as.numeric(input$eqn_options_chem_num_reverse_regulators_edit)
# 
#     eqn_LHS <- ""
#     for (i in seq(n.LHS)) {
#       coef <- eval(parse(text = paste0("input$LHS_Coeff_edit", 
#                                        as.character(i))))
#       var <- eval(parse(text = paste0("input$LHS_Var_edit", 
#                                       as.character(i))))
#       if (coef != "1") {eqn_LHS <- paste0(eqn_LHS, coef, "*")}
#       if (i == as.numeric(n.LHS)) {eqn_LHS <- paste0(eqn_LHS, var)}
#       else{eqn_LHS <- paste0(eqn_LHS, var, " + ")}
#     }
# 
#     eqn_RHS <- ""
#     for (i in seq(n.RHS)) {
#       coef <- eval(parse(text = paste0("input$RHS_Coeff_edit", 
#                                        as.character(i))))
#       var <- eval(parse(text = paste0("input$RHS_Var_edit", 
#                                       as.character(i))))
#       if (coef != "1") {eqn_RHS <- paste0(eqn_RHS, coef, "*")}
#       if (i == as.numeric(n.RHS)) {eqn_RHS <- paste0(eqn_RHS, var)}
#       else{eqn_RHS <- paste0(eqn_RHS, var, " + ")}
#     }
# 
#     if (input$eqn_chem_forward_or_both_edit == "both_directions") {
#       arrow <- "<-->"
#       if (input$eqn_options_chem_modifier_forward_edit &&
#           input$eqn_options_chem_modifier_reverse_edit) {
#         #find regulators and add them together in form ([regulator/constant, regulator2/constant2, etc...])
#         forwardModifiers <- c()
#         for (i in seq(n.f.reg)) {
#           regulator <-
#             eval(parse(text = paste0(
#               "input$eqn_forward_regulator_edit",
#               as.character(i)
#             )))
#           
#           rateConstant <-
#             eval(parse(text = paste0(
#               "input$eqn_forward_rateConstant_edit",
#               as.character(i)
#             )))
#           
#           modifierExpression <- paste0(regulator, "/", rateConstant)
#           forwardModifiers <- c(forwardModifiers, modifierExpression)
#         }
#         forwardModifiers <- paste(forwardModifiers, collapse = ",")
# 
#         reverseModifiers <- c()
#         for (i in seq(n.r.reg)) {
#           regulator <-
#             eval(parse(text = paste0(
#               "input$eqn_reverse_regulator_edit",
#               as.character(i)
#             )))
#           
#           rateConstant <-
#             eval(parse(text = paste0(
#               "input$eqn_reverse_rateConstant_edit",
#               as.character(i)
#             )))
#           
#           modifierExpression <- paste0(regulator, "/", rateConstant)
#           reverseModifiers <- c(reverseModifiers, modifierExpression)
#         }
#         
#         reverseModifiers <- paste(reverseModifiers, collapse = ",")
# 
#         arrow <-
#           paste0("([",
#                  reverseModifiers,
#                  "])",
#                  arrow,
#                  "([",
#                  forwardModifiers ,
#                  "])")
#       }
#       else if (input$eqn_options_chem_modifier_forward_edit &&
#                !input$eqn_options_chem_modifier_reverse_edit) {
#         
#         forwardModifiers <- c()
#         
#         for (i in seq(n.f.reg)) {
#           regulator <-
#             eval(parse(text = paste0(
#               "input$eqn_forward_regulator_edit",
#               as.character(i)
#             )))
#           rateConstant <-
#             eval(parse(
#               text = paste0("input$eqn_forward_rateConstant_edit",
#                             as.character(i))
#             ))
#           modifierExpression <- paste0(regulator, "/", rateConstant)
#           forwardModifiers <-
#             c(forwardModifiers, modifierExpression)
#         }
#         
#         forwardModifiers <- paste(forwardModifiers, collapse = ",")
# 
#         arrow <- paste0("(",
#                         input$eqn_chem_back_k_edit,
#                         ")",
#                         arrow,
#                         "([",
#                         forwardModifiers,
#                         "])")
#       }
#       else if (!input$eqn_options_chem_modifier_forward_edit &&
#                input$eqn_options_chem_modifier_reverse_edit) {
#         
#         reverseModifiers <- c()
#         
#         for (i in seq(n.r.reg)) {
#           regulator <- eval(parse(text = paste0(
#             "input$eqn_reverse_regulator_edit",
#             as.character(i)
#           )))
#           rateConstant <- eval(parse(
#             text = paste0("input$eqn_reverse_rateConstant_edit",
#                           as.character(i))
#           ))
#           
#           modifierExpression <- paste0(regulator, "/", rateConstant)
#           reverseModifiers <-
#             c(reverseModifiers, modifierExpression)
#         }
#         reverseModifiers <- paste(reverseModifiers, collapse = ",")
#         arrow <- paste0("([",
#                         reverseModifiers,
#                         "])",
#                         arrow,
#                         "(",
#                         input$eqn_chem_forward_k_edit,
#                         ")")
#       }
#       else
#       {
#         arrow <- paste0("(",
#                         input$eqn_chem_back_k_edit,
#                         ")",
#                         arrow,
#                         "(",
#                         input$eqn_chem_forward_k_edit,
#                         ")"
#                         )
#       }
#     }
#     else if (input$eqn_chem_forward_or_both_edit == "forward_only") {
#       arrow = "--->"
#       if (input$eqn_options_chem_modifier_forward_edit) {
#         forwardModifiers <- c()
#         for (i in seq(n.f.reg)) {
#           regulator <- eval(parse(
#             text = paste0("input$eqn_forward_regulator_edit",
#                           as.character(i))
#           ))
#           rateConstant <- eval(parse(
#             text = paste0("input$eqn_forward_rateConstant_edit",
#                           as.character(i))
#           ))
#           modifierExpression <- paste0(regulator, "/", rateConstant)
#           forwardModifiers <-
#             c(forwardModifiers, modifierExpression)
#         }
#         forwardModifiers <- paste(forwardModifiers, collapse = ",")
#         arrow <- paste0(arrow, "([", forwardModifiers , "])")
#       }
#       else
#       {
#         arrow <- paste0(arrow,
#                         "(",
#                         input$eqn_chem_forward_k_edit,
#                         ")"
#                         )
#       }
#     }
# 
#     textOut <- paste(eqn_LHS, arrow, eqn_RHS)
#   }
#   else if (input$eqnCreate_type_of_equation_edit == "enzyme_rxn") {
#     substrate = input$eqn_enzyme_substrate_edit
#     product = input$eqn_enzyme_product_edit
#     arrow = "-->"
#     enzyme = input$eqn_enzyme_enzyme_edit
#     Km = input$eqn_enzyme_Km_edit
# 
#     if (!input$eqn_options_enzyme_useVmax_edit) {
#       kcat = input$eqn_enzyme_kcat_edit
#       textOut <- paste0(substrate,
#                         " + ",
#                         enzyme,
#                         " (",
#                         kcat,
#                         ")",
#                         arrow,
#                         "(",
#                         Km,
#                         ") ",
#                         product
#                         )
#     }
#     else if (input$eqn_options_enzyme_useVmax_edit) {
#       Vmax = input$eqn_enzyme_Vmax_edit
#       textOut <- paste0(substrate,
#                         " (",
#                         Vmax,
#                         ", Enzyme)",
#                         arrow,
#                         "(",
#                         Km,
#                         ") ",
#                         product)
#     }
#   }
#   else if (input$eqnCreate_type_of_equation_edit == "syn") {
#     if (input$eqn_syn_law_edit == "rate") {
#       arrow <- "-->"
#       var   <- input$eqn_syn_rate_var_edit
#       rc    <- input$eqn_syn_rate_RC_edit
#       type  <- "syn"
#       textOut <- paste0(arrow,
#                         "(", rc, ")",
#                         var
#       )
#     }
#     else if (input$eqn_syn_law_edit == "byFactor") {
#       arrow  <- "-->"
#       var    <- input$eqn_syn_sby_var_edit
#       rc     <- input$eqn_syn_sby_RC_edit
#       factor <- input$eqn_syn_sby_factor_edit
#       type   <- "syn"
#       textOut <- paste0(factor,
#                         arrow,
#                         "(", rc, ")",
#                         var
#       )
#     }
#   }
#   else if (input$eqnCreate_type_of_equation_edit == "deg") {
#     if (input$eqn_deg_to_products_edit) {
#       num.deg.products <- as.numeric(input$eqn_deg_num_products_edit)
#       product <- ""
#       for (i in seq(num.deg.products)) {
#         prod <- eval(parse(text = paste0("input$eqn_deg_product_edit",
#                                          as.character(i))))
#         if (i == num.deg.products) {
#           product <- paste0(product, Var2MathJ(prod))
#         } else {
#           product <- paste0(product, Var2MathJ(prod), " + ")
#         }
#       }
#     } else {
#       product <- ""
#     }
#     if (input$eqn_deg_law_edit == "rate") {
#       arrow <- "->"
#       var   <- input$eqn_deg_var_edit
#       rc    <- input$eqn_deg_rate_RC_edit
#       type  <- "deg"
#       textOut <- paste0(var,
#                         arrow,
#                         "(", rc, ")",
#                         product
#       )
# 
#     } else if (input$eqn_deg_law_edit == "byEnzyme") {
#       arrow <- "->"
#       var   <- input$eqn_deg_var
#       Km    <- input$eqn_deg_Km
#       type  <- "deg"
# 
#       if (input$eqn_deg_use_Vmax_edit) {
#         Vmax <- input$eqn_deg_Vmax_edit
#         textOut <- paste0(var,
#                           arrow,
#                           "(", Km, ", ", Vmax, ")",
#                           product
#         )
#       } else {
#         enz  <- input$eqn_deg_enzyme_edit
#         kcat <- input$eqn_deg_kcat_edit
#         textOut <- paste0(var,
#                           arrow,
#                           "(", Km, ", ", kcat, ", ", enz, ")",
#                           product
#         )
#       }
#     }
#   }
# 
#   # else if (input$eqnCreate_type_of_equation_edit == "rate_eqn") {
#   #   rate_left <- input$eqnCreate_rate_firstvar
#   #   rate_right <- input$eqnCreate_rate_equation
#   #   textOut <- paste0(rate_left, " = ", rate_right)
#   # }
#   # else if (input$eqnCreate_type_of_equation_edit == "time_dependent")
#   # {
#   #   TD_left <- input$eqnCreate_time_dependent_firstvar
#   #   TD_right <- input$eqnCreate_time_dependent_equation
#   #   textOut <- paste0(TD_left, "=", TD_right)
#   # }
#   else{textOut <- "ERROR"}
#   return(textOut)
# })
 

# Edit: Store New Equation -----------------------------------------------------

observeEvent(input$modal_editEqn_edit_button, {
  
  # JS Visual Runs
  waiter.rv.REACTIONS$show()
  shinyjs::disable("createEqn_store_edit_button")
  Sys.sleep(0.5)
  

  comp.id <- NA
  # Find equation in data structure
  eqn.num     <- as.numeric(input$eqnCreate_edit_select_equation)
  eqn.row     <- rv.REACTIONS$reactions[[eqn.num]]
  
  # Unpack Equation Information
  eqn.ID      <- eqn.row$ID
  eqn.type    <- eqn.row$Eqn.Type
  eqn.law     <- eqn.row$Law
  eqn.species <- eqn.row$Species
  eqn.RC      <- eqn.row$Rate.Constants
  eqn.compart <- eqn.row$Compartment
  eqn.var.id  <- eqn.row$Species.Id
  eqn.RCs.id  <- eqn.row$Parameters.Id
  eqn.comp.id <- eqn.row$Compartment.Id
  eqn.descrpt <- eqn.row$Description
  
  # Unpack Old Parameters in Equation
  old.params  <- str_split(eqn.RC, " ")[[1]]
  
  comp.id <- eqn.comp.id

  # Grab New Equation Type
  new.eqn.type <- input$eqnCreate_type_of_equation_edit
  switch(new.eqn.type,
         chem_rxn       = {law <- input$eqn_chem_law_edit},
         enzyme_rxn     = {law <- input$eqn_enzyme_law_edit},
         syn            = {law <- input$eqn_syn_law_edit},
         deg            = {law <- input$eqn_deg_law_edit},
         rate_eqn       = {law <- NA},
         time_dependent = {law <- NA}
         )
  
  p.add              <- c() # Parameter Variable Vector
  u.add              <- c() # parameter Unit Vector
  ud.add             <- c() # Parameter Unit Breakdown Vector
  d.add              <- c() # Parameter Description Vector
  b.unit             <- c() # Base Unit for calculations
  b.val              <- c() # Base Unit Values
  passed.error.check <- TRUE
  var.add            <- c() # Variables in model to add
  p.id               <- c() # Parameter Ids
  var.id             <- c() # Variable Ids

  # Build new equations
  if (new.eqn.type == "chem_rxn") {
    #this will hold all the functions for chemical reactions:
    # Currently holds: Mass Action, Regulated Mass Action
    compartment = eqn.compart #placeholder for compartments to be added in future

    # Number of variables on RHS/LHS of equation
    n.RHS = as.numeric(input$eqnCreate_num_of_eqn_RHS_edit) 
    n.LHS = as.numeric(input$eqnCreate_num_of_eqn_LHS_edit) 

    # Mass Action
    if (input$eqn_chem_law_edit == "MA") { 
      law = "MassAction"
      # Set regulators to null
      FM.bool <- FALSE
      FMs     <- NA
      FM.RC   <- NA
      RM.bool <- FALSE
      RMs     <- NA
      RM.RC   <- NA
      # Build left hand side of equation
      left     <- BuildEquationSide("input$LHS_Coeff_edit",
                                    "input$LHS_Var_edit",
                                    n.LHS)
      coef.LHS <- left["coefs"]
      var.LHS  <- left["vars"]
      id.LHS   <- left["ids"]

      # Build right hand side equation
      right    <- BuildEquationSide("input$RHS_Coeff_edit",
                                    "input$RHS_Var_edit",
                                    n.RHS)
      coef.RHS <- right["coefs"]
      var.RHS  <- right["vars"]
      id.RHS   <- right["ids"]

      arrow <- input$eqn_chem_forward_or_both_edit
      if (arrow == "both_directions") {
        # Rate Constants
        kf    <- input$eqn_chem_forward_k_edit
        kr    <- input$eqn_chem_back_k_edit
        
        kf.u <- DetermineRateConstantUnits(coef.LHS$coefs, 
                                           rv.UNITS$units.base$For.Var,
                                           rv.UNITS$units.base$Volume,
                                           rv.UNITS$units.base$Duration,
                                           rv.UNITS$units.selected$For.Var,
                                           rv.UNITS$units.selected$Volume,
                                           rv.UNITS$units.selected$Duration)
        kr.u <- DetermineRateConstantUnits(coef.RHS$coefs, 
                                           rv.UNITS$units.base$For.Var,
                                           rv.UNITS$units.base$Volume,
                                           rv.UNITS$units.base$Duration,
                                           rv.UNITS$units.selected$For.Var,
                                           rv.UNITS$units.selected$Volume,
                                           rv.UNITS$units.selected$Duration)
        kf.unit   <- kf.u$unit
        kr.unit   <- kr.u$unit
        kf.b.unit <- kf.u$unit.base
        kr.b.unit <- kr.u$unit.base
        kf.unit.d <- kf.u$unit.d
        kr.unit.d <- kr.u$unit.d
        
        kf.d <- paste0("Forward rate constant for the reaction of ",
                       paste0(str_split(var.LHS, " ")[[1]], collapse = ", "),
                       " to ",
                       paste0(str_split(var.RHS, " ")[[1]], collapse = ", "))
        kr.d <- paste0("Reverse rate constant for the reaction of ",
                       paste0(str_split(var.LHS, " ")[[1]], collapse = ", "),
                       " to ",
                       paste0(str_split(var.RHS, " ")[[1]], collapse = ", ")
        )
        
        p.add  <- c(p.add, kf, kr)
        u.add  <- c(u.add, kf.unit, kr.unit)
        ud.add <- c(ud.add, kf.unit.d, kr.unit.d)
        d.add  <- c(d.add, kf.d, kr.d)
        b.unit <- c(b.unit, kf.b.unit, kr.b.unit)
        b.val  <- c(b.val, 0, 0)

      } else if (arrow == "forward_only") {
        kf    <- input$eqn_chem_forward_k_edit
        kr    <- NA
        
        kf.u <- DetermineRateConstantUnits(coef.LHS$coefs, 
                                           rv.UNITS$units.base$For.Var,
                                           rv.UNITS$units.base$Volume,
                                           rv.UNITS$units.base$Duration,
                                           rv.UNITS$units.selected$For.Var,
                                           rv.UNITS$units.selected$Volume,
                                           rv.UNITS$units.selected$Duration)
        
        kf.unit      <- kf.u$unit
        kf.unit.base <- kf.u$unit.base
        kf.unit.d    <- kf.u$unit.d
        
        kf.d <- paste0("Forward rate constant for the reaction of ",
                       paste0(str_split(var.LHS, " ")[[1]], collapse = ", "),
                       " to ",
                       paste0(str_split(var.RHS, " ")[[1]], collapse = ", "))
        
        p.add  <- c(p.add, kf)
        u.add  <- c(u.add, kf.unit)
        ud.add <- c(ud.add, kf.unit.d)
        d.add  <- c(d.add, kf.d)
        b.unit <- c(b.unit, kf.unit.base)
        b.val  <- c(b.val, 0)
      }
      
      eqn.description <- ""
      var.add <- paste(var.LHS, var.RHS)
      var.id <- paste(id.LHS, id.RHS)

      # Mass Action w/ Regulation
    } else if (input$eqn_chem_law_edit == 'RegulatedMA') { 
      law = "RegulatedMA"
      
      # Number of regulators for forward/reverse reactions
      n.f.reg = as.numeric(input$eqn_options_chem_num_forward_regulators_edit)
      n.r.reg = as.numeric(input$eqn_options_chem_num_reverse_regulators_edit) 

      # Build left hand side of equation
      left     <- BuildEquationSide("input$LHS_Coeff_edit",
                                    "input$LHS_Var_edit",
                                    n.LHS)
      coef.LHS <- left["coefs"]
      var.LHS  <- left["vars"]
      id.LHS   <- left["ids"]

      # Build right hand side equation
      right    <- BuildEquationSide("input$RHS_Coeff_edit",
                                    "input$RHS_Var_edit",
                                    n.RHS)
      coef.RHS <- right["coefs"]
      var.RHS  <- right["vars"]
      id.RHS   <- right["ids"]

      arrow <- input$eqn_chem_forward_or_both_edit
      if (arrow == "both_directions") {
        if (input$eqn_options_chem_modifier_forward_edit) {
          
          kf      <- NA
          FM.bool <- TRUE

          f.regs <- BuildRegulatorSide("input$eqn_forward_regulator_edit",
                                       "input$eqn_forward_rateConstant_edit",
                                       n.f.reg,
                                       var.LHS,
                                       var.RHS,
                                       TRUE)
          
          FMs     <- f.regs["regulators"]
          FM.RC   <- f.regs["rateConstants"]
          FM.ids  <- f.regs["reg.ids"]
          
          p.add   <- c(p.add, f.regs["P.to.add"][[1]])
          d.add   <- c(d.add, f.regs["P.descriptions"][[1]])
          
          FMs     <- paste(FMs, collapse = " ")
          FM.RC   <- paste(FM.RC, collapse = " ")
          for (reg in strsplit(FM.RC, " ")[[1]]) {
            u <- DetermineRateConstantUnits("1",
                                            rv.UNITS$units.base$For.Var,
                                            rv.UNITS$units.base$Volume,
                                            rv.UNITS$units.base$Duration,
                                            rv.UNITS$units.selected$For.Var,
                                            rv.UNITS$units.selected$Volume,
                                            rv.UNITS$units.selected$Duration)
            u.add  <- c(u.add,  u$unit)
            b.unit <- c(b.unit, u$unit.base)
            b.val  <- c(b.val,  0)
            ud.add <- c(ud.add, u$unit.d)
          }
        } else {
          kf      <- input$eqn_chem_forward_k_edit
          
          p.add   <- c(p.add, kf)
          FM.bool <- FALSE
          FMs     <- NA
          FM.RC   <- NA
          FM.ids  <- NA
          
          
          kf.unit <- DetermineRateConstantUnits(coef.LHS$coefs, 
                                                rv.UNITS$units.base$For.Var,
                                                rv.UNITS$units.base$Volume,
                                                rv.UNITS$units.base$Duration,
                                                rv.UNITS$units.selected$For.Var,
                                                rv.UNITS$units.selected$Volume,
                                                rv.UNITS$units.selected$Duration)
          
          kf.d <- paste0("Reverse rate constant for the reaction of ",
                         paste0(str_split(var.LHS, " ")[[1]], collapse = ", "),
                         " to ",
                         paste0(str_split(var.RHS, " ")[[1]], collapse = ", ")
          )
          
          u.add  <- c(u.add,  kf.unit$unit)
          ud.add <- c(ud.add, kf.unit$unit.d)
          b.unit <- c(b.unit, kf.unit$base.unit)
          b.val  <- c(b.val,  0)
          d.add  <- c(d.add,  kf.d)
        }
        # Checks if regulator was used in reverse reaction, hence removing kr
        # and updating the appropriate values for the regulator
        if (input$eqn_options_chem_modifier_reverse_edit) {
          kr      <- NA
          RM.bool <- TRUE

          r.regs <- BuildRegulatorSide("input$eqn_reverse_regulator_edit",
                                       "input$eqn_reverse_rateConstant_edit",
                                       n.r.reg,
                                       var.LHS,
                                       var.RHS,
                                       FALSE)
          
          RMs     <- r.regs["regulators"]
          RM.RC   <- r.regs["rateConstants"]
          RM.ids  <- r.regs["reg.ids"]
          
          p.add   <- c(p.add, r.regs["P.to.add"][[1]])
          d.add   <- c(d.add, r.regs["P.descriptions"][[1]])
          RMs     <- paste(RMs, collapse = " ")
          RM.RC   <- paste(RM.RC, collapse = " ")
          RM.ids  <- paste(RM.ids, collapse = " ")
          
          for (reg in strsplit(RM.RC, " ")[[1]]) {
            u <- DetermineRateConstantUnits("1",
                                            rv.UNITS$units.base$For.Var,
                                            rv.UNITS$units.base$Volume,
                                            rv.UNITS$units.base$Duration,
                                            rv.UNITS$units.selected$For.Var,
                                            rv.UNITS$units.selected$Volume,
                                            rv.UNITS$units.selected$Duration)
            u.add  <- c(u.add,  u$unit)
            b.unit <- c(b.unit, u$unit.base)
            b.val  <- c(b.val,  0)
            ud.add <- c(ud.add, u$unit.d)
          }
        }
        else{
          kr      <- input$eqn_chem_back_k_edit
          RM.bool <- FALSE
          RMs     <- NA
          RM.RC   <- NA
          RM.ids  <- NA
          
          kr.unit <- DetermineRateConstantUnits(coef.RHS$coefs, 
                                                rv.UNITS$units.base$For.Var,
                                                rv.UNITS$units.base$Volume,
                                                rv.UNITS$units.base$Duration,
                                                rv.UNITS$units.selected$For.Var,
                                                rv.UNITS$units.selected$Volume,
                                                rv.UNITS$units.selected$Duration)
          kr.d <- paste0("Reverse rate constant for the reaction of ",
                         paste0(str_split(var.LHS, " ")[[1]], collapse = ", "),
                         " to ",
                         paste0(str_split(var.RHS, " ")[[1]], collapse = ", ")
          )
          
          p.add  <- c(p.add,  kr)
          u.add  <- c(u.add,  kr.unit$unit)
          ud.add <- c(ud.add, kr.unit$unit.d)
          b.unit <- c(b.unit, kr.unit$unit.base)
          b.val  <- c(b.val,  0)
          d.add  <- c(d.add,  kr.d)
        }
      } else if (arrow == "forward_only") {

        # Set reverse regulator variables to NA
        kr      <- NA
        RM.bool <- FALSE
        RMs     <- NA
        RM.RC   <- NA
        RM.Ids  <- NA

        if (input$eqn_options_chem_modifier_forward_edit) {
          kf      <- NA
          FM.bool <- TRUE

          f.regs <- BuildRegulatorSide("input$eqn_forward_regulator_edit",
                                       "input$eqn_forward_rateConstant_edit",
                                       n.f.reg,
                                       var.LHS,
                                       var.RHS,
                                       TRUE)
          
          FMs     <- f.regs["regulators"]
          FM.RC   <- f.regs["rateConstants"]
          FM.ids  <- f.regs["reg.ids"]
          
          p.add   <- c(p.add, f.regs["P.to.add"][[1]])
          d.add   <- c(d.add, f.regs["P.descriptions"][[1]])
          FMs     <- paste(FMs, collapse = " ")
          FM.RC   <- paste(FM.RC, collapse = " ")
          FM.ids  <- paste(FM.ids, collapse = " ")
          
          for (reg in strsplit(FM.RC, " ")[[1]]) {
            u <- DetermineRateConstantUnits("1",
                                            rv.UNITS$units.base$For.Var,
                                            rv.UNITS$units.base$Volume,
                                            rv.UNITS$units.base$Duration,
                                            rv.UNITS$units.selected$For.Var,
                                            rv.UNITS$units.selected$Volume,
                                            rv.UNITS$units.selected$Duration)
            u.add  <- c(u.add,  u$unit)
            b.unit <- c(b.unit, u$unit.base)
            b.val  <- c(b.val,  0)
            ud.add <- c(ud.add, u$unit.d)
          }
        } else {
          kf <- input$eqn_chem_forward_k_edit
          kf.unit <- DetermineRateConstantUnits(coef.LHS$coefs, 
                                                rv.UNITS$units.base$For.Var,
                                                rv.UNITS$units.base$Volume,
                                                rv.UNITS$units.base$Duration,
                                                rv.UNITS$units.selected$For.Var,
                                                rv.UNITS$units.selected$Volume,
                                                rv.UNITS$units.selected$Duration)
          u.add  <- c(u.add, kf.unit$unit)
          ud.add <- c(ud.add, kf.unit$unit.d)
          b.unit <- c(b.unit, kf.unit$unit.base)
          b.val  <- c(b.val,  0)
          p.add <- c(p.add, kf)
          FM.bool <- FALSE
          FMs <- NA
          FM.RC <- NA
          FM.ids <- NA
        }
      }

      eqn.description = ""
      to.add  <- c(var.LHS, var.RHS)
      to.add  <- to.add[!is.na(to.add)]
      var.add <- paste(to.add, collapse = " ")
      
      #ids
      to.add <- c(id.LHS, id.RHS)
      to.add <- to.add[!is.na(to.add)]
      var.id <- paste(to.add, collapse = " ")
    }

    # Add equation to DF
    error.check <- CheckParametersForErrors(p.add,
                                            rv.SPECIES$species.names,
                                            names(rv.PARAMETERS$parameters),
                                            onEdit = TRUE)
    passed.error.check <- error.check[[1]]
    
    if (passed.error.check) {
      
      par.id.2.store <- c()
      for (i in seq(length(p.add))) {
        p.to.add <- p.add[i]
        par.out <- BuildParameters(p.add[i],
                                   names(rv.PARAMETERS$parameters),
                                   rv.ID$id.param.seed,
                                   pUnit = u.add[i],
                                   pUnitD = ud.add[i],
                                   pBaseUnit = b.unit[i],
                                   pBaseValue = b.val[i],
                                   pDescription = d.add[i],
                                   pLocation = "Reaction",
                                   pLocationNote = new.eqn.type)
        StoreParameters(par.out)
        par.id.2.store <- c(par.id.2.store, par.out["par.id"])
        #Pull information
        # rv.PARAMETERS$all
      }
      par.id.2.store <- paste(par.id.2.store, collapse = " ")
      
      # Add overall data to eqn list data structure
      eqn.list.entry <- list(ID = eqn.ID,
                             Eqn.Type = new.eqn.type,
                             Law = law,
                             Species = var.add,
                             Rate.Constants = paste0(p.add, collapse = " "),
                             Compartment = eqn.compart,
                             Description = eqn.description,
                             Species.Id = var.id,
                             Parameters.Id = par.id.2.store,
                             Compartment.Id = comp.id,
                             Equation.Text = equationBuilder_edit(),
                             Equation.Latex = NA,
                             Equation.MathJax = equationBuilder_edit_mathJax())
      
      
      eqn.chem.entry <- list(ID = eqn.ID,
                             Law = law,
                             LHS.coef = coef.LHS, 
                             LHS.var = var.LHS, 
                             RHS.coef = coef.RHS, 
                             RHS.var = var.RHS, 
                             arrow = arrow,
                             kf = kf, 
                             kr = kr,
                             FM.bool = FM.bool, 
                             FMs = FMs, 
                             FM.rateC = FM.RC,
                             RM.bool = RM.bool, 
                             RMs = RMs, 
                             RM.rateC = RM.RC)
      
      # Replace entries in lists
      rv.REACTIONS$reactions[[eqn.ID]] <- eqn.list.entry
      rv.REACTIONS$massAction[[eqn.ID]] <- eqn.chem.entry
    }
  } 
  else if(new.eqn.type == "enzyme_rxn") {
    if (input$eqn_enzyme_law_edit == "MM") {

      eqn.description <- ""
      compartment     <- eqn.compart
      law             <- "Michaelis Menten"
      p.add           <- c()
      u.add           <- c()
      ud.add          <- c()
      var.add         <- c()
      b.unit          <- c()
      b.val           <- c()
      var.id          <- c()

      substrate  <- input$eqn_enzyme_substrate_edit
      product    <- input$eqn_enzyme_product_edit
      Km         <- input$eqn_enzyme_Km_edit
      arrow      <- "forward_only"
      Km.unit    <- rv.UNITS$units.selected$For.Var
      Km.b.u     <- rv.UNITS$units.base$For.Var
      Km.unit.d  <- paste0("conc (",input$GO_species_unit_choice, ")")
      
      p.add      <- c(p.add, Km)
      var.add    <- c(var.add, substrate, product)
      u.add      <- c(u.add, Km.unit)
      ud.add     <- c(ud.add, Km.unit.d)
      b.unit     <- c(b.unit, Km.b.u)
      b.val      <- c(b.val, 0)
      var.id     <- c(var.id, FindId(substrate), FindId(product))
      
      Km.d <- paste0("Michaelis Menten constant for the enzymatic conversion of ",
                     substrate,
                     " to ",
                     product
      )
      d.add <- c(Km.d)

      if (!input$eqn_options_enzyme_useVmax_edit) {
        kcat    <- input$eqn_enzyme_kcat_edit
        enzyme  <-  input$eqn_enzyme_enzyme_edit
        Vmax      <- NA
        kcat.unit <- paste0("1/", rv.UNITS$units.selected$Duration)
        kcat.b.u  <- paste0("1/", rv.UNITS$units.base$Duration)
        kcat.u.d  <- "num <div> time"
        kcat.d <- paste0("Rate constant for the enzymatic conversion of ",
                         substrate,
                         " to ",
                         product
        )
        
        var.add <- c(var.add, enzyme)
        p.add  <- c(p.add, kcat)
        d.add  <- c(d.add, kcat.d)
        u.add  <- c(u.add, kcat.unit)
        ud.add <- c(ud.add, kcat.u.d)
        b.unit <- c(b.unit, kcat.b.u)
        b.val  <- c(b.val, 0)
        var.id     <- c(var.id, FindId(enzyme))

      } else if (input$eqn_options_enzyme_useVmax_edit) {
        Vmax   <- input$eqn_enzyme_Vmax_edit
        kcat   <- NA
        enzyme <- NA
        p.add  <- c(p.add, Vmax)

        Vmax.unit <- paste0(rv.UNITS$units.selected$For.Var, "/",
                            rv.UNITS$units.selected$Duration)
        Vmax.b.u  <- paste0(rv.UNITS$units.base$For.Var, "/",
                            rv.UNITS$units.base$Duration)
        Vmax.u.d  <- paste0("conc (",
                            input$GO_species_unit_choice,
                            ") <div> time")
        Vmax.d <- paste0("Maximum velocity for the enzymatic conversion of ",
                         substrate,
                         " to ",
                         product
        )
        d.add <- c(d.add, Vmax.d)
        u.add <- c(u.add, Vmax.unit)
        ud.add <- c(ud.add, Vmax.u.d)
        b.unit <- c(b.unit, Vmax.b.u)
        b.val  <- c(b.val, 0)
      }

      error.check <- CheckParametersForErrors(p.add,
                                              rv.SPECIES$species.names,
                                              names(rv.PARAMETERS$parameters),
                                              onEdit = TRUE)
      passed.error.check <- error.check[[1]]

      if (passed.error.check) {

        par.id.2.store <- c()
        for (i in seq(length(p.add))) {
          par.out <- BuildParameters(p.add[i],
                                     names(rv.PARAMETERS$parameters),
                                     rv.ID$id.param.seed,
                                     pUnit = u.add[i],
                                     pUnitD = ud.add[i],
                                     pBaseUnit = b.unit[i],
                                     pBaseValue = b.val[i],
                                     pDescription = d.add[i],
                                     pLocation = "Reaction",
                                     pLocationNote = new.eqn.type)
          StoreParameters(par.out)
          par.id.2.store <- c(par.id.2.store, par.out["par.id"])
        }
        
        par.id.2.store <- paste(par.id.2.store, collapse = " ")

        
        # Add overall data to eqn list data structure
        eqn.list.entry <- list(ID = eqn.ID,
                               Eqn.Type = new.eqn.type,
                               Law = law,
                               Species = paste0(var.add, collapse = " "),
                               Rate.Constants = paste0(p.add, collapse = " "),
                               Compartment = eqn.compart,
                               Description = eqn.description,
                               Species.Id = paste0(var.id, collapse = " "),
                               Parameters.Id = par.id.2.store,
                               Compartment.Id = comp.id,
                               Equation.Text = equationBuilder_edit(),
                               Equation.Latex = NA(),
                               Equation.MathJax = equationBuilder_edit_mathJax()
                               )
        
        eqn.enz.entry  <- list(ID = eqn.ID,
                               Law = law,
                               Substrate = substrate, 
                               Product = product, 
                               Enzyme = enzyme, 
                               kcat = kcat,
                               Km = Km, 
                               Vmax = Vmax)
        
        # Replace entry in list
        rv.REACTIONS$reactions[[eqn.ID]] <- eqn.list.entry
        rv.REACTIONS$michaelisMenten[[eqn.ID]] <- eqn.enz.entry
      }
    }
  } 
  else if (new.eqn.type == "syn") {
    compartment <- eqn.compart
    # comp.id     <- FindId(compartment)
    p.add       <- c()
    u.add       <- c()
    ud.add      <- c()
    d.add       <- c()
    var.add     <- c()
    b.unit      <- c()
    b.val       <- c()
    var.id      <- c()
    
    if (input$eqn_syn_law_edit == "rate") {

      eqn.d   <- ""
      var     <- input$eqn_syn_rate_var_edit
      rc      <- input$eqn_syn_rate_RC_edit
      rc.b.u  <- paste0(rv.UNITS$units.base$For.Var, 
                        "/", 
                        rv.UNITS$units.base$Duration)
      rc.unit <- paste0(rv.UNITS$units.base$For.Var, 
                        "/", 
                        rv.UNITS$units.selected$Duration)
      rc.ud   <- paste0("conc (",
                        rv.UNITS$units.selected$For.Var,
                        ") <div> time")
      rc.b.v  <- 0
      rc.d    <- paste0("Synthesis rate constant for ", var)
      factor  <- NA
      
      p.add   <- c(p.add, rc)
      var.add <- c(var.add, var)
      d.add   <- c(d.add, rc.d)
      u.add   <- c(u.add, rc.unit)
      ud.add  <- c(ud.add, rc.ud)
      b.unit  <- c(b.unit, rc.b.u)
      b.val   <- c(b.val, rc.b.v)
      var.id  <- c(var.id, FindId(var))
    } else if (input$eqn_syn_law_edit == "byFactor") {

      eqn.d   <- ""
      var     <- input$eqn_syn_sby_var_edit
      rc      <- input$eqn_syn_sby_RC_edit
      factor  <- input$eqn_syn_sby_factor_edit
      rc.b.u  <- paste0("1/", rv.UNITS$units.base$Duration)
      rc.unit <- paste0("1/", rv.UNITS$units.selected$Duration)
      rc.ud   <- "num <div> time"
      rc.d    <- paste0("Synthesis rate constant of ", 
                        var, 
                        " by factor ", 
                        factor)
      
      d.add   <- c(d.add, rc.d)
      p.add   <- c(p.add, rc)
      var.add <- c(var.add, var)
      u.add   <- c(u.add, rc.unit)
      ud.add  <- c(ud.add, rc.ud)
      b.unit  <- c(b.unit, rc.b.u)
      b.val   <- c(b.val, 0)
      var.id  <- c(var.id, FindId(var), FindId(factor))
    }
    error.check <- CheckParametersForErrors(p.add,
                                            rv.SPECIES$species.names,
                                            names(rv.PARAMETERS$parameters),
                                            onEdit = TRUE)
    passed.error.check <- error.check[[1]]

    if (passed.error.check) {

      # Store parameters to parameter vector
      par.id.2.store <- c()
      for (i in seq(length(p.add))) {
        par.out <- BuildParameters(p.add[i],
                                   names(rv.PARAMETERS$parameters),
                                   rv.ID$id.param.seed,
                                   pUnit = u.add[i],
                                   pUnitD = ud.add[i],
                                   pBaseUnit = b.unit[i],
                                   pBaseValue = b.val[i],
                                   pDescription = d.add[i],
                                   pLocation = "Reaction",
                                   pLocationNote = new.eqn.type)
        StoreParameters(par.out)
        par.id.2.store <- c(par.id.2.store, par.out["par.id"])
      }
      par.id.2.store <- paste(par.id.2.store, collapse = " ")
      
      # Add overall data to eqn list data structure
      eqn.list.entry <- list(ID = eqn.id,
                             Eqn.Type = new.eqn.type,
                             Law = input$eqn_syn_law,
                             Species = paste0(var.add, collapse = " "),
                             Rate.Constants = paste0(p.add, collapse = " "),
                             Compartment = compartment,
                             Description = eqn.d,
                             Species.Id = paste0(var.id, collapse = " "),
                             Parameters.Id = par.id.2.store,
                             Compartment.Id = eqn.compart,
                             Equation.Text = equationBuilder_edit(),
                             Equation.Latex = NA,
                             Equation.MathJax = equationBuilder_edit_mathJax())
      
      
      eqn.syn.entry  <- list(ID = eqn.ID,
                             Law = input$eqn_syn_law,
                             VarSyn = var, 
                             RC = rc, 
                             Factor = factor)
      
      # Replace entry in list
      rv.REACTIONS$reactions[[eqn.ID]] <- eqn.list.entry
      rv.REACTIONS$synthesis[[eqn.ID]] <- eqn.syn.entry
    }
  } 
  else if (new.eqn.type == "deg") {
    compartment <- eqn.compart
    # comp.id     <- FindId(compartment)
    p.add       <- c()
    u.add       <- c()
    d.add       <- c()
    var.add     <- c()
    ud.add      <- c()
    b.unit      <- c()
    b.val       <- c()
    var.id      <- c()
    
    if (input$eqn_deg_to_products_edit) {
      num.deg.products <- as.numeric(input$eqn_deg_num_products_edit)
      product <- c()
      for (i in seq(num.deg.products)) {
        prod <- eval(parse(text = paste0("input$eqn_deg_product_edit", 
                                         as.character(i))))
        product <- c(product, prod)
      }
      var.add <- c(var.add, product)
      product <- paste0(product, collapse = " ")
      for (spec in var.add) {var.id <- c(var.id, FindId(spec))}
    } else {
      product <- NA
    }

    if (input$eqn_deg_law_edit == "rate") {

      eqn.d   <- ""
      var     <- input$eqn_deg_var_edit
      rc      <- input$eqn_deg_rate_RC_edit
      rc.b.u  <- paste0("1/", rv.UNITS$units.base$Duration)
      rc.unit <- paste0("1/", rv.UNITS$units.selected$Duration)
      rc.ud   <- "num <div> time"
      ConcDep <- input$eqn_deg_rate_conc_dependent
      rc.d    <- paste0("Degradation rate constant for ", var)
      
      d.add   <- c(d.add, rc.d)
      p.add   <- c(p.add, rc)
      var.add <- c(var.add, var)
      u.add   <- c(u.add, rc.unit)
      ud.add  <- c(ud.add, rc.ud)
      b.unit  <- c(b.unit, rc.b.u)
      b.val   <- c(b.val, 0)
      var.id  <- c(var.id, FindId(var))
      
      enz    <- NA
      Km     <- NA
      Vmax   <- NA
      kcat   <- NA

    } else if (input$eqn_deg_law_edit == "byEnzyme") {

      eqn.d   <- ""
      ConcDep <- FALSE
      var     <- input$eqn_deg_var_edit
      Km      <- input$eqn_deg_Km_edit
      
      Km.unit <- rv.UNITS$units.selected$For.Var
      Km.b.u  <- rv.UNITS$units.base$For.Var
      Km.ud   <- paste0("conc (",input$GO_species_unit_choice, ")")
      Km.d    <- paste0("Michelias Menten constant for degradation of ", var)
      
      var.add <- c(var.add, var)
      p.add   <- c(p.add, Km)
      u.add   <- c(u.add, Km.unit)
      d.add   <- c(d.add, Km.d)
      ud.add  <- c(ud.add, Km.ud)
      b.unit  <- c(b.unit, Km.b.u)
      b.val   <- c(b.val, 0)
      var.id  <- c(var.id, FindId(var))

      if (input$eqn_deg_use_Vmax_edit) {
        Vmax  <- input$eqn_deg_Vmax_edit
        Vmax.b.u  <- paste0(rv.UNITS$units.base$For.Var, "/",
                            rv.UNITS$units.base$Duration)
        Vmax.unit <- paste0(rv.UNITS$units.selected$For.Var, "/",
                            rv.UNITS$units.selected$Duration)
        Vmax.ud   <- paste0("conc (",
                            rv.UNITS$units.selected$For.Var,
                            ") <div> time")
        Vmax.d    <- paste0("Maximum Velocity for degradation of ", var)
        
        p.add  <- c(p.add, Vmax)
        u.add  <- c(u.add, Vmax.unit)
        ud.add <- c(ud.add, Vmax.ud)
        d.add  <- c(d.add, Vmax.d)
        b.unit  <- c(b.unit, Vmax.b.u)
        b.val   <- c(b.val, 0)
        
        rc    <- NA
        enz   <- NA
      } else {
        rc    <- input$eqn_deg_kcat_edit
        enz   <- input$eqn_deg_enzyme_edit
        p.add <- c(p.add, rc)

        kcat.d <- paste0("Enzymatic degradation rate constant of ", var, " by  ", enz)
        d.add  <- c(d.add, kcat.d)
        Vmax <- NA
      }
    }
    error.check <- CheckParametersForErrors(p.add,
                                            rv.SPECIES$species.names,
                                            names(rv.PARAMETERS$parameters),
                                            onEdit = TRUE)

    if (passed.error.check) {

      par.id.2.store <- c()
      # Store parameters to parameter vector
      for (i in seq(length(p.add))) {
        par.out <- BuildParameters(p.add[i],
                                   names(rv.PARAMETERS$parameters),
                                   rv.ID$id.param.seed,
                                   pUnit = u.add[i],
                                   pUnitD = ud.add[i],
                                   pBaseUnit = b.unit[i],
                                   pBaseValue = b.val[i],
                                   pDescription = d.add[i],
                                   pLocation = "Reaction",
                                   pLocationNote = new.eqn.type)
        StoreParameters(par.out)
        par.id.2.store <- c(par.id.2.store, par.out["par.id"])
      }
      par.id.2.store <- paste(par.id.2.store, collapse = " ")
      
      # Generate eqn ID
      ID.gen <- GenerateId(rv.ID$id.eqn.seed, "eqn")
      rv.ID$id.eqn.seed <- rv.ID$id.eqn.seed + 1
      ID.to.add <- ID.gen[["id"]]
      
    
      # Add overall data to eqn list data structure
      eqn.list.entry <- list(ID = eqn.ID,
                             Eqn.Type = new.eqn.type,
                             Law = input$eqn_deg_law,
                             Species = paste0(var.add, collapse = " "),
                             Rate.Constants = paste0(p.add, collapse = " "),
                             Compartment = eqn.compart,
                             Description = eqn.d,
                             Species.Id = paste0(var.id, collapse = " "),
                             Parameters.Id = par.id.2.store,
                             Compartment.Id = comp.id,
                             Equation.Text = equationBuilder_edit(),
                             Equation.Latex = NA,
                             Equation.MathJax = equationBuilder_edit_mathJax())
      
      n.eqns <- length(rv.REACTIONS$reactions)
      rv.REACTIONS$reactions[[n.eqns + 1]] <- eqn.list.entry
      names(rv.REACTIONS$reactions)[n.eqns+1] <- ID.to.add
      
      eqn.deg.entry  <- list(ID = eqn.ID,
                             Law = input$eqn_deg_law,
                             VarDeg = var,
                             ConcDep = ConcDep,
                             RC = rc,
                             Km = Km, 
                             Enz = enz,
                             Vmax = Vmax,
                             Prods = product
      )
      
      # Replace entry in list
      rv.REACTIONS$reactions[[eqn.ID]] <- eqn.list.entry
      rv.REACTIONS$degradation[[eqn.ID]] <- eqn.deg.entry
    }
  }

  
  # Remove Parameters if they were changed
  params.to.remove <- setdiff(old.params, p.add)

  # Check if old parameters are used elsewhere
  p.remove <- c()
  p.save <- c()

  # Search Other Eqns for Parameters
  
  # Search Input/Output for Parameters
  
  # If they are not found, remove from rv.PARAMETERS$param.info

  #if so, store in message of variables not removed
  if (length(p.save) > 0) {
    message.out <- 
      paste0("The following parameter(s) were not deleted because they are used
             elsewhere: ",
             paste0(p.save, collapse=", ")
    )
    session$sendCustomMessage(type = 'testmessage',
                              message = message.out)
  }

  #  JS UI functions
  waiter.rv.REACTIONS$hide()
  shinyjs::enable("createEqn_store_edit_button")
})
