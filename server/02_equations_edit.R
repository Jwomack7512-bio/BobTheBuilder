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
    Product       <- Info$Product
    Product.id    <- Info$Product.id
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
        inputId = "CB_michaelis_menten_useVmax_edit",
        label = "Use Vmax",
        value = UseVmax
      ) 
    )
  }
})

# Main Box (Right): RenderUI ---------------------------------------------------
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
                  inputId = paste0("NI_MA_p_stoichiometry_edit_", 
                                   as.character(i)),
                  label = NULL,
                  value = p.stoichiometry[i],
                  min = 1,
                  step = 1),
                pickerInput(
                  inputId = paste0("PI_MA_product_edit_", 
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
    Product       <- Info$Product
    Product.id    <- Info$Product.id
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


# Equation Text UI Show --------------------------------------------------------
output$build_equation_edit <- renderUI({
  withMathJax(equationBuilder_edit_mathJax())
})

# Equation Builder Text Builder ------------------------------------------------
equationBuilder_edit <- reactive({
  if (input$eqnCreate_reaction_law_edit == "mass_action") {
    number.reactants <- as.numeric(input$NI_mass_action_num_reactants_edit)
    number.products  <- as.numeric(input$NI_mass_action_num_products_edit)
    
    kf <- input$TI_mass_action_forward_k_edit
    kr <- input$TI_mass_action_reverse_k_edit
    
    eqn_LHS <- ""
    for (i in seq(number.reactants)) {
      coef <- eval(parse(text = paste0("input$NI_MA_r_stoichiometry_edit_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MA_reactant_edit_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_LHS <- paste0(eqn_LHS, coef, "*")
        }
      } else {
        eqn_LHS <- ""
      }
      
      if (i == as.numeric(number.reactants)) {
        eqn_LHS <- paste0(eqn_LHS, var)
      } else {
        eqn_LHS <- paste0(eqn_LHS, var, " + ")
      }
    }
    
    eqn_RHS <- ""
    for (i in seq(number.products)) {
      coef <- eval(parse(text = paste0("input$NI_MA_p_stoichiometry_edit_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MA_product_edit_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_RHS <- paste0(eqn_RHS, coef, "*")
        }
      } else {
        eqn_RHS <- ""
      }
      
      if (i == as.numeric(number.products)) {
        eqn_RHS <- paste0(eqn_RHS, var)
      }
      else{
        eqn_RHS <- paste0(eqn_RHS, var, " + ")
      }
    }
    
    if (input$PI_mass_action_reverisble_option_edit == "both_directions") {
      arrow <- paste0("(", kr, ")",
                      "<->",
                      "(", kf, ")"
      )
    } else if (input$PI_mass_action_reverisble_option_edit == "forward_only") {
      arrow <- paste0("->",
                      "(", kf, ")"
      )
      
    }
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
  }
  else if (input$eqnCreate_reaction_law_edit == "mass_action_w_reg") {
    arrow <- "->"
    
    number.reactants <- 
      as.numeric(input$NI_mass_action_wReg_num_reactants_edit)
    number.products  <- 
      as.numeric(input$NI_mass_action_wReg_num_products_edit)
    
    has.f.reg <- input$CB_MAwR_chem_modifier_forward_edit
    has.r.reg <- input$CB_MAwR_chem_modifier_reverse_edit
    
    number_forward_regulators <-
      as.numeric(input$NI_MAwR_n_forward_regulators_edit)
    number_reverse_regulators <-
      as.numeric(input$NI_MAwR_n_reverse_regulators_edit)
    
    reversible <- input$PI_mass_action_reverisble_option_edit
    
    # Build Reactant Equation Side
    eqn_LHS <- ""
    for (i in seq(number.reactants)) {
      coef <- eval(parse(text = paste0("input$NI_MAwR_r_stoichiometry_edit_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MAwR_reactant_edit_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_LHS <- paste0(eqn_LHS, coef, "*")
        }
      } else {
        eqn_LHS <- ""
      }
      
      if (i == as.numeric(number.reactants)) {
        eqn_LHS <- paste0(eqn_LHS, var)
      } else {
        eqn_LHS <- paste0(eqn_LHS, var, " + ")
      }
    }
    
    # Build Product Equation Side
    eqn_RHS <- ""
    for (i in seq(number.products)) {
      coef <- eval(parse(text = paste0("input$NI_MAwR_p_stoichiometry_edit_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MAwR_product_edit_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_RHS <- paste0(eqn_RHS, coef, "*")
        }
      } else {
        eqn_RHS <- ""
      }
      
      if (i == as.numeric(number.products)) {
        eqn_RHS <- paste0(eqn_RHS, var)
      }
      else{
        eqn_RHS <- paste0(eqn_RHS, var, " + ")
      }
    }
    
    # Check For Forward Regulators
    
    if (has.f.reg) {
      #find regulators and add them together in form ([regulator/constant, 
      #regulator2/constant2, etc...])
      forwardModifiers <- c()
      for (i in seq(number_forward_regulators)) {
        regulator <-
          eval(parse(text = paste0(
            "input$PI_MAwR_forward_regulator_edit_", as.character(i)
          )))
        rateConstant <-
          eval(parse(text = paste0(
            "input$TI_MAwR_forward_regulator_RC_edit_", as.character(i)
          )))
        modifierExpression <- paste0(regulator,
                                     ":",
                                     rateConstant)
        forwardModifiers <- c(forwardModifiers, modifierExpression)
      }
      forwardModifiers <- paste(forwardModifiers, collapse = ", ")
    } 
    else {
      # If no forward regulators, use kf
      forwardModifiers <- input$TI_MAwR_forward_k_edit
    }
    forwardModifiers <- paste0("(",
                               forwardModifiers,
                               ")")
    # Check If Reaction Is Reversible
    if (reversible == "both_directions") {
      arrow <- "<->"
      # Check if Reverse Regulator is used
      if (has.r.reg) {
        reverseModifiers <- c()
        for (i in seq(number_reverse_regulators)) {
          regulator <-
            eval(parse(text = paste0(
              "input$PI_MAwR_reverse_regulator_edit_", as.character(i)
            )))
          rateConstant <-
            eval(parse(text = paste0(
              "input$TI_MAwR_reverse_regulator_RC_edit_", as.character(i)
            )))
          modifierExpression <- paste0(regulator,
                                       ":",
                                       rateConstant)
          reverseModifiers <- c(reverseModifiers, modifierExpression)
        }
        reverseModifiers <- paste(reverseModifiers, collapse = ", ")
      }
      else {
        # If no regulators, use kr
        reverseModifiers <- input$TI_MAwR_reverse_k_edit
      }
      reverseModifiers <- paste0("(", 
                                 reverseModifiers, 
                                 ")")
    } 
    else {
      reverseModifiers <- ""
    }
    
    arrow <- paste0(reverseModifiers,
                    arrow,
                    forwardModifiers
    )
    
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
  }
  else if (input$eqnCreate_reaction_law_edit == "synthesis") {
    if (input$CB_synthesis_factor_checkbox_edit) {
      arrow  <- "-->"
      var    <- input$PI_synthesis_byFactor_var_edit
      rc     <- input$TI_synthesis_byFactor_RC_edit
      factor <- input$PI_synthesis_byFactor_factor_edit
      textOut <- paste0(factor,
                        " ",
                        arrow,
                        "(", rc, ")",
                        " ",
                        var
      )
    } else {
      arrow <- "-->"
      var   <- input$PI_synthesis_rate_var_edit
      rc    <- input$TI_synthesis_rate_RC_edit
      textOut <- paste0(arrow,
                        "(", rc, ")",
                        " ",
                        var
      )
    }
  }
  else if (input$eqnCreate_reaction_law_edit == "degradation_rate") {
    num.deg.products <- as.numeric(input$NI_degradation_rate_num_products_edit)
    product <- ""
    if (input$CB_degradation_rate_toProducts_edit) {
      for (i in seq(num.deg.products)) {
        prod <- eval(
          parse(text = paste0("input$PI_degradation_rate_product_edit_", 
                                         as.character(i))))
        if (i == num.deg.products) {
          product <- paste0(product, prod)
        } else {
          product <- paste0(product, prod, " + ")
        }
      }
    }
    
    # Build Equations
    arrow  <- "-->"
    var   <- input$PI_degradation_rate_species_edit
    rc    <- input$TI_degradation_rate_RC_edit
    textOut <- paste0(var,
                      arrow,
                      "[", rc, "]",
                      product
    )
  }
  else if (input$eqnCreate_reaction_law_edit == "degradation_by_enzyme") {
    # Get products if they exist
    if (input$CB_degradation_enzyme_toProducts_edit) {
      num.deg.products <- 
        as.numeric(input$NI_degradation_enzyme_num_products_edit)
      product <- ""
      for (i in seq(num.deg.products)) {
        prod <- eval(
          parse(text = paste0("input$PI_degradation_enzyme_product_edit_", 
                                         as.character(i))))
        if (i == num.deg.products) {
          product <- paste0(product, Var2MathJ(prod))
        } else {
          product <- paste0(product, Var2MathJ(prod), " + ")
        }
      }
    } else {
      product <- ""
    }
    
    # Build Equations
    arrow <- "-->"
    var   <- input$PI_degradation_enzyme_species_edit
    Km    <- input$TI_degradation_enzyme_Km_edit
    
    if (input$CB_degradation_enzyme_useVmax_edit) {
      Vmax <- input$TI_degradation_enzyme_Vmax
      textOut <- paste0(var,
                        "[", Km, ", ", Vmax, "] ",
                        arrow,
                        " ",
                        product
      )
    } else {
      enz  <- input$PI_degradation_enzyme_enzyme_edit
      kcat <- input$TI_degradation_enzyme_kcat_edit
      textOut <- paste0(var,
                        "[", Km, ", ", kcat, ", ", enz, "]",
                        arrow,
                        " ",
                        product
      )
    }
  } 
  else if (input$eqnCreate_reaction_law_edit == "michaelis_menten") {
    substrate <- input$PI_michaelis_menten_substrate_edit
    product   <- input$PI_michaelis_menten_product_edit
    arrow     <- "-->"
    enzyme    <- input$PI_michaelis_menten_enzyme_edit
    Km        <- input$TI_michaelis_menten_Km_edit
    
    if (!input$CB_michaelis_menten_useVmax_edit) {
      kcat    <- input$TI_michaelis_menten_kcat_edit
      textOut <- paste0(substrate,
                        " + ",
                        enzyme, " ",
                        "[", Km , ", ", kcat, "]",
                        arrow,
                        " ",
                        product)
    }
    else if (input$CB_michaelis_menten_useVmax_edit) {
      Vmax <- input$TI_michaelis_menten_vmax_edit
      textOut <- paste0(substrate,
                        "[", Km , ", ", Vmax, "]",
                        arrow,
                        " ",
                        product
      )
    }
  }

  # else if (input$eqnCreate_type_of_equation_edit == "rate_eqn") {
  #   rate_left <- input$eqnCreate_rate_firstvar
  #   rate_right <- input$eqnCreate_rate_equation
  #   textOut <- paste0(rate_left, " = ", rate_right)
  # }
  # else if (input$eqnCreate_type_of_equation_edit == "time_dependent")
  # {
  #   TD_left <- input$eqnCreate_time_dependent_firstvar
  #   TD_right <- input$eqnCreate_time_dependent_equation
  #   textOut <- paste0(TD_left, "=", TD_right)
  # }
  else{textOut <- "ERROR"}
  return(textOut)
})
 

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
  
  # Unpack Old Parameters in Equation
  old.params  <- str_split(eqn.parameters, " ")[[1]]
  
  comp.id <- eqn.compartment.id


  parameters          <- c() # Parameter Variable Vector
  param.vals          <- c() # Parameter Values
  param.units         <- c() # parameter Unit Vector
  unit.descriptions   <- c() # Parameter Unit Breakdown Vector
  param.descriptions  <- c() # Parameter Description Vector
  base.units          <- c() # Base Unit for calculations
  base.values         <- c() # Base Unit Values
  species             <- c() # Variables in model to add
  parameters.id       <- c() # Parameter Ids
  species.id          <- c() # Variable Ids
  passed.error.check  <- TRUE
  
  # Get Compartment information
  compartment    <- input$eqnCreate_active_compartment
  compartment.id <- FindId(compartment)
  
  # Mass Action
  if (eqn.reaction.law == "mass_action") {
    reaction.id <- NA
    eqn.display <- "Mass Action"
    # browser()
    # browser()
    modifiers    <- NA
    modifiers.id <- NA
    
    number.reactants <- as.numeric(input$NI_mass_action_num_reactants_edit)
    number.products  <- as.numeric(input$NI_mass_action_num_products_edit)
    
    # Build left hand side of equation
    left     <- BuildEquationSide("input$NI_MA_r_stoichiometry_edit_", 
                                  "input$PI_MA_reactant_edit_", 
                                  number.reactants)
    r.stoich      <- left[["coefs"]]
    reactants     <- left[["vars"]]
    reactants.id  <- left[["ids"]]
    
    # Build right hand side equation
    right    <- BuildEquationSide("input$NI_MA_p_stoichiometry_edit_",
                                  "input$PI_MA_product_edit_", 
                                  number.products)
    p.stoich    <- right[["coefs"]]
    products    <- right[["vars"]]
    products.id <- right[["ids"]]
    
    eqn.description <- ""
    eqn.d <- "Mass Action Reaction"
    species    <- c(strsplit(reactants, ", ")[[1]], 
                    strsplit(products, ", ")[[1]])
    species.id <- c(strsplit(reactants.id, ", ")[[1]],
                    strsplit(products.id, ", ")[[1]])
    
    # Find Kf information
    kf    <- input$TI_mass_action_forward_k_edit
    
    # Rate Constant Values
    kf.val <- input$TI_mass_action_forward_k_value_edit
    
    # Build Rate Constant Units
    kf.unit <- DetermineRateConstantUnits(
      p.stoich,
      rv.UNITS$units.base$For.Var,
      rv.UNITS$units.base$Volume,
      rv.UNITS$units.base$Duration,
      rv.UNITS$units.selected$For.Var,
      rv.UNITS$units.selected$Volume,
      rv.UNITS$units.selected$Duration
    )
    
    # Convert rate constant units if necessary
    if (kf.unit$unit != kf.unit$unit.base) {
      kf.base.val <- UnitConversion(kf.base$unit.description,
                                    kf.unit$unit,
                                    kf.unit$base.unit,
                                    as.numeric(kf.val))
    } else {
      kf.base.val <- kf.val
    }
    
    # Write Unit Descriptions
    kf.d <- paste0("Forward rate constant for the reaction of ",
                   reactants,
                   " to ",
                   products)
    
    parameters         <- c(parameters, kf)
    param.vals         <- c(param.vals, kf.val)
    param.units        <- c(param.units, kf.unit$unit)
    unit.descriptions  <- c(unit.descriptions, kf.unit$unit.description)
    param.descriptions <- c(param.descriptions, kf.d)
    base.units         <- c(base.units, kf.unit$unit.base)
    base.values        <- c(base.values, kf.base.val)
    
    reversible <- input$PI_mass_action_reverisble_option_edit
    if (reversible == "both_directions") {
      isReversible <- TRUE
      # If the reaction is reversible then we need to build the reverse
      # rate constant for the reaction
      kr     <- input$TI_mass_action_reverse_k_edit
      kr.val <- input$TI_mass_action_reverse_k_value_edit
      
      # Build Rate Constant Units
      kr.unit <- DetermineRateConstantUnits(
        r.stoich,
        rv.UNITS$units.base$For.Var,
        rv.UNITS$units.base$Volume,
        rv.UNITS$units.base$Duration,
        rv.UNITS$units.selected$For.Var,
        rv.UNITS$units.selected$Volume,
        rv.UNITS$units.selected$Duration
      )
      
      # Convert rate constant units if necessary
      if (kr.unit$unit != kr.unit$unit.base) {
        kr.base.val <- UnitConversion(kr.base$unit.description,
                                      kr.unit$unit,
                                      kr.unit$base.unit,
                                      as.numeric(kr.val))
      } else {
        kr.base.val <- kr.val
      }
      
      # Write Unit Descriptions
      kr.d <- paste0("Reverse rate constant for the reaction of ",
                     reactants,
                     " to ",
                     products
      )
      
      parameters         <- c(parameters, kr)
      param.vals         <- c(param.vals, kr.val)
      param.units        <- c(param.units,kr.unit$unit)
      unit.descriptions  <- c(unit.descriptions, kr.unit$unit.description)
      param.descriptions <- c(param.descriptions, kr.d)
      base.units         <- c(base.units, kr.unit$unit.base)
      base.values        <- c(base.values, kr.base.val)
      
    } 
    else if (reversible == "forward_only") {
      kr     <- NA
      kr.val <- NA
    }
    # browser()
    # Build Rate Law
    laws <- Law_Of_Mass_Action(r.stoich,
                               reactants,
                               p.stoich,
                               products,
                               reversible,
                               kf,
                               kr)
    
    rate.law    <- laws$string
    p.rate.law  <- laws$pretty.string
    latex.law   <- laws$latex
    mathjax.law <- laws$mj
    mathml.law  <- laws$mathml
    
  } 
  
  #Error Check
  error.check <- CheckParametersForErrors(parameters, 
                                          rv.SPECIES$species.names,
                                          names(rv.PARAMETERS$parameters))
  passed.error.check <- error.check[[1]]
  
  if (passed.error.check) {
    
    # Parameters
    par.ids <- c()
    for (i in seq_along(parameters)) {
      # Generate Parameter ID
      par.gen <- GenerateId(rv.ID$id.param.seed, "parameter")
      rv.ID$id.param.seed <- par.gen$seed
      par.id <- par.gen$id
      par.ids <- c(par.ids, par.id)
      
      # Store ID to database
      idx.to.add <- nrow(rv.ID$id.df) + 1
      rv.ID$id.df[idx.to.add, ] <- c(par.id, parameters[i])
      
      # Write out to parameter
      to.par.list <- list("Name"            = parameters[i],
                          "ID"              = par.id,
                          "Value"           = as.numeric(param.vals[i]),
                          "Unit"            = param.units[i],
                          "UnitDescription" = unit.descriptions[i],
                          "BaseUnit"        = base.units[i],
                          "BaseValue"       = as.numeric(base.values[i]),
                          "Description"     = param.descriptions[i],
                          "Type"            = "Reaction",
                          "Type.Note"       = input$eqnCreate_reaction_law,
                          "Used.In"         = ID.to.add)
      
      # Store to parameter list
      rv.PARAMETERS$parameters[[par.id]] <- to.par.list
    }
    # browser()
    
    if (isTruthy(species.id)) {
      # Loop through species id to begin addition
      for (i in seq_along(species.id)) {
        # Check that the species id has IO.ids already or if its NA
        if (is.na(rv.SPECIES$species[[species.id[i]]]$Reaction.ids)) {
          # If its NA, make current id  the id
          rv.SPECIES$species[[species.id[i]]]$Reaction.ids <- ID.to.add
        } else {
          # Else paste0 collapse current id with ", "
          items <- 
            strsplit(
              rv.SPECIES$species[[species.id[i]]]$Reaction.ids, ", ")[[1]]
          items <- c(items, ID.to.add)
          rv.SPECIES$species[[species.id[i]]]$Reaction.ids <- 
            paste0(items, collapse = ", ")
        }
      }
    }
    
    # Extract reaction laws 
    rate.law    <- laws$string
    p.rate.law  <- laws$pretty.string
    latex.law   <- laws$latex
    mathjax.law <- laws$mj
    mathml.law  <- laws$mathml
    
    # We need to collapse these vector terms otherwise when the list is 
    # converted to a dataframe there will be errors
    
    par.collapsed          <- collapseVector(parameters)
    par.id.collapsed       <- collapseVector(par.ids)
    reactants.collapsed    <- collapseVector(reactants)
    reactants.id.collapsed <- collapseVector(reactants.id)
    products.collapsed     <- collapseVector(products)
    products.id.collapsed  <- collapseVector(products.id)
    species.collapsed      <- collapseVector(species)
    species.id.collapsed   <- collapseVector(species.id)
    modifiers.collapsed    <- collapseVector(modifiers)
    modifiers.id.collapsed <- collapseVector(modifiers.id)
    
    # Add overall reaction information
    reaction.entry <- list(
      "ID"               = eqn.ID,
      "Eqn.Display.Type" = eqn.display,
      "Reaction.Law"     = input$eqnCreate_reaction_law,
      "Species"          = species.collapsed,
      "Reactants"        = reactants.collapsed,
      "Products"         = products.collapsed, 
      "Modifiers"        = modifiers.collapsed,
      "Parameters"       = par.collapsed,
      "Compartment"      = compartment,
      "Description"      = eqn.d,
      "Species.id"       = species.id.collapsed,
      "Reactants.id"     = reactants.id.collapsed,
      "Products.id"      = products.id.collapsed,
      "Modifiers.id"     = modifiers.id.collapsed, 
      "Parameters.id"    = par.id.collapsed,
      "Compartment.id"   = compartment.id,
      "Equation.Text"    = equationBuilder(),
      "Equation.Latex"   = equationLatexBuilder(),
      "Equation.MathJax" = equationMathJaxBuilder(),
      "String.Rate.Law"  = rate.law,
      "Pretty.Rate.Law"  = p.rate.law,
      "Latex.Rate.Law"   = latex.law,
      "MathJax.Rate.Law" = mathjax.law,
      "MathMl.Rate.Law"  = mathml.law,
      "Reversible"       = isReversible
    )
    
    rv.REACTIONS$reactions[[eqn.ID]] <- reaction.entry
    
    # Build specific reaction type reactive variable
    if (eqn.reaction.law == "mass_action") {
      if (length(par.ids) == 1) {
        kf.id = par.ids[1]
        kr.id = NA
      } else {
        kf.id = par.ids[1]
        kr.id = par.ids[2]
      }
      
      sub.entry <- list(
        "ID" = eqn.ID,
        "Reaction.Law"    = eqn.reaction.law,
        "r.stoichiometry" = r.stoich,
        "Reactants"       = reactants,
        "Reactants.id"    = reactants.id,
        "p.stoichiometry" = p.stoich,
        "Products"        = products,
        "Products.id"     = products.id,
        "Reversible"      = reversible,
        "kf"              = kf,
        "kr"              = kr,
        "kf.id"           = kf.id,
        "kr.id"           = kr.id
      )
      
      # Add to mass action RV
      rv.REACTIONS$massAction[[eqn.ID]] <- sub.entry
    } 
    else if (input$eqnCreate_reaction_law == "mass_action_w_reg") {
      
      pc <- 1
      # Determine with param ids are which
      if (!is.na(kf)) {
        kf.id <- par.ids[pc]
        pc <- pc + 1
      }
      
      if (!is.na(kr)) {
        kr.id <- par.ids[pc]
        pc <- pc + 1
      }
      
      if (has.f.reg) {
        n.f.reg <- length(strsplit(Forward.Pars, ", ")[[1]])
        Forward.Pars.id <- par.ids[pc:(pc+n.f.reg-1)]
        pc <- pc + n.f.reg
        Forward.Pars.id <- paste0(Forward.Pars.id, collapse = ", ")
      } else {
        Forward.Pars.id <- NA
      }
      
      if (has.r.reg) {
        n.r.reg <- length(strsplit(Reverse.Pars, ", ")[[1]])
        Reverse.Pars.id <- par.ids[pc:(pc+n.r.reg-1)]
        Reverse.Pars.id <- paste0(Reverse.Pars.id, collapse = ", ")
      } else {
        Reverse.Pars.id <- NA
      }
      
      sub.entry <- list(
        "ID" = ID.to.add,
        "Reaction.Law"    = input$eqnCreate_reaction_law,
        "r.stoichiometry" = r.stoich,
        "Reactants"       = reactants,
        "Reactants.id"    = reactants.id,
        "p.stoichiometry" = p.stoich,
        "Products"        = products,
        "Products.id"     = products.id,
        "Reversible"      = reversible,
        "kf"              = kf,
        "kr"              = kr,
        "kf.id"           = kf.id,
        "kr.id"           = kr.id,
        "Use.Forward.Mod" = has.f.reg,
        "Forward.Mods"    = Forward.Mods,
        "Forward.Mods.id" = Forward.Mods.id,
        "Forward.Pars"   = Forward.Pars,
        "Forward.Pars.id" = Forward.Pars.id,
        "Use.Reverse.Mod" = has.r.reg,
        "Reverse.Mods"    = Reverse.Mods,
        "Reverse.Mods.id" = Reverse.Mods.id,
        "Reverse.Pars"    = Reverse.Pars,
        "Reverse.Pars.id" = Reverse.Pars.id
      )
      
      # Add to mass action RV
      n <- length(rv.REACTIONS$massActionwReg)
      rv.REACTIONS$massActionwReg[[n+1]] <- sub.entry
      names(rv.REACTIONS$massActionwReg)[n+1] <- ID.to.add
    }
    else if (input$eqnCreate_reaction_law == "synthesis") {
      sub.entry <- list(
        "ID"               = ID.to.add,
        "Reaction.Law"     = input$eqnCreate_reaction_law,
        "VarSyn"           = var.syn,
        "VarSyn.id"        = var.syn.id,
        "Rate.Constant"    = parameter,
        "Rate.Constant.id" = par.ids[1],
        "Factor"           = factor,
        "Factor.id"        = factor.id
      )
      
      # Add to mass action RV
      n <- length(rv.REACTIONS$synthesis)
      rv.REACTIONS$synthesis[[n+1]] <- sub.entry
      names(rv.REACTIONS$synthesis)[n+1] <- ID.to.add
      
    }
    else if (input$eqnCreate_reaction_law == "degradation_rate") {
      sub.entry <- list(
        "ID"               = ID.to.add,
        "Reaction.Law"     = input$eqnCreate_reaction_law,
        "VarDeg"           = deg.species,
        "VarDeg.id"        = deg.species.id,
        "ConcDep"          = ConcDep,
        "Rate.Constant"    = parameter,
        "Rate.Constant.id" = par.ids[1],
        "Products"         = products.collapsed,
        "Products.id"      = products.id.collapsed
      )
      
      # Add to mass action RV
      n <- length(rv.REACTIONS$degradation.by.rate)
      rv.REACTIONS$degradation.by.rate[[n+1]] <- sub.entry
      names(rv.REACTIONS$degradation.by.rate)[n+1] <- ID.to.add
    }
    else if (input$eqnCreate_reaction_law == "degradation_by_enzyme") {
      # Gets ids based on use.Vmax
      Vmax.id <- NA
      kcat.id <- NA
      Km.id   <- par.ids[1]
      
      if (Use.Vmax) {
        Vmax.id <- par.ids[2]
      } else {
        kcat.id <- par.ids[2]
      }
      
      sub.entry <- list(
        "ID"               = ID.to.add,
        "Reaction.Law"     = input$eqnCreate_reaction_law,
        "VarDeg"           = deg.species,
        "VarDeg.id"        = deg.species.id,
        "UseVmax"          = Use.Vmax,
        "Km"               = Km,
        "Km.id"            = Km.id,
        "Vmax"             = Vmax,
        "Vmax.id"          = Vmax.id,
        "Enzyme"           = enzyme,
        "Enzyme.id"        = enzyme.id,
        "kcat"             = kcat,
        "kcat.id"          = kcat.id,
        "Products"         = products.collapsed,
        "Products.id"      = products.id.collapsed
      )
      
      # Add to mass action RV
      n <- length(rv.REACTIONS$degradation.by.enzyme)
      rv.REACTIONS$degradation.by.enzyme[[n+1]] <- sub.entry
      names(rv.REACTIONS$degradation.by.enzyme)[n+1] <- ID.to.add
    }
    else if (input$eqnCreate_reaction_law == "michaelis_menten") {
      # Gets ids based on use.Vmax
      Vmax.id <- NA
      kcat.id <- NA
      Km.id   <- par.ids[1]
      
      if (Use.Vmax) {
        Vmax.id <- par.ids[2]
      } else {
        kcat.id <- par.ids[2]
      }
      
      sub.entry <- list(
        "ID"               = ID.to.add,
        "Reaction.Law"     = input$eqnCreate_reaction_law,
        "Substrate"        = substrate,
        "Substrate.id"     = substrate.id,
        "Product"          = product,
        "Product.id"       = product.id,
        "UseVmax"          = Use.Vmax,
        "Km"               = Km,
        "Km.id"            = Km.id,
        "Vmax"             = Vmax,
        "Vmax.id"          = Vmax.id,
        "Enzyme"           = enzyme,
        "Enzyme.id"        = enzyme.id,
        "kcat"             = kcat,
        "kcat.id"          = kcat.id
      )
      
      # Add to mass action RV
      n <- length(rv.REACTIONS$michaelisMenten)
      rv.REACTIONS$michaelisMenten[[n+1]] <- sub.entry
      names(rv.REACTIONS$michaelisMenten)[n+1] <- ID.to.add
    }
    
    # Resolve Diffeqs
    solveForDiffEqs()
    
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
