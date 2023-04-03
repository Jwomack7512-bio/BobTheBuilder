# This file contains all inputs and output related server functions for species
# entering or leaving compartments.

# Update UI --------------------------------------------------------------------

# Render Text ------------------------------------------------------------------

output$CIO_fi_vo_text <- renderText(
  out <- paste0("Flow Value (",
                rv.UNITS$units.selected$Volume, 
                "/", 
                rv.UNITS$units.selected$Duration, 
                ")"
  )
)

output$CIO_fb_vo_text <- renderText(
  out <- paste0("Flow Value (",
                rv.UNITS$units.selected$Volume, 
                "/", 
                rv.UNITS$units.selected$Duration, 
                ")"
  )
)  

output$CIO_fb_sv1_text <- renderText(
  out <- paste0("Flow Value (",
                rv.UNITS$units.selected$Volume, 
                "/", 
                rv.UNITS$units.selected$Duration, 
                ")"
  )
)  

# Render UI --------------------------------------------------------------------


## Render for flowbetween ------------------------------------------------------
output$CIO_flow_between_render_compartments <- renderUI({
  c.names <- rv.COMPARTMENTS$compartments.names
  in.choices <- c.names[! c.names %in% input$CIO_flowbetween_compartment_out]
  num_flow_to_add <- as.numeric(input$CIO_flowbetween_number_split) - 1
  if(input$CIO_flowbetween_split && num_flow_to_add > 0) {
    div(
      lapply(seq(num_flow_to_add), function(i){
        pickerInput(
          inputId = paste0("CIO_flowbetween_compartment_in_", 
                           as.character(i+1)),
          label = "Flow Into",
          choices = in.choices
        )
      })
    )
  }
})

output$CIO_flow_between_render_species <- renderUI({
  num_flow_to_add <- as.numeric(input$CIO_flowbetween_number_split) - 1
  if(input$CIO_flowbetween_split && num_flow_to_add > 0) {
    div(
      lapply(seq(num_flow_to_add), function(i){
        pickerInput(
          inputId = paste0("CIO_flowbetween_species_in_",
                           as.character(i+1)),
          label = paste0("Species In ", as.character(i+1)),
          choices = unlist(rv.SPECIES$species.df %>%
                dplyr::filter(Compartment %in%
                   eval(parse(
                     text =
                       paste0("input$CIO_flowbetween_compartment_in_",
                              as.character(i + 1))
                   ))) %>%
                select(Name),
                use.names = FALSE)
        )
      })
    )
  }
})

output$CIO_flow_between_render_flow_variables <- renderUI({
  num_flow_to_add <- as.numeric(input$CIO_flowbetween_number_split) - 1
  if(input$CIO_flowbetween_split && num_flow_to_add > 0) {
    div(
      lapply(seq(num_flow_to_add), function(i){
        textInput(
          inputId = paste0("CIO_flowbetween_flow_variable_in_",
                           as.character(i + 1)),
          label = paste0("Flow Variable ", as.character(i + 1)),
          value = "",
          placeholder = paste0("F_", as.character(i + 1))
        )
      })
    )
  }
})

output$CIO_flow_between_render_flow_values <- renderUI({
  num_flow_to_add <- as.numeric(input$CIO_flowbetween_number_split) - 1
  to.units <- paste0("Flow Value (",
                     rv.UNITS$units.selected$Volume, 
                     "/", 
                     rv.UNITS$units.selected$Duration, 
                     ")"
                     )
  if(input$CIO_flowbetween_split && num_flow_to_add > 0) {
    div(
      lapply(seq(num_flow_to_add), function(i){
        textInput(
          inputId = paste0("CIO_flowbetween_flow_value_in_",
                           as.character(i + 1)),
          label = to.units,
          value = 1
        )
      })
    )
  }
})


# Events -----------------------------------------------------------------------
## Flow In ---------------------------------------------------------------------
observeEvent({input$CIO_flow_in_compartment
  rv.SPECIES$species.df
  rv.COMPARTMENTS$compartments}, {
    req(!is_empty(rv.SPECIES$species.df))
    
    for.choice <- 
      rv.SPECIES$species.df %>% 
      filter(Compartment == input$CIO_flow_in_compartment) %>%
      select(Name)
    
    for.choice <- unlist(for.choice, use.names = FALSE)
    
    updatePickerInput(session, 
                      "CIO_flow_in_species", 
                      choices = for.choice)
  })

## Flow out --------------------------------------------------------------------
observeEvent({input$CIO_flow_out_compartment
  rv.SPECIES$species.df
  rv.COMPARTMENTS$compartments}, {
    req(!is_empty(rv.SPECIES$species.df))
    
    for.choice <- 
      rv.SPECIES$species.df %>% 
      filter(Compartment == input$CIO_flow_out_compartment) %>%
      select(Name)
    for.choice <- unlist(for.choice, use.names = FALSE)
    

    updatePickerInput(session, 
                      "CIO_flow_out_species", 
                      choices = for.choice)
  })

## Flow between - out ----------------------------------------------------------
observeEvent({input$CIO_flowbetween_compartment_out
  rv.SPECIES$species.df
  rv.COMPARTMENTS$compartments}, {
    req(!is_empty(rv.SPECIES$species.df))
    # For out species
    for.choice <- 
      rv.SPECIES$species.df %>% 
      filter(Compartment == input$CIO_flowbetween_compartment_out) %>%
      select(Name)
    for.choice <- unlist(for.choice, use.names = FALSE)
    
    updatePickerInput(session, 
                      "CIO_flowbetween_species_out", 
                      choices = for.choice)
    
    # For into compartment 1 (that is hard coded)
    # Want to remove the selected compartment out from choices in
    c.names <- rv.COMPARTMENTS$compartments.names
    c.1.choices <- c.names[! c.names %in% input$CIO_flowbetween_compartment_out]
    # if one compartment this will be empty and throw error
    #c.1.choices <- ifelse(is_empty(c.1.choices), NULL, c.1.choices)
    updatePickerInput(session, 
                      "CIO_flowbetween_compartment_in_1",
                      choices = c.1.choices)
  })

## Flow between - in -----------------------------------------------------------
observeEvent({input$CIO_flowbetween_compartment_in_1
  rv.SPECIES$species.df
  rv.COMPARTMENTS$compartments}, {
  req(!is_empty(rv.SPECIES$species.df))
  if (is.null(input$CIO_flowbetween_compartment_in_1)) {
    for.choice <- NULL
  } else {
    for.choice <-
      rv.SPECIES$species.df %>%
      filter(Compartment == input$CIO_flowbetween_compartment_in_1) %>%
      select(Name) %>%
      unlist(use.names = FALSE)
  }
  
  updatePickerInput(session,
                    "CIO_flowbetween_species_in_1",
                    choices = for.choice)
})

## Clearance -------------------------------------------------------------------
observeEvent({input$CIO_clearance_compartment
  rv.SPECIES$species.df
  rv.COMPARTMENTS$compartments}, {
    req(!is_empty(rv.SPECIES$species.df))
    
    for.choice <- 
      rv.SPECIES$species.df %>% 
      dplyr::filter(Compartment == input$CIO_clearance_compartment) %>%
      dplyr::select(Name)
    for.choice <- unlist(for.choice, use.names = FALSE)
    
    updatePickerInput(session, 
                      "CIO_clearance_species", 
                      choices = for.choice,
                      selected = for.choice[1])
  })

## Simple Diffusion ------------------------------------------------------------
observeEvent({input$CIO_simpdiff_compartment1
  rv.SPECIES$species.df
  rv.COMPARTMENTS$compartments}, {
    req(!is_empty(rv.SPECIES$species.df))
    
    for.choice <- 
      rv.SPECIES$species.df %>% 
      dplyr::filter(Compartment == input$CIO_simpdiff_compartment1) %>%
      select(Name)
    for.choice <- unlist(for.choice, use.names = FALSE)
    
    updatePickerInput(session, 
                      "CIO_simpdiff_species1", 
                      choices = for.choice)
  })

observeEvent({input$CIO_simpdiff_compartment2
  rv.SPECIES$species.df
  rv.COMPARTMENTS$compartments}, {
    req(!is_empty(rv.SPECIES$species.df))
    
    for.choice <- 
      rv.SPECIES$species.df %>% 
      dplyr::filter(Compartment == input$CIO_simpdiff_compartment2) %>%
      select(Name)
    for.choice <- unlist(for.choice, use.names = FALSE)
    
    updatePickerInput(session,
                      "CIO_simpdiff_species2", 
                      choices = for.choice)
  })

## Facilitated Diffusion ------------------------------------------------------
observeEvent({input$CIO_facilitatedDiff_compartment1
  rv.SPECIES$species.df
  rv.COMPARTMENTS$compartments}, {
    req(!is_empty(rv.SPECIES$species.df))

    for.choice <- 
      rv.SPECIES$species.df %>% 
      dplyr::filter(Compartment == input$CIO_facilitatedDiff_compartment1) %>%
      select(Name)
    for.choice <- unlist(for.choice, use.names = FALSE)
    
    updatePickerInput(session, 
                      "CIO_facilitatedDiff_species1", 
                      choices = for.choice)
  })

observeEvent({input$CIO_facilitatedDiff_compartment2
  rv.SPECIES$species.df
  rv.COMPARTMENTS$compartments}, {
    req(!is_empty(rv.SPECIES$species.df))
    
    for.choice <- 
      rv.SPECIES$species.df %>% 
      dplyr::filter(Compartment == input$CIO_facilitatedDiff_compartment2) %>%
      select(Name)
    for.choice <- unlist(for.choice, use.names = FALSE)
    
    updatePickerInput(session, 
                      "CIO_facilitatedDiff_species2", 
                      choices = for.choice)
  })


# ADD IO -----------------------------------------------------------------------

observeEvent(input$CIO_add_IO, {
  
  # Dataframe Storage
  in.or.out <- NA
  type      <- NA  # Type of Input/Output 
  c.out     <- NA  # Compartment from
  c.in      <- NA  # Compartment to
  s.out     <- NA  # Species from
  s.in      <- NA  # Species to
  flow.rate <- NA  # Flow Rate Constant
  flow.unit <- NA  # Flow Rate Unit
  flow.spec <- NA  # Species in Flow
  sol.const <- NA  # Solubility Constant (PS)
  sol.unit  <- NA
  fac.Vmax  <- NA  # Facilitated Diffusion Vmax
  fac.Km    <- NA  # Facilitated Diffusion Km
  fac.Vmax.u<- NA
  fac.Km.u  <- NA
  log       <- NA
  
  # Parameter Storage
  p.add  <- c()
  d.add  <- c()
  u.add  <- c()
  ud.add <- c()
  b.unit <- c()
  b.val  <- c()
  f.val  <- c()

  ## Flow In -------------------------------------------------------------------
  if (input$CIO_IO_options == "FLOW_IN") {
    in.or.out <- "In"
    type      <- input$CIO_IO_options
    c.in      <- input$CIO_flow_in_compartment
    s.in      <- input$CIO_flow_in_species
    flow.rate <- input$CIO_flow_in_rate_constant
    flow.unit <- paste0(rv.UNITS$units.selected$Volume, "/",
                        rv.UNITS$units.selected$Duration)
    log       <- paste0("Flow into compartment (",
                        c.in,
                        ") with species (",
                        s.in, 
                        ") at rate of ",
                        flow.rate, " ", flow.unit, ".")
    
    b.u  <- paste0(rv.UNITS$units.base$Volume, "/", rv.UNITS$units.base$Duration)
    
    u.d  <- "volume <div> time"
    d    <- paste0("Flow rate into compartment ",
                   c.in)
    
    
    
    p.add  <- c(p.add, flow.rate)
    d.add  <- c(d.add, d)
    u.add  <- c(u.add, flow.unit)
    ud.add <- c(ud.add, u.d)
    b.unit <- c(b.unit, b.u)
    b.val  <- c(b.val, 0)

  } else if (input$CIO_IO_options == "FLOW_OUT") {
  ## Flow out ------------------------------------------------------------------
    in.or.out <- "Out"
    type      <- input$CIO_IO_options
    c.out    <- input$CIO_flow_out_compartment
    s.out    <- input$CIO_flow_out_species
    flow.rate   <- input$CIO_flow_out_rate_constant
    flow.unit <- paste0(rv.UNITS$units.selected$Volume, "/",
                        rv.UNITS$units.selected$Duration)
    log       <- paste0("Flow out of compartment (",
                        c.out,
                        ") with species (",
                        s.out, 
                        ") at rate of ",
                        flow.rate, " ", flow.unit, ".")
    
    b.u  <- paste0(rv.UNITS$units.base$Volume, "/", rv.UNITS$units.base$Duration)
    
    u.d  <- "volume <div> time"
    d    <- paste0("Flow rate out of compartment ",
                   c.out)
    
    p.add  <- c(p.add, flow.rate)
    d.add  <- c(d.add, d)
    u.add  <- c(u.add, flow.unit)
    ud.add <- c(ud.add, u.d)
    b.unit <- c(b.unit, b.u)
    b.val  <- c(b.val, 0)
    
  } else if (input$CIO_IO_options == "FLOW_BETWEEN") {
  ## Flow between --------------------------------------------------------------
    #browser()
    in.or.out <- "Both"
    type      <- input$CIO_IO_options
    # Out Flow Components
    c.out     <- input$CIO_flowbetween_compartment_out
    s.out     <- input$CIO_flowbetween_species_out
    f.out     <- input$CIO_flowbetween_flow_variable_out
    f.v       <- input$CIO_flowbetween_flow_value_out
    d         <- paste0("Flow rate out from ",c.out)
    f.u       <- paste0(rv.UNITS$units.selected$Volume, "/",
                        rv.UNITS$units.selected$Duration)
    b.u       <- paste0(rv.UNITS$units.base$Volume, "/",
                        rv.UNITS$units.base$Duration)
    u.d       <- "volume <div> time"
    
    # Convert base unit if needed
    if (f.u != b.u) {
      b.v <- UnitConversion(u.d, f.u, b.u, as.numeric(f.v))
    } else {
      b.v <- f.v
    }
    
    p.add  <- c(p.add, f.out)
    d.add  <- c(d.add, d)
    f.val  <- c(f.val,f.v)
    u.add  <- c(u.add, f.u)
    ud.add <- c(ud.add, u.d)
    b.unit <- c(b.unit, b.u)
    b.val  <- c(b.val, b.v)
    
    
    # In Flow Components (this could have multiple components)
    if (!input$CIO_flowbetween_split) {
      # No Splits
      c.in  <- input$CIO_flowbetween_compartment_in_1
      s.in  <- input$CIO_flowbetween_species_in_1
      log   <- paste0("Flow between compartments ", 
                      c.out, " and ", c.in,
                      " at flow of ", f.out, ".")
      flow.rate <- f.out
    } else {
      # Input Flow is Split into Multiple Flows
      c.in <- c()
      s.in <- c()
      f.in <- c()
      f.u  <- c()
      f.v  <- c()
      b.u  <- c()
      u.d  <- c()
      d    <- c()
      b.v  <- c()
      n.split <- input$CIO_flowbetween_number_split
      for (i in seq(n.split)) {
        c.in <- 
          c(c.in, 
            eval(parse(text = paste0("input$CIO_flowbetween_compartment_in_",
                                     as.character(i))))
            )
        s.in <- 
          c(s.in, 
            eval(parse(text = paste0("input$CIO_flowbetween_species_in_",
                                     as.character(i))))
          )
        f.in <- 
          c(f.in, 
            eval(parse(text = paste0("input$CIO_flowbetween_flow_variable_in_",
                                     as.character(i))))
          )
        
        f.v <- 
          c(f.v,
            eval(parse(text = paste0("input$CIO_flowbetween_flow_value_in_",
                                     as.character(i)))))
          
        
        b.u  <- c(b.u, paste0(rv.UNITS$units.base$Volume, "/",
                      rv.UNITS$units.base$Duration))
        f.u  <- c(f.u, paste0(rv.UNITS$units.selected$Volume, "/",
                      rv.UNITS$units.selected$Duration))
        u.d  <- c(u.d, "volume <div> time")
        d    <- c(d, paste0("Flow rate from ",c.out, " to ", c.in[i]))
        
        # Convert base unit if needed
        cur.idx <- length(b.u)
        if (f.u[cur.idx] != b.u[cur.idx]) {
          b.v <- c(b.v, UnitConversion(u.d[cur.idx], 
                                       f.u[cur.idx], 
                                       b.u[cur.idx], 
                                       as.numeric(f.v[cur.idx]
                                                  )))
        } else {
          b.v <- c(b.v, f.v[cur.idx])
        }
      }
    
      flow.rate <- c(f.out, f.in)
      flow.rate <- paste0(flow.rate, collapse = " ")
      c.in      <- paste0(c.in, collapse = " ")
      s.in      <- paste0(s.in, collapse = " ")
      
      log       <- paste0("Flow between compartments.")
      
      p.add  <- c(p.add, f.in)
      d.add  <- c(d.add, d)
      f.val  <- c(f.val,f.v)
      u.add  <- c(u.add, f.u)
      ud.add <- c(ud.add, u.d)
      b.unit <- c(b.unit, b.u)
      b.val  <- c(b.val, b.v)
    }
  } else if (input$CIO_IO_options == "CLEARANCE") {
  ## Clearance -----------------------------------------------------------------
    in.or.out <- "Out"
    type      <- input$CIO_IO_options
    c.out     <- input$CIO_clearance_compartment
    s.out     <- input$CIO_clearance_species
    flow.rate <- input$CIO_clearance_rate_constant
    flow.unit <- paste0("1/", rv.UNITS$units.selected$Duration)
    log       <- paste0("Clearance of ",
                        paste0(input$CIO_clearance_species, collapse = ", "),
                        " by flow rate of ",
                        flow.rate, " (", flow.unit, ").")
    
    b.u  <- paste0("1/", rv.UNITS$units.base$Duration)
    
    u.d  <- "num <div> time"
    d    <- paste0("Clearance rate constant for ",
                   s.out, 
                   " of compartment ", 
                   c.out)
    
    p.add  <- c(p.add, flow.rate)
    d.add  <- c(d.add, d)
    u.add  <- c(u.add, flow.unit)
    ud.add <- c(ud.add, u.d)
    b.unit <- c(b.unit, b.u)
    b.val  <- c(b.val, 0)
    
  } 
    else if (input$CIO_IO_options == "SIMPDIFF") {
  ## Simple Diffusion ----------------------------------------------------------
    in.or.out <- "Both"
    type      <- input$CIO_IO_options
    c.out     <- input$CIO_simpdiff_compartment1
    c.in      <- input$CIO_simpdiff_compartment2
    s.out     <- input$CIO_simpdiff_species1
    s.in      <- input$CIO_simpdiff_species2
    sol.const <- input$CIO_simpdiff_rate_constant
    sol.unit  <- paste0(rv.UNITS$units.selected$Volume, "/",
                        rv.UNITS$units.selected$Duration)
    log       <- paste0("Simple Diffusion of ",
                        s.out,
                        " to ",
                        s.in,
                        " from compartment ",
                        c.out, " to ", c.in)
    
    # Parameter Storage
    sol.b.u  <- paste0(rv.UNITS$units.base$Volume, "/", 
                       rv.UNITS$units.base$Duration)
    
    sol.u.d  <- "volume <div> time"
    sol.d    <- paste0("Solubility constant for the simple diffusion of ",
                       s.out, 
                       " to ", 
                       s.in)
    
    p.add  <- c(p.add, sol.const)
    d.add  <- c(d.add, sol.d)
    u.add  <- c(u.add, sol.unit)
    ud.add <- c(ud.add, sol.u.d)
    b.unit <- c(b.unit, sol.b.u)
    b.val  <- c(b.val, 0)
    
  } 
  else if (input$CIO_IO_options == "FACILITATED_DIFF") {
  ## Faciliated Diffusion ------------------------------------------------------
    in.or.out <- "Both"
    type      <- input$CIO_IO_options
    c.out     <- input$CIO_facilitatedDiff_compartment1
    c.in      <- input$CIO_facilitatedDiff_compartment2
    s.out     <- input$CIO_facilitatedDiff_species1
    s.in      <- input$CIO_facilitatedDiff_species2
    fac.Vmax  <- input$CIO_facilitatedDiff_Vmax
    fac.Km    <- input$CIO_facilitatedDiff_Km
    fac.Vmax.u<- NA
    fac.Km.u  <- NA
    log       <- paste0("Facilated Diffusion of ",
                        s.out,
                        " to ",
                        s.in,
                        " from compartment ",
                        c.out, " to ", c.in)
    
    Km.unit    <- rv.UNITS$units.selected$For.Var
    Km.b.u     <- rv.UNITS$units.base$For.Var
    Km.unit.d  <- paste0("conc (",input$GO_species_unit_choice, ")")
    
    Km.d <- paste0("Michaelis Menten constant for the ", 
                   "facilitated Diffusion of ",
                   s.out,
                   " to ",
                   s.in)
    
    Vmax.unit <- paste0(rv.UNITS$units.selected$For.Var, "/",
                        rv.UNITS$units.selected$Duration)
    Vmax.b.u  <- paste0(rv.UNITS$units.base$For.Var, "/",
                        rv.UNITS$units.base$Duration)
    Vmax.u.d  <- paste0("conc (",
                        input$GO_species_unit_choice,
                        ") <div> time")
    
    Vmax.d <- paste0("Maximum velocity for the facilitated Diffusion of ",
                     s.out,
                     " to ",
                     s.in
    )
    
    p.add  <- c(p.add, fac.Vmax, fac.Km)
    d.add  <- c(d.add, Vmax.d, Km.d)
    u.add  <- c(u.add, Vmax.unit, Km.unit)
    ud.add <- c(ud.add, Vmax.u.d, Km.unit.d)
    b.unit <- c(b.unit, Vmax.b.u, Km.b.u)
    b.val  <- c(b.val, 0, 0)
  }
  # browser()
  ## Store/Error Check ---------------------------------------------------------
  error.check <- CheckParametersForErrors(p.add, 
                                          rv.SPECIES$species.names,
                                          names(rv.PARAMETERS$parameters),
                                          allowRepeatParams = TRUE)
  
  passed.error.check <- error.check[[1]]
  param.already.defined <- error.check[[2]]
  if (passed.error.check) {
    par.id.2.store <-c()
    for (i in seq(length(p.add))) {
      if (!(p.add[i] %in% rv.PARAMETERS$parameters.names && param.already.defined)) {
        if (type == "FLOW_BETWEEN") {
          par.out <- BuildParameters(p.add[i],
                                     names(rv.PARAMETERS$parameters),
                                     rv.ID$id.param.seed,
                                     pValue = as.numeric(f.val[i]),
                                     pUnit = u.add[i],
                                     pUnitD = ud.add[i],
                                     pBaseUnit = b.unit[i],
                                     pBaseValue = as.numeric(b.val[i]),
                                     pDescription = d.add[i],
                                     pLocation = "Input/Output",
                                     pLocationNote = type)
        } else {
          par.out <- BuildParameters(p.add[i],
                                     names(rv.PARAMETERS$parameters),
                                     rv.ID$id.param.seed,
                                     pUnit = u.add[i],
                                     pUnitD = ud.add[i],
                                     pBaseUnit = b.unit[i],
                                     pBaseValue = as.numeric(b.val[i]),
                                     pDescription = d.add[i],
                                     pLocation = "Input/Output",
                                     pLocationNote = type)
        }
        StoreParameters(par.out)
        par.id.2.store <- c(par.id.2.store, par.out["par.id"])
      } else {
        print("Repeated Parameter, skipped parameter overwrite")
        # Find par Id to stores
        par.id.2.store <- c(par.id.2.store, FindId(p.add[i]))
      }
    }
    par.id.2.store <- paste(par.id.2.store, collapse = " ")
    
    # Find all species ID from species.out and species.in
    if (!(is.na(s.out)) & !(is.na(s.in))) {
      s.to.id <- c(strsplit(s.out, " ")[[1]], strsplit(s.in, " ")[[1]])
    } else if (!(is.na(s.out)) & is.na(s.in)) {
      s.to.id <- strsplit(s.out, " ")[[1]]
    } else if (is.na(s.out) & !(is.na(s.in))) {
      s.to.id <- strsplit(s.in, " ")[[1]]
    }
    
    var.ids <- c()
    for (species in s.to.id) {
      var.ids <- c(var.ids, FindId(species))
    }
    var.ids <- paste0(var.ids, collapse = " ")
    
    # Create Id
    ids <- GenerateId(rv.ID$id.io.seed, "io")
    unique.id <- ids[[2]]
    rv.ID$id.io.seed <- rv.ID$id.io.seed + 1
    
    to.list <- list("id" = unique.id,
                    "in.or.out" = in.or.out,
                    "type" = type,
                    "compartment.out" = c.out,
                    "compartment.in" = c.in,
                    "species.out" = s.out,
                    "species.in" = s.in,
                    "flow.rate" = flow.rate,
                    "flow.unit" = flow.unit,
                    "flow.species" = flow.spec,
                    "solubility.constant" = sol.const,
                    "solubility.unit" = sol.unit,
                    "FD.Vmax" = fac.Vmax,
                    "FD.Km" = fac.Km,
                    "FD.vmax.unit" = fac.Vmax.u,
                    "FD.Km.u" = fac.Km.u,
                    "parameter.id" = par.id.2.store,
                    "species.id" = var.ids
                    )
    print("adding io to df")
    print(length(rv.IO$InputOutput))
    #rv.IO$InputOutput[[length(rv.IO$InputOutput)+1]] <- to.list
    rv.IO$InputOutput[[unique.id]] <- to.list
    #names(rv.IO$InputOutput)[length(rv.IO$InputOutput)] <- unique.id
    print(length(rv.IO$InputOutput))
    
    rv.IO$IO.logs[length(rv.IO$IO.logs) + 1] <- log
    
    rv.IO$IO.id.counter <- rv.IO$IO.id.counter + 1
    
    updateTextInput(
      session = session, 
      inputId = "CIO_flow_in_rate_constant",
      value = paste0("F_in_", rv.IO$IO.id.counter)
    )
    
    # Close Modal Logic
    if (!input$checkbox_modal_io_keep_open) {
      toggleModal(
        session = session, 
        modalId = "modal_add_IO",
        toggle = "close"
      )
    }
  }
  
})


# Event Change: IO#rv.IO$InputOutput
observeEvent(rv.IO$InputOutput, {
  rv.IO$IO.df <- bind_rows(rv.IO$InputOutput)
  
  updatePickerInput(
    session = session,
    inputId = "PI_delete_select_io",
    choices = seq(length(rv.IO$InputOutput))
  )
})

# Delete IO Button -------------------------------------------------------------
observeEvent(input$modal_delete_io_button, {
  # browser()
  to.delete <- as.numeric(input$PI_delete_select_io)
  io.ids <- rv.IO$IO.df$id[to.delete]
  
  # Extract parameter ids used in removed equations
  parameter.ids <- rv.IO$InputOutput$parameter.id[to.delete]
  
  # Delete Io from Reactive Variables
  for (i in io.ids) {
    rv.IO$InputOutput[[i]] <- NULL
  }
  
  # Rebuild IO df
  rv.IO$IO.df <- bind_rows(rv.IO$InputOutput)
  
  # Remove Parameters from model if they are not located elsewhere
  pars.to.check <- c()
  for (par.ids in parameter.ids) {
    pars.to.check <- c(pars.to.check, strsplit(par.ids, " ")[[1]])
  }
  
  # Gather params from equations
  pars.in.eqns <- c()
  par.extraction <- rv.REACTIONS$reactions.df$Parameters.Id
  for (par.ids in par.extraction) {
    pars.in.eqns <- c(pars.in.eqns, strsplit(par.ids, " ")[[1]])
  }
  
  # Gather params from Input/Outputs
  pars.in.IO <- c()
  par.extraction <- rv.IO$IO.df$parameter.id
  for (par.ids in par.extraction) {
    pars.in.IO <- c(pars.in.IO, strsplit(par.ids, " ")[[1]])
  }
  
  # Join par vectors
  pars.in.model <- c(pars.in.eqns, pars.in.IO)
  
  # Check IO for parameters and other equations
  pars.to.remove <- c()
  for (i in pars.to.check) {
    # Check other equations
    if (!(i %in% pars.in.model)) {
      pars.to.remove <- c(pars.to.remove, i)
    }
  }
  
  # Remove Parameters
  for (p in pars.to.remove) {
    rv.PARAMETERS$parameters[[p]] <- NULL 
  }
  
  if (input$checkbox_modal_delete_io_keep_modal_active) {
    toggleModal(session,
                "modal_delete_io",
                toggle = "close")
  }
  
})

output$deleteIO_table_viewer <- renderRHandsontable({
  
  io.num <- as.numeric(input$PI_delete_select_io)
  myindex = io.num - 1
  
  to.show <- rv.IO$IO.df %>%
    select(type, 
           compartment.out, 
           compartment.in, 
           species.out, 
           species.in)
  
  colnames(to.show) <- c("Type",
                         "Compartment Out",
                         "Compartment In",
                         "Species Out",
                         "Species In")

  rhandsontable(to.show,
                myindex = myindex) %>%
    hot_cols(renderer = 
               "function(instance, td, row, col, prop, value, cellProperties) {
       Handsontable.renderers.TextRenderer.apply(this, arguments);
       if (instance.params) {
       mhrows = instance.params.myindex;
       mhrows = mhrows instanceof Array ? mhrows : [mhrows];
       }
       if (instance.params && mhrows.includes(row)) td.style.background = '#FFCCCB';
      }"
    )
})


# Logs -------------------------------------------------------------------------
output$CIO_IO_Logs <- renderText({
  
  if (length(rv.IO$IO.logs) < 1) {
    "Output Logs will appear here."
  } else {
    paste0("(", 
           seq(length(rv.IO$IO.logs)),
           ") ",
           rv.IO$IO.logs, 
           collapse = "<br>")
  }
})

# Table Render: IO -------------------------------------------------------------
output$createModel_IO_logs_table <- renderRHandsontable(
  
  if (length(rv.IO$IO.df) == 0) {
    temp <- data.frame(c("Logs for Input/Output will appear here."))
    temp <- transpose(temp)
    colnames(temp) <- c("Instructions")
    rhandsontable(temp,
                  rowHeaders = NULL,
                  colHeaderWidth = 100,
                  stretchH = "all",
                  readOnly = TRUE
    )
  } else {
    to.show <- rv.IO$IO.df %>%
      select(type, 
             compartment.out, 
             compartment.in, 
             species.out, 
             species.in)
    
    colnames(to.show) <- c("Type",
                           "Compartment Out",
                           "Compartment In",
                           "Species Out",
                           "Species In")
    
    rhandsontable(to.show, 
                  width = "100%",
                  readOnly = TRUE,
                  stretchH = "all",
                  fillHandle = FALSE) %>%
      hot_cols(
        manualColumnMove = FALSE,
        manualColumnResize = TRUE,
        halign = "htCenter",
        valign = "htMiddle",
        renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (row % 2 == 0) {
              td.style.background = '#f9f9f9';
             } else {
              td.style.background = 'white';
             };
           }"
      ) %>%
      hot_context_menu(
        allowRowEdit = FALSE,
        allowColEdit = FALSE
      )
  }
)
