
# Update UI --------------------------------------------------------------------

## Flow In ---------------------------------------------------------------------
observeEvent({input$CIO_flow_in_compartment
              input$createVar_addVarToList
              input$createVar_add_compartment}, {
  req(!is_empty(vars$var.df))
  
  for.choice <- 
    vars$var.df %>% filter(Compartment == input$CIO_flow_in_compartment) %>%
    select(Name)
  
  for.choice <- unlist(for.choice, use.names = FALSE)
  
  updatePickerInput(session, 
                    "CIO_flow_in_species", 
                    choices = for.choice)
  })

## Flow out --------------------------------------------------------------------
observeEvent({input$CIO_flow_out_compartment
              input$createVar_addVarToList
              input$createVar_add_compartment}, {
  req(!is_empty(vars$var.df))
    
  for.choice <- 
    vars$var.df %>% 
      filter(Compartment == input$CIO_flow_out_compartment) %>%
      select(Name)
  for.choice <- unlist(for.choice, use.names = FALSE)
  
  updatePickerInput(session, 
                    "CIO_flow_out_species", 
                    choices = for.choice)
  })

## Flow between - out ----------------------------------------------------------
observeEvent({input$CIO_flowbetween_compartment_out
            input$createVar_addVarToList
            input$createVar_add_compartment}, {
  req(!is_empty(vars$var.df))
  
  for.choice <- 
    vars$var.df %>% 
      filter(Compartment == input$CIO_flowbetween_compartment_out) %>%
      select(Name)
  for.choice <- unlist(for.choice, use.names = FALSE)
  
  updatePickerInput(session, 
                    "CIO_flowbetween_species", 
                    choices = for.choice)
  })

# Flow between - in - need to separate species for this one
# observeEvent({input$CIO_flow_compartment_out
#   input$createVar_addVarToList
#   input$createVar_add_compartment}, {
#     req(!is_empty(vars$var.df))
#     
#     for.choice <- 
#       vars$var.df %>% filter(Compartment == input$CIO_flow_compartment_out) %>%
#       select(Name)
#     
#     updatePickerInput(session, "CIO_flowbetween_species", for.choice)
#   })

## Clearance -------------------------------------------------------------------
observeEvent({input$CIO_clearance_compartment
              input$createVar_addVarToList
              input$createVar_add_compartment}, {
  req(!is_empty(vars$var.df))
  
  for.choice <- 
    vars$var.df %>% 
    dplyr::filter(Compartment == input$CIO_clearance_compartment) %>%
      select(Name)
  for.choice <- unlist(for.choice, use.names = FALSE)
  
  updatePickerInput(session, 
                    "CIO_clearance_species", 
                    choices = for.choice,
                    selected = for.choice[1])
})

## Simple Diffusion ------------------------------------------------------------
observeEvent({input$CIO_simpdiff_compartment1
              input$createVar_addVarToList
              input$createVar_add_compartment}, {
    req(!is_empty(vars$var.df))
    
  for.choice <- 
    vars$var.df %>% 
    dplyr::filter(Compartment == input$CIO_simpdiff_compartment1) %>%
    select(Name)
  for.choice <- unlist(for.choice, use.names = FALSE)
  
  updatePickerInput(session, 
                    "CIO_simpdiff_species1", 
                    choices = for.choice)
})

observeEvent({input$CIO_simpdiff_compartment2
              input$createVar_addVarToList
              input$createVar_add_compartment}, {
    req(!is_empty(vars$var.df))
    
    for.choice <- 
      vars$var.df %>% 
      dplyr::filter(Compartment == input$CIO_simpdiff_compartment2) %>%
      select(Name)
    for.choice <- unlist(for.choice, use.names = FALSE)
    
    updatePickerInput(session,
                      "CIO_simpdiff_species2", 
                      choices = for.choice)
})

## Facilitated Diffusion ------------------------------------------------------
observeEvent({input$CIO_facilitatedDiff_compartment1
              input$createVar_addVarToList
              input$createVar_add_compartment}, {
  req(!is_empty(vars$var.df))
  
  for.choice <- 
    vars$var.df %>% 
    dplyr::filter(Compartment == input$CIO_facilitatedDiff_compartment1) %>%
    select(Name)
  for.choice <- unlist(for.choice, use.names = FALSE)
  
  updatePickerInput(session, 
                    "CIO_facilitatedDiff_species1", 
                    choices = for.choice)
  })

observeEvent({input$CIO_facilitatedDiff_compartment2
              input$createVar_addVarToList
              input$createVar_add_compartment}, {
  req(!is_empty(vars$var.df))
  
  for.choice <- 
    vars$var.df %>% 
    dplyr::filter(Compartment == input$CIO_facilitatedDiff_compartment2) %>%
    select(Name)
  for.choice <- unlist(for.choice, use.names = FALSE)
  
  updatePickerInput(session, 
                    "CIO_facilitatedDiff_species2", 
                    choices = for.choice)
  })

## Flows Update Compartment PickerInputs----------------------------------------
observeEvent(vars$compartments.info, {
  
  c.names <- names(vars$compartments.info)
  
  # Flow In
  updatePickerInput(session, 
                    "CIO_flow_in_compartment", 
                    choices = c.names)
  
  # Flow Out
  updatePickerInput(session, 
                    "CIO_flow_out_compartment", 
                    choices = c.names)
  
  # Flow Between
  updatePickerInput(session, 
                    "CIO_flowbetween_compartment_out", 
                    choices = c.names)
  updatePickerInput(session, 
                    "CIO_flowbetween_compartment_in",
                    choices = c.names)
  
  # Clearance
  updatePickerInput(session, 
                    "CIO_clearance_compartment", 
                    choices = c.names)
  
  #Simple Diffusion
  updatePickerInput(session, 
                    "CIO_simpdiff_compartment1", 
                    choices = c.names)
  updatePickerInput(session, 
                    "CIO_simpdiff_compartment2", 
                    choices = c.names)
  
  # Facillitated Diffusion
  updatePickerInput(session, 
                    "CIO_facilitatedDiff_compartment1", 
                    choices = c.names)
  updatePickerInput(session, 
                    "CIO_facilitatedDiff_compartment2", 
                    choices = c.names)
  
})

# Compartment IO Add -----------------------------------------------------------

observeEvent(input$CIO_add_IO, {
  
  # Dataframe Storage
  in.or.out <- NA
  type      <- NA  # Type of Input/Output 
  c.from    <- NA  # Compartment from
  c.to      <- NA  # Compartment to
  s.from    <- NA  # Species from
  s.to      <- NA  # Species to
  flow.rate <- NA
  flow.unit <- NA
  flow.spec <- NA
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
  
  # Check what type of IO is being used. 
  if (input$CIO_IO_options == "FLOW_IN") {
    in.or.out <- "In"
    type      <- input$CIO_IO_options
    c.to      <- input$CIO_flow_in_compartment
    s.to      <- input$CIO_flow_in_species
    flow.rate <- input$CIO_flow_in_rate
    flow.unit <- units$selected.units$Flow
    log       <- paste0("Flow into compartment (",
                        c.to,
                        ") with species (",
                        s.to, 
                        ") at rate of ",
                        flow.rate, " ", flow.unit, ".")
  } else if (input$CIO_IO_options == "FLOW_OUT") {
    in.or.out <- "Out"
    type      <- input$CIO_IO_options
    c.from    <- input$CIO_flow_out_compartment
    s.from    <- input$CIO_flow_out_species
    flow.rate <- input$CIO_flow_out_rate
    flow.unit <- units$selected.units$Flow
    log       <- paste0("Flow out of compartment (",
                        c.from,
                        ") with species (",
                        s.from, 
                        ") at rate of ",
                        flow.rate, " ", flow.unit, ".")
    
  } else if (input$CIO_IO_options == "FLOW_BETWEEN") {
    in.or.out <- "Both"
    type      <- input$CIO_IO_options
    c.in      <- input$CIO_flow_compartment_in
    c.out     <- input$CIO_flow_compartment_out
    flow.rate <- input$CIO_flow_rate
    flow.unit <- units$selected.units$Flow
    flow.spec <- paste0(input$CIO_flow_species, collapse = " ")
    log       <- paste0("Flow of Species (",
                        paste0(input$CIO_flow_species, collapse = ", "),
                        ") at rate ",
                        flow.rate,
                        " (", flow.unit, ") ",
                        "between compartments: ",
                        c.out, " & ", c.in, ".")
    
  } else if (input$CIO_IO_options == "CLEARANCE") {
    in.or.out <- "Out"
    type      <- input$CIO_IO_options
    c.out     <- input$CIO_flow_compartment_out
    flow.rate <- input$CIO_clearance_rate
    flow.unit <- units$selected.units$Flow
    flow.spec <- paste0(input$CIO_clearance_species, collapse = " ")
    log       <- paste0("Clearance of ",
                        paste0(input$CIO_clearance_species, collapse = ", "),
                        " by flow rate of ",
                        flow.rate, " (", flow.unit, ").")
  } else if (input$CIO_IO_options == "SIMPDIFF") {
    in.or.out <- "Both"
    type      <- input$CIO_IO_options
    c.out     <- input$CIO_simpdiff_compartment1
    c.in      <- input$CIO_simpdiff_compartment2
    s.out     <- input$CIO_simpdiff_species1
    s.in      <- input$CIO_simpdiff_species2
    sol.const <- input$CIO_simpdiff_rate
    sol.unit  <- units$selected.units$Flow
    log       <- paste0("Simple Diffusion of ",
                        s.out,
                        " to ",
                        s.in,
                        " from compartment ",
                        c.out, " to ", c.in)
  } else if (input$CIO_IO_options == "FACILITATED_DIFF") {
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
    
    Km.unit    <- units$selected.units$For.Var
    Km.b.u     <- units$base.units$For.Var
    Km.unit.d  <- paste0("conc (",input$GO_species_unit_choice, ")")
    
    Km.d <- paste0("Michaelis Menten constant for the ", 
                   "facilitated Diffusion of ",
                   s.out,
                   " to ",
                   s.in)
    
    Vmax.unit <- paste0(units$selected.units$For.Var, "/",
                        units$selected.units$Duration)
    Vmax.b.u  <- paste0(units$base.units$For.Var, "/",
                        units$base.units$Duration)
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
  
  passed.error.check <- CheckParametersForErrors(p.add, 
                                                 vars$species, 
                                                 params$vars.all)
  if (passed.error.check) {
    for (i in seq(length(p.add))) {
      par.out <- BuildParameters(p.add[i],
                                 params$vars.all,
                                 id$id.var.seed,
                                 pUnit = u.add[i],
                                 pUnitD = ud.add[i],
                                 pBaseUnit = b.unit[i],
                                 pBaseValue = b.val[i],
                                 pDescription = d.add[i],
                                 pLocation = "Input/Output",
                                 pLocationNote = type)
      StoreParameters(par.out)
    }
    
    row.to.df <- c(in.or.out,
                   type,
                   c.from,
                   c.to,
                   s.from,
                   s.to,
                   flow.rate,
                   flow.unit,
                   flow.spec,
                   sol.const,
                   sol.unit, 
                   fac.Vmax,
                   fac.Km, 
                   fac.Vmax.u,
                   fac.Km.u)
    IO$IO.df[nrow(IO$IO.df) + 1,] <- row.to.df
    print(" IO DF")
    print(IO$IO.df)
    
    IO$IO.logs[length(IO$IO.logs) + 1] <- log
  }

})

# Logs -------------------------------------------------------------------------
output$CIO_IO_Logs <- renderText({
  
  if (length(IO$IO.logs) < 1) {
    "Output Logs will appear here."
  } else {
    paste0("(", 
           seq(length(IO$IO.logs)),
           ") ",
           IO$IO.logs, 
           collapse = "<br>")
  }
})