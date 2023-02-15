# This file contains all inputs and output related server functions for species
# entering or leaving compartments.

# Update UI --------------------------------------------------------------------

# Render UI --------------------------------------------------------------------
output$CIO_flow_between_render_compartments <- renderUI({
  c.names <- vars$compartments.names
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
          choices = unlist(vars$var.df %>%
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
          value = ""
        )
      })
    )
  }
})

output$CIO_flow_between_render_flow_values <- renderUI({
  num_flow_to_add <- as.numeric(input$CIO_flowbetween_number_split) - 1
  if(input$CIO_flowbetween_split && num_flow_to_add > 0) {
    div(
      lapply(seq(num_flow_to_add), function(i){
        textInput(
          inputId = paste0("CIO_flowbetween_flow_value_in_",
                           as.character(i + 1)),
          label = "Flow Value (units)",
          value = 1
        )
      })
    )
  }
})


## Flow In ---------------------------------------------------------------------
observeEvent({input$CIO_flow_in_compartment
  vars$var.df
  vars$compartments.info}, {
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
  vars$var.df
  vars$compartments.info}, {
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
  vars$var.df
  vars$compartments.info}, {
    req(!is_empty(vars$var.df))
    # For out species
    for.choice <- 
      vars$var.df %>% 
      filter(Compartment == input$CIO_flowbetween_compartment_out) %>%
      select(Name)
    for.choice <- unlist(for.choice, use.names = FALSE)
    
    updatePickerInput(session, 
                      "CIO_flowbetween_species_out", 
                      choices = for.choice)
    
    # For into compartment 1 (that is hard coded)
    # Want to remove the selected compartment out from choices in
    c.names <- vars$compartments.names
    c.1.choices <- c.names[! c.names %in% input$CIO_flowbetween_compartment_out]
    # if one compartment this will be empty and throw error
    #c.1.choices <- ifelse(is_empty(c.1.choices), NULL, c.1.choices)
    updatePickerInput(session, 
                      "CIO_flowbetween_compartment_in_1",
                      choices = c.1.choices)
  })

# Flow between - in ------------------------------------------------------------
observeEvent({input$CIO_flowbetween_compartment_in_1
  vars$var.df
  vars$compartments.info}, {
  req(!is_empty(vars$var.df))
  print(vars$var.df)
  print(input$CIO_flowbetween_compartment_in_1)
  if (is.null(input$CIO_flowbetween_compartment_in_1)) {
    for.choice <- NULL
  } else {
    for.choice <-
      vars$var.df %>%
      filter(Compartment == input$CIO_flowbetween_compartment_in_1) %>%
      select(Name) %>%
      unlist(use.names = FALSE)
  }
  
  print(for.choice)

  updatePickerInput(session,
                    "CIO_flowbetween_species_in_1",
                    choices = for.choice)
})

## Clearance -------------------------------------------------------------------
observeEvent({input$CIO_clearance_compartment
  vars$var.df
  vars$compartments.info}, {
    req(!is_empty(vars$var.df))
    
    for.choice <- 
      vars$var.df %>% 
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
  vars$var.df
  vars$compartments.info}, {
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
  vars$var.df
  vars$compartments.info}, {
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
  vars$var.df
  vars$compartments.info}, {
    req(!is_empty(vars$var.df))
    print("EVENT CIO FACIL SPEC 1")
    print(vars$var.df)
    print(vars$var.df$Compartment)
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
  vars$var.df
  vars$compartments.info}, {
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


# IO Add -----------------------------------------------------------------------

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
  
  # Check what type of IO is being used. 
  if (input$CIO_IO_options == "FLOW_IN") {
    in.or.out <- "In"
    type      <- input$CIO_IO_options
    c.in      <- input$CIO_flow_in_compartment
    s.in      <- input$CIO_flow_in_species
    flow.rate <- input$CIO_flow_in_rate_constant
    flow.unit <- paste0(units$selected.units$Volume, "/",
                        units$selected.units$Duration)
    log       <- paste0("Flow into compartment (",
                        c.in,
                        ") with species (",
                        s.in, 
                        ") at rate of ",
                        flow.rate, " ", flow.unit, ".")
    
    b.u  <- paste0(units$base.units$Volume, "/", units$base.units$Duration)
    
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
    in.or.out <- "Out"
    type      <- input$CIO_IO_options
    c.out    <- input$CIO_flow_out_compartment
    s.out    <- input$CIO_flow_out_species
    flow.rate   <- input$CIO_flow_out_rate_constant
    flow.unit <- paste0(units$selected.units$Volume, "/",
                        units$selected.units$Duration)
    log       <- paste0("Flow out of compartment (",
                        c.out,
                        ") with species (",
                        s.out, 
                        ") at rate of ",
                        flow.rate, " ", flow.unit, ".")
    
    b.u  <- paste0(units$base.units$Volume, "/", units$base.units$Duration)
    
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
    #browser()
    in.or.out <- "Both"
    type      <- input$CIO_IO_options
    # Out Flow Components
    c.out     <- input$CIO_flowbetween_compartment_out
    s.out     <- input$CIO_flowbetween_species_out
    f.out     <- input$CIO_flowbetween_flow_variable_out
    f.v       <- input$CIO_flowbetween_flow_value_out
    d         <- paste0("Flow rate out from ",c.out)
    f.u       <- paste0(units$selected.units$Volume, "/",
                        units$selected.units$Duration)
    b.u       <- paste0(units$base.units$Volume, "/",
                        units$base.units$Duration)
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
          
        
        b.u  <- c(b.u, paste0(units$base.units$Volume, "/",
                      units$base.units$Duration))
        f.u  <- c(f.u, paste0(units$selected.units$Volume, "/",
                      units$selected.units$Duration))
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
    in.or.out <- "Out"
    type      <- input$CIO_IO_options
    c.out     <- input$CIO_clearance_compartment
    s.out     <- input$CIO_clearance_species
    flow.rate <- input$CIO_clearance_rate_constant
    flow.unit <- paste0("1/", units$selected.units$Duration)
    log       <- paste0("Clearance of ",
                        paste0(input$CIO_clearance_species, collapse = ", "),
                        " by flow rate of ",
                        flow.rate, " (", flow.unit, ").")
    
    b.u  <- paste0("1/", units$base.units$Duration)
    
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
    
  } else if (input$CIO_IO_options == "SIMPDIFF") {
    in.or.out <- "Both"
    type      <- input$CIO_IO_options
    c.out     <- input$CIO_simpdiff_compartment1
    c.in      <- input$CIO_simpdiff_compartment2
    s.out     <- input$CIO_simpdiff_species1
    s.in      <- input$CIO_simpdiff_species2
    sol.const <- input$CIO_simpdiff_rate_constant
    sol.unit  <- paste0(units$selected.units$Volume, "/",
                        units$selected.units$Duration)
    log       <- paste0("Simple Diffusion of ",
                        s.out,
                        " to ",
                        s.in,
                        " from compartment ",
                        c.out, " to ", c.in)
    
    # Parameter Storage
    sol.b.u  <- paste0(units$base.units$Volume, "/", 
                       units$base.units$Duration)
    
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
  # browser()
  error.check <- CheckParametersForErrors(p.add, 
                                          vars$species,
                                          names(params$params),
                                          allowRepeatParams = TRUE)
  
  passed.error.check <- error.check[[1]]
  param.already.defined <- error.check[[2]]
  if (passed.error.check) {
    for (i in seq(length(p.add))) {
      if (!(p.add[i] %in% names(params$params) && param.already.defined)) {
        if (type == "FLOW_BETWEEN") {
          par.out <- BuildParameters(p.add[i],
                                     names(params$params),
                                     id$id.param.seed,
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
                                     names(params$params),
                                     id$id.param.seed,
                                     pUnit = u.add[i],
                                     pUnitD = ud.add[i],
                                     pBaseUnit = b.unit[i],
                                     pBaseValue = as.numeric(b.val[i]),
                                     pDescription = d.add[i],
                                     pLocation = "Input/Output",
                                     pLocationNote = type)
        }
        
        StoreParameters(par.out)
      } else {
        print("Repeated Parameter, skipped parameter overwrite")
      }
      
    }
    
    # Create Id
    ids <- GenerateId(id$id.io.seed, "io")
    unique.id <- ids[[2]]
    id$id.io.seed <- id$id.io.seed + 1
    
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
                    "FD.Km.u" = fac.Km.u
                    )

    
    IO$IO.info[[length(IO$IO.info)+1]] <- to.list
    names(IO$IO.info)[length(IO$IO.info)] <- unique.id
    
    # row.to.df <- c(in.or.out,
    #                type,
    #                c.out,
    #                c.in,
    #                s.out,
    #                s.in,
    #                flow.rate,
    #                flow.unit,
    #                flow.spec,
    #                sol.const,
    #                sol.unit,
    #                fac.Vmax,
    #                fac.Km,
    #                fac.Vmax.u,
    #                fac.Km.u)
    # print(length(row.to.df))
    # print(row.to.df)
    # IO$IO.df[nrow(IO$IO.df) + 1,] <- row.to.df
    # print(" IO DF")
    # print(IO$IO.df)

    IO$IO.logs[length(IO$IO.logs) + 1] <- log
  }
  
})

observeEvent(IO$IO.info, {
  IO$IO.df <- bind_rows(IO$IO.info)
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