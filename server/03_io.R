# This file contains all inputs and output related server functions for species
# entering or leaving compartments.

# Update UI --------------------------------------------------------------------

# Render Text ------------------------------------------------------------------

# Flow in
output$CIO_fi_vo_text <- renderText(
  out <- paste0("Flow Value (",
                rv.UNITS$units.selected$Volume, 
                "/", 
                rv.UNITS$units.selected$Duration, 
                ")"
  )
)

# Flow out
output$CIO_fo_vo_text <- renderText(
  out <- paste0("Flow Value (",
                rv.UNITS$units.selected$Volume, 
                "/", 
                rv.UNITS$units.selected$Duration, 
                ")"
  )
)

# Flow between
output$CIO_fb_vo_text <- renderText(
  out <- paste0("Flow Value (",
                rv.UNITS$units.selected$Volume, 
                "/", 
                rv.UNITS$units.selected$Duration, 
                ")"
  )
)  

# Flow between 2
output$CIO_fb_sv1_text <- renderText(
  out <- paste0("Flow Value (",
                rv.UNITS$units.selected$Volume, 
                "/", 
                rv.UNITS$units.selected$Duration, 
                ")"
  )
)

# Clearance
output$CIO_clearance_unit_text <- renderText(
  out <- paste0("Value (",
                "1/",
                rv.UNITS$units.selected$Duration,
                ")")
)

# Simple Diffusion
output$CIO_simpdiff_unit_text <- renderText(
  out <- paste0("Value (",
                rv.UNITS$units.selected$Volume, "/",
                rv.UNITS$units.selected$Duration,
                ")")
)

# Facilitated Diffusion
output$CIO_fd_vmax_unit_text <- renderText(
  # Vmax 
  out <- paste0("Value (",
                rv.UNITS$units.selected$For.Var, "/",
                rv.UNITS$units.selected$Duration,
                ")")
)

output$CIO_fd_km_unit_text <- renderText(
  # Km
  out <- paste0("Value (",
                rv.UNITS$units.selected$For.Var,
                ")")
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
          value = paste0("F_in_",
                         rv.IO$IO.id.counter,
                         ".",
                         as.character(i + 1))
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
          value = 0
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
  direction <- NA
  type      <- NA  # Type of Input/Output 
  c.out     <- NA  # Compartment from
  c.in      <- NA  # Compartment to
  c.out.id  <- NA
  c.in.id   <- NA
  s.out     <- NA  # Species from
  s.in      <- NA  # Species to
  s.out.id  <- NA
  s.in.id   <- NA
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
    direction <- "Input"
    display   <- "Flow In"
    type      <- input$CIO_IO_options
    law       <- "flow"
    c.in      <- input$CIO_flow_in_compartment
    s.in      <- input$CIO_flow_in_species
    c.in.id   <- FindId(c.in)
    s.in.id   <- FindId(s.in)
    
    flow.rate <- input$CIO_flow_in_rate_constant
    flow.unit <- paste0(rv.UNITS$units.selected$Volume, "/",
                        rv.UNITS$units.selected$Duration)
    
    f.v <- input$CIO_flow_in_value
    log       <- paste0("Flow into compartment (",
                        c.in,
                        ") with species (",
                        s.in, 
                        ") at rate of ",
                        flow.rate, " ", flow.unit, ".")
    f.u       <- paste0(rv.UNITS$units.selected$Volume, "/",
                        rv.UNITS$units.selected$Duration)
    b.u  <- paste0(rv.UNITS$units.base$Volume, "/", 
                   rv.UNITS$units.base$Duration)
    
    u.d  <- "volume <div> time"
    d    <- paste0("Flow rate into compartment ",
                   c.in)
    # Convert base unit if needed
    if (f.u != b.u) {
      b.v <- UnitConversion(u.d, f.u, b.u, as.numeric(f.v))
    } else {
      b.v <- f.v
    }
    
    p.add  <- c(p.add, flow.rate)
    d.add  <- c(d.add, d)
    f.val  <- c(f.val,f.v)
    u.add  <- c(u.add, flow.unit)
    ud.add <- c(ud.add, u.d)
    b.unit <- c(b.unit, b.u)
    b.val  <- c(b.val, b.v)
    
    laws <- Flow(s.in, flow.rate)
    
    description <- paste0("Flow of ", 
                          s.in, 
                          " into ",
                          c.in,
                          " with rate, ",
                          flow.rate)
  } 
  else if (input$CIO_IO_options == "FLOW_OUT") {
  ## Flow out ------------------------------------------------------------------
    direction  <- "Output"
    display    <- "Flow In"
    type       <- input$CIO_IO_options
    law       <- "flow"
    c.out      <- input$CIO_flow_out_compartment
    s.out      <- input$CIO_flow_out_species
    c.out.id   <- FindId(c.out)
    s.out.id   <- FindId(s.out)
    
    flow.rate   <- input$CIO_flow_out_rate_constant
    flow.unit   <- paste0(rv.UNITS$units.selected$Volume, "/",
                          rv.UNITS$units.selected$Duration)
    
    f.v <- input$CIO_flow_in_value
    
    log       <- paste0("Flow out of compartment (",
                        c.out,
                        ") with species (",
                        s.out, 
                        ") at rate of ",
                        flow.rate, " ", flow.unit, ".")
    
    f.u       <- paste0(rv.UNITS$units.selected$Volume, "/",
                        rv.UNITS$units.selected$Duration)
    
    b.u  <- paste0(rv.UNITS$units.base$Volume, "/", 
                   rv.UNITS$units.base$Duration)
    
    u.d  <- "volume <div> time"
    d    <- paste0("Flow rate out of compartment ",
                   c.out)
    
    if (f.u != b.u) {
      b.v <- UnitConversion(u.d, f.u, b.u, as.numeric(f.v))
    } else {
      b.v <- f.v
    }
    
    p.add  <- c(p.add, flow.rate)
    d.add  <- c(d.add, d)
    f.val  <- c(f.val,f.v)
    u.add  <- c(u.add, flow.unit)
    ud.add <- c(ud.add, u.d)
    b.unit <- c(b.unit, b.u)
    b.val  <- c(b.val, b.v)
    
    laws <- Flow(s.out, flow.rate)
    
    description <- paste0("Flow of ", 
                          s.out, 
                          " out of ",
                          c.out,
                          " with rate, ",
                          flow.rate)
  } 
  else if (input$CIO_IO_options == "FLOW_BETWEEN") {
  ## Flow between --------------------------------------------------------------
    # browser()
    direction <- "Both"
    type      <- input$CIO_IO_options
    
    # Out Flow Components
    c.out     <- input$CIO_flowbetween_compartment_out
    s.out     <- input$CIO_flowbetween_species_out
    c.out.id   <- FindId(c.out)
    s.out.id   <- FindId(s.out)
    
    # Out Flow Parameter
    f.out     <- input$CIO_flowbetween_flow_variable_out
    f.v       <- input$CIO_flowbetween_flow_value_out
    d         <- paste0("Flow rate out from ", c.out)
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
      c.in.id   <- FindId(c.in)
      s.in.id   <- FindId(s.in)
      n.split <- 1
      f.in <- f.out
      log   <- paste0("Flow between compartments ", 
                      c.out, " and ", c.in,
                      " at flow of ", f.out, ".")
    } else {
      # Input Flow is Split into Multiple Flows
      c.in    <- c()
      s.in    <- c()
      c.in.id <- c()
      s.in.id <- c()
      f.in    <- c()
      f.u     <- c()
      f.v     <- c()
      b.u     <- c()
      u.d     <- c()
      d       <- c()
      b.v     <- c()
      n.split <- input$CIO_flowbetween_number_split
      # browser()
      for (i in seq(n.split)) {
        c.in <- 
          c(c.in, 
            eval(parse(text = paste0("input$CIO_flowbetween_compartment_in_",
                                     as.character(i))))
            )
        c.in.id <- c(c.in.id, FindId(c.in[i]))
        s.in <- 
          c(s.in, 
            eval(parse(text = paste0("input$CIO_flowbetween_species_in_",
                                     as.character(i))))
          )
        s.in.id <- c(s.in.id, FindId(s.in[i]))
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
      
      log       <- paste0("Flow between compartments.")
      
      p.add  <- c(p.add, f.in)
      d.add  <- c(d.add, d)
      f.val  <- c(f.val,f.v)
      u.add  <- c(u.add, f.u)
      ud.add <- c(ud.add, u.d)
      b.unit <- c(b.unit, b.u)
      b.val  <- c(b.val, b.v)
    }
  } 
  else if (input$CIO_IO_options == "CLEARANCE") {
  ## Clearance -----------------------------------------------------------------
    direction <- "Output"
    display    <- "Clearance"
    type      <- input$CIO_IO_options
    law       <- "clearance"
    c.out     <- input$CIO_clearance_compartment
    s.out     <- input$CIO_clearance_species
    c.out.id   <- FindId(c.out)
    s.out.id   <- FindId(s.out)
    
    flow.rate <- input$CIO_clearance_rate_constant
    flow.unit <- paste0("1/", rv.UNITS$units.selected$Duration)
    
    f.v <- input$CIO_clearance_value
    
    log       <- paste0("Clearance of ",
                        paste0(input$CIO_clearance_species, collapse = ", "),
                        " by flow rate of ",
                        flow.rate, " (", flow.unit, ").")
    
    f.u  <- paste0("1/", rv.UNITS$units.selected$Duration)
    b.u  <- paste0("1/", rv.UNITS$units.base$Duration)
    
    u.d  <- "num <div> time"
    d    <- paste0("Clearance rate constant for ",
                   s.out, 
                   " of compartment ", 
                   c.out)
    
    if (f.u != b.u) {
      b.v <- UnitConversion(u.d, f.u, b.u, as.numeric(f.v))
    } else {
      b.v <- f.v
    }
    
    p.add  <- c(p.add, flow.rate)
    d.add  <- c(d.add, d)
    f.val  <- c(f.val,f.v)
    u.add  <- c(u.add, flow.unit)
    ud.add <- c(ud.add, u.d)
    b.unit <- c(b.unit, b.u)
    b.val  <- c(b.val, b.v)
    
  } 
  else if (input$CIO_IO_options == "SIMPDIFF") {
  ## Simple Diffusion ----------------------------------------------------------
    direction <- "Both"
    type      <- input$CIO_IO_options
    c.out     <- input$CIO_simpdiff_compartment1
    c.in      <- input$CIO_simpdiff_compartment2
    s.out     <- input$CIO_simpdiff_species1
    s.in      <- input$CIO_simpdiff_species2
    
    # Store Ids
    c.in.id    <- FindId(c.in)
    s.in.id    <- FindId(s.in)
    c.out.id   <- FindId(c.out)
    s.out.id   <- FindId(s.out)
    
    # Parameter Storage
    # Parameter Variable
    sol.const <- input$CIO_simpdiff_rate_constant
    # Value
    f.v   <- input$CIO_simpdiff_rate_constant
    # Unit
    f.u   <- paste0(rv.UNITS$units.selected$Volume, "/",
                        rv.UNITS$units.selected$Duration)
    
    # Base Imot
    b.u  <- paste0(rv.UNITS$units.base$Volume, "/", 
                       rv.UNITS$units.base$Duration)
    
    # Unit Description
    u.d  <- "volume <div> time"
    
    # Base value determination
    if (f.u != b.u) {
      b.v <- UnitConversion(u.d, f.u, b.u, as.numeric(f.v))
    } else {
      b.v <- f.v
    }
    
    # Reaction Description
    sol.d    <- paste0("Solubility constant for the simple diffusion of ",
                       s.out, 
                       " to ", 
                       s.in)
    
    # Log Outputs
    log       <- paste0("Simple Diffusion of ",
                        s.out,
                        " to ",
                        s.in,
                        " from compartment ",
                        c.out, " to ", c.in)
    
    # Store to output vectors
    p.add  <- c(p.add, sol.const)
    d.add  <- c(d.add, sol.d)
    f.val  <- c(f.val,f.v)
    u.add  <- c(u.add, f.u)
    ud.add <- c(ud.add, u.d)
    b.unit <- c(b.unit, b.u)
    b.val  <- c(b.val, b.v)
    
  } 
  else if (input$CIO_IO_options == "FACILITATED_DIFF") {
  ## Facilitated Diffusion ------------------------------------------------------
    direction <- "Both"
    type      <- input$CIO_IO_options
    c.out     <- input$CIO_facilitatedDiff_compartment1
    c.in      <- input$CIO_facilitatedDiff_compartment2
    s.out     <- input$CIO_facilitatedDiff_species1
    s.in      <- input$CIO_facilitatedDiff_species2
    
    # Store Ids
    c.in.id    <- FindId(c.in)
    s.in.id    <- FindId(s.in)
    c.out.id   <- FindId(c.out)
    s.out.id   <- FindId(s.out)
    
    # Parameters
    Vmax.var  <- input$CIO_facilitatedDiff_Vmax
    Vmax.val  <- input$CIO_facilitatedDiff_Vmax_value
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
    
    # Base value determination
    if (Vmax.unit != Vmax.b.u) {
      Vmax.b.v <- 
        UnitConversion(Vmax.u.d, Vmax.unit, Vmax.b.u, as.numeric(Vmax.val))
    } else {
      Vmax.b.v <- Vmax.val
    }
    
    Km.var     <- input$CIO_facilitatedDiff_Km
    Km.val     <- input$CIO_facilitatedDiff_Km_value
    Km.unit    <- rv.UNITS$units.selected$For.Var
    Km.b.u     <- rv.UNITS$units.base$For.Var
    Km.unit.d  <- paste0("conc (",input$GO_species_unit_choice, ")")
    
    Km.d <- paste0("Michaelis Menten constant for the ", 
                   "facilitated Diffusion of ",
                   s.out,
                   " to ",
                   s.in)
    
    # Base value determination
    if (Km.unit != Km.b.u) {
      Km.b.v <- UnitConversion(u.d, Km.unit, Km.b.u, as.numeric(Km.val))
    } else {
      Km.b.v <- Km.val
    }
    
    log       <- paste0("Facilated Diffusion of ",
                        s.out,
                        " to ",
                        s.in,
                        " from compartment ",
                        c.out, " to ", c.in)
    
    p.add  <- c(p.add, Vmax.var, Km.var)
    d.add  <- c(d.add, Vmax.d, Km.d)
    f.val  <- c(f.val, Vmax.val, Km.val)
    u.add  <- c(u.add, Vmax.unit, Km.unit)
    ud.add <- c(ud.add, Vmax.u.d, Km.unit.d)
    b.unit <- c(b.unit, Vmax.b.u, Km.b.u)
    b.val  <- c(b.val, Vmax.b.v, Km.b.v)
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
    # Create InputOutput ID
    ids <- GenerateId(rv.ID$id.io.seed, "IO")
    IO.id <- ids$id
    rv.ID$id.io.seed <- ids$seed
    
    par.ids <- c()
    for (i in seq(length(p.add))) {
      if (!(p.add[i] %in% rv.PARAMETERS$parameters.names && 
            param.already.defined)) {
        
        # Generate Parameter ID
        par.gen <- GenerateId(rv.ID$id.param.seed, "parameter")
        rv.ID$id.param.seed <- par.gen$seed
        par.id <- par.gen$id
        par.ids <- c(par.ids, par.id)
        # Store ID to database
        idx.to.add <- nrow(rv.ID$id.df) + 1
        rv.ID$id.df[idx.to.add, ] <- c(par.id, p.add[i])
        
        # Write out to parameter
        to.par.list <- list("Name"            = p.add[i],
                            "ID"              = par.id,
                            "Value"           = as.numeric(f.val[i]),
                            "Unit"            = u.add[i],
                            "UnitDescription" = ud.add[i],
                            "BaseUnit"        = b.unit[i],
                            "BaseValue"       = as.numeric(b.val[i]),
                            "Description"     = d.add[i],
                            "Type"            = "Input/Output",
                            "Type.Note"       = type,
                            "Used.In"         = ID.to.add)
        
        # Store to parameter list
        rv.PARAMETERS$parameters[[par.id]] <- to.par.list
        

      } else {
        print("Repeated Parameter, skipped parameter overwrite")
      }
    }
    # browser()
    
    # TODO: Store I/0 to species ids (IO.ids)
    
    s.id.all <- c(s.in.id, s.out.id)
    s.id.all <- s.id.all[complete.cases(s.id.all)]
    
    c.id.all <- c(c.out.id, c.in.id)
    c.id.all <- c.id.all[complete.cases(c.id.all)]
    
    # Collapse variables
    c.out.collapsed  <- paste0(c.out, collapse = ", ")
    c.in.collapsed   <- paste0(c.in, collapse = ", ")
    s.out.collapsed  <- paste0(s.out, collapse = ", ")
    s.in.collapsed   <- paste0(s.in, collapse = ", ")
    par.collapsed    <- paste0(p.add, collapse = ", ")
    
    # Collapse Id Vars
    c.in.id.collapsed  <- paste0(c.in.id, collapse = ", ")
    c.out.id.collapsed <- paste0(c.out.id, collapse = ", ")
    par.ids.collapsed  <- paste0(par.ids, collapse = ", ")
    s.id.collapsed     <- paste0(s.id.all, collapse = ", ")
    s.in.id.collapsed  <- paste0(s.in.id, collapse = ", ")
    s.out.id.collapsed <- paste0(s.out.id, collapse = ", ")
    comp.ids.collapsed <- paste0(c.id.all, collapse = ", ")
    
    # Create Overall lists
    to.list <- list("ID" = IO.id,
                    "Direction" = direction,
                    "Type" = type,
                    "Compartment.Out" = c.out.collapsed,
                    "Compartment.In" = c.in.collapsed,
                    "Species.Out" = s.out.collapsed,
                    "Species.In" = s.in.collapsed,
                    "Parameters" = par.collapsed,
                    "Compartment.Ids" = comp.ids.collapsed,
                    "Species.Ids" = s.id.collapsed, 
                    "Parameter.Ids" = par.ids.collapsed
    )
    
    rv.IO$InputOutput[[IO.id]] <- to.list
    
    
    # Create Individual Lists Depending on IO type
    if (input$CIO_IO_options == "FLOW_IN") {
      to.add <- list("ID" = IO.id,
                     "Compartment" = c.in.collapsed,
                     "Compartment.Id" = comp.ids.collapsed,
                     "Species" = s.in.collapsed,
                     "Species.Id" = s.id.collapsed,
                     "Flow.Parameter" = par.collapsed,
                     "Parameter.Id" = par.ids.collapsed)
      
     rv.IO$Flow.In[[IO.id]] <- to.add 
     
    } else if (input$CIO_IO_options == "FLOW_OUT") {
      to.add <- list("ID" = IO.id,
                     "Compartment" = c.in.collapsed,
                     "Compartment.Id" = comp.ids.collapsed,
                     "Species" = s.in.collapsed,
                     "Species.Id" = s.id.collapsed,
                     "Flow.Parameter" = par.collapsed,
                     "Parameter.Id" = par.ids.collapsed)
      
      rv.IO$Flow.Out[[IO.id]] <- to.add
      
    } else if (input$CIO_IO_options == "FLOW_BETWEEN") {
      # Break par.ids
      flow.out.id <- par.ids[1]
      flow.in.id <- paste0(par.ids[seq(2, n.split+1)], collapse = ", ")
      flow.out <- f.out
      flow.in <- paste0(f.in, collapse = ", ")
      
      to.add <- list("ID" = IO.id,
                     "n.Split" = n.split,
                     "Compartment.Out" = c.out.collapsed,
                     "Compartment.Out.Id" = c.out.id.collapsed,
                     "Compartment.In" = c.in.collapsed,
                     "Compartment.In.Id" = c.in.id.collapsed,
                     "Species.Out" = s.out.collapsed,
                     "Species.Out.Id" = s.out.id.collapsed,
                     "Species.In" = s.in.collapsed,
                     "Species.In.Id" = s.in.id.collapsed,
                     "Flow.out" = flow.out,
                     "Flow.in" = flow.in,
                     "Flow.out.id" = flow.out.id,
                     "Flow.in.id" = flow.in.id)
      
      rv.IO$Flow.Between[[IO.id]] <- to.add 
      
    } else if (input$CIO_IO_options == "CLEARANCE") {
      to.add <- list("ID" = IO.id,
                     "Compartment" = c.in.collapsed,
                     "Compartment.Id" = comp.ids.collapsed,
                     "Species" = s.in.collapsed,
                     "Species.Id" = s.id.collapsed,
                     "Flow.Parameter" = par.collapsed,
                     "Parameter.Id" = par.ids.collapsed)
      
      rv.IO$Clearance[[IO.id]] <- to.add 
      
    } else if (input$CIO_IO_options == "SIMPDIFF") {
      to.add <- list("ID" = IO.id,
                     "Compartment.1" = c.out,
                     "Compartment.1.Id" = c.out.id,
                     "Compartment.2" = c.in,
                     "Compartment.2.Id" = c.in.id,
                     "Species.1" = s.out,
                     "Species.1.Id" = s.out.id,
                     "Species.2" = s.in,
                     "Species.2.Id" = s.in.id,
                     "PS" = par.collapsed,
                     "PS.id" = par.ids.collapsed)
      
      rv.IO$Simple.Diffusion[[IO.id]] <- to.add 
      
    } else if (input$CIO_IO_options == "FACILITATED_DIFF") {
      to.add <- list("ID" = IO.id,
                     "Compartment.Out" = c.out,
                     "Compartment.Out.Id" = c.out.id,
                     "Compartment.In" = c.in,
                     "Compartment.In.Id" = c.in.id,
                     "Species.Out" = s.out,
                     "Species.Out.Id" = s.out.id,
                     "Species.In" = s.in,
                     "Species.In.Id" = s.in.id,
                     "Vmax" = Vmax.var,
                     "Km" = Km.var,
                     "Vmax.id" = par.ids[1],
                     "Km.id" = par.ids[2])
      
      rv.IO$Facilitated.Diffusion[[IO.id]] <- to.add
    }
    
    print(rv.IO$InputOutput)
    print(rv.IO$Flow.In)
    print(rv.IO$Flow.Out)
    print(rv.IO$Flow.Between)
    
    rv.IO$IO.logs[length(rv.IO$IO.logs) + 1] <- log
    
    rv.IO$IO.id.counter <- rv.IO$IO.id.counter + 1
    
    # Flow In Rate Value
    updateTextInput(
      session = session, 
      inputId = "CIO_flow_in_rate_constant",
      value = paste0("F_in_", rv.IO$IO.id.counter)
    )
    
    # Flow Out Rate Value
    updateTextInput(
      session = session, 
      inputId = "CIO_flow_out_rate_constant",
      value = paste0("F_out_", rv.IO$IO.id.counter)
    )
    
    # Flow Between - Out
    updateTextInput(
      session = session, 
      inputId = "CIO_flowbetween_flow_variable_out",
      value = paste0("F_out_", rv.IO$IO.id.counter)
    )
    
    # Flow Between - In
    updateTextInput(
      session = session, 
      inputId = "CIO_flowbetween_flow_variable_in_1",
      value = paste0("F_in_", rv.IO$IO.id.counter)
    )
    
    # Clearance Rate Value
    updateTextInput(
      session = session, 
      inputId = "CIO_clearance_rate_constant",
      value = paste0("ke_", rv.IO$IO.id.counter)
    )
    
    # Simple Diffusion PS Value
    updateTextInput(
      session = session, 
      inputId = "CIO_simpdiff_rate_constant",
      value = paste0("PS_", rv.IO$IO.id.counter)
    )
    
    # Facilitated Diffusion Vmax Value
    updateTextInput(
      session = session, 
      inputId = "CIO_facilitatedDiff_Vmax_value",
      value = paste0("fVmax_", rv.IO$IO.id.counter)
    )
    
    # Facilitated Diffusion Km Value
    updateTextInput(
      session = session, 
      inputId = "CIO_facilitatedDiff_Km_value",
      value = paste0("fKm_", rv.IO$IO.id.counter)
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
      select(Type, 
             Compartment.Out, 
             Compartment.In, 
             Species.Out, 
             Species.In,
             Parameters)
    
    colnames(to.show) <- c("Type",
                           "Compartment Out",
                           "Compartment In",
                           "Species Out",
                           "Species In",
                           "Parameters")
    
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
