
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
                    choices = for.choice)
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
observeEvent({input$CIO_facillDiff_compartment1
              input$createVar_addVarToList
              input$createVar_add_compartment}, {
  req(!is_empty(vars$var.df))
  
  for.choice <- 
    vars$var.df %>% 
    dplyr::filter(Compartment == input$CIO_facillDiff_compartment1) %>%
    select(Name)
  for.choice <- unlist(for.choice, use.names = FALSE)
  
  updatePickerInput(session, 
                    "CIO_facillDiff_species1", 
                    choices = for.choice)
  })

observeEvent({input$CIO_facillDiff_compartment2
              input$createVar_addVarToList
              input$createVar_add_compartment}, {
  req(!is_empty(vars$var.df))
  
  for.choice <- 
    vars$var.df %>% 
    dplyr::filter(Compartment == input$CIO_facillDiff_compartment2) %>%
    select(Name)
  for.choice <- unlist(for.choice, use.names = FALSE)
  
  updatePickerInput(session, 
                    "CIO_facillDiff_species2", 
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
                    "CIO_facillDiff_compartment1", 
                    choices = c.names)
  updatePickerInput(session, 
                    "CIO_facillDiff_compartment2", 
                    choices = c.names)
  
})

# Compartment IO Add -----------------------------------------------------------

observeEvent(input$CIO_add_IO, {
  
  in.or.out <- NA
  type      <- NA
  c.in      <- NA
  c.out     <- NA
  flow.rate <- NA
  flow.unit <- NA
  flow.spec <- NA
  log       <- NA
  
  # Check what type of IO is being used. 
  if (input$CIO_IO_options == "FLOW") {
    in.or.out <- "Both"
    type      <- "Dual_Flow"
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
    type      <- "Clearance"
    c.out     <- input$CIO_flow_compartment_out
    flow.rate <- input$CIO_clearance_rate
    flow.unit <- units$selected.units$Flow
    flow.spec <- paste0(input$CIO_clearance_species, collapse = " ")
    log       <- paste0("Clearance of ",
                        paste0(input$CIO_clearance_species, collapse = ", "),
                        " by flow rate of ",
                        flow.rate, " (", flow.unit, ").")
  }
  
  row.to.df <- c(in.or.out,
                 type,
                 c.in,
                 c.out,
                 flow.rate,
                 flow.unit,
                 flow.spec)
  
  IO$IO.df[nrow(IO$IO.df) + 1,] <- row.to.df
  print(" IO DF")
  print(IO$IO.df)
  
  IO$IO.logs[length(IO$IO.logs) + 1] <- log
  
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