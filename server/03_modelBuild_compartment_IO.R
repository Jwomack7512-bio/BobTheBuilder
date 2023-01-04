
# Update UI --------------------------------------------------------------------

observeEvent({input$CIO_flow_compartment_out
              input$createVar_addVarToList}, {
  req(!is_empty(vars$var.df))
  
  for.choice <- 
    vars$var.df %>% filter(Compartment == input$CIO_flow_compartment_out) %>%
                    select(Name)
  
  updatePickerInput(
    session,
    "CIO_flow_species",
    choices = for.choice
  )
})

observeEvent({input$CIO_clearance_compartment
              input$createVar_addVarToList}, {
  req(!is_empty(vars$var.df))
  
  for.choice <- 
    vars$var.df %>% 
      filter(Compartment == input$CIO_clearance_compartment) %>%
      select(Name)
  
  updatePickerInput(
    session,
    "CIO_clearance_species",
    choices = for.choice
  )
})

observeEvent(vars$compartments.info, {
  
  compartment.names <- names(vars$compartments.info)
  
  updatePickerInput(
    session,
    "CIO_flow_compartment_out",
    choices = compartment.names
  )
  
  updatePickerInput(
    session,
    "CIO_flow_compartment_in",
    choices = compartment.names
  )
  
  updatePickerInput(
    session,
    "CIO_clearance_compartment",
    choices = compartment.names
  )
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