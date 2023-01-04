
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