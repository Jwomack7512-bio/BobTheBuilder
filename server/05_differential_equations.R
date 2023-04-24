############################## DiffEQ Server #################################

# Function to solve and extract diffeqs ----------------------------------------
solveForDiffEqs <- function() {
  # Solve the differential equations using RVs.
  # Store results to their respective RVs. 
  
  print("Solving For differential Equations")
  # browser()
  results <- DeriveDifferentialEquations(rv.COMPARTMENTS,
                                         rv.SPECIES,
                                         rv.REACTIONS,
                                         rv.IO,
                                         rv.ID)
  print(results)
  
  # Extract results to proper reactive variables
  rv.DE$de.equations.list <- results
  rv.DE$de.string.eqns    <- unname(sapply(results,
                                           get,
                                           x = "ODES.eqn.string"))
  rv.DE$de.latex.eqns     <- unname(sapply(results, 
                                           get,
                                           x = "ODES.latex.string"))
  rv.DE$de.mathjax.eqns   <- unname(sapply(results, 
                                           get,
                                           x = "ODES.mathjax.string"))
}

# Events -----------------------------------------------------------------------
observeEvent(rv.SPECIES$species, {
  picker.choices <- c()
  i = 0
  for (var in rv.SPECIES$species.names) {
    i = i + 1
    choice <- paste0(i, ") ", 'd(', var, ")/dt")
    picker.choices <- c(picker.choices, choice)
  }
  updatePickerInput(session, 
                    "diffeq_var_to_custom", 
                    choices = picker.choices)
})

observeEvent(rv.DE$custom.diffeq.var, {
  picker.choices <- rv.DE$custom.diffeq.var
  updatePickerInput(session, 
                    "diffeq_multi_custom_eqns", 
                    choices = picker.choices)
})

observeEvent(input$diffeq_custom_eqn_button, {
  new.eqn <- input$diffeq_custom_eqn
  idx <- as.numeric(strsplit(input$diffeq_var_to_custom, ")")[[1]][1])

  rv.DE$de.eqns[idx] <- new.eqn
  rv.DE$custom.diffeq.var <- c(rv.DE$custom.diffeq.var, 
                               rv.SPECIES$species.names[idx])
  rv.DE$custom.diffeq <- c(rv.DE$custom.diffeq, new.eqn)
  rv.DE$custom.diffeq.df[nrow(rv.DE$custom.diffeq.df)+1, ] <- 
    c(rv.SPECIES$species.names[idx], 
      new.eqn)
})

# Diff Eqn Button --------------------------------------------------------------
observeEvent(input$diffeq_generate_equations, {
  print("Generate diff eq button pressed")
  solveForDiffEqs()
})

# Render diffeqn text viewer ---------------------------------------------------
output$diffeq_display_diffEqs <- renderText({
  
  if (length(rv.SPECIES$species) == 0) {
    "No variables entered"
  }
  else {
    n_eqns = length(rv.SPECIES$species)
    eqns_to_display <- c()
    for (i in seq(n_eqns)) {
      # Find Corresponding Volumes for compartments
      comp.of.variable <- rv.SPECIES$species[[i]]$Compartment
      row.idx <- which(rv.COMPARTMENTS$compartments.df$Name %in% comp.of.variable)
      comp.vol <- rv.COMPARTMENTS$compartments.df$Volume[row.idx]
      if (input$diffeq_option_simplify) {
        new_eqn <- paste0("(",i, ") ",
                          comp.vol, "*",
                          'd(', 
                          rv.SPECIES$species.names[i], 
                          ")/dt = ", 
                          Deriv::Simplify(rv.DE$de.string.eqns[i]))
      } else {
        new_eqn <- paste0("(",i, ") ",
                          comp.vol, "*",
                          'd(',
                          rv.SPECIES$species.names[i],
                          ")/dt = ",
                          rv.DE$de.string.eqns[i])
      }
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse = "<br><br>")
  }
})

