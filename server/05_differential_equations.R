############################## DiffEQ Server #################################
solveForDiffEqs <- function() {
  # Solve the differential equations using RVs.
  # Store results to their respective RVs. 
  
  jPrint("Solving For differential Equations")
  # jPrint(rv.REACTIONS$eqn.info)
  # jPrint(rv.SPECIES$species.names)
  # jPrint(rv.IO$input.info)
  # jPrint(rv.IO$output.info)
  # jPrint(rv.IO$bool.input.added)
  # jPrint(rv.IO$bool.output.added)
  # vars.in.model <- rv.SPECIES$species.df[["Name"]]

  results <- calc_differential_equations(rv.REACTIONS$reactions.df,
                                         rv.REACTIONS$massAction.df,
                                         rv.REACTIONS$michaelisMenten.df,
                                         rv.REACTIONS$synthesis.df,
                                         rv.REACTIONS$degradation.df,
                                         vars, 
                                         rv.IO$IO.df,
                                         rv.ID$id.df,
                                         rv.DE$custom.diffeq.var,
                                         input$diffeq_multi_custom_eqns,
                                         rv.DE$custom.diffeq.df
                                         )
  rv.DE$de.eqns               <- unlist(results["diff.eqns"])
  rv.DE$de.eqns.in.latex      <- unlist(results["latex.diff.eqns"])
  rv.DE$de.eqns.for.solver <- unlist(results["diff.eqns.for.solver"])
  jPrint(rv.DE$de.eqns.for.solver)
}

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
  rv.DE$custom.diffeq.var <- c(rv.DE$custom.diffeq.var, rv.SPECIES$species.names[idx])
  rv.DE$custom.diffeq <- c(rv.DE$custom.diffeq, new.eqn)
  rv.DE$custom.diffeq.df[nrow(rv.DE$custom.diffeq.df)+1, ] <- c(rv.SPECIES$species.names[idx], 
                                                          new.eqn)
  jPrint(rv.DE$custom.diffeq.df)
})

observeEvent(input$diffeq_generate_equations, {
  print("Generate diff eq button pressed")
  solveForDiffEqs()
})

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
                          Deriv::Simplify(rv.DE$de.eqns[i]))
      } else {
        new_eqn <- paste0("(",i, ") ",
                          comp.vol, "*",
                          'd(',
                          rv.SPECIES$species.names[i],
                          ")/dt = ",
                          rv.DE$de.eqns[i])
      }
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse = "<br><br>")
  }
})

