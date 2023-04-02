############################## DiffEQ Server #################################
solveForDiffEqs <- function() {
  # Solve the differential equations using RVs.
  # Store results to their respective RVs. 
  
  jPrint("Solving For differential Equations")
  # jPrint(eqns$eqn.info)
  # jPrint(rv.SPECIES$species.names)
  # jPrint(IO$input.info)
  # jPrint(IO$output.info)
  # jPrint(IO$bool.input.added)
  # jPrint(IO$bool.output.added)
  # vars.in.model <- rv.SPECIES$species.df[["Name"]]

  results <- calc_differential_equations(eqns$eqn.info.df,
                                         eqns$eqn.info.chem.df,
                                         eqns$eqn.info.enz.df,
                                         eqns$eqn.info.syn.df,
                                         eqns$eqn.info.deg.df,
                                         vars, 
                                         IO$IO.df,
                                         id$id.df,
                                         DE$custom.diffeq.var,
                                         input$diffeq_multi_custom_eqns,
                                         DE$custom.diffeq.df
                                         )
  DE$eqns               <- unlist(results["diff.eqns"])
  DE$eqns.in.latex      <- unlist(results["latex.diff.eqns"])
  DE$de.eqns.for.solver <- unlist(results["diff.eqns.for.solver"])
  jPrint(DE$de.eqns.for.solver)
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

observeEvent(DE$custom.diffeq.var, {
  picker.choices <- DE$custom.diffeq.var
  updatePickerInput(session, 
                    "diffeq_multi_custom_eqns", 
                    choices = picker.choices)
})

observeEvent(input$diffeq_custom_eqn_button, {
  new.eqn <- input$diffeq_custom_eqn
  idx <- as.numeric(strsplit(input$diffeq_var_to_custom, ")")[[1]][1])

  DE$eqns[idx] <- new.eqn
  DE$custom.diffeq.var <- c(DE$custom.diffeq.var, rv.SPECIES$species.names[idx])
  DE$custom.diffeq <- c(DE$custom.diffeq, new.eqn)
  DE$custom.diffeq.df[nrow(DE$custom.diffeq.df)+1, ] <- c(rv.SPECIES$species.names[idx], 
                                                          new.eqn)
  jPrint(DE$custom.diffeq.df)
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
                          Deriv::Simplify(DE$eqns[i]))
      } else {
        new_eqn <- paste0("(",i, ") ",
                          comp.vol, "*",
                          'd(',
                          rv.SPECIES$species.names[i],
                          ")/dt = ",
                          DE$eqns[i])
      }
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse = "<br><br>")
  }
})

