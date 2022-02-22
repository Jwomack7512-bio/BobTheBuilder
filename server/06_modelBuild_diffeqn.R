############################## DiffEQ Server #################################


observeEvent(input$diffeq_generate_equations, {
  results <- calc_differential_equations(eqns$eqn.info, 
                                         vars$species, 
                                         IO$input.info, 
                                         IO$output.info,
                                         IO$bool.input.added,
                                         IO$bool.output.added)
  DE$eqns <- unlist(results["diff.eqns"])
  DE$eqns.in.latex <- unlist(results["latex.diff.eqns"])
})

output$diffeq_display_diffEqs <- renderText({
  # paste(paste0('d(', vars$species, ")/dt = ", DE$eqns), collapse="<br><br>")
  
  if (length(vars$species) == 0) {
    "No variables entered"
  }
  else {
    n_eqns = length(vars$species)
    eqns_to_display <- c()
    for (i in seq(n_eqns)) {
      if (input$diffeq_option_simplify) {
        new_eqn <- paste0("(",i, ") ", 'd(', vars$species[i], ")/dt = ", Deriv::Simplify(DE$eqns[i]))
      } else {
        new_eqn <- paste0("(",i, ") ", 'd(', vars$species[i], ")/dt = ", DE$eqns[i])
      }
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse = "<br><br>")
  }
})

