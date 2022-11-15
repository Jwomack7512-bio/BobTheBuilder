# We need to create an objective function that read in our ode solution on the parameters
ssd_objective <- function(par.to.estimate, 
                          par.in.model,
                          ics.in.model,
                          var.in.model,
                          time,
                          rateEqns,
                          diffEqns,
                          d_of_var,
                          observed.data) {
  # Calculates the residuals of our model with comparison data uploaded by the
  # user
  # Inputs:
  # (1) par.to.estimate - list of parameters we want to estimate
  # (2) par.in.model - list of all parameters in model
  # (3) ics.in.model - list of all initial conditions and values in model
  # (4) var.in.model - vector of variables in model
  # (5) time - time vector that the model is ran for
  # (6) observed.data - data frame containing time in first column and observed
  #                     data in the subsequent columns. Column names should 
  #                     match variables in model
  # Outputs: 
  # (1) out - vector of residuals to be analyzed with a minimization function 
  
  # Unpack parameters 
  par.to.run <- listReplace(par.to.estimate, par.in.model)
  
  # Rework time variable to include time points in obv data
  times <- time
  tvs   <- sort(unique(c(times, as.numeric(unlist(observed.data[,1])))))
  
  # Run ODE solver to get concentration values
  
  Lorenz <- function(t, state, parameters, rateEqns, diffEqns, d_of_var){
    with(as.list(c(state, parameters)), {
      eval(parse(text = rateEqns))
      eval(parse(text = diffEqns))
      list(eval(parse(text = d_of_var)))
    })
  }
  # print(ics.in.model)
  # print(tvs)
  # print(par.to.run)
  # print(rateEqns)
  out <- ode(y = ics.in.model, 
             times = tvs, 
             func = myModel, 
             parms = par.to.run,
             rateEqns = rateEqns,
             diffEqns = diffEqns,
             d_of_var = d_of_var)
  
  # Remove time points from simulated data to match observed
  df <- data.frame(out)
  df <- df[df[,1] %in% as.numeric(unlist(observed.data[,1])),]
  
  # Get columns for observed data (remove time column)
  df.expected <- observed.data[,-1]
  expected.names <- colnames(df.expected)
  
  # Remove excess rows and columns from predicted data
  df.pred <- df[,expected.names]
  
  # Evaluate predicted data - expected
  ssqres <- (df.pred - df.expected)
  # Sum all terms
  out <- unname(unlist(ssqres))
  # print(out)
  return(out)
}


listReplace <- function(list1, list2) {
  # Takes the values in list1, finds them in list2, and replaces them with list1
  # values.  Returns the list with replace values.
  # Inputs:
  #   (1) list1 - list of variables with values wanting to replace
  #   (2) list2 - list of variables to replace with list1 vars
  # Outputs:
  #   (1) out - list2 with replaced list1 values.
  
  list1.names <- names(list1)
  list2.names <- names(list2)
  
  for (i in seq_along(list1.names)) {
    if (list1.names[i] %in% list2.names) {
      eval(parse(text=paste0("list2[['", list1.names[i], "']] <- list1[[i]]"))) 
    }
  }
  
  return(list2)
}


myModel <- function (t, 
                     state,
                     parameters, 
                     rateEqns,
                     diffEqns, 
                     d_of_var) {
  
    with(as.list(c(state, parameters)), {
      eval(parse(text = rateEqns))
      eval(parse(text = diffEqns))
      list(eval(parse(text = d_of_var)))
    })

}
# Begin main server functions for parameter estimation.

# Create waiter for calculations
w.pe <- Waiter$new(#id = "eqnCreate_showEquations",
                   html =  tagList(
                     div(
                       style = "color:black",
                       spin_whirly(),
                       hr(),
                       h4("Performing Parameter Estimation...")
                     )
                   ),
                   color = transparent(0.7))



# Read imported data
data.for.estimation <- reactive({
  req(input$pe_obs_data)
  #fread(input$data$datapath, na.strings=c("", NA))
  if(endsWith(input$pe_obs_data$datapath, ".csv")){
     out <- read.csv(input$pe_obs_data$datapath)
  } else if(endsWith(input$pe_obs_data$datapath, ".txt")){
    out <- read.table(input$pe_obs_data$datapath,header = T)
  }else if(endsWith(input$pe_obs_data$datapath, ".xls")){
    out <- read_excel(input$pe_obs_data$datapath)
  } else if(endsWith(input$pe_obs_data$datapath, ".xlsx")){
    out <- read_xlsx(input$pe_obs_data$datapath,sheet=1)
  }
  
  species <- colnames(out)[-1]
  pe$loaded.species <- species
  
  return(out)
})

# Fill pickerinput with parameters to estimate options
observeEvent(params$vars.all, {
  updatePickerInput(session = session,
                    "pe_select_par",
                    choices = params$vars.all)
})

# Function to update PE RV for selected parameters
observeEvent(input$pe_select_par, {
  pars <- input$pe_select_par
  
  # Remove vars from RV that are no longer selected
  idx.to.remove <- c()
  for (i in seq_along(pe$pars)) {
    if (!(pe$pars[i] %in% pars)) {
      idx.to.remove <- c(idx.to.remove, i)
    }
  }
  
  if (length(idx.to.remove > 0)) {
    pe$pars <- pe$pars[-idx.to.remove]
    pe$initial.guess <- pe$initial.guess[-idx.to.remove]
    pe$lb <- pe$lb[-idx.to.remove]
    pe$ub <- pe$ub[-idx.to.remove]
    pe$calculated.values <- pe$calculated.values[-idx.to.remove]
  }
  
  # Add parameters that are not in RV
  for (x in pars) {
    if (!(x %in% pe$pars)) {
      pe$pars <- c(pe$pars, x)
      pe$initial.guess <- c(pe$initial.guess, 1)
      pe$lb <- c(pe$lb, -Inf)
      pe$ub <- c(pe$ub, Inf)
      pe$calculated.values <- c(pe$calculated.values, "-")
    }
  }
})

# Generate Rhandsontable for parameters to estimate
output$pe_parameter_value_table <- renderRHandsontable({
  # TODO 
  # Make first column uneditable
  # Make second column needs to be numeric between certain values
  # Bound columns need to be same as last with Infs
  # Make Calculated Column uneditable
  
  # Create df from parameter estimation (pe) RV
  df <- data.frame(pe$pars,
                   pe$initial.guess,
                   pe$lb,
                   pe$ub,
                   pe$calculated.values)
  colnames(df) <- c("Parameters", 
                    "Initial Guess",
                    "Lower Bound",
                    "Upper Bound", 
                    "Calculated")
  
  rhandsontable(df) %>%
    hot_col(col = c(1, 5), readOnly = TRUE) %>%
    hot_validate_numeric(cols = 2, min = 0) %>%
    hot_validate_numeric(cols = c(3,4))
})

# Edit and save changed to pe parameter value table (rhandontable)
observeEvent(input$pe_parameter_value_table$changes$changes, {
  
  # xi, yi are table coordinates (remember js starts at 0, so we add 1 for R)
  # xi is 
  # old, new are the old value in that cell and the new value in the cell
  xi  <- input$pe_parameter_value_table$changes$changes[[1]][[1]] + 1
  yi  <- input$pe_parameter_value_table$changes$changes[[1]][[2]] + 1
  old <- input$pe_parameter_value_table$changes$changes[[1]][[3]]
  new <- input$pe_parameter_value_table$changes$changes[[1]][[4]] 

  # Change effect based on which row is changed (remember js starts at 0)
  
  if (yi == 2) {
    # Store initial guess
    pe$initial.guess[xi] <- new
  } else if (yi == 3) {
    # Store lower bound
    pe$lb[xi] <- new
    # if (is.numeric(new)) {
    #   if (new == "-Inf" | new == "Inf") {new = -Inf}
    #   pe$lb[xi] <- new
    # } else {
    #   pe$lb[xi] <- old
    # }
    
  } else if (yi == 4) {
    # Store upper bound 
    pe$ub[xi] <- new
    # if (is.numeric(new)) {
    #   pe$ub[xi] <- new 
    # } else {
    #   pe$ub[xi] <- old
    # }
  }
  PrintVar(pe$initial.guess)
  PrintVar(pe$lb)
  PrintVar(pe$ub)
})

# Plot output that takes the input data as a scatter plot and model as line
output$pe_import_data_table <- renderRHandsontable({
  rhandsontable(data.for.estimation())
})

# Run parameter estimation when button is pressed
observeEvent(input$pe_run_parameter_estimation, {
  
  w.pe$show()
  # Grab information needed for parameter estimation
  parameters <- as.list(output_param_for_ode_solver(params$vars.all,
                                                    params$vals.all))
  state <- output_ICs_for_ode_solver(vars$species, ICs$vals)
  time_in <- as.numeric(input$execute_time_start)
  time_out <- as.numeric(input$execute_time_end)
  time_step <- as.numeric(input$execute_time_step)
  times <- seq(time_in, time_out, by = time_step)
  data <- data.for.estimation()
  
  #set up differential equations input string form
  diff_eqns <- diffeq_to_text(DE$eqns, vars$species)
  
  d_of_var <- output_var_for_ode_solver(vars$species)
  
  rate_eqns <- rateEqns_to_text(eqns$additional.eqns)
  
  if (input$execute_turnOn_time_scale_var) {
    d_of_var = paste0(input$execute_time_scale_var, "*", d_of_var)
  }
  
  # Perform parameter estimation
  #   -- Grab parameters from data upload
  pars <- pe$pars
  vals <- pe$initial.guess
  p.0 <- as.list(output_param_for_ode_solver(pars, vals))
  
  lower <- pe$lb
  upper <- pe$ub
  PrintVar(pe$pars)
  PrintVar(rate_eqns)
  #  --Run ssd objective
  withConsoleRedirect("pe_logs", {
    nls.out <- nls.lm(par = p.0,
                      lower = lower,
                      upper = upper,
                      fn = ssd_objective,
                      par.in.model = parameters,
                      ics.in.model = state,
                      var.in.model = names(state),
                      time = times,
                      rateEqns = rate_eqns,
                      diffEqns = diff_eqns,
                      d_of_var = d_of_var,
                      observed.data = data,
                      control = nls.lm.control(nprint=1))
  })
  
  
  # Store estimation data to its respective place
  new.pars <- pe$pars
  for (i in seq_along(pars)) {
    # pe$pars[[i]] <- unname(unlist(nls.out$par[i]))
    new.pars[[eval(parse(text="pars[i]"))]] <- as.numeric(
      unname(unlist(nls.out$par[i])))
    # new.pars[[i]] <- unname(unlist(nls.out$par[i]))
    pe$calculated.values[i] <- as.numeric(unname(unlist(nls.out$par[i])))
  }
  print("Got this far")
  new.pars <- listReplace(new.pars, parameters)
  for (i in seq_along(new.pars)) {
    new.pars[[i]] <- as.numeric(new.pars[[i]])
  }
  print(parameters)
  print("new Pars")
  print(new.pars)
  # Rerun 
  out <- ode(y = state, 
             times = times, 
             func = myModel, 
             parms = new.pars,
             rateEqns = rate_eqns,
             diffEqns = diff_eqns,
             d_of_var = d_of_var)
  print("ode last solved")
  print(head(out))
  
  # Pass information to graph in some way
  pe$solved.model <- out
  pe$successful.run <- TRUE
  
  w.pe$hide()
  
})

output$pe_parameter_estimation_plot <- renderPlot({
  
  # browser()
  # Observed enter data
  data <- data.for.estimation()
  colnames(data)[1] <- "time"
  
  data.m <- reshape2::melt(data, id.vars="time")
  
  # df <- data.frame(t,A,B,P)
  # df.m <- reshape2::melt(df, id.vars="t")
  
  p <- ggplot(NULL, aes(col=variable)) +
    #geom_line(data = df.m, aes(t, value)) +
    geom_point(data = data.m, 
               aes(time, value),
               size = 3.5)
  if (pe$successful.run) {
    # Pull Results from data
    to.pull <- pe$loaded.species
    
    df <- data.frame(pe$solved.model)
    to.plot <- df[c("time", to.pull)]

    df.m <- reshape2::melt(to.plot, id.vars="time")
    p <- p + geom_line(data = df.m, 
                       aes(time, value),
                       size = 2)
  }
  
  p <- p + theme_classic()
  
  return(p)
  
})

# Store estimated parameters as main parameters
observeEvent(input$pe_store_estimated_parameters, {
  # Find calculated parameters and their values
  new.pars <- pe$pars
  new.vals <- pe$calculated.values
  
  # Replace parameters in main model with new parameters
  # (1) Find parameter location in RV of parameters
  indices <- c()
  for (i in seq_along(new.pars)) {
    indices <- c(indices, match(new.pars[i], params$vars.all))
  }
  # (2) Store old parameters
  pe$previous.values <- params$vals.all
  
  # (3) Overwrite old params with new
  count <- 1
  for (idx in indices) {
    params$vals.all[idx] <- new.vals[count]
    params$param.table[count, 2] <- new.vals[count]
    loop$parameters[count, 2] <- new.vals[count]
    count <- count + 1 
  }
  print("Store PE was pressed")
  # Rerun model with new parameters?
})




