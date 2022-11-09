# We need to create an objective function that read in our ode solution on the parameters
ssd_objective <- function(par.to.estimate, 
                          par.in.model,
                          ics.in.model,
                          var.in.model,
                          time,
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
  out <- ode(y = ics.in.model, 
             times = tvs, 
             func = myModel, 
             parms = par.to.run)
  
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

# Begin main server functions for parameter estimation.


# Read imported data
data.for.estimation <- reactive({
  req(input$pe_obs_data)
  #fread(input$data$datapath, na.strings=c("", NA))
  if(endsWith(input$pe_obs_data$datapath, ".csv")){
    read.csv(input$pe_obs_data$datapath)
  } else if(endsWith(input$pe_obs_data$datapath, ".txt")){
    read.table(input$pe_obs_data$datapath,header = T)
  }else if(endsWith(input$pe_obs_data$datapath, ".xls")){
    read_excel(input$pe_obs_data$datapath)
  } else if(endsWith(input$pe_obs_data$datapath, ".xlsx")){
    read_xlsx(input$pe_obs_data$datapath,sheet=1)
  }
})

# Fill pickerinput with parameters to estimate options
observeEvent(params$vars.all, {
  updatePickerInput(session = session,
                    "pe_select_par",
                    choices = params$vars.all)
})

observeEvent(input$pe_select_par, {
  pars <- input$pe_select_par
  # Remove vars from RV that are no longer selected
  
  # Add parameters that are not in RV
  for (x in pars) {
    if (!(x %in% pe$pars)) {
      pe$pars <- c(pe$pars, x)
      pe$initial.guess <- c(pe$initial.guess, 1)
      pe$lb <- c(pe$lb, "-Inf")
      pe$ub <- c(pe$ub, "Inf")
      pe$calculated.values <- c(pe$calculated.values, "-")
    }
  }

})

# Generate Rhandsontable for parameters to estimate
output$pe_parameter_value_table <- renderRHandsontable({
  
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
  
  rhandsontable(df)
})

# Plot output that takes the input data as a scatter plot and model as line
output$pe_import_data_table <- renderRHandsontable({
  rhandsontable(data.for.estimation())
})

output$pe_parameter_estimation_plot <- renderPlot({
  
  # Observed enter data
  data <- data.for.estimation()
  colnames(data)[1] <- "t"
  data.m <- reshape2::melt(data, id.vars="t")
  
  # df <- data.frame(t,A,B,P)
  # df.m <- reshape2::melt(df, id.vars="t")
  
  
  ggplot(NULL, aes(col=variable)) +
    #geom_line(data = df.m, aes(t, value)) +
    geom_point(data = data.m, aes(t, value))
  
})




