diffeq_to_text <- function(list_of_diffeqs, list_of_vars){
  output <- "" #initialize output
  for (i in seq(length(list_of_diffeqs))) {
    current_eqn <- paste0("d", list_of_vars[i], " <- ", list_of_diffeqs[i])
    output <- paste0(output, current_eqn, "\n ")
  }
  #print(output)
  return(output)
}

rateEqns_to_text <- function(rate_equations)
{
  output <- ""
  for (i in seq(length(rate_equations)))
  {
    output <- paste0(output, rate_equations[i], "\n ")
  }
  #print(output)
  return(output)
}

output_var_for_ode_solver <- function(list_of_vars){
  output <- paste0("d", list_of_vars, collapse = ", ")
  output <- paste0("c(", output, ")")
  
  return(output)
}

output_param_for_ode_solver <- function(paramList){
  # Convert parmeter list to named vector for differential equation execution
  # Input 
  #   @paramList - list of params (RV params$params)
  #     needs to have sublists of Name and Base.Value
  # Output 
  #   @out - named vector of parameter values
  nPar <- length(paramList)
  param.values <- vector()
  param.names  <- vector()
  for (i in seq_along(paramList)) {
    param.values[i] <- paramList[[i]]$Base.Value
    param.names[i]  <- paramList[[i]]$Name
  }
  
  out <-  as.numeric(param.values)
  names(out) <- param.names
  return(out)
}

output_ICs_for_ode_solver <- function(IC_Data_Structure){
  
  nVar <- length(IC_Data_Structure)
  var.names <- vector(mode = "character", nVar)
  var.vals  <- rep(0, nVar)
  
  for (i in seq_along(IC_Data_Structure)) {
    var.names[i] <- IC_Data_Structure[[i]]$Name
    var.vals[i]  <- as.numeric(IC_Data_Structure[[i]]$BaseValue)
  }

  names(var.vals) <- var.names
  return(var.vals)
}