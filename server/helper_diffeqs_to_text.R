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
  #     needs to have sublists of Name and Value
  # Output 
  #   @out - named vector of parameter values
  nPar <- length(paramList)
  param.values <- vector()
  param.names  <- vector()
  for (i in seq_along(paramList)) {
    param.values[i] <- paramList[[i]]$Value
    param.names[i]  <- paramList[[i]]$Name
  }
  
  out <-  as.numeric(param.values)
  names(out) <- param.names
  print(out)
  return(out)
}

output_ICs_for_ode_solver <- function(IC_vars, IC_values){
  output <-  as.numeric(IC_values)
  # print(output)
  # print(typeof(output))
  # print(IC_vars)
  # print(length(output))
  # print(length(IC_vars))
  names(output) <- IC_vars
  return(output)
}