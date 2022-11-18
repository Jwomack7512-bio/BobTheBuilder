#Functions in this file:
# jPrint
# RenameParameterVector
# VarToLatexForm
PrintVar <- function(variable,
                     sameLine = TRUE) {
  # Prints a variable and tells what that variable is
  var.name <- deparse(substitute(variable))
  
  if (sameLine) {
    cat(var.name, "-", variable, "\n")
  }
  else {
    cat(var.name)
    cat('\n')
    cat(variable)
    cat("\n")
  }
}

jPrint <- function(string) {
  # print through function since Rshiny won't seem to let me bring in my events without it
  print(string)
}

ParameterSearchDF <- function(searchVar, dfToSearch) {
  n.rows <- nrow(dfToSearch)
  n.cols <- ncol(dfToSearch)
  new.df <- dfToSearch
  par.exists.elsewhere <- FALSE
  #check to make sure rows exist as some dateframes in this program are initiated without columns (columsn just extra check)
  if (n.rows != 0 & n.cols != 0) {
    for (i in seq(n.rows)) {
      for (j in seq(n.cols)) {
        has.var <- grepl(searchVar, dfToSearch[i,j], fixed = TRUE)
        if (has.var) {
          par.exists.elsewhere <- TRUE
        }
      }
    }
  }
  return(par.exists.elsewhere)
}



RenameParameterDF <- function(oldName, newName, dfToSearch) {
  # When the parameter is renamed it needs to be renamed in many places 
  # Function is used on dataframes
  # Inputs:
  #   @oldName - String name of the parameter to be changed
  #   @newName - Desired String name of the new parameter
  #   @vectorToCheck - Vector of data to look for string in
  # Output:   
  #   Returns df with changed name values (if any) 
  # Places that Variables need to be changed:
  #   eqns$eqn.info
  #   IO$IO.info
  n.rows <- nrow(dfToSearch)
  n.cols <- ncol(dfToSearch)
  new.df <- dfToSearch
  #check to make sure rows exist as some dateframes in this program are initiated without columns (columsn just extra check)
  if (n.rows != 0 & n.cols != 0) {
    for (i in seq(n.rows)) {
      for (j in seq(n.cols)) {
        has.var <- grepl(oldName, dfToSearch[i,j], fixed = TRUE)
        if (has.var) {
          new.df[i,j] <- gsub(oldName, newName, dfToSearch[i,j])
        }
      }
    }
  }
  return(new.df)
}

#TODO: 
#     Note there is a bug when using rate equations wiht this.  have to fix.
RenameParameterVector <- function(oldName, newName, vectorToSearch) {
  # When the parameter is renamed it needs to be renamed in many places including
  # all parameter tables, eqns, eqn tables, differential eqns and the such.
  # function is used on vectors
  # Inputs:
  #   @oldName - String name of the parameter to be changed
  #   @newName - Desired String name of the new parameter
  #   @vectorToCheck - Vector of data to look for string in
  # Output:   
  #   Returns vector with changed named values (if any) 
  # Places that Variables need to be changed:
  #   eqns$main, eqns$additional.eqns, eqns$rate.eqns, eqns$time.dep.eqns, 
 
  #   DE$eqns?
  #   PP section?
  #   params - pretty much all sections
  
  idx = 0
  print("Running Rename Parameter")
  print(vectorToSearch)
  for (string.var in vectorToSearch) {
    idx = idx + 1 
    print(oldName)
    print(newName)
    print(string.var)
    has.var <- grepl(oldName, string.var, fixed = TRUE)
    if (has.var) {
      new.eqn <- gsub(oldName, newName, string.var) #replace old name with new and place in new variable
      vectorToSearch[idx] <- new.eqn #replace string in variable its stored
    }
  }
  return(vectorToSearch)
}

withConsoleRedirect <- function(containerId, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
  txt <- capture.output(results <- expr, type = "output")
  removeUI(paste0("#", containerId))
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), 
             where = "beforeEnd",
             ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}

PassConsoleOutputToVar <- function(var, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
  txt <- capture.output(results <- expr, type = "output")
  removeUI(paste0("#", containerId))
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), 
             where = "beforeEnd",
             ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}

ParameterNameCheck <- function(oldParam, newParam, vectorOfPossibleParams) {
  
  if (newParam %in% vectorOfPossibleParams) {
    out <- newParam
  } else {
    out <- oldParam
  }
  
  return(out)
}

DetermineRateConstantUnits <- function(coefs, massUnit, volumeUnit, timeUnit) {
  # Input: 
  #   coefs: string of coefficents for rate law separated by space
  # Output:
  #   out: text rate law
  
  # Split and sum coefficients
  num.coefs <- as.numeric(strsplit(coefs, " ")[[1]])
  # print(num.coefs)
  sum.coefs <- sum(num.coefs)
  # print(sum.coefs)
  
  # First Order
  if (sum.coefs == 1) {
    # print("First Order")
    out <- paste0("1/", timeUnit)
  } else if (sum.coefs > 1) {
    # order relates to the exponents
    out <- paste0(massUnit, "^", sum.coefs, 
                  "/(", volumeUnit, "^", sum.coefs, "*", timeUnit, ")")

  }
  
  # print(out)
  return(out)
}
