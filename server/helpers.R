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
  #   coefs: string of coefficients for rate law separated by space
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
    u   <- paste0("1/", timeUnit)
    u.d <- "num <div> time"
  } else if (sum.coefs == 2) {
    # order relates to the exponents
    u <- paste0(massUnit, 
                  "/(", volumeUnit, "*", timeUnit, ")")
    u.d <- paste0("conc (",
                    massUnit,
                    ") <div> ",
                    "<group> volume <multiply> time <endgroup>")
  } else if (sum.coefs > 2) {
    # order relates to the exponents
    coef = sum.coefs - 1
    u <- paste0(massUnit, "^", coef, 
                  "/(", volumeUnit, "^", coef, "*", timeUnit, ")")
    u.d <- paste0("conc (",
                    massUnit,
                    ") <power>(", 
                    coef, ") <div> ",
                    "<group> volume",
                    "<power>(", coef, ") ",
                    "<multiply> time <endgroup>")
  }
  
  out <- list("unit" = u,
              "unit.d" = u.d)
  
  return(out)
}

RemoveFromVector <- function(value, vector, firstOnly = FALSE) {
  # Removes value from vector. Will remove all occurances if firstOnly = F
  # Inputs
  #   @value - value(s) to remove from vector
  #   @vector - vector to remove values from by name
  #   @firstOnly - Bool, if true will only remove first instance of value
  # Output
  #   @Out - vector with removed value(s) by specifications
  # Remove Value from vector
  # Example:
  #   vec <- c(1,2, 2, 4, 6, 2)
  #   vec2 <- RemoveFromVector(2, vec)
  #   >>> vec2 = c(1,4,6)
  
  all.idxs.to.remove <- c()
  if (firstOnly) {
    for (i in seq_along(value)) {
      all.idxs.to.remove <- c(all.idxs.to.remove, match(value, vector))
    }
    out <- vector[-sort(all.idxs.to.remove)]
  } else {
    for (i in seq_along(value)) {
      all.idxs.to.remove <- c(all.idxs.to.remove, which(vector %in% value))
    }
    out <- vector[-sort(all.idxs.to.remove)]
  }
  
  return(out)
}


VectorizeListValue <- function(l, value, init.mode = "character") {
  # Takes a list item and creates a vector of its components
  # Example:
  # a <- list("gone" = c(1,2,3,4,4),
  #           "girl" = c("not", "too", "hot"))
  # b <- VectorizeListValue(a, gone, init.mode="numeric")
  #     >>> b = c(1,2,3,4,4)
  out <- vector(mode=init.mode, length=length(l))
  for (i in seq_along(l)) {
    out[i] <- eval(parse(text=paste0("l[[i]]$", value)))
  }
  return(out)
}

UnitCompare <- function(unitDescriptor, 
                        unitToCompare,
                        possibleConcUnits,
                        possibleTimeUnits) {
  # Take in unit descriptor, break it down and make sure it matches new input
  # Input: 
  #   unitDescriptor - word break down of units (num <div> time)
  #   unitToCompare - units to compare to descriptor (1/min)
  #   possibleConcUnits - vector of possible concentration units for check
  #   possibleConcUnits - vector of possible time units for check
  
  # Split descriptor
  ud.split   <- strsplit(unitDescriptor, " ")[[1]]
  comp.split <- UnitBreak(unitToCompare)
  is.match <- TRUE
  error.message <- "No Error: Unit Matches Descriptor"
  PrintVar(ud.split)
  PrintVar(comp.split)
  
  # Check if lengths of splits are the same
  if (length(ud.split) != length(comp.split)) {
    out <- list("is.match" = FALSE,
                "message" = "Size Difference in Inputs")
    return(out)
  }
  
  # Perform analysis/comparison
  for (i in seq_along(ud.split)) {
    
    element <- ud.split[i]
    comp    <- comp.split[i]
    PrintVar(element)
    PrintVar(comp)
    
    if (startsWith(element, "<power>")) {
      print("Power Fxn")
      if (comp != "^") {
        is.match <- FALSE
        error.message <- "Exponent Does Not Match up"
        break
      }

    } else if (startsWith(element, "<")) {
      print("Operator")
      if (element == "<div>") {
        if (comp != "/") {
          is.match <- FALSE
          error.message <- "Division Does Not Match up"
          break
        }
      } else if (element == "<multiply>") {
        if (comp != "*") {
          is.match <- FALSE
          error.message <- "Division Does Not Match up"
          break
        }
      } else if (element == "<addition>") {
        if (comp != "+") {
          is.match <- FALSE
          error.message <- "Addition Does Not Match up"
          break
        }
      } else if (element == "<subtraction>") {
        if (comp != "-") {
          is.match <- FALSE
          error.message <- "Subtraction Does Not Match up"
          break
        }
      } else if (element == "<group>") {
        if (comp != "(") {
          is.match <- FALSE
          error.message <- "Beginning Parenthesis Does Not Match up"
          break
        }
      } else if (element == "<endgroup>") {
        if (comp != ")") {
          is.match <- FALSE
          error.message <- "End Parenthesis Does Not Match up"
          break
        }
      }
    } else if(element == "num") {
      print("Number")
      is.num <- as.numeric(comp)
      if (is.na(is.num)) {
        # Return error because not numeric
        is.match <- FALSE
        error.message <- "Number is not a number"
        break
      }
    } else if (element == "conc") {
      print("Concentration")
      # Check if new term is a concentration term
      # Pull list of concentration terms
      if (!(comp %in% possibleConcUnits)) {
        is.match <- FALSE
        error.message <- paste0("Unit: '", 
                                comp,
                                "' not a possible concentration unit. ",
                                "Possible units are: ",
                                paste0(possibleConcUnits, collapse = ", ")
        )
        break
      }
    } else if (element == "time") {
      print("Time")
      if (!(comp %in% possibleTimeUnits)) {
        is.match <- FALSE
        error.message <- paste0("Unit: '", 
                                comp,
                                "' not a possible time unit. ",
                                "Possible units are: ",
                                paste0(possibleTimeUnits, collapse = ", ")
        )
        break
      }
    }
  }
  
  out <- list("is.match" = is.match,
              "message" = error.message)
  return(out)
}

UnitBreak <- function(unitFxn,
                      splitExponents = TRUE) {
  
  # Split Parenthesis
  break.terms <- c("(", ")")
  group.terms <- SplitOnValue(unitFxn, break.terms)                                                                                       
  
  # Split on mathematical operators
  operator.terms <- c()
  break.terms <- c("/", "+", "-", "*")
  for (term in group.terms) {
    operator.terms <- c(operator.terms, SplitOnValue(term, break.terms))
  }
  # split.terms <- SplitOnValue(unitFxn, break.terms)

  # Further split terms by powers
  out <- c()
  if (splitExponents) {
    for (term in operator.terms) {
      if ("^" %in% S2V(term)) {
        term.out <- SplitOnValue(term, "^")
        out <- c(out, term.out)
      } else {
        out <- c(out, term)
      }
    }
  } else {
    out <- operator.terms
  }
  
  return(out)
}

S2V <- function(string) {
  # Quick way to split a string to a single term vector
  vec <- strsplit(string, "")[[1]]
  return(vec)
}

SplitOnValue <- function(string, break.terms) {
  # Split string on break.terms but retain the break.terms
  
  split.terms <- c()
  running.terms <- c()
  for (i in seq(nchar(string))) {
    val <- strsplit(string, "",)[[1]][i]
    if (val %in% break.terms) {
      split.terms <- c(split.terms, paste(running.terms, collapse = ""), val)
      running.terms <- c()
    } else if (i == nchar(string)) {
      running.terms <- c(running.terms, val)
      split.terms <- c(split.terms, paste(running.terms, collapse = ""))
    } else {
      running.terms <- c(running.terms, val)
    }
  }
  return(split.terms)
}


UnitConversion <- function(unitDescriptor,
                           previousUnits,
                           newUnits,
                           unitValue) {
  
  # Take in unit descriptor, break it down and make sure it matches new input
  # Input: 
  #   unitDescriptor - word break down of units (num <div> time)
  #   unitToCompare - units to compare to descriptor (1/min)
  #   possibleConcUnits - vector of possible concentration units for check
  #   possibleConcUnits - vector of possible time units for check
  
  # Split descriptor
  ud.split   <- strsplit(unitDescriptor, " ")[[1]]
  prev.units <- UnitBreak(previousUnits)
  new.units  <- UnitBreak(newUnits)
  unit.terms <- c("time", "conc", "volume")
  
  conversion.val <- 1
  next.term.div  <- FALSE
  in.group       <- FALSE
  # Conversions for after div and power, have to account for groups
  
  # Basic
  # Perform analysis/comparison
  for (i in seq_along(ud.split)) {
    
    ud   <- ud.split[i]
    prev <- prev.units[i]
    new  <- new.units[i]
    
    if (ud == "<div>") {
      next.term.div = TRUE
    } 
    
    if (ud %in% unit.terms){
      print(ud)
      i.conversion.val <- conv_unit(1, prev, new)
      if (next.term.div) {
        i.conversion.val <- 1/i.conversion.val
        next.term.div <- FALSE
      }
      conversion.val <- conversion.val * i.conversion.val
    }
  }
  
  new.val <- unitValue * conversion.val
  
  return(new.val)
}

#   
#   if (startsWith(element, "<power>")) {
#     print("Power Fxn")
#     if (comp != "^") {
#       is.match <- FALSE
#       error.message <- "Exponent Does Not Match up"
#       break
#     }
#     
#   } else if (startsWith(element, "<")) {
#     print("Operator")
#     if (element == "<div>") {
#       if (comp != "/") {
#         is.match <- FALSE
#         error.message <- "Division Does Not Match up"
#         break
#       }
#     } else if (element == "<multiply>") {
#       if (comp != "*") {
#         is.match <- FALSE
#         error.message <- "Division Does Not Match up"
#         break
#       }
#     } else if (element == "<addition>") {
#       if (comp != "+") {
#         is.match <- FALSE
#         error.message <- "Addition Does Not Match up"
#         break
#       }
#     } else if (element == "<subtraction>") {
#       if (comp != "-") {
#         is.match <- FALSE
#         error.message <- "Subtraction Does Not Match up"
#         break
#       }
#     } else if (element == "<group>") {
#       if (comp != "(") {
#         is.match <- FALSE
#         error.message <- "Beginning Parenthesis Does Not Match up"
#         break
#       }
#     } else if (element == "<endgroup>") {
#       if (comp != ")") {
#         is.match <- FALSE
#         error.message <- "End Parenthesis Does Not Match up"
#         break
#       }
#     }
#   } else if(element == "num") {
#     print("Number")
#     is.num <- as.numeric(comp)
#     if (is.na(is.num)) {
#       # Return error because not numeric
#       is.match <- FALSE
#       error.message <- "Number is not a number"
#       break
#     }
#   } else if (element == "conc") {
#     print("Concentration")
#     # Check if new term is a concentration term
#     # Pull list of concentration terms
#     if (!(comp %in% possibleConcUnits)) {
#       is.match <- FALSE
#       error.message <- paste0("Unit: '", 
#                               comp,
#                               "' not a possible concentration unit. ",
#                               "Possible units are: ",
#                               paste0(possibleConcUnits, collapse = ", ")
#       )
#       break
#     }
#   } else if (element == "time") {
#     print("Time")
#     if (!(comp %in% possibleTimeUnits)) {
#       is.match <- FALSE
#       error.message <- paste0("Unit: '", 
#                               comp,
#                               "' not a possible time unit. ",
#                               "Possible units are: ",
#                               paste0(possibleTimeUnits, collapse = ", ")
#       )
#       break
#     }
#   }
# }
# 
# out <- list("is.match" = is.match,
#             "message" = error.message)
# return(out)