#Functions in this file:
# RenameParameterVector
# VarToLatexForm


jPrint <- function(string) {
  # print through function since Rshiny won't seem to let me bring in my events without it
  print(string)
}

#TODO: Determine if parameter needs $param$ or not based on if itll be in math mode of not
VarToLatexForm <- function(variable) {
  # Takes input variable and changes it to a pretty latex form for latex reader
  # Inputs:
  #   @variable - string variable to be changed to nice formating
  
  # Output: Variable in latex form
  # Example: "My_var" --> "My_{var}
  
  split.var <- str_split(variable, "")[[1]]
  length.of.var <- length(split.var)
  has.underscore = FALSE
  count = 0
  for (letter in split.var) {
    count = count + 1
    if (letter == "_" & count != length.of.var & count == 1) { #prevent splitting if firest/last letter is _
      idx <- count
      has.underscore = TRUE
      break
    }
  }
  if (has.underscore) {
    before <- paste0(split.var[1:idx], collapse = "")
    after <- paste0(split.var[(idx + 1):length.of.var], collapse = "")
    new.var <- paste0(before, "{", after, "}")
    print(new.var)
  }
  else {
    new.var <- variable
  }
  out <- new.var
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
  print("Running Rename Parameter Change for Df")
  n.rows <- nrow(dfToSearch)
  n.cols <- ncol(dfToSearch)
  print(n.rows)
  print(n.cols)
  new.df <- dfToSearch
  #check to make sure rows exist as some dateframes in this program are initiated without columns (columsn just extra check)
  if (n.rows != 0 & n.cols != 0) {
    print("columns not zero")
    for (i in seq(n.rows)) {
      for (j in seq(n.cols)) {
        print(oldName)
        print(dfToSearch[i,j])
        has.var <- grepl(oldName, dfToSearch[i,j], fixed = TRUE)
        if (has.var) {
          #print(gsub(oldName, newName, dfToSearch[i,j]))
          new.df[i,j] <- gsub(oldName, newName, dfToSearch[i,j])
        }
      }
    }
  }

  print(new.df)
  return(new.df)
}

#TODO: Find a way to search strings and db for variable name and replace if exists
#     Cycle through all listed values searching for parameters to change
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



