
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

#TODO: Find a way to search strings and db for variable name and replace if exists
#     Cycle through all listed values searching for parameters to change
RenameParameter <- function(oldName, newName) {
  # When the parameter is renamed it needs to be renamed in many places including
  # all parameter tables, eqns, eqn tables, differential eqns and the such.
  # Inputs:
  #   @oldName - String name of the parameter to be changed
  #   @newName - Desired String name of the new parameter
  # Output:   
  #   No output. This function will change all reactive variables in it. 
  # Places that Variables need to be changed:
  #   eqns$main, eqns$additional.eqns, eqns$rate.eqns, eqns$time.dep.eqns, eqns$eqn.info
  #   IO$IO.info
  #   DE$eqns?
  #   PP section?
  #   params - pretty much all sections
  
  # Because I am using the specific reactive variables I don't think I can created a function to do this.  I have to 
  idx = 0
  for (string.var in eqns$main) {
    idx = idx + 1 
    has.var <- grepl(oldName, string.var, fixed = TRUE)
    if (has.var) {
      new.eqn <- gsub(oldName, newName, string.var) #replace old name with new and place in new variable
      eqns$main[idx] <- new.eqn #replace string in variable its stored
    }
  }
  eqns$main <- SearchingVectorsForVar(eqns$main, oldName, newName)
  # Have a way to find a match in string
  # finding indices in df that match word: https://stackoverflow.com/questions/39450003/find-string-in-data-frame/39450588
  # grepl(needle, haystack, fixed = TRUE) will tell you if needle is in haystack
  # use gsub(needle, replacement, string)
}

SearchingVectorsForVar <- function(vectorToSearch, oldName, newName) {
  # Idea here is to pass RV through.  Copy it.  Change in code. and Return output that will be assigned to the RV
  idx = 0
  for (string.var in vectorToSearch) {
    idx = idx + 1 
    has.var <- grepl(oldName, string.var, fixed = TRUE)
    if (has.var) {
      new.eqn <- gsub(oldName, newName, string.var) #replace old name with new and place in new variable
      vectorToSearch[idx] <- new.eqn #replace string in variable its stored
    }
  }
  return(vectorToSearch)
}


