

varToLatexForm <- function(variable) {
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


