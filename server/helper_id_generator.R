

GenerateId <- function(currentSeed, type) {
  # Generate an unique id for variables in the model to be referenced by if need be
  # Inputs: 
  #   @currentSeed - value of number of seed being created. This increments to make unique var
  #   @type - where "variable" or "parameter" or "eqn" or "diffeq"
  # Outputs: 
  #   @list: seed - incremented seed Values, id - unique id of variable
  
  #format the seed to a nice format and convert it to a string
  if (currentSeed < 10) {
    seed <- paste0("000", as.character(currentSeed))
  } else if (currentSeed < 100) {
    seed <- paste0("00", as.character(currentSeed))
  } else if (currentSeed < 1000) {
    seed <- paste0("0", as.character(currentSeed))
  } else {
    seed <- as.character(currentSeed)
  }
  
  #create unique id
  if (type == "variable") {
    id <- paste0("var", seed)
  } else if (type == "parameter") {
    id <- paste0("par", seed)
  } else if (type == "eqn") {
    id <- paste0("eqn", seed)
  } else if (type == "diffeq") {
    id <- paste0("dif", seed)
  }
  
  #increment seed
  currentSeed <- currentSeed + 1
  
  # package output
  out <- list(currentSeed, id)
  names(out) <- c("seed", "id")
  return(out)
}




