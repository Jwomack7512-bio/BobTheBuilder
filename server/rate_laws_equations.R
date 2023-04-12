Henri_Michaelis_Menten_Vmax <- function(substrate, Km, Vmax) {
  
  eqn.out <- paste0(Vmax, "*", substrate, "/", "(", Km, "+", substrate, ")")
  
  return(eqn.out)
}

Henri_Michaelis_Menten_no_Vmax <- function(substrate, Km, kcat, enzyme) {
  eqn.out <- paste0(kcat, "*", enzyme, "*", substrate, "/",
                    "(", Km, "+", substrate, ")")
  
  return(eqn.out)
}

Synthesis_By_Rate <- function(RateConstant) {
  return(RateConstant)
}

Synthesis_By_Factor <- function(rateConstant, factor) {
  return(paste0(rateConstant, "*", factor))
}

Degradation_By_Rate <- function(rateConstant, 
                                concentrationDependent, 
                                degradatedVariable) {
  
  if (concentrationDependent) {
    eqn.out <- paste0(rateConstant, "*", concentrationDependent)
  } else {
    eqn.out <- rateConstant
  }
  
  return(eqn.out)
}

Degradation_By_Enzyme_Vmax <- function(degradatedVariable, 
                                  Km,
                                  Vmax) {
  # This is just michaleis menten
  eqn.out <- paste0(Vmax, "*", degradatedVariable, "/", 
                    "(", Km, "+", degradatedVariable, ")")
  
  return(eqn.out)
}

Degradation_By_Enzyme_no_Vmax <- function(degradatedVariable, 
                                          Km, 
                                          kcat, 
                                          enzyme) {
  eqn.out <- paste0(kcat, "*", enzyme, "*", degradatedVariable, "/",
                    "(", Km, "+", degradatedVariable, ")")
  
  return(eqn.out)
}