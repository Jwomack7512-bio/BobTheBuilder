
SimpleDiffusion <- function(species1, species2, PS) {
  # if species1 is on 
  
  eqn <- paste0("-", "PS", "*(", species1, "-", species2, ")")
  
  return(eqn)
}

FacilitatedDiffusion <- function(species, Vmax, Km) {
  
  eqn <- paste0(Vmax, "*", species, "\(", Km, "+", species, ")")
  
  return(eqn)
}

Clearance <- function(species, rateConstant, compartmentVol) {
  
  eqn <- paste0("-", rateConstant, "*", species, "*", compartmentVol)
  
  return(eqn)
}

FlowIn <- function(species, rateConstant) {
  
  eqn <- paste0(flowRate, "*", species)
  
  return(eqn)
}

FlowOut <- function(species, rateConstant) {
  
  eqn <- paste0("-", flowRate, "*", species)
  
  return(eqn)
}

FlowBetween <- function(speciesIn, 
                        speciesOut, 
                        compartmentIn, 
                        compartmentOut, 
                        flowRates) {
  # @speciesIn - string of species leaving in cOut order ("Sa Sb Sc etc")
  # @speciesOut - species leaving beginning flow
  # @compartmentIn - string of compartments flow going to ("C1 C2 C3")
  # @compartmentOut - compartment flow leaving from 
  # @flowrate - flowrate variables in order of flowOut, flowIn1, flowIn2, etc
  
  flows <- strsplit(flowRates, ", ")[[1]]
  
}