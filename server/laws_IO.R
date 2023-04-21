
SimpleDiffusion <- function(species1, species2, PS) {
  # if species1 is on 
  
  str.out <- paste0("-", PS, "*(", species1, "-", species2, ")")
  
  ps <- paste0("-", PS, "*([", species1, "]-[", species2, "])")
  
  latex <- paste0("-", 
                  VarToLatexForm(PS),
                  "*(",
                  VarToLatexForm(species1),
                  "-",
                  VarToLatexForm(species2),
                  ")")
  
  mj <- paste0("-", 
               Var2MathJ(PS),
               "*(",
               Var2MathJ(species1),
               "-",
               Var2MathJ(species2),
               ")")
  
  ml <- katex_mathml(latex)
  
  out.list <- list("string" = str.out,
                   "pretty.string" = ps,
                   "latex" = latex,
                   "mj" = mj,
                   "mathml" = ml)
  
  return(out.list)
}

FacilitatedDiffusion <- function(species, Vmax, Km) {
  
  str.out <- paste0(Vmax, "*", species, "\(", Km, "+", species, ")")
  
  ps <- paste0(Vmax, "[", species, "]", "\(", Km, "+[", species, "])")
  
  # Latex
  latex <- paste0("\\frac{",
                  VarToLatexForm(Vmax), "*", VarToLatexForm(species), 
                  "}{",
                  VarToLatexForm(Km), 
                  "+",
                  VarToLatexForm(species),
                  "}")
  
  # MathJax
  mj <- paste0("\\frac{",
               Var2MathJ(Vmax), "*", Var2MathJ(species), 
               "}{",
               Var2MathJ(Km), 
               "+",
               Var2MathJ(species),
               "}")
  
  # Mathml
  ml <- katex_mathml(latex)
  
  # 
  out.list <- list("string" = str.out,
                   "pretty.string" = ps,
                   "latex" = latex,
                   "mj" = mj,
                   "mathml" = ml)
  
  return(out.list)
}

Clearance <- function(species, rateConstant, compartmentVol) {
  
  str.out <- paste0("-", rateConstant, "*", species, "*", compartmentVol)
  
  ps <- paste0("-", rateConstant, "*[", species, "]*", compartmentVol)
  
  latex <- paste0("-",
                  VarToLatexForm(rateConstant),
                  "*",
                  VarToLatexForm(species),
                  "*",
                  VarToLatexForm(compartmentVol))
  
  latex <- paste0("-",
                  Var2MathJ(rateConstant),
                  "*",
                  Var2MathJ(species),
                  "*",
                  Var2MathJ(compartmentVol))
  
  ml <- katex_mathml(latex)
  
  out.list <- list("string" = str.out,
                   "pretty.string" = ps,
                   "latex" = latex,
                   "mj" = mj,
                   "mathml" = ml)
  
  return(out.list)
}

Flow <- function(species, rateConstant) {
  
  str.out <- paste0(rateConstant, "*", species)
  
  ps <- paste0(rateConstant, "[", species, "]")
  
  latex <- paste0(VarToLatexForm(rateConstant),
                  "*",
                  VarToLatexForm(species))
  
  mj <- paste0(Var2MathJ(rateConstant),
               "*",
               Var2MathJ(species))
  
  ml <- katex_mathml(latex)
  
  out.list <- list("string" = str.out,
                   "pretty.string" = ps,
                   "latex" = latex,
                   "mj" = mj,
                   "mathml" = ml)
  
  return(out.list)
}

# FlowOut <- function(species, rateConstant) {
#   
#   eqn <- paste0("-", flowRate, "*", species)
#   
#   return(eqn)
# }

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