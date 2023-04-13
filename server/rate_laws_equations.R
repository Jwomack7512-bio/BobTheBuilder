Henri_Michaelis_Menten_Vmax <- function(substrate, Km, Vmax) {
  
  latex = NA
  mj = NA
  ml = NA
  
  # Functional String
  
  str.out <- paste0(Vmax, "*", substrate, "/", "(", Km, "+", substrate, ")")
  
  # Pretty String
  ps <- paste0(Vmax, "[", substrate, "]", 
               "/", 
               "(", Km, " + ", "[", substrate, "]", ")")
  # Latex
  latex <- paste0("//frac{",
                  VarToLatexForm(Vmax), "*", VarToLatexForm(substrate), 
                  "}{",
                  VarToLatexForm(Km), 
                  "+",
                  VarToLatexForm(Km),
                  "}")
  # MathJax
  mj <- Var2MathJ(str.out)
  
  # Mathml
  ml <- katex_mathml(latex)
  
  out.list <- list("string" = str.out,
                   "pretty.string" = ps,
                   "latex" = latex,
                   "mj" = mj,
                   "mathml" = ml)
  print(out.list)
  
  return(out.list)
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

Law_Of_Mass_Action <- function(r.stoich, 
                               reactants,
                               p.stoich,
                               products,
                               reversible,
                               kf,
                               kr) {
  
  # Generated equations for law of mass action for given inputs.
  # Where law of mass action is:
  # for equation aA + bB (kr)<-->(kf) cC + dD then the derviation is as follows:
  # -(1/a)*A^a = -(1/b)*B^b = (1/c)*C^c = (1/d)*D^d = kf*A^a*B^b - kr*C^c*D^d
  
  # Example equation A + 2B ->(kf1) C used here
  # Inputs:
  #   @r.stoich - Coefficients of variables on RHS of equation in vector form c(1)
  #   @reactants - Var Name on right hand side in vector form: c(C)
  #   @p.stoich - Coefficients of var on LHS of equation in vector form: c(1,2)
  #   @products - Variable names of left hand side eqns in vector form: c(A, B)
  #   @reversible - Describes if reaction is forward (forward_only) or
  #             both (both_directions): "both_directions"
  #   @kf - numerical value of forward rate constant: kf1
  #   @kr - numerical value of reverse rate constant: NULL
  
  # Outputs:
  # String of law of mass action result.  For example for A:
  # Case1: A -> B, one var on each side
  
  #split to vectors
  r.stoich  <- strsplit(r.stoich, ", ")[[1]]
  p.stoich  <- strsplit(p.stoich, ", ")[[1]]
  reactants <- strsplit(reactants, ", ")[[1]]
  products  <- strsplit(products, ", ")[[1]]
  n.reactants <- length(reactants)
  n.products  <- length(products)
  
  
  # Build Reactant Part Of Rate Law
  builder <- ""
  for (i in seq(n.reactants)) {
    if (i == 1) {
      if (r.stoich[i] == "1") {
        builder <- reactants[i]
      } else {
        builder <- paste0(r.stoich[i], "*", reactants[i], "^", r.stoich[i])
      }
    } else {
      if (r.stoich[i] == "1") {
        builder <- paste0(builder, "*", reactants[i])
      } else {
        builder <- paste0(builder, "*",
                          r.stoich[i], "*", reactants[i], "^", r.stoich[i])
      }
    }
  }
  # Add rate constant
  rate.from.reactant <- paste0(kf, "*", builder)
  rate.law <- rate.from.reactant
  
  if (reversible == "both_directions") {
    # Build Product Part Of Rate Law
    builder <- ""
    for (i in seq(n.products)) {
      if (i == 1) {
        if (p.stoich[i] == "1") {
          builder <- products[i]
        } else {
          builder <- paste0(p.stoich[i], "*", products[i], "^", p.stoich[i])
        }
      } else {
        if (p.stoich[i] == "1") {
          builder <- paste0(builder, "*", products[i])
        } else {
          builder <- paste0(builder, "*",
                            p.stoich[i], "*", products[i], "^", p.stoich[i])
        }
      }
    }
    # Add rate constant
    rate.from.product <- paste0(kr, "*", builder)
    rate.law <- paste0(rate.law, "-", rate.from.product)
  }
  return(rate.law)
}