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
  latex <- paste0("\\frac{",
                  VarToLatexForm(Vmax), "*", VarToLatexForm(substrate), 
                  "}{",
                  VarToLatexForm(Km), 
                  "+",
                  VarToLatexForm(substrate),
                  "}")
  # MathJax
  mj <- paste0("\\frac{",
               Var2MathJ(Vmax), "*", Var2MathJ(substrate), 
               "}{",
               Var2MathJ(Km), 
               "+",
               Var2MathJ(substrate),
               "}")
  Var2MathJ(str.out)
  
  # Mathml
  ml <- katex_mathml(latex)
  
  # mathml_pieces <- paste0("<apply>",
  #                         "<addition")
  # ml <-   mathml_expr <- paste0(
  #   "<math xmlns='http://www.w3.org/1998/Math/MathML'>",
  #   paste0(mathml_pieces, collapse = ""),
  #   "</math>")
  # 
  out.list <- list("string" = str.out,
                   "pretty.string" = ps,
                   "latex" = latex,
                   "mj" = mj,
                   "mathml" = ml)
  
  return(out.list)
}

Henri_Michaelis_Menten_no_Vmax <- function(substrate, Km, kcat, enzyme) {
  
  ps = NA
  latex = NA
  mj = NA
  ml = NA
  
  # Functional String
  
  str.out <- paste0(kcat, "*", enzyme, "*", substrate, "/",
                    "(", Km, "+", substrate, ")")
  
  # Pretty String
  ps <- paste0(kcat, "[", enzyme, "]", "[", substrate, "]", 
               "/", 
               "(", Km, " + ", "[", substrate, "]", ")")
  # Latex
  latex <- paste0("\\frac{",
                  VarToLatexForm(kcat), "*", VarToLatexForm(enzyme),
                  "*", VarToLatexForm(substrate), 
                  "}{",
                  VarToLatexForm(Km), 
                  "+",
                  VarToLatexForm(substrate),
                  "}")
  # MathJax
  mj <- paste0("\\frac{",
               Var2MathJ(kcat), "*", Var2MathJ(enzyme),
               "*", Var2MathJ(substrate),
               "}{",
               Var2MathJ(Km),
               "+",
               Var2MathJ(substrate),
               "}")
  # Mathml
  ml <- katex_mathml(latex)
  
  
  out.list <- list("string" = str.out,
                   "pretty.string" = ps,
                   "latex" = latex,
                   "mj" = mj,
                   "mathml" = ml)
  
  return(out.list)
}

Synthesis_By_Rate <- function(rateConstant) {
  
  str.out <- rateConstant
  
  ps <- rateConstant
  
  latex <- VarToLatexForm(rateConstant)
  
  mj <- Var2MathJ(rateConstant)
  
  # Mathml
  ml <- katex_mathml(latex)
  
  out.list <- list("string" = str.out,
                   "pretty.string" = ps,
                   "latex" = latex,
                   "mj" = mj,
                   "mathml" = ml)
  
  return(out.list)
}

Synthesis_By_Factor <- function(rateConstant, factor) {
  
  str.out <- paste0(rateConstant, "*", factor)
  
  ps <- paste0(rateConstant, "*", "[", factor, "]")
  
  latex <- paste0(VarToLatexForm(rateConstant), "*", VarToLatexForm(factor))
  
  mj <- paste0(Var2MathJ(rateConstant), "*", Var2MathJ(factor))
  
  # Mathml
  ml <- katex_mathml(latex)
  
  out.list <- list("string" = str.out,
                   "pretty.string" = ps,
                   "latex" = latex,
                   "mj" = mj,
                   "mathml" = ml)
  return(out.list)
}

Degradation_By_Rate <- function(rateConstant, 
                                concentrationDependent, 
                                degradatedVariable) {
  
  if (concentrationDependent) {

    str.out <- paste0(rateConstant, "*", degradatedVariable)
    
    ps <- paste0(rateConstant, "*", "[", degradatedVariable, "]")
    
    latex <- paste0(VarToLatexForm(rateConstant), "*", VarToLatexForm(degradatedVariable))
    
    mj <- paste0(Var2MathJ(rateConstant), "*", Var2MathJ(degradatedVariable))
    
    # Mathml
    ml <- katex_mathml(latex)
  } else {
    
    str.out <- rateConstant
    
    ps <- rateConstant
    
    latex <- VarToLatexForm(rateConstant)
    
    mj <- Var2MathJ(rateConstant)
    
    # Mathml
    ml <- katex_mathml(latex)
  }
  
  out.list <- list("string" = str.out,
                   "pretty.string" = ps,
                   "latex" = latex,
                   "mj" = mj,
                   "mathml" = ml)
  return(out.list)
}

Degradation_By_Enzyme_Vmax <- function(degradatedVariable, 
                                       Km,
                                       Vmax) {
  
  # Functional String
  str.out <- paste0(Vmax, "*", degradatedVariable, "/",
                    "(", Km, "+", degradatedVariable, ")")
  
  # Pretty String
  ps <- paste0(Vmax, "[", degradatedVariable, "]", 
               "/", 
               "(", Km, " + ", "[", degradatedVariable, "]", ")")
  # Latex
  latex <- paste0("\\frac{",
                  VarToLatexForm(Vmax), "*", VarToLatexForm(degradatedVariable), 
                  "}{",
                  VarToLatexForm(Km), 
                  "+",
                  VarToLatexForm(degradatedVariable),
                  "}")
  # MathJax
  mj <- paste0("\\frac{",
               Var2MathJ(Vmax), "*", Var2MathJ(degradatedVariable), 
               "}{",
               Var2MathJ(Km), 
               "+",
               Var2MathJ(degradatedVariable),
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

Degradation_By_Enzyme_no_Vmax <- function(degradatedVariable, 
                                          Km, 
                                          kcat, 
                                          enzyme) {
  # Functional String
  str.out <- paste0(kcat, "*", enzyme, "*", degradatedVariable, "/",
                    "(", Km, "+", degradatedVariable, ")")
  
  # Pretty String
  ps <- paste0(kcat, "[", enzyme, "]", "[", degradatedVariable, "]", 
               "/", 
               "(", Km, " + ", "[", degradatedVariable, "]", ")")
  # Latex
  latex <- paste0("\\frac{",
                  VarToLatexForm(kcat), "*", VarToLatexForm(enzyme),
                  "*", VarToLatexForm(degradatedVariable), 
                  "}{",
                  VarToLatexForm(Km), 
                  "+",
                  VarToLatexForm(degradatedVariable),
                  "}")
  # MathJax
  mj <- paste0("\\frac{",
                  Var2MathJ(kcat), "*", Var2MathJ(enzyme),
                  "*", Var2MathJ(degradatedVariable), 
                  "}{",
                  Var2MathJ(Km), 
                  "+",
                  Var2MathJ(degradatedVariable),
                  "}")
  
  # Mathml
  ml <- katex_mathml(latex)
  
  out.list <- list("string" = str.out,
                   "pretty.string" = ps,
                   "latex" = latex,
                   "mj" = mj,
                   "mathml" = ml)
  
  return(out.list)
}

Law_Of_Mass_Action <- function(r.stoich, 
                               reactants,
                               p.stoich,
                               products,
                               reversible,
                               kf,
                               kr,
                               kf.latex = NULL,
                               kr.latex = NULL) {
  
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
  #   @kf.latex - latex expression for kf, used for regulator expressions
  #   @kr.latex - latex expression for kr, used for reulator expressions
  
  # Outputs:
  # String of law of mass action result.  For example for A:
  # Case1: A -> B, one var on each side
  
  
  ps = NA
  latex = NA
  mj = NA
  ml = NA
  
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
        lat.builder <- VarToLatexForm(reactants[i])
      } else {
        builder <- paste0(reactants[i], "^", r.stoich[i])
        lat.builder <- paste0(VarToLatexForm(reactants[i]), 
                              "^{", 
                              r.stoich[i],
                              "}")
      }
    } else {
      if (r.stoich[i] == "1") {
        builder <- paste0(builder, "*", reactants[i])
        lat.builder <- paste0(lat.builder, "*", VarToLatexForm(reactants[i]))
      } else {
        builder <- paste0(builder, 
                          "*", 
                          reactants[i], 
                          "^", 
                          r.stoich[i])
        
        lat.builder <- paste0(lat.builder,
                              "*",
                              VarToLatexForm(reactants[i]),
                              "^{",
                              r.stoich[i],
                              "}")
      }
    }
  }
  # Add rate constant
  rate.from.reactant <- paste0(kf, "*", builder)
  rate.law <- rate.from.reactant  # Add rate constant
  
  # Latex
  if (is.null(kf.latex)) {
    lat.rate.from.reactant <- paste0(VarToLatexForm(kf), "*", lat.builder)
  } else {
    lat.rate.from.reactant <- paste0(kf.latex, "*", lat.builder)
  }
  latex.rate.law <- lat.rate.from.reactant
  
  if (reversible == "both_directions") {
    # Build Product Part Of Rate Law
    builder <- ""
    for (i in seq(n.products)) {
      if (i == 1) {
        if (p.stoich[i] == "1") {
          builder <- products[i]
          lat.builder <- VarToLatexForm(products[i])
        } else {
          builder <- paste0(products[i], "^", p.stoich[i])
          lat.builder <- paste0(VarToLatexForm(products[i]), 
                                "^{", 
                                p.stoich[i],
                                "}")
        }
      } else {
        if (p.stoich[i] == "1") {
          builder <- paste0(builder, "*", products[i])
          lat.builder <- paste0(builder, "*", VarToLatexForm(products[i]))
        } else {
          builder <- paste0(builder, "*",products[i], "^", p.stoich[i])
          lat.builder <- paste0(lat.builder, 
                                "*",
                                VarToLatexForm(products[i]), 
                                "^{", 
                                p.stoich[i],
                                "}")
        }
      }
    }
    # Add rate constant
    rate.from.product <- paste0(kr, "*", builder)
    rate.law <- paste0(rate.law, "-", rate.from.product)
    
    # Latex
    if (is.null(kr.latex)) {
      lat.rate.from.product <- paste0(VarToLatexForm(kr), "*", lat.builder)
      latex.rate.law <- paste0(latex.rate.law, "-", lat.rate.from.product)
    } else {
      lat.rate.from.product <- paste0(kr.latex, "*", lat.builder)
      latex.rate.law <- paste0(latex.rate.law, "-", lat.rate.from.product)
    }

  }
  
  # Mathjax
  mj <- latex.rate.law
  
  # Mathml
  ml <- katex_mathml(latex.rate.law)
  
  out.list <- list("string" = rate.law,
                   "pretty.string" = ps,
                   "latex" = latex.rate.law,
                   "mj" = mj,
                   "mathml" = ml)
  
  return(out.list)
}

Regulated_Law_Of_Mass_Action <- function(r.stoich, 
                                         reactants,
                                         p.stoich,
                                         products,
                                         reversible,
                                         kf,
                                         kr,
                                         Use.Forward.Mod,
                                         Forward.Mods,
                                         Forward.Pars,
                                         Use.Reverse.Mod,
                                         Reverse.Mods,
                                         Reverse.Pars) {
  
  # assign initial values to these
  kf.latex <- NULL
  kr.latex <- NULL
  
  # This function will (for now) calculate a new kf and kr based on the 
  # regulator values and then pass that info to the Law_of_Mass_Action fxn. 
  if (Use.Forward.Mod) {
    kf       <- regulatorToRate(Forward.Mods, Forward.Pars)
    kf.latex <- regulatorToRateLatex(Forward.Mods, Forward.Pars)
  }
  if (Use.Reverse.Mod) {
    kr       <- regulatorToRate(Reverse.Mods, Reverse.Pars)
    kr.latex <- regulatorToRateLatex(Reverse.Mods, Reverse.Pars)
  }
  

  results <- Law_Of_Mass_Action(r.stoich, 
                                reactants,
                                p.stoich,
                                products,
                                reversible,
                                kf,
                                kr,
                                kf.latex = kf.latex,
                                kr.latex = kr.latex)
  
  out.list <- list("string" = results$string,
                   "pretty.string" = results$pretty.string,
                   "latex" = results$latex,
                   "mj" = results$mj,
                   "mathml" = results$mathml)
}