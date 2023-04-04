

DeriveDifferentialEquations <- function(compartments.rv,
                                        species.rv,
                                        reactions.rv,
                                        id.rv) 
{
  # Model Description: 
  # Inputs
  # @myModel - df to parse containing all equation parameters (this is typically
  # an output of Rhinsy)
  # @vars_to_diffeq - vector of variables that we want to create differential 
  # equations for
  # @InOutModel - df containing all In/out parameter and values
  
  # Outputs
  #list:
  # @diff.eqns - vector of differential equations in string form
  # @latex.diff.eqns - vector of differential equations in latex form
  # Account for custom differential eqns
  custom.vars <- setdiff(listOfCustomVars, customVarToIgnore)
  
  # Break down var data structure.
  var.list  <- var.datastructure$var.info
  var.names <- var.datastructure$var.names
  comp.df   <- var.datastructure$compartments.df
  comp.list <- var.datastructure$compartments.info
  
  #initialize values
  differential.equations     <- vector()
  differential.eqns.latex    <- vector()
  differential.eqns.for.calc <- vector()
  ifelse(nrow(Input.Output.Df) > 0, runIO <- TRUE, runIO <- FALSE)
  
  #choosing variable to solve the differential equation for
  iter <- 0
  # browser()
  for (var in var.names) {
    iter <- iter + 1
    diff.eqn  <- ""
    latex.eqn <- ""
    if (var %in% custom.vars) {
      idx <- match(var, customVarDF[, 1])
      differential.equations <- c(differential.equations,
                                  customVarDF[idx, 2])
    } else {
      # Differential Equation Solver if Custom Equation is not used
      
      # Solve Eqns for corrresponding differential equations
      out <- CalcDiffForEqns(var,
                             comp.list,
                             reactions.df,
                             eqn.chem.df,
                             eqn.enz.df,
                             eqn.syn.df,
                             eqn.deg.df)
      
      diff.eqn.eqns  <- out["Diff"][[1]]
      latex.eqn.eqns <- out["Latex"][[1]]
      
      if (is.na(diff.eqn.eqns)) {
        no.equation <- TRUE
      } else {
        diff.eqn    <- diff.eqn.eqns
        latex.eqn   <- latex.eqn.eqns
        no.equation <- FALSE
      }
      
      # Add differential equations for IO
      if (runIO) {
        eqn.out <- CalcIOTree_DEQ(Input.Output.Df, 
                                  var, 
                                  var.datastructure,
                                  id.df)
        if (eqn.out$exists) {
          diff.eqn.IO  <- eqn.out$diff.eqn
          latex.eqn.IO <- eqn.out$latex.eqn
          # diff.eqn     <- paste0(diff.eqn, diff.eqn.IO)
          # latex.eqn    <- paste0(latex.eqn, latex.eqn.IO)
          #figure out if this is first addition.
          if (diff.eqn != "") {
            minus <- EqnStartMinus(diff.eqn.IO)
            if (minus) {
              diff.eqn     <- paste0(diff.eqn, diff.eqn.IO)
              latex.eqn    <- paste0(latex.eqn, latex.eqn.IO)
            } else {
              diff.eqn     <- paste0(diff.eqn, "+", diff.eqn.IO)
              latex.eqn    <- paste0(latex.eqn, "+", latex.eqn.IO)
            }
          } else {
            diff.eqn     <- paste0(diff.eqn, diff.eqn.IO)
            latex.eqn    <- paste0(latex.eqn, latex.eqn.IO)
          }
          
          
        }
      }
      #ERROR IS HERE  - NEED TO REROUTE VAR TO VAR ID
      # Find compartment/Volume for variable
      comp.of.variable <- var.list[[iter]]$Compartment
      row.idx <- which(comp.df$Name %in% comp.of.variable)
      comp.vol <- comp.df$Volume[row.idx]
      
      #Sets to zero if no differential solvers were used
      if (diff.eqn == "") {
        diff.eqn <- 0
        latex.eqn <- 0
        diff.eqn.div.vol <- 0
      } else {
        # Divide by compartment volume TODO adding comp.vol and syncying
        # this equation into the diff.eqn datastrucutre. 
        diff.eqn.div.vol <- paste0("(", diff.eqn, ")/", comp.vol)
      }
      differential.equations <-
        c(differential.equations, diff.eqn)
      differential.eqns.latex <-
        c(differential.eqns.latex, latex.eqn)
      differential.eqns.for.calc <- c(
        differential.eqns.for.calc, diff.eqn.div.vol)
    }
  }
  out.list <- list("diff.eqns" = differential.equations,
                   "latex.diff.eqns" = differential.eqns.latex,
                   "diff.eqns.for.solver" = differential.eqns.for.calc)
  return(out.list)
}