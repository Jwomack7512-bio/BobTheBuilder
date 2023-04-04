

DeriveDifferentialEquations <- function(compartments.rv,
                                        species.rv,
                                        reactions.rv,
                                        IO.rv,
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

  # Break down var data structure.
  species.list  <- species.rv$species
  species.names <- species.rv$species.names
  
  comp.df   <- compartments.rv$compartments.df
  comp.list <- compartments.rv$compartments
  
  # Initialize Output Vectors
  differential.equations     <- vector()
  differential.eqns.latex    <- vector()
  differential.eqns.for.calc <- vector()
  
  
  for (i in seq_along(species.list)) {
    # Extract species list entry
    species.entry <- species.list[[i]]
  }
  
  # Check to see if model contains Inputs/Outputs to calculate
  if (length(IO.rv$InputOutput) > 0) {runIO <- TRUE} else {runIO <- FALSE}

  #choosing variable to solve the differential equation for
  iter <- 0
  # browser()
  for (var in species.names) {
    iter <- iter + 1
    diff.eqn  <- ""
    latex.eqn <- ""
      
    # Solve Eqns for corresponding differential equations
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
    comp.of.variable <- species.list[[iter]]$Compartment
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
  out.list <- list("diff.eqns" = differential.equations,
                   "latex.diff.eqns" = differential.eqns.latex,
                   "diff.eqns.for.solver" = differential.eqns.for.calc)
  return(out.list)
}


DeriveEquationBasedODEs <- function(speciesEntry,
                            species.list,
                            compartments.list,
                            reactions.rv) {
  
  # species.df - df of species in model with information
  # compartments.df - df of compartments from rv.COMPARTMENTS
  # reactions.rv - RV datastructure for reactions
  
  reactions.ids <- speciesEntry$ReactionIds
  reaction.db <- reactsion.rv$reactions
  
  if (is.na(reactions.ids)) {
    
  } else {
    ode <- c()
    # Loop through reactions
    for (i in seq_along(reactions.ids)) {
      id <- reactions.ids[i]
      # Find chem law
      reaction.law <- reaction.db[[id]]$Law
      
      # Logic for found law
      if (reaction.law == "mass_action") {
        entry <- reactions.rv$massAction[[id]]
        
        reactants <- strsplit(entry$reactants, ", ")[[1]]
        r.stoichiometry <- strsplit(entry$r.stoichiometry, ", ")[[1]]
        products <- strsplit(entry$products, ", ")[[1]]
        p.stoichiometry <- strsplit(entry$p.stoichiometry, ", ")[[1]]
        reversible <- entry$reversible
        kf <- entry$kf
        kr <- entry$kr
        
        if (speciesEntry$name %in% reactants) {
          # Do search to find stoich coeffiecent (var.coef)
          
          # Set bool for direction of equation (var.on.left)
        }
        
        diff.eqn <- law_mass_action(r.stoichiometry, 
                                    reactants, 
                                    p.stoichiometry, 
                                    products, 
                                    reversible, 
                                    kf, 
                                    kr, 
                                    var.on.left, 
                                    var.coef)
      } else if (reaction.law == "mass_action_w_regulation") {
        
      } else if (reaction.law == "michaelis_menton") {
        reaction.entry <- reactions.rv$michaelisMenten[[id]]
        
      } else if (reaction.law == "synthesis") {
        reaction.entry <- reactions.rv$synthesis[[id]]
        
      } else if (reaction.law == "degradation") {
        reaction.entry <- reactions.rv$degradation[[id]]
        
      }
    }
  }
  reactions.df <- reactions.rv$reactions.df
  
  # Unpack eqn info structure
  eqn.id      <- reactions.df$ID
  eqn.type    <- reactions.df$Eqn.Type
  eqn.law     <- reactions.df$Law
  eqn.var     <- reactions.df$Species
  eqn.RCs     <- reactions.df$Rate.Constants
  eqn.comp    <- reactions.df$Compartment
  eqn.var.id  <- reactions.df$Species.Id
  eqn.RCs.id  <- reactions.df$Parameters.Id
  eqn.comp.id <- reactions.df$Compartment.Id
  
  # Initialize algorithm booleans
  diff.eqn <- NA
  latex.eqn <- NA
  first.eqn <- TRUE
  n.eqns <- nrow(reactions.df)
  
  # Begin Algorithm
  if (n.eqns > 0) {
    for (row in 1:n.eqns) {
      vars <- strsplit(reactions.df$Species[row], " ")[[1]]
      for (var in vars) {
        if (var == species) {
          skip <- FALSE
          id   <- reactions.df$ID[row]
          type <- reactions.df$Eqn.Type[row]
          #check other dataframes for id
          # Parse Chem Dataframe
          if (type == "chem_rxn") {
            for (i in 1:nrow(eqn.chem.df)) {
              chem.id <- eqn.chem.df$ID[i]
              if (id == chem.id) {
                row.info   <- eqn.chem.df[i,]
                temp       <-
                  CalcDiffEqnsForChem(row.info, 
                                      var, 
                                      compartments, 
                                      eqn.comp.id[i])
                temp.eqn   <- temp["Diff"][[1]]
                temp.latex <- temp["Latex"][[1]]
              }
            }
          }
          # Parse Enzyme Dataframe
          else if (type == "enzyme_rxn") {
            for (i in 1:nrow(eqn.enz.df)) {
              enz.id <- eqn.enz.df$ID[i]
              if (id == enz.id) {
                row.info   <- eqn.enz.df[i,]
                temp    <-
                  CalcDiffEqnsForEnzyme(row.info, var)
                if(!is.na(temp)) {
                  temp.eqn   <- temp["Diff"][[1]]
                  temp.latex <- temp["Latex"][[1]]
                } else {
                  skip <- TRUE
                }
              }
            }
          }
          else if (type == "syn") {
            for (i in 1:nrow(eqn.syn.df)) {
              syn.id <- eqn.syn.df$ID[i]
              if (id == syn.id) {
                row.info   <- eqn.syn.df[i,]
                temp       <-
                  CalcDiffEqnsForSyn(row.info, var)
                temp.eqn   <- temp["Diff"][[1]]
                temp.latex <- temp["Latex"][[1]]
              }
            }
          }
          else if (type == "deg") {
            for (i in 1:nrow(eqn.deg.df)) {
              deg.id <- eqn.deg.df$ID[i]
              if (id == deg.id) {
                row.info   <- eqn.deg.df[i,]
                temp       <-
                  CalcDiffEqnsForDeg(row.info, var)
                temp.eqn   <- temp["Diff"][[1]]
                temp.latex <- temp["Latex"][[1]]
              }
            }
          }
          # Add single differential equation to all equations
          if (!skip) {
            if (first.eqn) {
              first.eqn <- FALSE
              diff.eqn  <- temp.eqn
              latex.eqn <- temp.latex
            } else {
              minus <- EqnStartMinus(temp.eqn)
              if (minus) {
                diff.eqn <- paste0(diff.eqn, temp.eqn)
                latex.eqn <- paste0(latex.eqn, temp.latex)
              } else {
                diff.eqn <- paste0(diff.eqn, "+", temp.eqn)
                latex.eqn <-
                  paste0(latex.eqn, "+", temp.latex)
              }
            }
          }
        }
      }
    }
  }
  out <- list("Diff" = diff.eqn, "Latex" = latex.eqn)
  return(out)
}