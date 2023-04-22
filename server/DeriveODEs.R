

DeriveDifferentialEquations <- function(compartments.rv,
                                        species.rv,
                                        reactions.rv,
                                        IO.rv,
                                        id.rv) 
{
  # Derive all equation/IO based differential equations from stored model 
  # information. The idea is to loop through each species, deriving its 
  # equations and then moving on to the next species. Each species list has a 
  # variable noting its equations and IOs IDs to derive from. These ids are 
  # looked up in their respective lists and the rate law is taken from there
  # and concatenated to the existing chain.
  
  # Model Description: 
  # Inputs
  # @compartments.rv - reactive variable containing all compartment info
  # @species.rv - reactive variable containing all species info (rv.SPECIES)
  # @reactions.rv - reactive variable containing all reaction info
  # @IO.rv - reactive variable containing all input/output info
  # @id.rv - dataframe (RV) containing all id pairs
  
  # Outputs
  #list:
  # @eqns - vector of differential equations in string form
  # @latex.eqns - vector of differential equations in latex form
  # @eqns.for.calc - vector of diffeqs with proper volume divided

  # I think we want to create a list output for each species where the species
  # is the key and the pairs are vectors for the information we want
  # results <- list(species_1, species_2, etc....)
  # where, 
  # speices_1 <- list(diff.eqn., 
  #                   styled.eqn,
  #                   eqn.for.calc,
  #                   latex.eqn,
  #                   mathjax.eqn)
  # Break down var data structure.
  species.list  <- species.rv$species
  species.names <- species.rv$species.names
  species.ids   <- names(species.list)
  
  comp.df   <- compartments.rv$compartments.df
  comp.list <- compartments.rv$compartments
  
  # Initialize Output Vectors
  differential.equations     <- vector()
  differential.eqns.latex    <- vector()
  differential.eqns.for.calc <- vector()
  
  results <- list()
  # Cycle through each speices
  for (i in seq_along(species.list)) {
    eqn.ODEs <- NA
    IO.ODEs  <- NA
    
    # Solve for eqn based odes
    eqn.ODEs <- DeriveEquationBasedODEs(species.list[[i]],
                                        compartments.rv,
                                        reactions.rv)
    
    # Solve for IO based odes 
    IO.ODEs <- DeriveIOBasedODEs(species.list[[i]],
                                 compartments.rv,
                                 IO.rv) 
    
    # Solve for IO based Odes
    results[[species.ids[i]]] <- list(
      "Name" = species.names[i],
      "ODES.eqn.vector"     = eqn.ODEs$eqn.vector,
      "ODES.eqn.string"     = eqn.ODEs$eqn.string,
      "ODES.latex.vector"   = eqn.ODEs$latex.vector,
      "ODES.latex.string"   = eqn.ODEs$latex.string,
      "ODES.mathjax.vector" = eqn.ODEs$mathjax.vector,
      "ODES.mathjax.string" = eqn.ODEs$mathjax.string
      )
  }
  # Calculate eqn based 

  # out.list <- list("diff.eqns" = differential.equations,
  #                  "latex.diff.eqns" = differential.eqns.latex,
  #                  "diff.eqns.for.solver" = differential.eqns.for.calc)
  return(results)
}

DeriveEquationBasedODEs <- function(species.list.entry,
                                    compartments.rv,
                                    reactions.rv) {
  # Derive the ODEs for equation based problems. Here we are passed the entry 
  # For the species list, use that to find corresponding reaction ids.
  # If there are none, NA is returned. Else those reaction ids search
  # the reactions.rv to find the corresponding rate law, which is stored to 
  # a vector. 
  
  # Args
  #   @species.list.entry - specific species entry containing all info 
  #   @compartments.rv - reactive variable containing all compartment info - 
  #   @reactions.rv - reactive variable containing all reaction info
  
  #   @ ODE - vector of rate law expressions for each reaction (NA if none)
  
  # Find in species.list 
  name <- species.list.entry$Name
  id   <- species.list.entry$ID

  
  ODE         <- c()
  latex.ODE   <- c()
  mathjax.ODE <- c()
  if (is.na(species.list.entry$Reaction.ids)) {
    
  } else {
    reactions <- strsplit(species.list.entry$Reaction.ids, ", ")[[1]]
    # browser()
    for (eqn.id in reactions) {
      # Extract equation by ID and appropriate laws
      eqn        <- reactions.rv$reactions[[eqn.id]]
      rate       <- eqn$String.Rate.Law
      latex.rate <- eqn$Latex.Rate.Law
      mj.rate    <- eqn$MathJax.Rate.Law
      law        <- eqn$Reaction.Law
      
      applyMultiple <- FALSE
      multiple      <- "1"
      
      # Find if species Entry is in reactant or product
      inReactant <- id %in% strsplit(eqn$Reactants.id, ", ")[[1]]
      
      # Check for mass action reaction, then check stoich for modifiers
      if (law == "mass_action" || law == "mass_action_w_reg") {
        #if in mass action, search mass action df
        if (law == "mass_action") {
          ma.list <- reactions.rv$massAction[[eqn.id]]
        } else if (law == "mass_action_w_reg") {
          ma.list <- reactions.rv$massActionwReg[[eqn.id]]
        }
        # check for stoich modifier
        if (inReactant) {
          # Determine which index
          reactant.names <- strsplit(ma.list$Reactants, ", ")[[1]]
          idx <- which(reactant.names %in% name)
          stoich <- strsplit(ma.list$r.stoichiometry, ", ")[[1]]
          if (stoich[idx] != "1") {
            applyMultiple <- TRUE
            multiple <- stoich[idx]
          }
        } else {
          product.names <- strsplit(ma.list$Products, ", ")[[1]]
          idx <- which(product.names %in% name)
          stoich <- strsplit(ma.list$p.stoichiometry, ", ")[[1]]
          if (stoich[idx] != "1") {
            applyMultiple <- TRUE
            multiple <- stoich[idx]
          }
        }
      } 
      
      # Build ODE expression 
      if (inReactant) {sign <- "-"} else {sign <- "+"}
      
      if (applyMultiple) {
        ODE <- c(ODE, 
                 paste0(sign, multiple, "*(", rate,")"))
        
        latex.ODE <- c(latex.ODE, 
                       paste0(sign, multiple, "*(", latex.rate, ")"))
        
        mathjax.ODE <- c(mathjax.ODE, 
                         paste0(sign, multiple, "*", 
                                "\\left(", mj.rate, "\\right)"))
      } else {
        ODE <- c(ODE, 
                 paste0(sign, "(", rate, ")"))
        latex.ODE <- c(latex.ODE, 
                       paste0(sign, "(", latex.rate, ")"))
        mathjax.ODE <- c(mathjax.ODE, 
                         paste0(sign, "\\left(", mj.rate, "\\right)"))
      }
    }
  }
  
  # Output list of ODE values
  out <- list("eqn.vector"     = ODE,
              "eqn.string"     = paste0(ODE, collapse = ""),
              "latex.vector"   = latex.ODE,
              "latex.string"   = paste0(latex.ODE, collapse=""),
              "mathjax.vector" = mathjax.ODE,
              "mathjax.string" = paste0(mathjax.ODE, collapse = "")
              )
  
  return(out)
}

DeriveIOBasedODEs <- function(species.list.entry, 
                              compartments.rv,
                              IO.rv) {
  # Derive the ODEs for I\O based problems. Here we are passed the entry 
  # For the species list, use that to find corresponding reaction ids.
  # If there are none, NA is returned. Else those reaction ids search
  # the reactions.rv to find the corresponding rate law, which is stored to 
  # a vector. 
  
  # Args
  #   @species.list.entry - specific species entry containing all info 
  #   @compartments.rv - reactive variable containing all compartment info - 
  #   @IO.rv - reactive variable containing all IO info
  
  #   @ ODE - vector of rate law expressions for each reaction (NA if none)
  
  # Find in species.list 
  name <- species.list.entry$Name
  id   <- species.list.entry$ID
  
  
  ODE         <- c()
  latex.ODE   <- c()
  mathjax.ODE <- c()
  print("PH: Deriving IO ODEs")
  
  if (is.na(species.list.entry$IO.ids)) {
    IOs <- strsplit(species.list.entry$IO.ids, ", ")[[1]]
    for (io.id in IOs) {
      eqn        <- IO.rv$InputOutput[[eqn.id]]
      rate       <- eqn$String.Rate.Law
      latex.rate <- eqn$Latex.Rate.Law
      mj.rate    <- eqn$MathJax.Rate.Law
      law        <- eqn$Reaction.Law
      
      isInput <- id %in% strsplit(eqn$Reactants.id, ", ")[[1]]
      
    }
  }
  
  
  
}


