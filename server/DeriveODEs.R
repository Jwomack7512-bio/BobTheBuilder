

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
    for (eqn.id in reactions) {
      # Extract equation by ID and appropriate laws
      eqn        <- reactions.rv$reactions[[eqn.id]]
      rate       <- eqn$String.Rate.Law
      latex.rate <- eqn$Latex.Rate.Law
      mj.rate    <- eqn$MathJax.Rate.Law
      
      # Find if species Entry is in reactant or product
      inReactant <- id %in% strsplit(eqn$Reactants.id , ", ")[[1]]
      
      # Build ODE expression 
      if (inReactant) {
        ODE <- c(ODE, paste0("-(", rate, ")"))
        latex.ODE <- c(latex.ODE, paste0("-(", latex.rate, ")"))
        mathjax.ODE <- c(mathjax.ODE, paste0("-(", mj.rate, ")"))
      } else {
        ODE <- c(ODE, paste0("+(", rate, ")"))
        latex.ODE <- c(latex.ODE, paste0("+(", latex.rate, ")"))
        mathjax.ODE <- c(mathjax.ODE, paste0("+(", mj.rate, ")"))
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


# DeriveEquationBasedODEs <- function(speciesEntry,
#                                     species.list,
#                                     compartments.list,
#                                     reactions.rv) {
#   
#   # species.df - df of species in model with information
#   # compartments.df - df of compartments from rv.COMPARTMENTS
#   # reactions.rv - RV datastructure for reactions
#   
#   reactions.ids <- speciesEntry$ReactionIds
#   reaction.db <- reactions.rv$reactions
#   
#   if (is.na(reactions.ids)) {
#     
#   } else {
#     ode <- c()
#     # Loop through reactions
#     for (i in seq_along(reactions.ids)) {
#       id <- reactions.ids[i]
#       # Find chem law
#       reaction.law <- reaction.db[[id]]$Law
#       
#       # Logic for found law
#       if (reaction.law == "mass_action") {
#         entry <- reactions.rv$massAction[[id]]
#         
#         reactants <- strsplit(entry$reactants, ", ")[[1]]
#         r.stoichiometry <- strsplit(entry$r.stoichiometry, ", ")[[1]]
#         products <- strsplit(entry$products, ", ")[[1]]
#         p.stoichiometry <- strsplit(entry$p.stoichiometry, ", ")[[1]]
#         reversible <- entry$reversible
#         kf <- entry$kf
#         kr <- entry$kr
#         
#         if (speciesEntry$name %in% reactants) {
#           # Do search to find stoich coeffiecent (var.coef)
#           
#           # Set bool for direction of equation (var.on.left)
#         }
#         
#         diff.eqn <- law_mass_action(r.stoichiometry, 
#                                     reactants, 
#                                     p.stoichiometry, 
#                                     products, 
#                                     reversible, 
#                                     kf, 
#                                     kr, 
#                                     var.on.left, 
#                                     var.coef)
#       } else if (reaction.law == "mass_action_w_regulation") {
#         
#       } else if (reaction.law == "michaelis_menton") {
#         reaction.entry <- reactions.rv$michaelisMenten[[id]]
#         
#       } else if (reaction.law == "synthesis") {
#         reaction.entry <- reactions.rv$synthesis[[id]]
#         
#       } else if (reaction.law == "degradation") {
#         reaction.entry <- reactions.rv$degradation[[id]]
#         
#       }
#     }
#   }
#   reactions.df <- reactions.rv$reactions.df
#   
#   # Unpack eqn info structure
#   eqn.id      <- reactions.df$ID
#   eqn.type    <- reactions.df$Eqn.Type
#   eqn.law     <- reactions.df$Law
#   eqn.var     <- reactions.df$Species
#   eqn.RCs     <- reactions.df$Rate.Constants
#   eqn.comp    <- reactions.df$Compartment
#   eqn.var.id  <- reactions.df$Species.Id
#   eqn.RCs.id  <- reactions.df$Parameters.Id
#   eqn.comp.id <- reactions.df$Compartment.Id
#   
#   # Initialize algorithm booleans
#   diff.eqn <- NA
#   latex.eqn <- NA
#   first.eqn <- TRUE
#   n.eqns <- nrow(reactions.df)
#   
#   # Begin Algorithm
#   if (n.eqns > 0) {
#     for (row in 1:n.eqns) {
#       vars <- strsplit(reactions.df$Species[row], " ")[[1]]
#       for (var in vars) {
#         if (var == species) {
#           skip <- FALSE
#           id   <- reactions.df$ID[row]
#           type <- reactions.df$Eqn.Type[row]
#           #check other dataframes for id
#           # Parse Chem Dataframe
#           if (type == "chem_rxn") {
#             for (i in 1:nrow(eqn.chem.df)) {
#               chem.id <- eqn.chem.df$ID[i]
#               if (id == chem.id) {
#                 row.info   <- eqn.chem.df[i,]
#                 temp       <-
#                   CalcDiffEqnsForChem(row.info, 
#                                       var, 
#                                       compartments, 
#                                       eqn.comp.id[i])
#                 temp.eqn   <- temp["Diff"][[1]]
#                 temp.latex <- temp["Latex"][[1]]
#               }
#             }
#           }
#           # Parse Enzyme Dataframe
#           else if (type == "enzyme_rxn") {
#             for (i in 1:nrow(eqn.enz.df)) {
#               enz.id <- eqn.enz.df$ID[i]
#               if (id == enz.id) {
#                 row.info   <- eqn.enz.df[i,]
#                 temp    <-
#                   CalcDiffEqnsForEnzyme(row.info, var)
#                 if(!is.na(temp)) {
#                   temp.eqn   <- temp["Diff"][[1]]
#                   temp.latex <- temp["Latex"][[1]]
#                 } else {
#                   skip <- TRUE
#                 }
#               }
#             }
#           }
#           else if (type == "syn") {
#             for (i in 1:nrow(eqn.syn.df)) {
#               syn.id <- eqn.syn.df$ID[i]
#               if (id == syn.id) {
#                 row.info   <- eqn.syn.df[i,]
#                 temp       <-
#                   CalcDiffEqnsForSyn(row.info, var)
#                 temp.eqn   <- temp["Diff"][[1]]
#                 temp.latex <- temp["Latex"][[1]]
#               }
#             }
#           }
#           else if (type == "deg") {
#             for (i in 1:nrow(eqn.deg.df)) {
#               deg.id <- eqn.deg.df$ID[i]
#               if (id == deg.id) {
#                 row.info   <- eqn.deg.df[i,]
#                 temp       <-
#                   CalcDiffEqnsForDeg(row.info, var)
#                 temp.eqn   <- temp["Diff"][[1]]
#                 temp.latex <- temp["Latex"][[1]]
#               }
#             }
#           }
#           # Add single differential equation to all equations
#           if (!skip) {
#             if (first.eqn) {
#               first.eqn <- FALSE
#               diff.eqn  <- temp.eqn
#               latex.eqn <- temp.latex
#             } else {
#               minus <- EqnStartMinus(temp.eqn)
#               if (minus) {
#                 diff.eqn <- paste0(diff.eqn, temp.eqn)
#                 latex.eqn <- paste0(latex.eqn, temp.latex)
#               } else {
#                 diff.eqn <- paste0(diff.eqn, "+", temp.eqn)
#                 latex.eqn <-
#                   paste0(latex.eqn, "+", temp.latex)
#               }
#             }
#           }
#         }
#       }
#     }
#   }
#   out <- list("Diff" = diff.eqn, "Latex" = latex.eqn)
#   return(out)
# }