# Flow of solvers 
# Call Main Function: calc_differential_equations
# This will call Parsers to parse the data structures for eqns and IO
# Starting with eqns: CalcDiffForEqns which will pass data to individual parsers
# for chem, enz, syn, deg, etc. 
# After eqns, data gets passed to CalcIOTree_DEQ to solve for IOs
# This will parse the IO dataframe/list and pass relevant data to its 
# Corresponding laws: flow, diffusion, clearance, etc. 


# Helper Functions -------------------------------------------------------------


extract_data <- function(myModel, var_to_subset_with){
# This searches a dataframe for every instance of a variable in RHS and LHS var
# and extracts those rows, returning this subsetted dataframe

# Inputs:
# myModel - saved df that contains model information with RHS variables in the 
# 3rd column and LHS variables in the 5th colun
# var_to_subset_with - variable to search for in 'myModel' dataframe

# Outputs:
# Outputs dataframe subsetting with var_to_subset_with (this is meant to be used
#with in the differential equation solver)
  index_of_rows_with_var <- vector()
  for (row in 1:nrow(myModel)) {
    #grabs RHS vars, splits them so they can be searched for wanted variable
    RHS_var <- str_split(myModel[row,3], " ")[[1]] 
    LHS_var <- str_split(myModel[row,5], " ")[[1]] 
    #find indices containing var name
    if (var_to_subset_with %in% RHS_var | var_to_subset_with %in% LHS_var) { 
      #adds index to vector to subset main df later
      index_of_rows_with_var <- c(index_of_rows_with_var, row) 
    }
  }    
  temp_df <- myModel[index_of_rows_with_var, ] #extract var rows
  return(temp_df)
}

RemovePlusSignFromStart <- function(string) {
# Removes the first letter of a string if it is a plus sign
# Inputs:
#   @string - string to remove letter from 
# Output:
#   @out -string without a plus in first letter if it exists
# ex. string <- "+k_r3",   out <- "k_r3"
  
  split.str <- str_split(string, "")[[1]]
  out <- ""
  for (i in seq(length(split.str))) {
    if (i == 1 & split.str[i] == "+") {
      #pass
    } else {
      out <- paste0(out, split.str[i])
    }
  }
  return(out)
}

EqnStartMinus <- function(eqn) {
# Determines if eqn starts with a minus or not
# Inputs:
#   eqn - string of eqn to be tested
# Outputs:
#   TRUE if eqn beings with minus, FALSE if not
  first.letter <- strsplit(eqn, "")[[1]][1]
  if (first.letter == "-") {
    begins.with.minus = TRUE
  } else {
    begins.with.minus = FALSE
  }
  return(begins.with.minus)
}

# Equation Laws-----------------------------------------------------------------

law_mass_action <- function(RHS_coef, 
                            RHS_var, 
                            LHS_coef, 
                            LHS_var, 
                            arrow_type, 
                            kf, 
                            kr, 
                            var_on_left, 
                            var_coef){
# Generated equations for law of mass action for given inputs.
# Where law of mass action is:
# for equation aA + bB (kr)<-->(kf) cC + dD then the derviation is as follows:
# -(1/a)*A^a = -(1/b)*B^b = (1/c)*C^c = (1/d)*D^d = kf*A^a*B^b - kr*C^c*D^d

# Example equation A + 2B ->(kf1) C used here
# Inputs:
#   @RHS_coef - Coefficients of variables on RHS of equation in vector form c(1)
#   @RHS_var - Var Name on right hand side in vector form: c(C)
#   @LHS_coef - Coefficients of var on LHS of equation in vector form: c(1,2)
#   @LHS_var - Variable names of left hand side eqns in vector form: c(A, B)
#   @arrowtype - Describes if reaction is forward (forward_only) or
#             both (both_directions): "both_directions"
#   @kf - numerical value of forward rate constant: kf1
#   @kr - numerical value of reverse rate constant: NULL
#   @var_on_left - bool that tells if the current variable is on the LHS
#     For example if this was deriving for A, then TRUE.  
#     IF this was deriving for C then false, (B=TRUE)
#   @var_coef - coefficient of lookup variable. A-->1, B-->2, C-->1,
#     so if looking up B value would be 2.

# Outputs:
# String of law of mass action result.  For example for A:
# Case1: A -> B, one var on each side
  
  if (length(RHS_var) == 1 & length(LHS_var) == 1) {
    #Case 1.1 A <--> B, Reaction flows both ways
    if (arrow_type == "both_directions") {
      ifelse(
        as.numeric(RHS_coef) == 1,
        RHS_eqn <- paste0(kr, "*", RHS_var),
        RHS_eqn <-
          paste0(kr, "*", RHS_var, "^", RHS_coef)
      )
      ifelse(
        as.numeric(LHS_coef) == 1,
        LHS_eqn <- paste0(kf, "*", LHS_var),
        LHS_eqn <-
          paste0(kf, "*", LHS_var, "^", LHS_coef)
      )
      
      #this if/else multiplies the coefficient of the var to the equation
      #dependent on which var is being processed using var_on_left
      if (var_coef > 1) {
        ifelse(
          var_on_left,
          eqn_out <-
            paste0("-", var_coef, "*", LHS_eqn,
                   "+", var_coef, "*", RHS_eqn),
          eqn_out <-
            paste0("+", var_coef, "*", LHS_eqn,
                   "-", var_coef, "*", RHS_eqn)
        )
      } else{
        ifelse(
          var_on_left,
          eqn_out <-
            paste0("-", LHS_eqn, "+", RHS_eqn),
          eqn_out <- paste0(LHS_eqn, "-", RHS_eqn)
        )
      }
    }
    #Case 1.2 A->B, only in forward direction
    else if (arrow_type == "forward_only") {
      ifelse(
        as.numeric(LHS_coef) == 1,
        LHS_eqn <- paste0(kf, "*", LHS_var),
        LHS_eqn <-
          paste0(kf, "*", LHS_var, "^", LHS_coef)
      )
      eqn_out <- LHS_eqn
      if (var_coef > 1) {
        ifelse(
          var_on_left,
          eqn_out <-
            paste0("-", var_coef, "*", eqn_out),
          eqn_out <- paste0(var_coef, "*", eqn_out)
        )
      } else{
        ifelse(var_on_left,
               eqn_out <- paste0("-", eqn_out),
               eqn_out <- eqn_out)
      }
    }
  }
  #Case 2
  else if (length(LHS_var) > 1 & length(RHS_var) == 1) {
    #Case A + B <--> C
    if (arrow_type == "both_directions") {
      ifelse(
        as.numeric(RHS_coef) == 1,
        RHS_eqn <- paste0(kr, "*", RHS_var),
        RHS_eqn <- paste0(kr, RHS_var, "^", RHS_coef)
      )
      #RHS_eqn <- paste0("(", RHS_eqn, ")")
      for (i in seq(length(LHS_var))) {
        if (i == 1) {
          ifelse(
            as.numeric(LHS_coef[i]) == 1,
            LHS_eqn <- paste0(kf, "*", LHS_var[i]),
            LHS_eqn <- paste0(kf, "*", LHS_var[i],
                              "^", LHS_coef[i])
          )
        } else{
          ifelse(
            as.numeric(LHS_coef[i]) == 1,
            LHS_eqn <-
              paste0(LHS_eqn, "*", LHS_var[i]),
            LHS_eqn <-
              paste0(LHS_eqn, "*", LHS_var[i],
                     "^", LHS_coef[i])
          )
        }
      }
      #eqn_out <- paste0("(", LHS_eqn, " - ", RHS_eqn, ")")
      
      if (var_coef > 1) {
        ifelse(
          var_on_left,
          eqn_out <-
            paste0("-", var_coef, "*", LHS_eqn,
                   "+", var_coef, "*", RHS_eqn),
          eqn_out <-
            paste0("+", var_coef, "*", LHS_eqn,
                   "-", var_coef, "*", RHS_eqn)
        )
      } else{
        ifelse(
          var_on_left,
          eqn_out <-
            paste0("-", LHS_eqn, "+", RHS_eqn),
          eqn_out <- paste0(LHS_eqn, "-", RHS_eqn)
        )
      }
    }
    #Case A + B --> C
    else if (arrow_type == "forward_only") {
      for (i in seq(length(LHS_var))) {
        if (i == 1) {
          ifelse(
            as.numeric(LHS_coef[i]) == 1,
            LHS_eqn <- paste0(kf, "*", LHS_var[i]),
            LHS_eqn <- paste0(kf, "*", LHS_var[i],
                              "^", LHS_coef[i])
          )
        } else{
          ifelse(
            as.numeric(LHS_coef[i]) == 1,
            LHS_eqn <-
              paste0(LHS_eqn, "*", LHS_var[i]),
            LHS_eqn <-
              paste0(LHS_eqn, "*", LHS_var[i],
                     "^", LHS_coef[i])
          )
        }
      }
      eqn_out <- LHS_eqn
      if (var_coef > 1) {
        ifelse(
          var_on_left,
          eqn_out <-
            paste0("-", var_coef, "*", eqn_out),
          eqn_out <- paste0(var_coef, "*", eqn_out)
        )
      } else{
        ifelse(var_on_left,
               eqn_out <- paste0("-", eqn_out),
               eqn_out <- eqn_out)
      }
    }
  }
  #Case 3
  else if (length(RHS_var) > 1 & length(LHS_var) == 1) {
    #print("EC:3")
    #Case A <--> B + C
    if (arrow_type == "both_directions") {
      ifelse(
        as.numeric(LHS_coef) == 1,
        LHS_eqn <- paste0(kf, "*", LHS_var),
        LHS_eqn <-
          paste0(kf, "*", LHS_var, "^", LHS_coef)
      )
      
      for (i in seq(length(RHS_var))) {
        if (i == 1) {
          ifelse(
            as.numeric(RHS_coef[i]) == 1,
            RHS_eqn <- paste0(kr, "*", RHS_var[i]),
            RHS_eqn <- paste0(kr, "*", RHS_var[i],
                              "^", RHS_coef[i])
          )
        } else{
          ifelse(
            as.numeric(RHS_coef[i]) == 1,
            RHS_eqn <-
              paste0(RHS_eqn, "*", RHS_var[i]),
            RHS_eqn <-
              paste0(RHS_eqn, "*", RHS_var[i],
                     "^", RHS_coef[i])
          )
        }
      }
      if (var_coef > 1) {
        ifelse(
          var_on_left,
          eqn_out <-
            paste0("-", var_coef, "*", LHS_eqn,
                   "+", var_coef, "*", RHS_eqn),
          eqn_out <-
            paste0("+", var_coef, "*", LHS_eqn,
                   "-", var_coef, "*", RHS_eqn)
        )
      } else{
        ifelse(
          var_on_left,
          eqn_out <-
            paste0("-", LHS_eqn, "+", RHS_eqn),
          eqn_out <- paste0(LHS_eqn, "-", RHS_eqn)
        )
      }
    }
    #Case: A --> B + c
    else if (arrow_type == "forward_only") {
      for (i in seq(length(LHS_var))) {
        if (i == 1) {
          ifelse(
            as.numeric(LHS_coef[i]) == 1,
            LHS_eqn <- paste0(kf, "*", LHS_var[i]),
            LHS_eqn <- paste0(kf, "*", LHS_var[i],
                              "^", LHS_coef[i])
          )
        } else{
          ifelse(
            as.numeric(LHS_coef[i]) == 1,
            LHS_eqn <-
              paste0(LHS_eqn, "*", LHS_eqn[i]),
            LHS_eqn <-
              paste0(LHS_eqn, "*", LHS_eqn[i],
                     "^", LHS_eqn[i])
          )
        }
      }
      eqn_out <- LHS_eqn
      if (var_coef > 1) {
        ifelse(
          var_on_left,
          eqn_out <-
            paste0("-", var_coef, "*", eqn_out),
          eqn_out <- paste0(var_coef, "*", eqn_out)
        )
      } else{
        ifelse(var_on_left,
               eqn_out <- paste0("-", eqn_out),
               eqn_out <- eqn_out)
      }
    }
  }
  #Case 4
  else{
    #need to finish.  Here will go if RHS&&LHS>1.
    #Just copy an paste things from the above sections.  Run and test and
    # hopefully it doens't take too long.
    #Case A + B <--> C + D
    if (arrow_type == "both_directions") {
      for (i in seq(length(RHS_var))) {
        if (i == 1) {
          ifelse(
            as.numeric(RHS_coef[i]) == 1,
            RHS_eqn <- paste0(kr, "*", RHS_var[i]),
            RHS_eqn <- paste0(kr, "*", RHS_var[i],
                              "^", RHS_coef[i])
          )
        } else{
          ifelse(
            as.numeric(RHS_coef[i]) == 1,
            RHS_eqn <-
              paste0(RHS_eqn, "*", RHS_var[i]),
            RHS_eqn <-
              paste0(RHS_eqn, "*", RHS_var[i],
                     "^", RHS_coef[i])
          )
        }
      }
      for (i in seq(length(LHS_var))) {
        if (i == 1) {
          ifelse(
            as.numeric(LHS_coef[i]) == 1,
            LHS_eqn <- paste0(kf, "*", LHS_var[i]),
            LHS_eqn <- paste0(kf, "*", LHS_var[i],
                              "^", LHS_coef[i])
          )
        } else{
          ifelse(
            as.numeric(LHS_coef[i]) == 1,
            LHS_eqn <-
              paste0(LHS_eqn, "*", LHS_var[i]),
            LHS_eqn <-
              paste0(LHS_eqn, "*", LHS_var[i],
                     "^", LHS_coef[i])
          )
        }
      }
      if (var_coef > 1) {
        ifelse(
          var_on_left,
          eqn_out <-
            paste0("-", var_coef, "*", LHS_eqn,
                   "+", var_coef, "*", RHS_eqn),
          eqn_out <-
            paste0("+", var_coef, "*", LHS_eqn,
                   "-", var_coef, "*", RHS_eqn)
        )
      } else{
        ifelse(
          var_on_left,
          eqn_out <-
            paste0("-", LHS_eqn, "+", RHS_eqn),
          eqn_out <- paste0(LHS_eqn, "-", RHS_eqn)
        )
      }
    }
    #Case: A + B --> C + D
    else if (arrow_type == "forward_only") {
      for (i in seq(length(LHS_var))) {
        if (i == 1) {
          ifelse(
            as.numeric(LHS_coef[i]) == 1,
            LHS_eqn <- paste0(kf, "*", LHS_var[i]),
            LHS_eqn <- paste0(kf, "*", LHS_var[i],
                              "^", LHS_coef[i])
          )
        } else {
          ifelse(
            as.numeric(LHS_coef[i]) == 1,
            LHS_eqn <-
              paste0(LHS_eqn, "*", LHS_var[i]),
            LHS_eqn <-
              paste0(LHS_eqn, "*", LHS_var[i],
                     "^", LHS_coef[i])
          )
        }
      }
      eqn_out <- LHS_eqn
      if (var_coef > 1) {
        ifelse(
          var_on_left,
          eqn_out <-
            paste0("-", var_coef, "*", eqn_out),
          eqn_out <- paste0(var_coef, "*", eqn_out)
        )
      } else {
        ifelse(var_on_left,
               eqn_out <- paste0("-", eqn_out),
               eqn_out <- eqn_out)
      }
    }
  }
  return(eqn_out)
}

enzyme_reaction <- function(substrate, km, Vmax, kcat, enzyme, var_on_left) {
# Creates string equation for a substrate degraded by an enyzme
# Inputs:
#   @substrate - the species being degraded 
#   @km - Michealis Menton Constant
#   @Vmax - Maximum Velocity
#   @kcat - catalytic rate
#   @enzyme - enzyme performing degradation

# Outputs:
#   Outputs string equation for enzyme reaction (Vmax*S/(km+S) )
  if (!is.na(Vmax)) {
    #if vmax used
    #-Vmax*S/(km+S)
    eqn = paste0(Vmax, "*", substrate, "/(", km, "+", substrate, ")")
    # Determines if this is a "-" or "+" reaction
    eqn = ifelse(var_on_left, paste0("-", eqn), eqn)
  } else {
    #-km*E*S/(km+S)
    eqn = paste0(kcat, "*", enzyme, "*", substrate,
                 "/(", km, "+", substrate, ")")
    eqn = ifelse(var_on_left, paste0("-", eqn), eqn)
  }
  return(eqn)
}

regulatorToRate <- function(regulators, rateConstants) {
  #break values from space separated string to vector
  regulators <- str_split(regulators, " ")[[1]]
  rateConstants <- str_split(rateConstants, " ")[[1]]
  
  numRegulators <- length(regulators)
  eqnOut <- c()
  for (i in seq(numRegulators)) {
    #add each regulator equation to a list (regulator*rateConstant)
    eqnForRegulator <- paste0(rateConstants[i], "*", regulators[i])
    eqnOut <- c(eqnOut, eqnForRegulator)
  }
  out <- paste(eqnOut, collapse = "+")
  if (numRegulators > 1) {
    out <- paste0("(", out, ")")
  }
  return(out)
}

enzyme_degradation <- function(substrate, km, Vmax, kcat, enzyme, isProd) {
# Creates string equation for a substrate degraded by an enyzme
# Inputs:
#   @substrate - the species being degraded 
#   @km - Michealis Menton Constant
#   @Vmax - Maximum Velocity
#   @kcat - catylic rate
#   @enzyme - enzyme performing degradation

# Outputs:
# Outputs string equation for enzyme deg (-Vmax*S/(km+S) )
  
  if (!is.na(Vmax)) { #if vmax used
    if (isProd) {
      #-Vmax*S/(km+S)
      eqn = paste0(Vmax, "*", substrate, "/(", km, "+", substrate, ")")
      
    } else {
      #-Vmax*S/(km+S)
      eqn = paste0("-", Vmax, "*", substrate, 
                   "/(", km, "+", substrate, ")") 
    }
  } else {
    if (isProd) {
      #-km*E*S/(km+S)
      eqn = paste0(kcat, "*", enzyme, "*", substrate, 
                   "/(", km, "+", substrate, ")") 
    } else {
      #-km*E*S/(km+S)
      eqn = paste0("-", kcat, "*", enzyme, "*", substrate, 
                   "/(", km, "+", substrate, ")") 
    }
  }
  return(eqn)
}


# Input/Output Reaction Derivation ---------------------------------------------

SimpleDiffusion_DEQ <- function(diff_var, LHS_var, RHS_var, PS){
#Uses Ficks law to generate a simple model of diffusion (PS)(C2-C1)
# Inputs:
#   @diff_var - Var differential equation is being found for
#   @RHS_var - Var Name on right hand side in vector form: c(C)
#   @LHS_var - Variable names of left hand side equations in vector form: c(A,B)
#   @PS - diffusion constant variable string


# Outputs:
# String of law of mass action result.  For example for A:
  if (diff_var == LHS_var) {
    eqn = paste0("-", PS, "*(", LHS_var, "-", RHS_var, ")")
  } else if (diff_var == RHS_var) {
    eqn = paste0("-", PS, "*(", RHS_var, "-", LHS_var, ")")
  } else {
    print("Something went wrong")
    eqn = NA
  }
  
  # if (direction == "Out") {
  #   #PS*(C1-c2) where C1 is left hand side variable
  #   eqn = paste0(PS, "*(", RHS_var, "-", LHS_var, ")")
  # } else {
  #   eqn = paste0("-", PS, "*(", LHS_var, "-", RHS_var, ")")
  # }
  
  return(eqn)
}

FacilitatedDiffusion_DEQ <- function(speciesDiffused, Vmax, Km, direction) {
# Derive differential equation related to the facilitated diffusion of a species
# from two different compartments.  Follows Michelis-Menton kinetics and is 
# not dependent on the concentration of the receiving department.
# Inputs: 
#   All Inputs are variable names, not values.
#   @speciesOut - species leaving compartment
#   @rateConstant - rate that species leaves compartment
#   @compartmentVolume - volume of compartment species in leaving 
  eqn.out <- paste0(ifelse(direction == "In", "", "-"), 
                    Vmax, "*", speciesDiffused, 
                    "/(", Km, "+", speciesDiffused, ")")
}

Clearance_DEQ <- function(speciesOut, 
                          rateConstant, 
                          compartmentVolume) {
# Derive differential equation related to clearance of species from compartment.
# Inputs: 
#   All Inputs are variable names, not values.
#   @speciesOut - species leaving compartment
#   @rateConstant - rate that species leaves compartment
#   @compartmentVolume - volume of compartment species in leaving  
  eqn.out <- paste0("-", 
                    rateConstant, "*",
                    speciesOut, "*",
                    compartmentVolume)
  
  return(eqn.out)
}

Flow_DEQ <- function(speciesOut, 
                     flowRate, 
                     direction) {
# Derive differential equation related to flow of species in/out of compartments
# Inputs: 
#   All Inputs are variable names, not values.
#   speciesOut - species that is leaving in the flow.
#   flowRate - Rate at which flow is leaving a compartment
#   direction - string telling direction of flow.  "In"/"Out"
  
  eqn.out <- "FLOW_DEQ"
  
  if (direction == "In") {
    eqn.out <- paste0(flowRate, "*", speciesOut)
  } else if (direction == "Out") {
    eqn.out <- paste0("-", flowRate, "*", speciesOut)
    
  }
  
  return(eqn.out)
}

FLOW_BTWN <- function(species,
                      speciesIn, 
                      speciesOut,
                      compartmentIn,
                      compartmentOut,
                      flowRate) {
  # @species - species equation being derived for
  # @speciesIn - string of species leaving in cOut order ("Sa Sb Sc etc")
  # @speciesOut - species leaving beginning flow
  # @compartmentIn - string of compartments flow going to ("C1 C2 C3")
  # @compartmentOut - compartment flow leaving from 
  # @flowrate - flowrate variables in order of flowOut, flowIn1, flowIn2, etc
  
  eqn.out <- "FLOW_BTWN"
  
  
  # Determine if species in inflow or outflow
  species.in.outflow <- ifelse(species == speciesOut, TRUE, FALSE)
  
  if (species.in.outflow) {
    eqn.out <- paste0("-", strsplit(flowRate, " ")[[1]][1], "*", speciesOut)
  } else {
    all.species <- strsplit(speciesIn, " ")[[1]]
    idx <- which(all.species %in% species)
    if (length(strsplit(flowRate, " ")[[1]]) > 1) {
      # Flow was split.  Meaning there is one more flow then species in df.
      idx = idx + 1
    }
    flow <- strsplit(flowRate, " ")[[1]][idx]
    eqn.out <- paste0(flow, "*", speciesOut)
  }
  


  return(eqn.out)
}


# Data Structure Parsers -------------------------------------------------------
CalcDiffForEqns <- function(species,
                            compartments,
                            eqn.info.df, 
                            eqn.chem.df,
                            eqn.enz.df,
                            eqn.syn.df,
                            eqn.deg.df) {
  
  # Unpack eqn info structure
  eqn.id      <- eqn.info.df$ID
  eqn.type    <- eqn.info.df$Eqn.Type
  eqn.law     <- eqn.info.df$Law
  eqn.var     <- eqn.info.df$Species
  eqn.RCs     <- eqn.info.df$Rate.Constants
  eqn.comp    <- eqn.info.df$Compartment
  eqn.var.id  <- eqn.info.df$Species.Id
  eqn.RCs.id  <- eqn.info.df$Parameters.Id
  eqn.comp.id <- eqn.info.df$Compartment.Id
  
  # Initialize algorithm booleans
  diff.eqn <- NA
  latex.eqn <- NA
  first.eqn <- TRUE
  n.eqns <- nrow(eqn.info.df)
  
  # Begin Algorithm
  if (n.eqns > 0) {
    for (row in 1:n.eqns) {
      vars <- strsplit(eqn.info.df$Species[row], " ")[[1]]
      for (var in vars) {
        if (var == species) {
          skip <- FALSE
          id   <- eqn.info.df$ID[row]
          type <- eqn.info.df$Eqn.Type[row]
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

CalcDiffEqnsForChem <- function(chemInfo, 
                                searchVar, 
                                compartmentList, 
                                compartmentID) {
  # Inputs
  # @compartmentList - list of compartment information
  # @compartmentID - compartment id of compartment eqn is taking place in
  # jPrint("Calc diff eqns for chem")
  ID         <- chemInfo$ID
  law        <- chemInfo$Law
  LHS.coef   <- str_split(chemInfo$LHS.coef[1], " ")[[1]]
  LHS.var    <- str_split(chemInfo$LHS.var[1],  " ")[[1]]
  RHS.coef   <- str_split(chemInfo$RHS.coef[1], " ")[[1]]
  RHS.var    <- str_split(chemInfo$RHS.var[1],  " ")[[1]] 
  arrow_type <- chemInfo$arrow
  kf         <- chemInfo$kf
  kr         <- chemInfo$kr
  FR.bool    <- chemInfo$FM.bool
  FRs        <- chemInfo$FMs
  FR.RCs     <- chemInfo$FM.rateC 
  RR.bool    <- chemInfo$RM.bool
  RRs        <- chemInfo$RMs
  RR.RCs     <- chemInfo$RM.rateC 
  # browser()

  # Find Volume Variable of Compartment
  volumeVar <- compartmentList[[compartmentID]]$Volume

  # Rate constant changes if regulators are involved
  if (FR.bool) {kf = regulatorToRate(FRs, FR.RCs)}
  if (RR.bool) {kr = regulatorToRate(RRs, RR.RCs)}
  #match returns index position of var, ex, var = A, list -> c(A,B) match 
  # returns 1 for A and 2 for B
  if (searchVar %in% LHS.var) {
    var.on.left = TRUE
    var.coef <- LHS.coef[match(searchVar, LHS.var)] 
  } else if (searchVar %in% RHS.var) {
    var.on.left = FALSE
    var.coef <- RHS.coef[match(searchVar, RHS.var)]
  }
  # jPrint("Finished search var")
  diff.eqn <- law_mass_action(RHS.coef, 
                              RHS.var, 
                              LHS.coef, 
                              LHS.var, 
                              arrow_type, 
                              kf, 
                              kr, 
                              var.on.left, 
                              var.coef)
  
  # Add Volume Component: diff.eqn = (diff.eqn)*V
  diff.eqn <- paste0("(", diff.eqn, ")*", volumeVar)
  
  latex.eqn <- massActionEqn2Latex(diff.eqn)
  
  out <- list("Diff" = diff.eqn, "Latex" = latex.eqn)
  return(out)
}

CalcDiffEqnsForEnzyme <- function(enz.info, searchVar) {
  print("Calculating diff eqn for enzymes")
  print(enz.info)
  # Unpack information
  ID        <- enz.info$ID
  law       <- enz.info$Law
  substrate <- enz.info$Substrate
  product   <- enz.info$Product
  enzyme    <- enz.info$Enzyme
  kcat      <- enz.info$kcat
  Km        <- enz.info$Km
  Vmax      <- enz.info$Vmax
  
  if (searchVar == enzyme) {return(NA)}
  if (searchVar == substrate) {
    var.on.left = TRUE
  } else if (searchVar == product ) {
    var.on.left = FALSE
  } 
  
  # Run solving law
  diff.eqn <- enzyme_reaction(substrate, 
                              Km, 
                              Vmax, 
                              kcat, 
                              enzyme, 
                              var.on.left)
  
  latex.eqn <- enzymeEqn2Latex(diff.eqn)
  
  # Package result
  out <- list("Diff" = diff.eqn, "Latex" = latex.eqn)
  return(out)
}

CalcDiffEqnsForSyn <- function(synInfo, searchVar) {
    
  # Unpack Information
  ID     <- synInfo$ID
  Law    <- synInfo$Law
  VarSyn <- synInfo$VarSyn
  RC     <- synInfo$RC
  Factor <- synInfo$Factor
  
  
  if (Law == "rate") {
    diff.eqn  <- RC
    latex.eqn <- VarToLatexForm(RC)
  }
  else if (Law == "byFactor") {
    diff.eqn  <- paste0(RC, "*", Factor)
    latex.eqn <-
      paste0(VarToLatexForm(RC), "*", VarToLatexForm(Factor))
  }
  out <- list("Diff" = diff.eqn, "Latex" = latex.eqn)
  return(out)
}

CalcDiffEqnsForDeg <- function(degInfo, searchVar) {
  # Unpack Information
  ID      <- degInfo$ID
  Law     <- degInfo$Law
  VarDeg  <- degInfo$VarDeg
  ConcDep <- degInfo$ConcDep
  RC      <- degInfo$RC
  Km      <- degInfo$Km
  Enz     <- degInfo$Enz
  Vmax    <- degInfo$Vmax
  Product <- degInfo$Prods
  is.Prod <- FALSE
  # Create Products if they exist
  if (!is.na(Product)) {
    Product <- str_split(Product, " ")[[1]]
    if (searchVar %in% Product) {
      is.Prod <- TRUE
    }
  }
  
  if (Law == "rate") {
    # if species being degraded
    if (is.Prod) {
      diff.eqn <- ifelse(ConcDep,
                         paste0(RC, "*", VarDeg),
                         paste0(RC))
    } else {
      # if species being generated
      diff.eqn <- ifelse(ConcDep,
                         paste0("-", RC, "*", VarDeg),
                         paste0("-", RC))
    }
    
    
    latex.eqn <- IO2Latex(diff.eqn)
  }
  else if (Law == "byEnzyme") {
    diff.eqn <- enzyme_degradation(VarDeg,
                                   Km,
                                   Vmax,
                                   RC,
                                   Enz,
                                   is.Prod)
    latex.eqn <- enzymeEqn2Latex(diff.eqn)
  }
  out <- list("Diff" = diff.eqn, "Latex" = latex.eqn)
  return(out)
}



# Main Call Function -----------------------------------------------------------
calc_differential_equations <- function(eqn.info.df,
                                        eqn.chem.df,
                                        eqn.enz.df,
                                        eqn.syn.df,
                                        eqn.deg.df,
                                        var.datastructure, 
                                        Input.Output.Df,
                                        id.df,
                                        listOfCustomVars,
                                        customVarToIgnore,
                                        customVarDF
                                        ) {
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
    jPrint(paste("Current differential variable: ", var))
    if (var %in% custom.vars) {
      idx <- match(var, customVarDF[, 1])
      differential.equations <- c(differential.equations,
                                  customVarDF[idx, 2])
    } else {
      # Differential Equation Solver if Custom Equation is not used
      
      # Solve Eqns for corrresponding differential equations
      out <- CalcDiffForEqns(var,
                             comp.list,
                             eqn.info.df,
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
  print(differential.eqns.for.calc)
  out.list <- list("diff.eqns" = differential.equations,
                   "latex.diff.eqns" = differential.eqns.latex,
                   "diff.eqns.for.solver" = differential.eqns.for.calc)
  return(out.list)
}

# What I think are old functions -----------------------------------------------
# Input_Outputs ----------------------------------------------------------------
#Generates Rate equations based on Input and Output of data
# Inputs:
# input_or_output - "input" or "output"
# varName - Name of variable controlling rate of in/out
# species - The element that is undergoing I/O
# 
# Outputs:
# String of i/o result

In_Out <- function(input_or_output, varName, species){
  if (input_or_output == "input") {
    eqn = paste0(species, "*", varName)
  }
  else{
    eqn = paste0("-", species, "*", varName)
  }
  return(eqn)
}




## Output: Mass Action ---------------------------------------------------------
# creates string equation for a substrate degraded by enzyme/transporter using MA

# Inputs:
# substrate - the species being degraded 
# kout - rate constant of reaction
# enzyme - enzyme performing degradation

# Outputs:
# Outputs string equation for output using mass action
  IO_mass_action <- function(substrate, kout, enzyme) {
    eqn = paste0("-", kout, "*", substrate, "*", enzyme)
    return(eqn)
  }

# CalcDiffEqForIO --------------------------------------------------------------
CalcDiffEqForIO <- function(IO_df, var, InOrOut) {
  # this function is meant to calculate the differential equations for 
  # input/output functions
  # Inputs:
  #   @IO_df - df containing all Input/output information
  #   @var - variable to generate differential equation for
  #   @InOrOut - string "input" or "output" depending on direction of IO
  # Output:
  #   @out - c(string version of differential equation relating to I/O of var,
  #            boolean that is TRUE is this var has IO to add,
  #            latex version of equation)
  
  diff.eqn <- ""
  latex.eqn.out <- ""
  input.output.exists <- FALSE
  
  for (row in 1:nrow(IO_df)) {
    # unpack IO_df
    
    input.or.output <- InOrOut          #Input or Output
    type.of.IO <- IO_df[row, 1]         #Rate, Enzyme, Synthesis, etc...
    species <- IO_df[row, 2]            #Species being in or out'd
    rate.constant <- IO_df[row, 3]      #rate associated with IO
    species.dependent <- IO_df[row, 4]  #T or F if rate dependent on species
    Vmax <- IO_df[row, 5]               #Vmax used in enzyme IO
    kcat <- IO_df[row, 6]               #Kcat used in enzyme IO
    enzyme <- IO_df[row, 7]             #enzyme used in enzyme IO
    
    if (species == var) {
      input.output.exists <- TRUE
      if (type.of.IO == "Rate") {
        eqn <- ifelse(species.dependent,
                      paste0(rate.constant, "*", species),
                      rate.constant) 
        
        diff.eqn <- ifelse(input.or.output == "input",
                           paste0(diff.eqn, "+", eqn),
                           paste0(diff.eqn, "-", eqn))
        
        latex.eqn <- IO2Latex(diff.eqn)
        
        latex.eqn.out <- ifelse(input.or.output == "input",
                                paste0(latex.eqn.out, "+", latex.eqn),
                                paste0(latex.eqn.out, "-", latex.eqn))
      } 
      else if (type.of.IO == "Synthesis") {
        #store factor in enzyme spot
        eqn <- paste0(rate.constant, "*", enzyme) 
        
        diff.eqn <- ifelse(input.or.output == "input",
                           paste0(diff.eqn, "+", eqn),
                           paste0(diff.eqn, "-", eqn))
        
        latex.eqn <- IO2Latex(eqn, type.of.IO)
        
        latex.eqn.out <- ifelse(input.or.output == "input",
                                paste0(latex.eqn.out, "+", latex.eqn),
                                paste0(latex.eqn.out, "-", latex.eqn))
      } 
      else if (type.of.IO == "Enzyme_Degradation") {
        eqn <- enzyme_degradation(species, rate.constant, Vmax, kcat, enzyme)
        diff.eqn <- paste0(diff.eqn, eqn)
        latex.eqn <- enzymeEqn2Latex(eqn)
        latex.eqn.out <- ifelse(startsWith(latex.eqn, "-"),
                                paste0(latex.eqn.out, latex.eqn),
                                paste0(latex.eqn.out, "+", latex.eqn))
      } 
      else if (type.of.IO == "mass_action") {
        eqn <- IO_mass_action(species, rate.constant, enzyme)
        diff.eqn <- paste0(diff.eqn, eqn)
      }
    } 
  }
  out <- c(diff.eqn, input.output.exists, latex.eqn.out)
}



CalcIOTree_DEQ <- function(IO_df, var, var.info, id.df) {
  
  diff.eqn <- ""
  latex.out <- ""
  IO.exists <- FALSE
  count <- 0
  # browser()
  for (i in seq(nrow(IO_df))) {
    io.id           <- IO_df$id[i]
    input.or.output <- IO_df$in.or.out[i]
    type.of.IO      <- IO_df$type[i]
    compartment.out <- IO_df$compartment.out[i]
    compartment.in  <- IO_df$compartment.in[i]
    species.out     <- IO_df$species.out[i]
    species.in      <- IO_df$species.in[i]
    flow.rate       <- IO_df$flow.rate[i]
    flow.unit       <- IO_df$flow.unit[i]
    flow.species    <- IO_df$flow.species[i]
    ps              <- IO_df$solubility.constant[i]
    ps.unit         <- IO_df$solubility.unit[i]
    Vmax            <- IO_df$FD.Vmax[i]
    Km              <- IO_df$FD.Km[i]
    Vmax.unit       <- IO_df$FD.vmax.unit[i]
    Km.unit         <- IO_df$FD.Km.u[i]

    # Check if the var is in the IO df
    if (var %in% c(strsplit(species.in, " ")[[1]], 
                   strsplit(species.out, " ")[[1]],
                   flow.species)) {
      # Find the type of IO and calculate accordingly
      IO.exists <- TRUE
      count = count + 1
      ifelse(var %in% strsplit(species.out, " ")[[1]], 
             direction <- "Out", 
             direction <- "In")
      PrintVar(direction)
      switch(
        type.of.IO,
        "FLOW_IN" = {
          print("flow in")
          calc.IO  <- Flow_DEQ(species.in, flow.rate, "In")
          latex.IO <- IO2Latex(calc.IO)
        },
        "FLOW_OUT" = {
          print("flow out")
          calc.IO  <- Flow_DEQ(species.out, flow.rate, "Out")
          latex.IO <- IO2Latex(calc.IO)
        },
        "FLOW_BETWEEN" = {
          print("flow between")
          calc.IO <- FLOW_BTWN(var,
                               species.in, 
                               species.out, 
                               compartment.in,
                               compartment.out,
                               flow.rate)
          latex.IO <- IO2Latex(calc.IO)
        },
        "CLEARANCE" = {
          # Find comparment volume
          print("clearance")
          idx <- which(id.df[,2] %in% compartment.out)
          comp.id <- id.df[idx, 1]
          compartment.vol <- 
            var.info$compartments.info[[comp.id]]$Volume
          PrintVar(compartment.vol)
          calc.IO  <- Clearance_DEQ(species.out, flow.rate, compartment.vol)
          latex.IO <- IO2Latex(calc.IO)
        }, 
        "SIMPDIFF" = {
          print("simple diffusion")
          calc.IO <- SimpleDiffusion_DEQ(var,
                                         species.out, 
                                         species.in, 
                                         ps)
          latex.IO <- IO2Latex(calc.IO)
        },
        "FACILITATED_DIFF" = {
          print("faciliated_diffusion")
          calc.IO <- FacilitatedDiffusion_DEQ(species.out, Vmax, Km, direction)
          latex.IO <- enzymeEqn2Latex(calc.IO)
        }
      )
      if (count > 1) {
        ifelse(direction == "Out", sign <- "", sign <- "+")
        diff.eqn  <-paste0(diff.eqn, sign, calc.IO)
        latex.out <- paste0(latex.out, sign, latex.IO)
      } else {
        diff.eqn  <- paste0(diff.eqn, calc.IO)
        latex.out <- paste0(latex.out, latex.IO)
      }
      PrintVar(diff.eqn)
    }
  }
  
  out <- list("diff.eqn" = diff.eqn,
              "latex.eqn" = latex.out,
              "exists" = IO.exists)
  
  return(out)
}


CalcInputsForEqns <- function(species,
                              InputDf,
                              noEquation) {
  # noEquation is a boolean telling if the differential equation has an 
  # equation portion
  
  diff.eqn  <- NA
  latex.eqn <- NA
  IO.out <- CalcDiffEqForIO(InputDf, species, "input")
  new.eqn <- IO.out[[1]]
  input.exists <- IO.out[[2]]
  new.latex.eqn <- IO.out[[3]]
  if (input.exists) {
    diff.eqn <- ifelse(noEquation,
                       RemovePlusSignFromStart(new.eqn),
                       new.eqn)
    
    latex.eqn <- ifelse(noEquation,
                        RemovePlusSignFromStart(new.latex.eqn),
                        new.latex.eqn)
  } 
  
  out <- list("Diff" = diff.eqn, "Latex" = latex.eqn)
  return(out)
}
## CalcOutputsForEqns ----------------------------------------------------------
CalcOutputsForEqns <- function(species,
                               OutputDf,
                               noEquation) {
  
  diff.eqn  <- ""
  latex.eqn <- ""
  IO.out <- CalcDiffEqForIO(OutputDf, species, "output")
  new.eqn <- IO.out[[1]]
  is.new.eqn <- IO.out[[2]]
  new.latex.eqn <- IO.out[[3]]
  if (is.new.eqn) {
    diff.eqn <- ifelse(noEquation,
                       RemovePlusSignFromStart(new.eqn),
                       paste0(diff.eqn, new.eqn))
    
    latex.eqn <- ifelse(noEquation,
                        RemovePlusSignFromStart(new.latex.eqn),
                        paste0(latex.eqn, new.latex.eqn))
  } 
  
  out <- list("Diff" = diff.eqn, "Latex" = latex.eqn)
  return(out)
}