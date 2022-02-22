################################################################################
################# FUNCTION: law_mass_action ####################
# Generated equations for law of mass action for given inputs. Where law of mass action is:
# for equation aA + bB (kr)<-->(kf) cC + dD then the derviation is as follows:
# -(1/a)*A^a = -(1/b)*B^b = (1/c)*C^c = (1/d)*D^d = kf*A^a*B^b - kr*C^c*D^d

# Example equation A + 2B ->(kf1) C used here
# Inputs:
# RHS_coef - Coefficients of variables on RHS of equation in vector form c(1)
# RHS_var - Var Name on right hand side in vector form: c(C)
# LHS_coef - Coefficients of variables on LHS of equation in vector form: c(1, 2)
# LHS_var - Variable names of left hand side equations in vector form: c(A, B)
# arrowtype - Describes if reaction is forward (forward_only) or both (both_directions): "both_directions"
# kf - numerical value of forward rate constant: kf1
# kr - numerical value of reverse rate constant: NULL
# var_on_left - boolean value that tells if the current variable is on the LHS.  For example if this was deriving for A, then TRUE.  IF this was deriving for C then false, (B=TRUE)
# var_coef - coefficient of lookup variable. A-->1, B-->2, C-->1, so if looking up B value would be 2.

# Outputs:
# String of law of mass action result.  For example for A:
################################################################################

law_mass_action <- function(RHS_coef, RHS_var, LHS_coef, LHS_var, arrow_type, kf, kr, var_on_left, var_coef){
    #Case1: A -> B, one var on each side
    if (length(RHS_var) == 1 & length(LHS_var) == 1) {
        #Case 1.1 A <--> B, Reaction flows both ways
        if (arrow_type == "both_directions") {
            ifelse(as.numeric(RHS_coef) == 1, RHS_eqn <- paste0(kr, "*", RHS_var), RHS_eqn <- paste0(kr, "*", RHS_var, "^", RHS_coef))
            ifelse(as.numeric(LHS_coef) == 1, LHS_eqn <- paste0(kf, "*", LHS_var), LHS_eqn <- paste0(kf, "*", LHS_var, "^", LHS_coef))
            
            #this if/else multiplies the coefficient of the var to the equation dependent on which var is being processed using var_on_left
            if (var_coef > 1 ) {
                ifelse(var_on_left,
                       eqn_out <- paste0("-", var_coef, "*", LHS_eqn, "+", var_coef, "*", RHS_eqn),
                       eqn_out <- paste0("+", var_coef, "*", LHS_eqn, "-", var_coef, "*", RHS_eqn)
                       )
            }else{
                ifelse(var_on_left,
                       eqn_out <- paste0("-", LHS_eqn, "+", RHS_eqn),
                       eqn_out <- paste0(LHS_eqn, "-", RHS_eqn)
                       )
            }
        }
        #Case 1.2 A->B, only in forward direction
        else if (arrow_type == "forward_only") {
            ifelse(as.numeric(LHS_coef) == 1, 
                   LHS_eqn <- paste0(kf, "*", LHS_var), 
                   LHS_eqn <- paste0(kf, "*", LHS_var, "^", LHS_coef)
                   )
            eqn_out <- LHS_eqn
            if (var_coef > 1) {
                ifelse(var_on_left, 
                       eqn_out <- paste0("-", var_coef, "*", eqn_out), 
                       eqn_out <- paste0(var_coef, "*", eqn_out))
            }else{
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
            ifelse(as.numeric(RHS_coef) == 1, 
                   RHS_eqn <- paste0(kr, "*", RHS_var), 
                   RHS_eqn <- paste0(kr, RHS_var, "^", RHS_coef))
            #RHS_eqn <- paste0("(", RHS_eqn, ")")
            for (i in seq(length(LHS_var))) {
                if (i == 1) {
                    ifelse(as.numeric(LHS_coef[i]) == 1, 
                           LHS_eqn <- paste0(kf, "*", LHS_var[i]), 
                           LHS_eqn <- paste0(kf, "*", LHS_var[i], "^", LHS_coef[i]))
                }else{
                    ifelse(as.numeric(LHS_coef[i]) == 1, 
                           LHS_eqn <- paste0(LHS_eqn, "*", LHS_var[i]), 
                           LHS_eqn <- paste0(LHS_eqn, "*", LHS_var[i], "^", LHS_coef[i]))
                }
            }
            #eqn_out <- paste0("(", LHS_eqn, " - ", RHS_eqn, ")")
            
            if (var_coef > 1 ) {
                ifelse(var_on_left,
                       eqn_out <- paste0("-", var_coef, "*", LHS_eqn, "+", var_coef, "*", RHS_eqn),
                       eqn_out <- paste0("+", var_coef, "*", LHS_eqn, "-", var_coef, "*", RHS_eqn)
                )
            }else{
                ifelse(var_on_left,
                       eqn_out <- paste0("-", LHS_eqn, "+", RHS_eqn),
                       eqn_out <- paste0(LHS_eqn, "-", RHS_eqn)
                )
            }
        }
        #Case A + B --> C
        else if (arrow_type == "forward_only") {
            for (i in seq(length(LHS_var))) {
                if (i == 1) {
                    ifelse(as.numeric(LHS_coef[i]) == 1, 
                           LHS_eqn <- paste0(kf, "*", LHS_var[i]), 
                           LHS_eqn <- paste0(kf, "*", LHS_var[i], "^", LHS_coef[i])
                           )
                }else{
                    ifelse(as.numeric(LHS_coef[i]) == 1, 
                           LHS_eqn <- paste0(LHS_eqn, "*", LHS_var[i]), 
                           LHS_eqn <- paste0(LHS_eqn, "*", LHS_var[i], "^", LHS_coef[i])
                           )
                }
            }
            eqn_out <- LHS_eqn
            if (var_coef > 1) {
                ifelse(var_on_left, 
                       eqn_out <- paste0("-", var_coef, "*", eqn_out), 
                       eqn_out <- paste0(var_coef, "*", eqn_out))
            }else{
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
            ifelse(as.numeric(LHS_coef) == 1, 
                   LHS_eqn <- paste0(kf, "*", LHS_var), 
                   LHS_eqn <- paste0(kf, "*", LHS_var, "^", LHS_coef)
                   )

            for (i in seq(length(RHS_var))) {
                if (i == 1) {
                    ifelse(as.numeric(RHS_coef[i]) == 1, 
                           RHS_eqn <- paste0(kr, "*", RHS_var[i]), 
                           RHS_eqn <- paste0(kr, "*", RHS_var[i], "^", RHS_coef[i])
                           )
                }else{
                    ifelse(as.numeric(RHS_coef[i]) == 1, 
                           RHS_eqn <- paste0(RHS_eqn, "*", RHS_var[i]), 
                           RHS_eqn <- paste0(RHS_eqn, "*", RHS_var[i], "^", RHS_coef[i])
                           )
                }
            }
            if (var_coef > 1 ) {
                ifelse(var_on_left,
                       eqn_out <- paste0("-", var_coef, "*", LHS_eqn, "+", var_coef, "*", RHS_eqn),
                       eqn_out <- paste0("+", var_coef, "*", LHS_eqn, "-", var_coef, "*", RHS_eqn)
                )
            }else{
                ifelse(var_on_left,
                       eqn_out <- paste0("-", LHS_eqn, "+", RHS_eqn),
                       eqn_out <- paste0(LHS_eqn, "-", RHS_eqn)
                )
            }
        }
        #Case: A --> B + c
        else if (arrow_type == "forward_only") {
            for (i in seq(length(LHS_var))) {
                if (i == 1) {
                    ifelse(as.numeric(LHS_coef[i]) == 1, 
                           LHS_eqn <- paste0(kf, "*", LHS_var[i]), 
                           LHS_eqn <- paste0(kf, "*", LHS_var[i], "^", LHS_coef[i])
                           )
                }else{
                    ifelse(as.numeric(LHS_coef[i]) == 1, 
                           LHS_eqn <- paste0(LHS_eqn, "*", 
                                             LHS_eqn[i]), LHS_eqn <- paste0(LHS_eqn, "*", LHS_eqn[i], "^", LHS_eqn[i])
                           )
                }
            }
            eqn_out <- LHS_eqn
            if (var_coef > 1) {
                ifelse(var_on_left, 
                       eqn_out <- paste0("-", var_coef, "*", eqn_out), 
                       eqn_out <- paste0(var_coef, "*", eqn_out))
            }else{
                ifelse(var_on_left, 
                       eqn_out <- paste0("-", eqn_out), 
                       eqn_out <- eqn_out)
            }
        }
    }
    #Case 4
    else{#need to finish.  Here will go if RHS&&LHS>1.  Just copy an paste things from the above sections.  Run and test and hopefully it doens't take too long. 
        #Case A + B <--> C + D
        if (arrow_type == "both_directions") {
            for (i in seq(length(RHS_var))) {
                if (i == 1) {
                    ifelse(as.numeric(RHS_coef[i]) == 1, 
                           RHS_eqn <- paste0(kr, "*", RHS_var[i]), 
                           RHS_eqn <- paste0(kr, "*", RHS_var[i], "^", RHS_coef[i])
                           )
                }else{
                    ifelse(as.numeric(RHS_coef[i]) == 1, 
                           RHS_eqn <- paste0(RHS_eqn, "*", RHS_var[i]), 
                           RHS_eqn <- paste0(RHS_eqn, "*", RHS_var[i], "^", RHS_coef[i])
                           )
                }
            }
            for (i in seq(length(LHS_var))) {
                if (i == 1) {
                    ifelse(as.numeric(LHS_coef[i]) == 1, 
                           LHS_eqn <- paste0(kf, "*", LHS_var[i]), 
                           LHS_eqn <- paste0(kf, "*", LHS_var[i], "^", LHS_coef[i])
                           )
                }else{
                    ifelse(as.numeric(LHS_coef[i]) == 1, 
                           LHS_eqn <- paste0(LHS_eqn, "*", LHS_var[i]),
                           LHS_eqn <- paste0(LHS_eqn, "*", LHS_var[i], "^", LHS_coef[i])
                           )
                }
            }
            if (var_coef > 1 ) {
                ifelse(var_on_left,
                       eqn_out <- paste0("-", var_coef, "*", LHS_eqn, "+", var_coef, "*", RHS_eqn),
                       eqn_out <- paste0("+", var_coef, "*", LHS_eqn, "-", var_coef, "*", RHS_eqn)
                )
            }else{
                ifelse(var_on_left,
                       eqn_out <- paste0("-", LHS_eqn, "+", RHS_eqn),
                       eqn_out <- paste0(LHS_eqn, "-", RHS_eqn)
                )
            }
        }
        #Case: A + B --> C + D
        else if (arrow_type == "forward_only") {
            for (i in seq(length(LHS_var))) {
                if (i == 1) {
                    ifelse(as.numeric(LHS_coef[i]) == 1, 
                           LHS_eqn <- paste0(kf, "*", LHS_var[i]), 
                           LHS_eqn <- paste0(kf, "*", LHS_var[i], "^", LHS_coef[i])
                           )
                } else {
                    ifelse(as.numeric(LHS_coef[i]) == 1, 
                           LHS_eqn <- paste0(LHS_eqn, "*", LHS_var[i]), 
                           LHS_eqn <- paste0(LHS_eqn, "*", LHS_var[i], "^", LHS_coef[i])
                           )
                }
            }
            eqn_out <- LHS_eqn
            if (var_coef > 1) {
                ifelse(var_on_left, 
                       eqn_out <- paste0("-", var_coef, "*", eqn_out), 
                       eqn_out <- paste0(var_coef, "*", eqn_out))
            } else {
                ifelse(var_on_left, 
                       eqn_out <- paste0("-", eqn_out), 
                       eqn_out <- eqn_out)
            }
        }
    }
    return(eqn_out)
}
################################################################################
############################# enzyme_reaction ##################################
# creates string equation for a substrate degraded by an enyzme

# Inputs:
# substrate - the species being degraded 
# km - Michealis Menton Constant
# Vmax - Maximum Velocity
# kcat - catylic rate
# enzyme - enzyme performing degradation

# Outputs:
# Outputs string equation for enzyme reaction (Vmax*S/(km+S) )
################################################################################
enzyme_reaction <- function(substrate, km, Vmax, kcat, enzyme, var_on_left) {
    #print("Var on left:")
    #print(var_on_left)
    if (!is.na(Vmax) )
    { #if vmax used
        eqn = paste0(Vmax, "*", substrate, "/(", km, "+", substrate, ")") #-Vmax*S/(km+S)
        eqn = ifelse(var_on_left, paste("-", eqn), eqn) #determines if this is a "-" or "+" reaction
    }
    else
    {
        eqn = paste0(kcat, "*", enzyme, "*", substrate, "/(", km, "+", substrate, ")") #-km*E*S/(km+S)
        eqn = ifelse(var_on_left, paste("-", eqn), eqn)
    }
    return(eqn)
}
################################################################################
######################## FUNCTION: simple_diffusion ############################

#Uses Ficks law to generate a simple model of diffusion (PS)(C2-C1)
# Inputs:
# RHS_var - Var Name on right hand side in vector form: c(C)
# LHS_var - Variable names of left hand side equations in vector form: c(A, B)
# arrowtype - Describes if reaction is forward (forward_only) or both (both_directions): "both_directions"
# PS - diffusion constant variable string
# var_on_left - boolean value that tells if the current variable is on the LHS.  For example if this was deriving for A, then TRUE.  IF this was deriving for C then false, (B=TRUE)

# Outputs:
# String of law of mass action result.  For example for A:
################################################################################

simple_diffusion <- function(LHS_var, RHS_var, PS, var_on_left){
    if (var_on_left) {
        #PS*(C1-c2) where C1 is left hand side variable
        eqn = paste0(PS, "*(", RHS_var, "-", LHS_var, ")")
    } else {
        eqn = paste0(PS, "*(", LHS_var, "-", RHS_var, ")")
    }
    
    return(eqn)
}


######################## FUNCTION: Input_Outputs ############################

#Generates Rate equations based on Input and Output of data
# Inputs:
# input_or_output - "input" or "output"
# varName - Name of variable controlling rate of in/out
# species - The element that is undergoing I/O
# 
# Outputs:
# String of i/o result
################################################################################

In_Out <- function(input_or_output, varName, species){
    if (input_or_output == "input") {
        eqn = paste0(species, "*", varName)
    }
    else{
        eqn = paste0("-", species, "*", varName)
    }
    return(eqn)
}

################# FUNCTION: extract_data ####################
# This searches a dataframe for every instance of a variable in RHS and LHS var and extracts those rows, returning this subsetted dataframe

# Inputs:
# myModel - saved df that contains model information with RHS variables in the 3rd column and LHS variables in the 5th colun
# var_to_subset_with - variable to search for in 'myModel' dataframe

# Outputs:
# Outputs dataframe subsetting with var_to_subset_with (this is meant to be used with in the differential equation solver)
################################################################################
extract_data <- function(myModel, var_to_subset_with){
    index_of_rows_with_var <- vector()
    for (row in 1:nrow(myModel)) {#search rows of data for the choosen variable and subset them to new df
        #print(myModel[row,])
        #law_of_derivation <- myModel[row,1]
        RHS_var <- str_split(myModel[row,3], " ")[[1]] #grabs RHS vars, splits them so they can be searched for wanted variable
        LHS_var <- str_split(myModel[row,5], " ")[[1]] #Does above for LHS variables
        if (var_to_subset_with %in% RHS_var | var_to_subset_with %in% LHS_var) { #find indices containing var name
            index_of_rows_with_var <- c(index_of_rows_with_var, row) #adds index to vector to subset main df later
        }
    }    
    temp_df <- myModel[index_of_rows_with_var, ] #extract var rows
    #print(temp_df)
    return(temp_df)
}


RemovePlusSignFromStart <- function(string) {
    #removes the first letter of a string if it is a plus sign
    #inputs:
    #   @string - string to remove letter from 
    #output:
    #   @out -string without a plus in first letter if it exists
    #ex. string <- "+k_r3",   out <- "k_r3"
    
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

################################################################################
##################### Function: regulatorToRate 
################################################################################
regulatorToRate <- function(regulators, rateConstants) {
    #break values from space separated string to vector
    regulators <- str_split(regulators, " ")[[1]]
    rateConstants <- str_split(rateConstants, " ")[[1]]
    
    numRegulators <- length(regulators)
    eqnOut <- c()
    for (i in seq(numRegulators)) { #add each regulator equation to a list (regulator*rateConstant)
        eqnForRegulator <- paste0(rateConstants[i], "*", regulators[i])
        eqnOut <- c(eqnOut, eqnForRegulator)
    }
    out <- paste(eqnOut, collapse = "+")
    if (numRegulators > 1) {
        out <- paste0("(", out, ")")
    }
    #out <- paste0("(", out, ")")
    print(out)
    return(out)
}

################################################################################
################# enzyme_degradation: extract_data ####################
# creates string equation for a substrate degraded by an enyzme

# Inputs:
# substrate - the species being degraded 
# km - Michealis Menton Constant
# Vmax - Maximum Velocity
# kcat - catylic rate
# enzyme - enzyme performing degradation

# Outputs:
# Outputs string equation for enzyme deg (-Vmax*S/(km+S) )
################################################################################
enzyme_degradation <- function(substrate, km, Vmax, kcat, enzyme)
{
    if (!is.na(Vmax)) { #if vmax used
        eqn = paste0("-", Vmax, "*", substrate, "/(", km, "+", substrate, ")") #-Vmax*S/(km+S)
    } else {
        eqn = paste0("-", kcat, "*", enzyme, "*", substrate, "/(", km, "+", substrate, ")") #-km*E*S/(km+S)
    }
    return(eqn)
}

########################### Output: Mass Action ################################
# creates string equation for a substrate degraded by enzyme/transporter using MA

# Inputs:
# substrate - the species being degraded 
# kout - rate constant of reaction
# enzyme - enzyme performing degradation

# Outputs:
# Outputs string equation for output using mass action
################################################################################
IO_mass_action <- function(substrate, kout, enzyme) {
    eqn = paste0("-", kout, "*", substrate, "*", enzyme)
    return(eqn)
}

CalcDiffEqForIO <- function(IO_df, var, InOrOut) {
    # this function is meant to calculate the differential equations for input/output functions
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
        species.dependent <- IO_df[row, 4]   #T or F if rate dependent on species
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
                
                latex.eqn <- IO2Latex(eqn, type.of.IO)
                
                latex.eqn.out <- ifelse(input.or.output == "input",
                                        paste0(latex.eqn.out, "+", latex.eqn),
                                        paste0(latex.eqn.out, "-", latex.eqn))
            } 
            else if (type.of.IO == "Synthesis") {
                eqn <- paste0(rate.constant, "*", enzyme) #store factor in enzyme spot
                
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

##################### FUNCTION: calc_differential_equations ####################
# Model Description: 
# Inputs
# @myModel - df to parse containing all equation parameters (this is typically an output of Rhinsy)
# @vars_to_diffeq - vector of variables that we want to create differential equations for
# @InOutModel - df containing all In/out parameter and values

# Outputs
#list:
# @diff.eqns - vector of differential equations in string form
# @latex.diff.eqns - vector of differential equations in latex form
#############
calc_differential_equations <- function(myModel, var_to_diffeq, InputDf, OutputDf, InAdded, OutAdded)
{
    count = 1
    differential_equations = vector()
    differential.eqns.in.latex = vector()
    #choosing variable to solve the differential equation for
    for (var in var_to_diffeq) {
        #diff_eqn <- ""
        no.IO.in <- FALSE #initialize
        no.IO.out <- FALSE
        no.equation <- FALSE
        ifelse(nrow(myModel) > 0,
               df_subset <- extract_data(myModel, var),
               df_subset <-  data.frame())
        flag_first_added <- TRUE
        
        #####################################################################################################
        
        #Checks each row of Dataframe for laws and calculates the equations for the laws, i.e. mass action, diffusion. etc
        
        #####################################################################################################
        if (nrow(df_subset) > 0) {
            for (new_row in 1:nrow(df_subset)) {
                eqn_type <- df_subset[new_row, 1]
                LHS_coef <- str_split(df_subset[new_row,2], " ")[[1]]
                LHS_var <-  str_split(df_subset[new_row,3], " ")[[1]] #Does above for LHS variables
                RHS_coef <- str_split(df_subset[new_row,4], " ")[[1]]
                RHS_var <-  str_split(df_subset[new_row,5], " ")[[1]] #grabs RHS vars, splits them so they can be searched for wanted variable
                arrow_type <- df_subset[new_row, 6]
                kf <- df_subset[new_row, 7]
                kr <- df_subset[new_row, 8]
                kcat <- df_subset[new_row, 9]
                Vmax <- df_subset[new_row, 10]
                Km <- df_subset[new_row, 11]
                enzyme <- df_subset[new_row, 12]
                FR_bool <- df_subset[new_row, 13] #boolean if forward regulator exists
                forward_regulators <- df_subset[new_row, 14] #all the forward regulators in equation (space separated)
                forward_regulators_rate_constants <- df_subset[new_row,15] #corresponding rate constant for each regulator
                RR_bool <- df_subset[new_row, 16] #boolean if reverse regulator exists
                reverse_regulators <- df_subset[new_row, 17] #all the reverse regulators in equation (space separated)
                reverse_regulators_rate_constants <- df_subset[new_row,18] #corresponding rate constant for each regulator
                
                #change the rate constants to regulator expressions for the law of mass action if their booleans are true
                if (FR_bool) {kf = regulatorToRate(forward_regulators, forward_regulators_rate_constants)}
                if (RR_bool) {kr = regulatorToRate(reverse_regulators, reverse_regulators_rate_constants)}

                if (var %in% LHS_var) {
                    var_on_left = TRUE
                    var_coef <- LHS_coef[match(var, LHS_var)] #match returns index position of var, ex, var = A, list -> c(A,B) match returns 1 for A and 2 for B
                } else if (var %in% RHS_var) {
                    var_on_left = FALSE
                    var_coef <- RHS_coef[match(var, RHS_var)]
                }
                
                if (!is.na(eqn_type)) {} #checks for rate i think
                    if (eqn_type == "chem_rxn") {
                        if (flag_first_added) {
                            temp.eqn <- law_mass_action(RHS_coef, RHS_var, LHS_coef, LHS_var, arrow_type, kf, kr, var_on_left, var_coef)
                            diff_eqn <- temp.eqn
                            latex.eqn <- massActionEqn2Latex(temp.eqn)
                            flag_first_added <- FALSE
                        } else {
                            temp.eqn <- law_mass_action(RHS_coef, RHS_var, LHS_coef, LHS_var, arrow_type, kf, kr, var_on_left, var_coef)
                            diff_eqn <- paste0(diff_eqn, "+", temp.eqn)
                            latex.eqn <- paste0(latex.eqn, "+", massActionEqn2Latex(temp.eqn))
                        }
                    } else if (eqn_type == "enzyme_rxn") {
                        if (flag_first_added) {
                            temp.eqn <- enzyme_reaction(LHS_var, Km, Vmax, kcat, enzyme, var_on_left)
                            diff_eqn <- temp.eqn
                            latex.eqn <- enzymeEqn2Latex(temp.eqn)
                            flag_first_added <- FALSE
                        } else {
                            temp.eqn <- enzyme_reaction(LHS_var, Km, Vmax, kcat, enzyme, var_on_left)
                            diff_eqn <- paste0(diff_eqn, "+", temp.eqn)
                            latex.eqn <- paste0(latex.eqn, "+", enzymeEqn2Latex(temp.eqn))
                        }
                    } else if (eqn_type == "simp_diff") {
                        if (flag_first_added) {
                            diff_eqn <- simple_diffusion(LHS_var, RHS_var, kf, var_on_left)
                        } else {
                            diff_eqn <- paste0(diff_eqn, "+", simple_diffusion(LHS_var, RHS_var, kf, var_on_left))
                        }
                }
                no.equation = FALSE
            }
        } else {
            no.equation = TRUE
        }
        
        
        #####################################################################################################
        
        #Checking for Input and Outputs
        
        #####################################################################################################
        
        if (InAdded) {
            IO.out <- CalcDiffEqForIO(InputDf, var, "input")
            new.eqn <- IO.out[[1]]
            is.new.eqn <- IO.out[[2]]
            new.latex.eqn <- IO.out[[3]]
            if (is.new.eqn) {
                diff_eqn <- ifelse(no.equation,
                                   RemovePlusSignFromStart(new.eqn),
                                   paste0(diff_eqn, new.eqn))
                
                latex.eqn <- ifelse(no.equation,
                                    RemovePlusSignFromStart(new.latex.eqn),
                                    paste0(latex.eqn, new.latex.eqn))
            } else {
                no.IO.in <- TRUE #no input or output for this specific variable
            }
        } else {
            no.IO.in <- TRUE
        }
        if (OutAdded) {
            IO.out <- CalcDiffEqForIO(OutputDf, var, "output")
            new.eqn <- IO.out[[1]]
            is.new.eqn <- IO.out[[2]]
            new.latex.eqn <- IO.out[[3]]
            if (is.new.eqn) {
                diff_eqn <- ifelse(no.equation,
                                   RemovePlusSignFromStart(new.eqn),
                                   paste0(diff_eqn, new.eqn))
                
                latex.eqn <- ifelse(no.equation,
                                    RemovePlusSignFromStart(new.latex.eqn),
                                    paste0(latex.eqn, new.latex.eqn))
            } else {
                no.IO.out <- TRUE #no input or output for this specific variable
            }
        } else {
            no.IO.out <- TRUE
        }

        if (no.equation & no.IO.in & no.IO.out) { #this is useful and needed if user is adding equations and checking derivations before adding all components (prevent error being thrown)
            diff_eqn = 0
            latex.eqn = 0
        }
        print(diff_eqn)
        differential_equations <- c(differential_equations, diff_eqn)
        differential.eqns.in.latex <- c(differential.eqns.in.latex, latex.eqn)
    }
    out.list <- list("diff.eqns" = differential_equations
                     ,"latex.diff.eqns" = differential.eqns.in.latex)
    return(out.list)
}