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
    if(length(RHS_var)==1 & length(LHS_var)==1){
        #Case 1.1 A <--> B, Reaction flows both ways
        if(arrow_type=="both_directions"){
            ifelse(as.numeric(RHS_coef) == 1, RHS_eqn <- paste0(kr, "*", RHS_var), RHS_eqn <- paste0(kr, "*", RHS_var, "^", RHS_coef))
            ifelse(as.numeric(LHS_coef) == 1, LHS_eqn <- paste0(kf, "*", LHS_var), LHS_eqn <- paste0(kf, "*", LHS_var, "^", LHS_coef))
            RHS_eqn <- paste0("(", RHS_eqn, ")")
            LHS_eqn <- paste0("(", LHS_eqn, ")")
            eqn_out <- paste0("(", LHS_eqn, " - ", RHS_eqn, ")")
            
            #this if/else multiplies the coefficient of the var to the equation dependant on which var is being processed using var_on_left
            if(var_coef>1){
                ifelse(var_on_left, eqn_out <- paste0("-", var_coef, "*", eqn_out), eqn_out <- paste0(var_coef, "*", eqn_out))
            }else{
                ifelse(var_on_left, eqn_out <- paste0("-1", "*", eqn_out), eqn_out <- eqn_out)
            }
        }
        #Case 1.2 A->B, only in forward direction
        else if(arrow_type=="forward_only"){
            ifelse(as.numeric(LHS_coef) == 1, LHS_eqn <- paste0(kf, "*", LHS_var), LHS_eqn <- paste0(kf, "*", LHS_var, "^", LHS_coef))
            eqn_out <- paste0("(", LHS_eqn, ")")
            if(var_coef>1){
                ifelse(var_on_left, eqn_out <- paste0("-", var_coef, "*", eqn_out), eqn_out <- paste0(var_coef, "*", eqn_out))
            }else{
                ifelse(var_on_left, eqn_out <- paste0("-1", "*", eqn_out), eqn_out <- eqn_out)
            }
        }
        
    }
    #Case 2
    else if(length(LHS_var)>1 & length(RHS_var)==1){
        #Case A + B <--> C
        if(arrow_type=="both_directions"){
            ifelse(as.numeric(RHS_coef) == 1, RHS_eqn <- paste0(kr, "*", RHS_var), RHS_eqn <- paste0(kr, RHS_var, "^", RHS_coef))
            #RHS_eqn <- paste0("(", RHS_eqn, ")")
            for(i in seq(length(LHS_var))){
                if(i==1){
                    ifelse(as.numeric(LHS_coef[i]) == 1, LHS_eqn <- paste0(kf, "*", LHS_var[i]), LHS_eqn <- paste0(kf, "*", LHS_var[i], "^", LHS_coef[i]))
                }else{
                    ifelse(as.numeric(LHS_coef[i]) == 1, LHS_eqn <- paste0(LHS_eqn, "*", LHS_var[i]), LHS_eqn <- paste0(LHS_eqn, "*", LHS_var[i], "^", LHS_coef[i]))
                }
            }
            #LHS_eqn <- paste0("(", LHS_eqn, ")")
            eqn_out <- paste0("(", LHS_eqn, " - ", RHS_eqn, ")")
            
            ifelse(var_on_left, eqn_out <- paste0("-", var_coef, "*", eqn_out), eqn_out <- paste0(var_coef, "*", eqn_out))
            
        }
        #Case A + B --> C
        else if(arrow_type=="forward_only"){
            for(i in seq(length(LHS_var))){
                if(i==1){
                    ifelse(as.numeric(LHS_coef[i]) == 1, LHS_eqn <- paste0(kf, "*", LHS_var[i]), LHS_eqn <- paste0(kf, "*", LHS_var[i], "^", LHS_coef[i]))
                }else{
                    ifelse(as.numeric(LHS_coef[i]) == 1, LHS_eqn <- paste0(LHS_eqn, "*", LHS_var[i]), LHS_eqn <- paste0(LHS_eqn, "*", LHS_var[i], "^", LHS_coef[i]))
                }
            }
            LHS_eqn <- paste0("(", LHS_eqn, ")")
            ifelse(var_on_left, LHS_eqn<- paste0("-", var_coef, "*", LHS_eqn), LHS_eqn <- paste0(var_coef, "*", LHS_eqn))
            eqn_out <- paste0("(", LHS_eqn, ")")
        }
    }
    #Case 3
    else if(length(RHS_var)>1 & length(LHS_var)==1){
        #print("EC:3")
        #Case A <--> B + C
        if(arrow_type=="both_directions"){
            ifelse(as.numeric(LHS_coef) == 1, LHS_eqn <- paste0(kf, "*", LHS_var), LHS_eqn <- paste0(kf, "*", LHS_var, "^", LHS_coef))
            LHS_eqn <- paste0("(", LHS_eqn, ")")
            for(i in seq(length(RHS_var))){
                if(i==1){
                    ifelse(as.numeric(RHS_coef[i]) == 1, RHS_eqn <- paste0(kr, "*", RHS_var[i]), RHS_eqn <- paste0(kr, "*", RHS_var[i], "^", RHS_coef[i]))
                }else{
                    ifelse(as.numeric(RHS_coef[i]) == 1, RHS_eqn <- paste0(RHS_eqn, "*", RHS_var[i]), RHS_eqn <- paste0(RHS_eqn, "*", RHS_var[i], "^", RHS_coef[i]))
                }
            }
            RHS_eqn <- paste0("(", RHS_eqn, ")")
            eqn_out <- paste0("(", LHS_eqn, " - ", RHS_eqn, ")")
            
            if(var_coef>1){
                ifelse(var_on_left, eqn_out <- paste0("-", var_coef, "*", eqn_out), eqn_out <- paste0(var_coef, "*", eqn_out))
            }else{
                ifelse(var_on_left, eqn_out <- paste0("-1", "*", eqn_out), eqn_out <- eqn_out)
            }
        }
        #Case: A --> B + c
        else if(arrow_type=="forward_only"){
            for(i in seq(length(LHS_var))){
                if(i==1){
                    ifelse(as.numeric(LHS_coef[i]) == 1, LHS_eqn <- paste0(kf, "*", LHS_var[i]), LHS_eqn <- paste0(kf, "*", LHS_var[i], "^", LHS_coef[i]))
                }else{
                    ifelse(as.numeric(LHS_coef[i]) == 1, LHS_eqn <- paste0(LHS_eqn, "*", LHS_eqn[i]), LHS_eqn <- paste0(LHS_eqn, "*", LHS_eqn[i], "^", LHS_eqn[i]))
                }
            }
            eqn_out <- paste0("(", LHS_eqn, ")")
            if(var_coef>1){
                ifelse(var_on_left, eqn_out <- paste0("-", var_coef, "*", eqn_out), eqn_out <- paste0(var_coef, "*", eqn_out))
            }else{
                ifelse(var_on_left, eqn_out <- paste0("-1", "*", eqn_out), eqn_out <- eqn_out)
            }
        }
    }
    #Case 4
    else{#need to finish.  Here will go if RHS&&LHS>1.  Just copy an paste things from the above sections.  Run and test and hopefully it doens't take too long. 
        #print("EC:4")
        #Case A + B <--> C + D
        if(arrow_type=="both_directions"){
            for(i in seq(length(RHS_var))){
                if(i==1){
                    ifelse(as.numeric(RHS_coef[i]) == 1, RHS_eqn <- paste0(kr, "*", RHS_var[i]), RHS_eqn <- paste0(kr, "*", RHS_var[i], "^", RHS_coef[i]))
                }else{
                    ifelse(as.numeric(RHS_coef[i]) == 1, RHS_eqn <- paste0(RHS_eqn, "*", RHS_var[i]), RHS_eqn <- paste0(RHS_eqn, "*", RHS_var[i], "^", RHS_coef[i]))
                }
            }
            RHS_eqn <- paste0("(", RHS_eqn, ")")
            
            for(i in seq(length(LHS_var))){
                if(i==1){
                    ifelse(as.numeric(LHS_coef[i]) == 1, LHS_eqn <- paste0(kf, "*", LHS_var[i]), LHS_eqn <- paste0(kf, "*", LHS_var[i], "^", LHS_coef[i]))
                }else{
                    ifelse(as.numeric(LHS_coef[i]) == 1, LHS_eqn <- paste0(LHS_eqn, "*", LHS_var[i]), LHS_eqn <- paste0(LHS_eqn, "*", LHS_var[i], "^", LHS_coef[i]))
                }
            }
            LHS_eqn <- paste0("(", LHS_eqn, ")")
            eqn_out <- paste0("(", LHS_eqn, " - ", RHS_eqn, ")")
            
            if(var_coef>1){
                ifelse(var_on_left, eqn_out <- paste0("-", var_coef, "*", eqn_out), eqn_out <- paste0(var_coef, "*", eqn_out))
            }else{
                ifelse(var_on_left, eqn_out <- paste0("-1", "*", eqn_out), eqn_out <- eqn_out)
            }
        }
        #Case: A + B --> C + D
        else if(arrow_type=="forward_only"){
            for(i in seq(length(LHS_var))){
                if(i==1){
                    ifelse(as.numeric(LHS_coef[i]) == 1, LHS_eqn <- paste0(kf, "*", LHS_var[i]), LHS_eqn <- paste0(kf, "*", LHS_var[i], "^", LHS_coef[i]))
                }else{
                    ifelse(as.numeric(LHS_coef[i]) == 1, LHS_eqn <- paste0(LHS_eqn, "*", LHS_var[i]), LHS_eqn <- paste0(LHS_eqn, "*", LHS_var[i], "^", LHS_coef[i]))
                }
            }
            eqn_out <- paste0("(", LHS_eqn, ")")
            if(var_coef>1){
                ifelse(var_on_left, eqn_out <- paste0("-", var_coef, "*", eqn_out), eqn_out <- paste0(var_coef, "*", eqn_out))
            }else{
                ifelse(var_on_left, eqn_out <- paste0("-1", "*", eqn_out), eqn_out <- eqn_out)
            }
        }
    }
    return(eqn_out)
}

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
enzyme_reaction <- function(substrate, km, Vmax, kcat, enzyme, var_on_left)
{
    #print("Var on left:")
    #print(var_on_left)
    if(!is.na(Vmax))
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
    if(var_on_left){
        #PS*(C1-c2) where C1 is left hand side variable
        eqn = paste0(PS, "*(", RHS_var, "-", LHS_var, ")")
    }else{
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
    if(input_or_output=="input"){
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
    for(row in 1:nrow(myModel)){#search rows of data for the choosen variable and subset them to new df
        #print(myModel[row,])
        #law_of_derivation <- myModel[row,1]
        RHS_var <- str_split(myModel[row,3], " ")[[1]] #grabs RHS vars, splits them so they can be searched for wanted variable
        LHS_var <- str_split(myModel[row,5], " ")[[1]] #Does above for LHS variables
        if(var_to_subset_with %in% RHS_var | var_to_subset_with %in% LHS_var){ #find indices containing var name
            index_of_rows_with_var <- c(index_of_rows_with_var, row) #adds index to vector to subset main df later
        }
    }    
    temp_df <- myModel[index_of_rows_with_var, ] #extract var rows
    #print(temp_df)
    return(temp_df)
}


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
    if(!is.na(Vmax))
    { #if vmax used
        eqn = paste0("-", Vmax, "*", substrate, "/(", km, "+", substrate, ")") #-Vmax*S/(km+S)
    }
    else
    {
        eqn = paste0("-", kcat, "*", enzyme, "*", substrate, "/(", km, "+", substrate, ")") #-km*E*S/(km+S)
    }
    return(eqn)
}

##################### FUNCTION: calc_differential_equations ####################
# Model Description: 
# Inputs
# @myModel - df to parse containing all equation parameters (this is typically an output of Rhinsy)
# @vars_to_diffeq - vector of variables that we want to create differential equations for
# @inOutModel - df containing all In/out parameter and values

# Outputs
# @Diffeqs_out - vector of differential equations in string form
################################################################################
calc_differential_equations <- function(myModel, var_to_diffeq, InOutModel, InOutAdded)
{
    count = 1
    differential_equations = vector()
    #choosing variable to solve the differential equation for
    for(var in var_to_diffeq) 
    {
        df_subset <- extract_data(myModel, var)
        flag_first_added <- TRUE
        
        #####################################################################################################
        
        #Checks each row of Dataframe for laws and calculates the equations for the laws, i.e. mass action, diffusion. etc
        
        #####################################################################################################
        if(nrow(df_subset) > 0)
        {
            for(new_row in 1:nrow(df_subset))
            {
                eqn_type <- df_subset[new_row, 1]
                LHS_coef <- str_split(df_subset[new_row,2],  " ")[[1]]
                LHS_var <- str_split(df_subset[new_row,3], " ")[[1]] #Does above for LHS variables
                RHS_coef <- str_split(df_subset[new_row,4], " ")[[1]]
                RHS_var <- str_split(df_subset[new_row,5], " ")[[1]] #grabs RHS vars, splits them so they can be searched for wanted variable
                arrow_type <- df_subset[new_row, 6]
                kf <- df_subset[new_row, 7]
                kr <- df_subset[new_row, 8]
                kcat <- df_subset[new_row, 9]
                Vmax <- df_subset[new_row, 10]
                Km <- df_subset[new_row, 11]
                enzyme <- df_subset[new_row, 12]
                # print(paste("Length RHS_Coef:", length(RHS_coef)))
                #print(paste("length RHS_Var:", length(RHS_var)))
                # print(paste("length LHS_Coef:", length(LHS_coef)))
                # print(paste("length LHS_Var:", length(LHS_var)))
                # print(paste("RHS_Coef:", RHS_coef))
                #print(paste("RHS_Var:", RHS_var))
                # print(paste("LHS_Coef:", LHS_coef))
                #print(paste("LHS_Var:", LHS_var))
                if(var %in% LHS_var)
                {
                    var_on_left = TRUE
                    var_coef <- LHS_coef[match(var, LHS_var)] #match returns index position of var, ex, var = A, list -> c(A,B) match returns 1 for A and 2 for B
                }
                else if(var %in% RHS_var)
                {
                    var_on_left = FALSE
                    var_coef <- RHS_coef[match(var, RHS_var)]
                }
                
                if(!is.na(eqn_type)) #checks for rate i think
                {
                    if(eqn_type == "chem_rxn")
                    {
                        if(flag_first_added)
                        {
                            diff_eqn <- law_mass_action(RHS_coef, RHS_var, LHS_coef, LHS_var, arrow_type, kf, kr, var_on_left, var_coef)
                            flag_first_added <- FALSE
                        }
                        else
                        {
                            diff_eqn <- paste0(diff_eqn, " + (", law_mass_action(RHS_coef, RHS_var, LHS_coef, LHS_var, arrow_type, kf, kr, var_on_left, var_coef), ")")
                        }
                    }
                    if(eqn_type=="enzyme_rxn")
                    {
                        if(flag_first_added)
                        {
                            diff_eqn <- enzyme_reaction(LHS_var, Km, Vmax, kcat, enzyme, var_on_left)
                            flag_first_added <- FALSE
                        }
                        else
                        {
                            diff_eqn <- paste0(diff_eqn, "+ (", enzyme_reaction(LHS_var, Km, Vmax, kcat, enzyme, var_on_left), ")")
                        }
                    }
                    if(eqn_type=="simp_diff")
                    {
                        #print("DIFF")
                        if(flag_first_added)
                        {
                            diff_eqn <- simple_diffusion(LHS_var, RHS_var, kf, var_on_left)
                        }
                        else
                        {
                            diff_eqn <- paste0(diff_eqn, "+ (",simple_diffusion(LHS_var, RHS_var, kf, var_on_left), ")")
                        }
                    }
                }
                no_equation = FALSE
            } 
        }
        else
        {
            no_equation = TRUE
        }
        
        #####################################################################################################
        
        #Checking for Input and Outputs
        
        #####################################################################################################
        if(InOutAdded)
        {
            #print("Input or Output exists with this species")
            for(row in 1:nrow(InOutModel))
            {
                species_id <- InOutModel[row, 3]
                
                if(species_id==var)
                {
                    input_or_output <- InOutModel[row,1]
                    InOutType <- InOutModel[row,2]
                    if(InOutType=="Rate")
                    {
                        rateConstant <- InOutModel[row,4]
                        multiply_rateConstant_by_var <- InOutModel[row, 5]
                        #determine if the user wishes to multiple the rate constant by the concentration of the species.
                        if(multiply_rateConstant_by_var)
                        {
                            eqn = paste0(rateConstant, "*", species_id)
                        }
                        else
                        {
                            eqn = paste0(rateConstant)
                        }
                        #determine if differential equation is input or output, i.e. if it should be added or subtracted to the model
                        if(input_or_output == "input")
                        {
                            diff_eqn <- paste0(diff_eqn,  " + ", eqn)
                        }
                        else
                        {
                            diff_eqn <- paste0(diff_eqn, " - ", eqn)
                        }
                    }
                    else if(InOutType=="Enzyme_Degradation")
                    {
                        substrate <- InOutModel[row, 3]
                        km <- InOutModel[row,4]
                        Vmax <- InOutModel[row,6]
                        kcat <- InOutModel[row,7]
                        enzyme <- InOutModel[row,8]
                        eqn <- enzyme_degradation(substrate, km, Vmax, kcat, enzyme)
                        diff_eqn <- paste0(diff_eqn, eqn)
                    }
                }
                else
                {
                    no_in_out = TRUE
                }
            }
        }
        else
        {
            no_in_out = TRUE
        }
        
        if(no_equation && no_in_out) #this is useful and needed if user is adding equations and checking derivations before adding all components (prevent error being thrown)
        {
            diff_eqn = 0
            no_equation = FALSE #reset value for next loop iteration
            no_in_out = FALSE
        }
        
        #print(diff_eqn)
        differential_equations <- c(differential_equations, diff_eqn)
        #print(differential_equations)
    }
    # c = 1
    # for(i in differential_equations)
    # {
    #     print(paste0("Differential Equation of ", var_to_diffeq[c], ": ", i))
    #     c = c + 1
    # }
    return(differential_equations)
}