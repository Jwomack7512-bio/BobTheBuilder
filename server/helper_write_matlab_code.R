create_matlab_model_function <- function(variables, parameters, equations, parameter_values, rate_eqns, ICs, time_scale_bool, time_scale_value){
    #print start of model function to matlab script includeing variable unpacking
    model_function <- "function dy = model(~, y, par)\n"
    for(var_num in seq(length(variables)))
    {
        line_to_add <- paste0("\t",variables[var_num], "=y(", var_num, ");\n")
        model_function <- paste0(model_function, line_to_add)
    }
    
    #paste parameters  to matlab script
    model_function <- paste0(model_function, "\n%Parameters\n")
    for(par_num in seq(length(parameters)))
    {
        line_to_add <- paste0("\t",parameters[par_num], "=", "par(", par_num, ");\n")
        model_function <- paste0(model_function, line_to_add)
    }
    
    #paste rate equations to matlab script
    model_function <- paste0(model_function, "\n%Rate Equations\n")
    for(i in seq(length(rate_eqns)))
    {
        line_to_add <- paste0("\t", rate_eqns[i], ";\n")
        model_function <- paste0(model_function, line_to_add)
    }
    
    #paste differential equations to matlab script
    model_function <- paste0(model_function, "\n%Differential Equations\n")
    for(eqn_num in seq(length(equations)))
    {
        line_to_add <- paste0("\t", "d", variables[eqn_num], "=", equations[eqn_num], ";\n")
        model_function <- paste0(model_function, line_to_add)
    }
    
    #package up differential equations to output variable dy
    model_function <- paste0(model_function, "\n%Package Differential Equations to Output\n")
    for(i in seq(length(equations)))
    {
        line_to_add <- paste0("\t", "dy(", i, ") = ", "d", variables[i], ";\n")
        model_function <- paste0(model_function, line_to_add)
    }
    ending_line <- ifelse(time_scale_bool, paste0("\tdy=", time_scale_value, "*dy';\n", "end\n"), paste0("\tdy=dy';\n", "end\n"))
    model_function <- paste0(model_function, ending_line)
    
    ############################################################################
    #generate parameter function that stores all parameters of the equation
    parameter_function <- "function p = pars()\n"
    
    for(par_num in seq(length(parameters)))
    {
        line_to_add <- paste0("\tp(", par_num, ")=", parameter_values[par_num], ";\t%", parameters[par_num]   ,"\n")
        parameter_function <- paste0(parameter_function, line_to_add)
    }
    parameter_function <- paste0(parameter_function, "end")
    
    model_function <- paste0(model_function, "\n\n", parameter_function)
    
    ############################################################################
    
    # Add matlab function for initial conditions
    IC_function <- "function y0 = Initial_Conditions()\n"
    
    for(i in seq(length(ICs)))
    {
        line_to_add <- paste0("\t", "y0(", i, ")=", ICs[i], ";\t%", variables[i], "\n")
        IC_function <- paste0(IC_function, line_to_add)
    }
    IC_function <- paste0(IC_function, "end")
    
    model_function <- paste0(model_function, "\n\n", IC_function)
    ############################################################################
    
    output <- model_function
    
    return(output)
}