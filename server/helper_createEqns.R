split_rate_to_components <- function(){
    #take right hand side of equation and split into variables and parameters
    #store parameters to their proper vector
}

remove_rate_parameters_from_vectors <- function(parameter_to_remove)
{
    #search all parameters lists for parameter and remove it from each. (input, output, eqn, total)
    if(parameter_to_remove %in% rv$param_inputs)
    {
        rv$param_inputs <- rv$param_inputs[!rv$param_inputs %in% parameter_to_remove]
    }
    if(parameter_to_remove %in% rv$param_outputs)
    {
        rv$param_outputs <- rv$param_outputs[!rv$param_outputs %in% parameter_to_remove]
    }
    if(parameter_to_remove %in% rv$param_eqns)
    {
        rv$param_eqns <- rv$param_eqns[!rv$param_eqns %in% parameter_to_remove]
    }
    if(parameter_to_remove %in% rv$parameters_in_model)
    {
        rv$parameters_in_model <- rv$parameters_in_model[!rv$parameters_in_model %in% parameter_to_remove]
    }
    #remove all excess variables from created lists if they exist (ie. we generated ui for parameter values and comments.  Will need to remove those)
}
