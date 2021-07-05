############################### I/O Server ###################################
observeEvent(input$createVar_addVarToList, {
  updatePickerInput(session
                    ,"InOut_selectVar"
                    ,choices = rv$vars_in_model)
  
  updatePickerInput(session #updates output substrate choices for enzyme degradation
                    ,"enzyme_deg_substrate"
                    ,choices = rv$vars_in_model)
  
  updatePickerInput(session
                    ,"enzyme_deg_enzyme"#updates output enzyme choices for enzyme degradation
                    ,choices = rv$vars_in_model)
})

observeEvent(input$createVar_removeVarFromList, {
  updatePickerInput(session #updates output substrate choices for enzyme degradation
                    ,"enzyme_deg_substrate"
                    ,choices = rv$vars_in_model)
  
  updatePickerInput(session
                    ,"enzyme_deg_enzyme"#updates output enzyme choices for enzyme degradation
                    ,choices = rv$vars_in_model)
})

observeEvent(input$Inout_addInVarToDf, {
  rv$In_out_added <- TRUE
  type = input$InOut_typeOfIn #gets the type of the input (rate, diffusion, synthesis, etc)
  speciesName = input$InOut_selectVar #actual name of the species going in (eg. A *rate1 where A is the species)
  if(type == "Rate"){ #if input is a simple rate in (species*rate)
    rateConstantIn = input$In_rate_id #name of the rate constant
    rv$param_inputs <- append(rv$param_inputs, rateConstantIn) #store rateConstant to parameters model
    row_to_df <- c("input", type, speciesName, rateConstantIn, input$In_rate_multiply_with_species, NA, NA, NA) #, varVal, varComment)
    log_row <- paste0("Input of '", speciesName, "' by ", tolower(type), " with rate constant, ", rateConstantIn, sep="")
  }
  
  if(rv$first_inOut)
  {
    rv$first_inOut <- FALSE
    rv$inputOutputs_df[1,] <- row_to_df
  }
  else
  {
    rv$inputOutputs_df <- rbind(rv$inputOutputs_df, row_to_df)
  }
  
  #log info
  logs$IO_logs <- append(log_row, logs$IO_logs)
  rv$number_of_IO = rv$number_of_IO + 1
  observe({print(rv$inputOutputs_df)})
})

observeEvent(input$Inout_addOutVarToDf, {
  rv$In_out_added <- TRUE
  type = input$InOut_typeOfOut  #gets the type of the output (rate, diffusion, synthesis, etc)
  speciesName = input$InOut_selectVar #actual name of the species going out (eg. A *rate1 where A is the species)
  if(type == "Rate")
  { #if output is a simple rate out (species*rate)
    rateConstantOut = input$Out_rate_id #name of the rate constant
    rv$param_outputs <- append(rv$param_outputs, rateConstantOut) #store rateConstant to parameters model
    
    row_to_df <- c("output", type, speciesName, rateConstantOut, input$Out_rate_multiply_with_species, NA, NA, NA) #create row to add to df read by differential equation solver
    
    log_row <- ifelse(input$Out_rate_multiply_with_species,
                      paste0("Output of '", speciesName, "' by ", tolower(type), " with rate constant, ", rateConstantOut, ", conc dependent", sep=""),
                      paste0("Output of '", speciesName, "' by ", tolower(type), " with rate constant, ", rateConstantOut, sep="")
                      )
    if(rv$first_inOut)
    {
      rv$first_inOut <- FALSE
      rv$inputOutputs_df[1,] <- row_to_df
    }
    else
    {
      rv$inputOutputs_df <- rbind(rv$inputOutputs_df, row_to_df)
    }
  }
  else if(type == "Enzyme_Degradation")
  {
    km = input$enzyme_deg_km #micheal menton Km value
    substrate = speciesName
    if(!input$enzyme_deg_vmax_opt)
    { #if michealis menton eqn uses vmax
      vmax = input$enzyme_deg_Vmax
      enzyme = NA
      kcat = NA
      rv$param_outputs <- append(rv$param_outputs, vmax) #store Vmax to parameters model
      rv$param_outputs <- append(rv$param_outputs, km) #store Michelis Menton Constant to parameters model
      row_to_df <- c("output", type, substrate, km, NA, vmax, NA, NA) #create row to add to df read by differential equation solver
      log_row <- paste0("Output of '", speciesName, "' by enzyme degradation", " with Vmax, ", vmax, sep="")
      if(rv$first_inOut)
      {
        rv$first_inOut <- FALSE
        rv$inputOutputs_df[1,] <- row_to_df
      }
      else
      {
        rv$inputOutputs_df <- rbind(rv$inputOutputs_df, row_to_df)
      }
    }
    else
    { #if vmax = kcat*enzyme
      vmax = NA
      enzyme = input$enzyme_deg_enzyme
      kcat = input$enzyme_deg_kcat
      rv$param_outputs <- append(rv$param_outputs, km) #store Michelis Menton Constant to parameters model
      rv$param_outputs <- append(rv$param_outputs, kcat) #store rateConstant to parameters model
      
      row_to_df <- c("output", type, substrate, km, NA, NA, kcat, enzyme) #create row to add to df read by differential equation solver
      log_row <- paste0("Output of '", speciesName, "' by enzyme degradation", " with enzyme, ", enzyme, ", and kcat, ", kcat)
      
      if(rv$first_inOut)
      {
        rv$first_inOut <- FALSE
        rv$inputOutputs_df[1,] <- row_to_df
      }
      else
      {
        rv$inputOutputs_df <- rbind(rv$inputOutputs_df, row_to_df)
      }
    }
  }
  #log info
  logs$IO_logs <- append(logs$IO_logs, log_row)
  rv$number_of_IO = rv$number_of_IO + 1
  observe({print(rv$inputOutputs_df)})
})

output$IO_Display_Logs <- renderText({
  if(length(logs$IO_logs)==0)
  {
    paste("No Input or Outputs Entered")
  }
  else
  {
    n_eqns = seq(length(logs$IO_logs))
    eqns_to_display <- c()
    for(i in n_eqns)
    {
      new_eqn <- paste0("(",i, ") ", logs$IO_logs[i])
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse="<br>")
  }
})

#-------------------------------------------------------------------------------

#Delete Equation 

#-------------------------------------------------------------------------------

#updates picker input to delete input output equations with the number of input/output equations there are
observeEvent(input$Inout_addInVarToDf | input$Inout_addOutVarToDf, {
  updatePickerInput(session
                    ,"Inout_delete_IO_eqn"
                    ,choices = seq(rv$number_of_IO))
})

#deletes the selected I/O equation from the model
observeEvent(input$Inout_button_delete_IO_eqn, {
  number_of_equation_to_delete <- as.numeric(input$Inout_delete_IO_eqn)
  if(number_of_equation_to_delete > 0) #nothing happens if there are no equations in the model
  {
    rv$inputOutputs_df <-rv$inputOutputs_df[-number_of_equation_to_delete, 1:ncol(rv$inputOutputs_df)] #remove in out data from dataframe that stores the information
    logs$IO_logs <- logs$IO_logs[-number_of_equation_to_delete] #remove the log entry of the input/output
    rv$number_of_IO <- rv$number_of_IO - 1 #change the number of Io in model 
    
    updatePickerInput(session
                      ,"Inout_delete_IO_eqn"
                      ,choices = seq(rv$number_of_IO)) #reset pickerinput to account for deleted value
  }
  observe({print(rv$inputOutputs_df)})
})

