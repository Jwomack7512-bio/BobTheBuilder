StoreParamsIO <- function(parameterToAdd, InOrOut) {
  #note InOrOut should be a string that is either "In" or "Out"
  #NEED TO ADD CHECK IF PARAM ALREADY EXISTS
  if (!(parameterToAdd %in% params$vars.all) &&
      !(parameterToAdd %in% params$rate.params)) {
    params$vars.all <- append(params$vars.all, parameterToAdd)
    params$vals.all <- append(params$vals.all, 0)
    params$comments.all <- append(params$comments.all, "")

    if (InOrOut == "In") {
      params$inputs.vars <- append(params$inputs.vars, parameterToAdd)
      params$inputs.vals <- append(params$inputs.vals, 0)
      params$inputs.comments <- append(params$inputs.comments, "")
    } else {#if output
      params$outputs.vars <- append(params$outputs.vars, parameterToAdd)
      params$outputs.vals <- append(params$outputs.vals, 0)
      params$outputs.comments <- append(params$outputs.comments, "")
    }
    
    #add parameter to parameter table
    row.to.add <- c(parameterToAdd, 0, "")
    if (nrow(params$param.table) == 0) {
      params$param.table[1,] <- row.to.add
    } else {
      params$param.table <- rbind(params$param.table, row.to.add)
    }
  }
}


############################### I/O Server ###################################
observeEvent(vars$species, {
  updatePickerInput(session = session
                    ,"InOut_selectVar"
                    ,choices = sort(vars$species))
  
  updatePickerInput(session = session
                    ,"IO_factor_for_syn"
                    ,choices = sort(vars$species))
  
  updatePickerInput(session #updates output substrate choices for enzyme degradation
                    ,"enzyme_deg_substrate"
                    ,choices = sort(vars$species))
  
  updatePickerInput(session
                    ,"enzyme_deg_enzyme"#updates output enzyme choices for enzyme degradation
                    ,choices = sort(vars$species))
  
  updatePickerInput(session,
                    "MA_species"
                    ,choices = sort(vars$species))
})

# adds inputs of model to appropiate df for analysis
observeEvent(input$Inout_addInVarToDf, {
  IO$bool.IO.added <- TRUE
  type = input$InOut_typeOfIn #gets the type of the input (rate, diffusion, synthesis, etc)
  speciesName = input$InOut_selectVar #actual name of the species going in (eg. A *rate1 where A is the species)
  if (type == "Rate") { #if input is a simple rate in (species*rate)
    rate.constant.in = input$In_rate_id #name of the rate constant
    # params$inputs.vars <- append(params$inputs.vars, rateConstantIn) #store rateConstant to parameters model
    StoreParamsIO(rate.constant.in, "In")
    row_to_df <- c("input", 
                   type, 
                   speciesName, 
                   rate.constant.in, 
                   input$In_rate_multiply_with_species,
                   NA, 
                   NA, 
                   NA) 
    log_row <- paste0("Input of '", 
                      speciesName, 
                      "' by ", 
                      tolower(type), 
                      " with rate constant, ", 
                      rate.constant.in, 
                      sep = "")
  } else if (type == "Synthesis") {
    rate.constant.in <- input$IO_rc_for_syn
    StoreParamsIO(rate.constant.in, "In")
    row_to_df <- c("input", 
                   type, 
                   speciesName, 
                   rate.constant.in, 
                   FALSE, 
                   NA, 
                   NA, 
                   input$IO_factor_for_syn) 
    log_row <- paste0("Synthesis of '", 
                      speciesName, 
                      "' by ", 
                      tolower(input$IO_factor_for_syn), 
                      " with rate constant, ", 
                      rate.constant.in, sep = "")
  }
  if (IO$bool.IO.exists) {
    IO$bool.IO.exists <- FALSE
    IO$IO.info[1,] <- row_to_df
  }
  else
  {
    IO$IO.info <- rbind(IO$IO.info, row_to_df)
  }
  
  #log info
  logs$IO.logs <- append(log_row, logs$IO.logs)
  IO$n.IO = IO$n.IO + 1
  jPrint(IO$IO.info)
})

# adds outputs of model to appropiate df for analysis
observeEvent(input$Inout_addOutVarToDf, {
  IO$bool.IO.added <- TRUE
  type = input$InOut_typeOfOut  #gets the type of the output (rate, diffusion, synthesis, etc)
  speciesName = input$InOut_selectVar #actual name of the species going out (eg. A *rate1 where A is the species)
  if (type == "Rate") { #if output is a simple rate out (species*rate)
    rateConstantOut = input$Out_rate_id #name of the rate constant
    #params$outputs.vars <- append(params$outputs.vars, rateConstantOut) #store rateConstant to parameters model
    StoreParamsIO(rateConstantOut, "Out")
    row_to_df <- c("output", type, speciesName, rateConstantOut, input$Out_rate_multiply_with_species, NA, NA, NA) #create row to add to df read by differential equation solver
    
    log_row <- ifelse(input$Out_rate_multiply_with_species,
                      paste0("Output of '", speciesName, "' by ", tolower(type), " with rate constant, ", rateConstantOut, ", conc dependent", sep = ""),
                      paste0("Output of '", speciesName, "' by ", tolower(type), " with rate constant, ", rateConstantOut, sep = "")
                      )
    if (IO$bool.IO.exists) {
      IO$bool.IO.exists <- FALSE
      IO$IO.info[1,] <- row_to_df
    }
    else
    {
      IO$IO.info <- rbind(IO$IO.info, row_to_df)
    }
  }
  else if (type == "Enzyme_Degradation") {
    km = input$enzyme_deg_km #micheal menton Km value
    substrate = speciesName
    if (!input$enzyme_deg_vmax_opt) { #if michealis menton eqn uses vmax
      vmax = input$enzyme_deg_Vmax
      enzyme = NA
      kcat = NA
      # params$outputs.vars <- append(params$outputs.vars, vmax) #store Vmax to parameters model
      # params$outputs.vars <- append(params$outputs.vars, km) #store Michelis Menton Constant to parameters model
      StoreParamsIO(Vmax, "Out")
      StoreParamsIO(km, "Out")
      row_to_df <- c("output", type, substrate, km, NA, vmax, NA, NA) #create row to add to df read by differential equation solver
      log_row <- paste0("Output of '", speciesName, "' by enzyme degradation", " with Vmax, ", vmax, sep = "")
      if (IO$bool.IO.exists) {
        IO$bool.IO.exists <- FALSE
        IO$IO.info[1,] <- row_to_df
      } else {
        IO$IO.info <- rbind(IO$IO.info, row_to_df)
      }
    } else {#if vmax = kcat*enzyme
      vmax = NA
      enzyme = input$enzyme_deg_enzyme
      kcat = input$enzyme_deg_kcat
      # params$outputs.vars <- append(params$outputs.vars, km) #store Michelis Menton Constant to parameters model
      # params$outputs.vars <- append(params$outputs.vars, kcat) #store rateConstant to parameters model
      StoreParamsIO(kcat, "Out")
      StoreParamsIO(km, "Out")
      row_to_df <- c("output", type, substrate, km, NA, NA, kcat, enzyme) #create row to add to df read by differential equation solver
      log_row <- paste0("Output of '", speciesName, "' by enzyme degradation", " with enzyme, ", enzyme, ", and kcat, ", kcat)
      
      if (IO$bool.IO.exists) {
        IO$bool.IO.exists <- FALSE
        IO$IO.info[1,] <- row_to_df
      } else {
        IO$IO.info <- rbind(IO$IO.info, row_to_df)
      }
    }
  }
  else if (type == "mass_action") {
    rateConstantOut = input$MA_deg_rate_constant
    params$outputs.vars <- append(params$outputs.vars, rateConstantOut) #store rateConstant to parameters model
    transporter_out <- input$MA_species
    row_to_df <- c("output", type, speciesName, rateConstantOut, NA, NA, NA, transporter_out)
    if (IO$bool.IO.exists) {
      IO$bool.IO.exists <- FALSE
      IO$IO.info[1,] <- row_to_df
    }
    else {
      IO$IO.info <- rbind(IO$IO.info, row_to_df)
    }
    
    log_row <- paste0("Output of '", speciesName, "' by ", tolower(type), " with rate constant, ", rateConstantOut, " by ", transporter_out)
  }
  #log info
  logs$IO.logs <- append(logs$IO.logs, log_row)
  IO$n.IO = IO$n.IO + 1
  observe({print(IO$IO.info)})
})

output$IO_Display_Logs <- renderText({
  if (length(logs$IO.logs) == 0) {
    paste("No Input or Outputs Entered")
  } else {
    n_eqns = seq(length(logs$IO.logs))
    eqns_to_display <- c()
    for (i in n_eqns) {
      new_eqn <- paste0("(",i, ") ", logs$IO.logs[i])
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse = "<br>")
  }
})

#-------------------------------------------------------------------------------

#Delete Equation 

#-------------------------------------------------------------------------------

#updates picker input to delete input output equations with the number of input/output equations there are
observeEvent(input$Inout_addInVarToDf | input$Inout_addOutVarToDf, {
  updatePickerInput(session
                    ,"Inout_delete_IO_eqn"
                    ,choices = seq(IO$n.IO))
})

#deletes the selected I/O equation from the model
observeEvent(input$Inout_button_delete_IO_eqn, {
  number_of_equation_to_delete <- as.numeric(input$Inout_delete_IO_eqn)
  if (number_of_equation_to_delete > 0) #nothing happens if there are no equations in the model
  {
    IO$IO.info <- IO$IO.info[-number_of_equation_to_delete, 1:ncol(IO$IO.info)] #remove in out data from dataframe that stores the information
    logs$IO.logs <- logs$IO.logs[-number_of_equation_to_delete] #remove the log entry of the input/output
    IO$n.IO <- IO$n.IO - 1 #change the number of Io in model 
    
    updatePickerInput(session
                      ,"Inout_delete_IO_eqn"
                      ,choices = seq(IO$n.IO)) #reset pickerinput to account for deleted value
  }
  observe({print(IO$IO.info)})
})

