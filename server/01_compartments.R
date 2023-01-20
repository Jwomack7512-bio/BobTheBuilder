# This script holds server side processing of compartment generation, editing,
# and related UI events in the create model tab


# Table Render -----------------------------------------------------------------
output$createVar_compartment_table <- renderRHandsontable({
  
  # Set up dataframe for table
  for.table <- vars$compartments.df %>%
    select("Name", "Volume", "Value", "Unit", "Description")
  
  colnames(for.table) <- c("Name", "Volume", "Value", "Unit", "Description")
  

  rhandsontable(for.table,
                rowHeaders = NULL,
                overflow = "visible",
                selectCallback = TRUE,
                colHeaderWidth = 100,
                stretchH = "all"
  ) %>%
    hot_cols(
      colWidth = c(30, 30, 20, 20, 40),
      manualColumnMove = FALSE,
      manualColumnResize = TRUE,
      halign = "htCenter",
      valign = "htMiddle",
      renderer = "
         function (instance, td, row, col, prop, value, cellProperties) {
           Handsontable.renderers.NumericRenderer.apply(this, arguments);
           if (row % 2 == 0) {
            td.style.background = '#f9f9f9';
           } else {
            td.style.background = 'white';
           };
         }") %>%
    #hot_col("Variable Name", readOnly = TRUE) %>%
    hot_rows(rowHeights = 30) %>%
    hot_context_menu(allowRowEdit = FALSE,
                     allowColEdit = FALSE
    )
})

# Rhandsontable: Cell Change ---------------------------------------------------
observeEvent(input$createVar_compartment_table$changes$changes, {
  xi  = input$createVar_compartment_table$changes$changes[[1]][[1]]
  yi  = input$createVar_compartment_table$changes$changes[[1]][[2]]
  old = input$createVar_compartment_table$changes$changes[[1]][[3]]
  new = input$createVar_compartment_table$changes$changes[[1]][[4]]
  
  # Find which variable is being changed
  # Set up dataframe for table
  for.table <- vars$compartments.df %>%
    select("Name", "Volume", "Value", "Unit", "Description")
  var.name <- as.character(for.table[xi+1, 1])
  if (yi == 0) {
    # Compartment Name Changed
    vars$compartments.info[[old]]$Name <- new
    idx <- which(names(vars$compartments.info) %in% old)
    names(vars$compartments.info)[idx] <- new
    
    
    #Search other areas affected by id
    comp.id <- FindId(old)
    # ___Var List____
    for (i in seq_along(vars$var.info)) {
      if (vars$var.info[[i]]$Compartment.id == comp.id) {
        vars$var.info[[i]]$Compartment <- new
      }
    }
    
    # ___Eqn DS___
    print(eqns$eqn.info)
    if (nrow(eqns$eqn.info) != 0) {
      for (i in seq(nrow(eqns$eqn.info))) {
        row.id <- eqns$eqn.info[i,]$Compartment.ID
        if (row.id == comp.id) {
          eqns$eqn.info[i,]$Compartment <- new
        }
      }
    }
    
    
    # Change name in ID database
    idx.for.id <- which(id$id.df[, 2] %in% old)
    var.id <- id$id.df[idx.for.id, 1]
    id$id.df[idx.for.id, 2] <- new
    
  } else if (yi == 1) {
    vars$compartments.info[[var.name]]$Volume <- new
  } else if (yi == 2) {
    # Volume Value Changed
    vars$compartments.info[[var.name]]$Value <- new
    
    # Change base value of volume in compartment if needed
    selected.unit <- vars$compartments.info[[var.name]]$Unit
    base.unit     <- vars$compartments.info[[var.name]]$BaseUnit
    if (selected.unit != base.unit) {
      # Perform unit conversion
      descriptor <- vars$compartments.info[[var.name]]$UnitDescription
      converted.value <- UnitConversion(descriptor,
                                        selected.unit,
                                        base.unit,
                                        as.numeric(new))
      vars$compartments.info[[var.name]]$BaseValue <- converted.value
      
      # Change volume in parameters
      vol.name <- vars$compartments.info[[var.name]]$Volume
      params$params[[vol.name]]$Value <- new
      params$params[[vol.name]]$BaseValue <- converted.value
    } else {
      vars$compartments.info[[var.name]]$BaseValue <- new
      # Change volume in parameters
      vol.name <- vars$compartments.info[[var.name]]$Volume
      params$params[[vol.name]]$Value <- new
      params$params[[vol.name]]$BaseValue <- new
    }
    
    
  } else if (yi == 3) {
    #Volume Unit Changed
    vars$compartments.info[[var.name]]$Unit <- new
  } else if (yi == 4) {
    # Volume Description Changed
    vars$compartments.info[[var.name]]$Description <- new
  }
  
})

# Add Compartment Button -------------------------------------------------------
observeEvent(input$createVar_add_compartment_button, {
  # Add entry to compartment list - need to add Compartment and Volume Param
  
  # Find Base Naming Variables
  current.n <- length(vars$compartments.info) + 1
  base = "comp"
  name.to.add <- paste0(base, "_", current.n)
  comp.name <- paste0(base, "_", current.n)
  # Generate ID
  ids <- GenerateId(id$id.comp.seed, "compartment")
  unique.id <- ids[[2]]
  id$id.comp.seed <- ids[[1]]
  idx.to.add <- nrow(id$id.df) + 1
  id$id.df[idx.to.add, ] <- c(unique.id, paste0(base, "_", current.n))
  
  passed.error.check <- FALSE
  count = 0
  # Volume Parameter
  while(!passed.error.check) {
    vol.name <- paste0("V_", base, (current.n + count))
    passed.error.check <- CheckParametersForErrors(vol.name,
                                                   vars$species,
                                                   names(params$params))
    count = count + 1
  }
  
  # Add Volume to Parameters
  par.out <- BuildParameters(vol.name,
                             names(params$params),
                             id$id.param.seed,
                             pValue = 1,
                             pUnit = units$selected.units$Volume,
                             pUnitD = "volume",
                             pBaseUnit = units$base.units$Volume,
                             pBaseValue = 1,
                             pDescription = paste0("Volume of ", comp.name),
                             pLocation = "Compartment",
                             pLocationNote = "Volume")
  StoreParameters(par.out)
  
  # Create List Entry
  to.add <- list(Name = comp.name,
                 ID = unique.id,
                 Value = 1,
                 Volume = vol.name,
                 par.Id = FindId(vol.name),
                 Unit = units$selected.units$Volume,
                 UnitDescription = "vol",
                 BaseUnit = "l",
                 BaseValue = 1,
                 Description = "")
  
  # Add Entry To RV
  vars$compartments.info[[current.n]] <- to.add
  names(vars$compartments.info)[current.n] <- name.to.add
  
  
})

#Delete Compartment Button -----------------------------------------------------
observeEvent(input$createVar_remove_compartment_button, {
  vars$compartments.info[[length(vars$compartments.info)]] <- NULL
})


# Events that change on compartment info change  -------------------------------
observeEvent(vars$compartments.info, {
  compartment.names <- names(vars$compartments.info)
  
  # Active compartment for variable creation
  updatePickerInput(session,
                    "createVar_active_compartment",
                    choices = compartment.names)
  
  # Active compartment for equation creation
  updatePickerInput(session,
                    "eqnCreate_active_compartment",
                    choices = compartment.names)
  
  #____ Flow Events ____ 
  c.names <- names(vars$compartments.info)
  
  # Flow In
  updatePickerInput(session, 
                    "CIO_flow_in_compartment", 
                    choices = c.names)
  
  # Flow Out
  updatePickerInput(session, 
                    "CIO_flow_out_compartment", 
                    choices = c.names)
  
  # Flow Between
  updatePickerInput(session, 
                    "CIO_flowbetween_compartment_out", 
                    choices = c.names)
  updatePickerInput(session, 
                    "CIO_flowbetween_compartment_in",
                    choices = c.names)
  
  # Clearance
  updatePickerInput(session, 
                    "CIO_clearance_compartment", 
                    choices = c.names)
  
  #Simple Diffusion
  updatePickerInput(session, 
                    "CIO_simpdiff_compartment1", 
                    choices = c.names)
  updatePickerInput(session, 
                    "CIO_simpdiff_compartment2", 
                    choices = c.names)
  
  # Facilitated Diffusion
  updatePickerInput(session, 
                    "CIO_facilitatedDiff_compartment1", 
                    choices = c.names)
  updatePickerInput(session, 
                    "CIO_facilitatedDiff_compartment2", 
                    choices = c.names)
})

# Converts compartment list to df for table filtering in other functions
observeEvent(vars$compartments.info, {
  vars$compartments.df <- bind_rows(vars$compartments.info)
  print(vars$compartments.df)
})
