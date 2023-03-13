# This script holds server side processing of compartment generation, editing,
# and related UI events in the create model tab


# Table Render -----------------------------------------------------------------
output$createVar_compartment_table <- renderRHandsontable({
  req(nrow(vars$compartments.df) > 0)

  # This value changes to rerender table in instances that R messes it up
  rerun.test <- TableOverrides$compartment.table
  
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
  comp.name <- as.character(for.table[xi+1, 1])
  comp.id   <- FindId(comp.name)
  if (yi == 0) {
    
    comp.id <- FindId(old)
    
    # Compartment Name Changed
    vars$compartments.info[[comp.id]]$Name <- new
    
    #Search other areas affected by id
    # ___Var List____
    for (i in seq_along(vars$var.info)) {
      if (vars$var.info[[i]]$Compartment.id == comp.id) {
        vars$var.info[[i]]$Compartment <- new
      }
    }
    
    # ___Eqn DS___
    if (length(eqns$eqn.info) != 0) {
      for (i in seq(length(eqns$eqn.info))) {
        row.id <- eqns$eqn.info[[i]]$Compartment.Id
        if (row.id == comp.id) {
          eqns$eqn.info[[i]]$Compartment <- new
        }
      }
    }
    
    
    # Change name in ID database
    idx.for.id <- which(id$id.df[, 2] %in% old)
    var.id <- id$id.df[idx.for.id, 1]
    id$id.df[idx.for.id, 2] <- new
    
  } else if (yi == 1) {
    # It doesn't look like volume variables are stored in eqns or IO
    # database which is good.  Don't have to change them in those locations.
    param.id <- FindId(old)
    
    # Change name in parameter database
    params$par.info[[param.id]]$Name <- new
    
    # Change name in ID database
    idx.for.id <- which(id$id.df[, 2] %in% old)
    var.id <- id$id.df[idx.for.id, 1]
    id$id.df[idx.for.id, 2] <- new
    
    vars$compartments.info[[comp.id]]$Volume <- new
    
  } else if (yi == 2) {
    # Volume Value Changed
    vars$compartments.info[[comp.id]]$Value <- new
    
    # Change base value of volume in compartment if needed
    selected.unit <- vars$compartments.info[[comp.id]]$Unit
    base.unit     <- vars$compartments.info[[comp.id]]$BaseUnit
    if (selected.unit != base.unit) {
      # Perform unit conversion
      descriptor <- vars$compartments.info[[comp.id]]$UnitDescription
      converted.value <- UnitConversion(descriptor,
                                        selected.unit,
                                        base.unit,
                                        as.numeric(new))
      vars$compartments.info[[comp.id]]$BaseValue <- converted.value
      
      # Change volume in parameters
      vol.name <- vars$compartments.info[[comp.id]]$Volume
      vol.id <- FindId(vol.name)
      
      params$par.info[[vol.id]]$Value <- new
      params$par.info[[vol.id]]$BaseValue <- converted.value
    } else {
      vars$compartments.info[[comp.id]]$BaseValue <- new
      # Change volume in parameters
      vol.name <- vars$compartments.info[[comp.id]]$Volume
      vol.id <- FindId(vol.name)
      params$par.info[[vol.id]]$Value <- new
      params$par.info[[vol.id]]$BaseValue <- new
    }
    
    
  } else if (yi == 3) {
    #Volume Unit Changed
    
    # check if units are acceptable
    descriptor <- vars$compartments.info[[comp.id]]$UnitDescription
    
    # Check to make sure units entered are the right ones
    comparison <- UnitCompare(descriptor,
                              new,
                              units$possible.units)
    
    if (comparison$is.match) {
      # Change units in compartment data structure
      vars$compartments.info[[comp.id]]$Unit <- new
      
      # Perform unit conversion if new units differ from base
      from.unit <- vars$compartments.info[[comp.id]]$Unit
      to.unit   <- vars$compartments.info[[comp.id]]$BaseUnit
      from.val  <- as.numeric(vars$compartments.info[[comp.id]]$Value)
      
      if (from.unit != to.unit) {
        new.value <- UnitConversion(descriptor,
                                    from.unit,
                                    to.unit, 
                                    from.val)
        
        vars$compartment.info[[comp.id]]$BaseValue <- new.value
      } else {
        vars$compartment.info[[comp.id]]$BaseValue <- from.val
      }
      
      # Change value in parameter table
      # Find Parameter Id
      vol.name <- vars$compartments.info[[comp.id]]$Volume
      vol.id <- FindId(vol.name) 
      params$par.info[[vol.id]]$Unit <- new
      params$par.info[[vol.id]]$BaseValue <- 
        vars$compartment.info[[comp.id]]$BaseValue
      
    } else {
      # if comparison is not a new real unit
      TableOverrides$compartment.table <- TableOverrides$compartment.table + 1
      vars$compartment.info[[comp.id]]$Unit <- old
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = comparison$message,
        type = "error"
      )
      print(comparison$message)
    }
    
  } else if (yi == 4) {
    # Volume Description Changed
    vars$compartments.info[[comp.id]]$Description <- new
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
    error.check <- CheckParametersForErrors(vol.name,
                                            vars$var.names,
                                            names(params$par.info))
    passed.error.check <- error.check[[1]]
    count = count + 1
  }
  
  # Add Volume to Parameters
  par.out <- BuildParameters(vol.name,
                             names(params$par.info),
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
                 UnitDescription = "volume",
                 BaseUnit = "l",
                 BaseValue = 1,
                 Description = "")
  
  # Add Entry To RV
  vars$compartments.info[[current.n]] <- to.add
  names(vars$compartments.info)[current.n] <- unique.id
  
  
})

#Delete Compartment Button -----------------------------------------------------
observeEvent(input$createVar_remove_compartment_button, {
  if (length(vars$compartments.info) > 1) {
    
    # browser()
    # Remove Compartment in list
    comp.id <- vars$compartments.info[[length(vars$compartments.info)]]$ID 
    
    # Remove volume parameter
    par.to.del.id <- vars$compartments.info[[comp.id]]$par.Id
    params$par.info[[par.to.del.id]] <- NULL
    vars$compartments.info[[comp.id]] <- NULL
    
    # Remove Parameter and Compartment from Ids
    to.remove <- which(id$id.df[,1] %in% par.to.del.id)
    id$id.df <- id$id.df[-to.remove,]
    
    to.remove <- which(id$id.df[,1] %in% comp.id)
    id$id.df <- id$id.df[-to.remove,]
  }
})


# Events that change on compartment info change  -------------------------------
observeEvent(vars$compartments.info, {
  
  # Unhide MultiCompartmentUI
  if (length(vars$compartments.info) > 1) {
    shinyjs::showElement(id = "species_hide_in_single_compartment")
  }
  
  
  vars$compartments.df <- bind_rows(vars$compartments.info)
  if (nrow(vars$compartments.df) > 0) {
    comp.names <- vars$compartments.df %>% dplyr::select(Name)
    vars$compartments.names <- as.vector(unlist(comp.names))
  } else {
    vars$compartments.names <- vector()
  }
  
  # Turn compartment button on/off
  if (length(vars$compartments.info) > 1) {
    shinyjs::enable("createVar_remove_compartment_button")
  } else {
    shinyjs::disable("createVar_remove_compartment_button")
  }
  
  c.names <- vars$compartments.names
  
  # Active compartment for variable creation
  updatePickerInput(session,
                    "createVar_active_compartment",
                    choices = c.names)
  
  # Active compartment for equation creation
  updatePickerInput(session,
                    "eqnCreate_active_compartment",
                    choices = c.names)
  
  #____ Flow Events ____ 
  
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
  
  # Want to remove compartment out from choices of compartment in 1
  c.1.choices <- c.names[! c.names %in% input$CIO_flowbetween_compartment_out]
  updatePickerInput(session, 
                    "CIO_flowbetween_compartment_in_1",
                    choices = c.1.choices)
  
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
# observeEvent(vars$compartments.info, {
#   
# })
