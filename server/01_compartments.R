# This script holds server side processing of compartment generation, editing,
# and related UI events in the create model tab


# Table Render -----------------------------------------------------------------
output$createVar_compartment_table <- renderRHandsontable({
  req(nrow(rv.COMPARTMENTS$compartments.df) > 0)

  # This value changes to rerender table in instances that R messes it up
  rerun.test <- rv.REFRESH$refresh.compartment.table
  
  # Set up dataframe for table
  for.table <- rv.COMPARTMENTS$compartments.df %>%
    select("Name", "Volume", "Value", "Unit", "Description")
  
  colnames(for.table) <- c("Name", "Volume", "Value", "Unit", "Description")
  

  rhandsontable(for.table,
                rowHeaders = NULL,
                overflow = "visible",
                selectCallback = TRUE,
                colHeaderWidth = 100,
                stretchH = "all",
                fillHandle = FALSE
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
  for.table <- rv.COMPARTMENTS$compartments.df %>%
    select("Name", "Volume", "Value", "Unit", "Description")
  comp.name <- as.character(for.table[xi+1, 1])
  comp.id   <- FindId(comp.name)
  if (yi == 0) {
    
    comp.id <- FindId(old)
    
    # Compartment Name Changed
    rv.COMPARTMENTS$compartments[[comp.id]]$Name <- new
    
    #Search other areas affected by id
    # ___Var List____
    for (i in seq_along(rv.SPECIES$species)) {
      if (rv.SPECIES$species[[i]]$Compartment.id == comp.id) {
        rv.SPECIES$species[[i]]$Compartment <- new
      }
    }
    
    # ___Eqn DS___
    if (length(rv.REACTIONS$reactions) != 0) {
      for (i in seq(length(rv.REACTIONS$reactions))) {
        row.id <- rv.REACTIONS$reactions[[i]]$Compartment.Id
        if (row.id == comp.id) {
          rv.REACTIONS$reactions[[i]]$Compartment <- new
        }
      }
    }
    
    
    # Change name in ID database
    idx.for.id <- which(rv.ID$id.df[, 2] %in% old)
    var.id <- rv.ID$id.df[idx.for.id, 1]
    rv.ID$id.df[idx.for.id, 2] <- new
    
  } else if (yi == 1) {
    # It doesn't look like volume variables are stored in eqns or IO
    # database which is good.  Don't have to change them in those locations.
    param.id <- FindId(old)
    
    # Change name in parameter database
    rv.PARAMETERS$parameters[[param.id]]$Name <- new
    
    # Change name in ID database
    idx.for.id <- which(rv.ID$id.df[, 2] %in% old)
    var.id <- rv.ID$id.df[idx.for.id, 1]
    rv.ID$id.df[idx.for.id, 2] <- new
    
    rv.COMPARTMENTS$compartments[[comp.id]]$Volume <- new
    
  } else if (yi == 2) {
    # Volume Value Changed
    rv.COMPARTMENTS$compartments[[comp.id]]$Value <- new
    
    # Change base value of volume in compartment if needed
    selected.unit <- rv.COMPARTMENTS$compartments[[comp.id]]$Unit
    base.unit     <- rv.COMPARTMENTS$compartments[[comp.id]]$BaseUnit
    if (selected.unit != base.unit) {
      # Perform unit conversion
      descriptor <- rv.COMPARTMENTS$compartments[[comp.id]]$UnitDescription
      converted.value <- UnitConversion(descriptor,
                                        selected.unit,
                                        base.unit,
                                        as.numeric(new))
      rv.COMPARTMENTS$compartments[[comp.id]]$BaseValue <- converted.value
      
      # Change volume in parameters
      vol.name <- rv.COMPARTMENTS$compartments[[comp.id]]$Volume
      vol.id <- FindId(vol.name)
      
      rv.PARAMETERS$parameters[[vol.id]]$Value <- new
      rv.PARAMETERS$parameters[[vol.id]]$BaseValue <- converted.value
    } else {
      rv.COMPARTMENTS$compartments[[comp.id]]$BaseValue <- new
      # Change volume in parameters
      vol.name <- rv.COMPARTMENTS$compartments[[comp.id]]$Volume
      vol.id <- FindId(vol.name)
      rv.PARAMETERS$parameters[[vol.id]]$Value <- new
      rv.PARAMETERS$parameters[[vol.id]]$BaseValue <- new
    }
    
    
  } else if (yi == 3) {
    #Volume Unit Changed
    
    # check if units are acceptable
    descriptor <- rv.COMPARTMENTS$compartments[[comp.id]]$UnitDescription
    
    # Check to make sure units entered are the right ones
    comparison <- UnitCompare(descriptor,
                              new,
                              rv.UNITS$units.choices)
    
    if (comparison$is.match) {
      # Change units in compartment data structure
      rv.COMPARTMENTS$compartments[[comp.id]]$Unit <- new
      
      # Perform unit conversion if new units differ from base
      from.unit <- rv.COMPARTMENTS$compartments[[comp.id]]$Unit
      to.unit   <- rv.COMPARTMENTS$compartments[[comp.id]]$BaseUnit
      from.val  <- as.numeric(rv.COMPARTMENTS$compartments[[comp.id]]$Value)
      
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
      vol.name <- rv.COMPARTMENTS$compartments[[comp.id]]$Volume
      vol.id <- FindId(vol.name) 
      rv.PARAMETERS$parameters[[vol.id]]$Unit <- new
      rv.PARAMETERS$parameters[[vol.id]]$BaseValue <- 
        vars$compartment.info[[comp.id]]$BaseValue
      
    } else {
      # if comparison is not a new real unit
      rv.REFRESH$refresh.compartment.table <- rv.REFRESH$refresh.compartment.table + 1
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
    rv.COMPARTMENTS$compartments[[comp.id]]$Description <- new
  }
  
})

# Add Compartment Button -------------------------------------------------------
observeEvent(input$createVar_add_compartment_button, {
  # Add entry to compartment list - need to add Compartment and Volume Param
  
  # Find Base Naming Variables
  current.n <- length(rv.COMPARTMENTS$compartments) + 1
  base = "comp"
  name.to.add <- paste0(base, "_", current.n)
  comp.name <- paste0(base, "_", current.n)
  # Generate ID
  ids <- GenerateId(rv.ID$id.comp.seed, "compartment")
  unique.id <- ids[[2]]
  rv.ID$id.comp.seed <- ids[[1]]
  idx.to.add <- nrow(rv.ID$id.df) + 1
  rv.ID$id.df[idx.to.add, ] <- c(unique.id, paste0(base, "_", current.n))
  
  passed.error.check <- FALSE
  count = 0
  # Volume Parameter
  while(!passed.error.check) {
    vol.name <- paste0("V_", base, (current.n + count))
    error.check <- CheckParametersForErrors(vol.name,
                                            rv.SPECIES$species.names,
                                            names(rv.PARAMETERS$parameters))
    passed.error.check <- error.check[[1]]
    count = count + 1
  }
  
  # Add Volume to Parameters
  par.out <- BuildParameters(vol.name,
                             names(rv.PARAMETERS$parameters),
                             rv.ID$id.param.seed,
                             pValue = 1,
                             pUnit = rv.UNITS$units.selected$Volume,
                             pUnitD = "volume",
                             pBaseUnit = rv.UNITS$units.base$Volume,
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
                 Unit = rv.UNITS$units.selected$Volume,
                 UnitDescription = "volume",
                 BaseUnit = "l",
                 BaseValue = 1,
                 Description = "")
  
  # Add Entry To RV
  rv.COMPARTMENTS$compartments[[current.n]] <- to.add
  names(rv.COMPARTMENTS$compartments)[current.n] <- unique.id
  
  
})

# Delete Compartment Button ----------------------------------------------------
observeEvent(input$createVar_remove_compartment_button, {
  if (length(rv.COMPARTMENTS$compartments) > 1) {
    
    # Remove Compartment in list
    comp.id <- rv.COMPARTMENTS$compartments[[length(rv.COMPARTMENTS$compartments)]]$ID 
    
    # Remove volume parameter
    par.to.del.id <- rv.COMPARTMENTS$compartments[[comp.id]]$par.Id
    rv.PARAMETERS$parameters[[par.to.del.id]] <- NULL
    rv.COMPARTMENTS$compartments[[comp.id]] <- NULL
    
    # Remove Parameter and Compartment from Ids
    to.remove <- which(rv.ID$id.df[,1] %in% par.to.del.id)
    rv.ID$id.df <- rv.ID$id.df[-to.remove,]
    
    to.remove <- which(rv.ID$id.df[,1] %in% comp.id)
    rv.ID$id.df <- rv.ID$id.df[-to.remove,]
  }
})


# Events that change on compartment info change  -------------------------------
observeEvent(rv.COMPARTMENTS$compartments, {
  
  # Unhide MultiCompartmentUI
  if (length(rv.COMPARTMENTS$compartments) > 1) {
    shinyjs::showElement(id = "species_hide_in_single_compartment")
  }
  
  
  rv.COMPARTMENTS$compartments.df <- bind_rows(rv.COMPARTMENTS$compartments)
  if (nrow(rv.COMPARTMENTS$compartments.df) > 0) {
    comp.names <- rv.COMPARTMENTS$compartments.df %>% dplyr::select(Name)
    rv.COMPARTMENTS$compartments.names <- as.vector(unlist(comp.names))
  } else {
    rv.COMPARTMENTS$compartments.names <- vector()
  }
  
  # Turn compartment button on/off
  if (length(rv.COMPARTMENTS$compartments) > 1) {
    shinyjs::enable("createVar_remove_compartment_button")
  } else {
    shinyjs::disable("createVar_remove_compartment_button")
  }
  
  c.names <- rv.COMPARTMENTS$compartments.names
  
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
# observeEvent(rv.COMPARTMENTS$compartments, {
#   
# })
