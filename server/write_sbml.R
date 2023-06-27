

writeSBML <- function(model, filename) {
  # Takes model object of class SBML and converts it to filename.xml
  # for now we will keep copying the out vector, this is inefficient
  # in future: preallocate large vector and add and increment.
  
  # Open file connection
  f.id <- file(filename, "w")
  
  # Grab Components of Model
  # sbml=model[["sbml"]]
  # id=model[["id"]]
  # notes=model[["notes"]]
  # htmlNotes=model[["htmlNotes"]]
  compartments <- model[["compartments"]]
  species      <- model[["species"]]
  parameters   <- model[["parameters"]]
  rules        <- model[["rules"]]
  reactions    <- model[["reactions"]]
  functions    <- model[["functions"]]
  # units        <- model[["units"]]
  
  # Find lengths
  n.compartments <- length(compartments)
  n.species      <- length(species)
  n.parameters   <- length(parameters)
  n.rules        <- length(rules)
  n.reactions    <- length(reactions)
  n.functions    <- length(functions)
  
  
  # Build SBML Beginning Text --------------------------------------
  cat("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", 
      file=f.id, sep="\n")
  cat("<sbml xmlns=\"http://www.sbml.org/sbml/level2/version5\" level=\"2\" 
      version=\"5\">", 
      file=f.id, sep="\n")
  cat(sprintf("<model id=\"%s\">", "TESTNAME"), file=f.id, sep="\n")
  
  tryCatch(expr = {
    if (n.compartments>0) {
      cat("<listOfCompartments>", file=f.id, sep="\n")
      for (i in seq_along(compartments)) {
        entry <- compartments[[i]]
        id    <- entry$id
        name  <- entry$name
        size  <- entry$size
        cont  <- ifelse(entry$constant, "true", "false")
        s.dim <- entry$spatialDimensions
        
        cat(
          sprintf(
            "   <compartment id=\'%s\'  size=\'%g\'  name=\'%s\'  constant=\'%s\'  spatialDimensions\'%g\'  />",
            id,
            size,
            name,
            cont,
            s.dim), 
          file=f.id, 
          sep="\n")
      }
      
      cat("</listOfCompartments>", file=f.id, sep="\n")
    }
    cat("</model>", file=f.id, sep="\n")
    cat("</sbml>", file=f.id, sep="\n")
  })
  # Store Compartments
  
  close(f.id)
}

createSBML <- function(model) {
  # Takes model object of class SBML and converts it to filename.xml
  
  # Open file connection
  # f.id <- file(filename, "w")
  
  # Grab Components of Model
  # sbml=model[["sbml"]]
  # id=model[["id"]]
  # notes=model[["notes"]]
  # htmlNotes=model[["htmlNotes"]]
  compartments <- model[["compartments"]]
  species      <- model[["species"]]
  parameters   <- model[["parameters"]]
  rules        <- model[["rules"]]
  reactions    <- model[["reactions"]]
  functions    <- model[["functions"]]
  # units        <- model[["units"]]
  
  # Find lengths
  n.compartments <- length(compartments)
  n.species      <- length(species)
  n.parameters   <- length(parameters)
  n.rules        <- length(rules)
  n.reactions    <- length(reactions)
  n.functions    <- length(functions)
  
  out <- c()
  # Build SBML Beginning Text --------------------------------------
  out <- c(out, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
  out <- 
    c(out, 
    "<sbml xmlns=\"http://www.sbml.org/sbml/level2\" level=\"2\" version=\"5\">")
  out <- c(out, paste0("<model id=", '"', "NAMETOADD", '"', ">"))
  
  tryCatch(expr = {
    # Write Functions ----------------------------------------------------------
    if (n.functions > 0) {
      out <- c(out, "<listOfFunctionDefinitions>")
      for (i in seq_along(functions)) {
        entry <- functions[[i]]
        
        id   <- entry$id
        name <- entry$name
        
        out <- c(out,
                 paste0("<functionDefinition id", '"', id, '" ',
                        "name=", '"', name, '"', 
                        ">"))
        
        # Build mathml expression
        vars <- strsplit(entry$variables, ", ")[[1]]
        out <- c(out, "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">")
        out <- c(out, "<lambda>")
        # Add function variables to mathml lambda expression
        for (j in seq_along(vars)) {
          out <- c(out, 
                   paste0("<bvar>",
                          "<ci> ",
                          vars[j], 
                          " </ci></bar>"))
        }
        # Add mathml term
        
        out <- c(out, "</lambda>")
        out <- c(out, "</math>")
      }
    }
    
    # Write Compartments -------------------------------------------------------
    if (n.compartments > 0) {
      out <- c(out, "<listOfCompartments>")
      for (i in seq_along(compartments)) {
        entry <- compartments[[i]]
        id    <- entry$id
        name  <- entry$name
        size  <- entry$size
        cont  <- entry$constant
        s.dim <- entry$spatialDimensions
        
        out <- c(out,
                 paste0("<compartment id=", '"', id, '" ',
                        "size=", '"', size, '" ',
                        "name=", '"', name, '" ',
                        "constant=", '"', cont, '" ',
                        "spatialDimensions=", '"', s.dim, '"', "/>")
                 )
      }

      out <- c(out, "</listOfCompartments>")
    }
    
    # Write Species ------------------------------------------------------------
    if (n.species > 0) {
      out <- c(out, "<listOfSpecies>")
      for (i in seq_along(species)) {
        entry      <- species[[i]]
        
        id         <- entry$id
        name       <- entry$name
        init.conc  <- entry$initialConcentration
        sub.units  <- entry$substanceUnits
        compart    <- entry$compartment
        cont       <- entry$constant
        bc         <- entry$boundaryCondition
        
        out <- c(out,
                 paste0("<species id=", '"', id, '" ',
                        "name=", '"', name, '" ',
                        "initialConcentration=", '"', init.conc, '" ',
                        #"substanceUnits=", '"', sub.units, '" ',
                        "compartment=", '"', compart, '" ',
                        "constant=", '"', cont, '" ',
                        "boundaryCondition=", '"', bc, '"', 
                        "/>")
        )
      }
      out <- c(out, "</listOfSpecies>")
    }
    
    # Write Parameters ---------------------------------------------------------
    if (n.parameters > 0) {
      out <- c(out, "<listOfParameters>")
      for (i in seq_along(parameters)) {
        entry      <- parameters[[i]]
        
        id         <- entry$id
        name       <- entry$name
        value      <- entry$value
        cont       <- entry$constant
        
        out <- c(out,
                 paste0("<parameter id=", '"', id, '" ',
                        "name=", '"', name, '" ',
                        "value=", '"', value, '" ',
                        "constant=", '"', cont, '" ',
                        "/>")
        )
      }
      out <- c(out, "</listOfParameters>")
    }
    
    # Write Reactions ----------------------------------------------------------
    if (n.reactions > 0) {
      out <- c(out, "<listOfReactions>")
      for (i in seq_along(reactions)) {
        entry <- reactions[[i]]
        
        # Create initial meta-tag (id, name, reversible, fast)
        id         <- entry$id
        name       <- entry$name
        reversible <- entry$reversible
        fast       <- entry$fast
        
        out <- c(out,
                 paste0("<reaction id=", '"', id, '" ',
                        "name=", '"', name, '" ',
                        "reversible=", '"', reversible, '" ',
                        "fast=", '"', fast, '" ',
                        ">")
        )
        
        # Build <listOfSpecies>
        if (!is.na(entry$reactants)) {
          out <- c(out, "<listOfReactants>")
          reactants <- strsplit(entry$reactants, ", ")[[1]]
          for (j in seq_along(reactants)) {
            r <- reactants[j]
            s <- 1
            out <- c(out, 
                     paste0("<speciesReference species=", '"', r, '" ',
                            "stoichiometry=", '"', s, '"',
                            "/>"))
          }
          
          out <- c(out, "</listOfReactants>")
        }
        
        # Build <listOfProducts>
        if (!is.na(entry$products)) {
          out <- c(out, "<listOfProducts>")
          products <- strsplit(entry$products, ", ")[[1]]
          for (j in seq_along(products)) {
            p <- products[j]
            s <- 1
            out <- c(out, 
                     paste0("<speciesReference species=", '"', p, '" ',
                            "stoichiometry=", '"', s, '"',
                            "/>"))
          }
          
          out <- c(out, "</listOfProducts>")
        }
        
        # Build <listOfModifiers>
        if (!is.na(entry$modifiers)) {
          out <- c(out, "<listOfModifiers>")
          modifiers <- strsplit(entry$modifiers, ", ")[[1]]
          for (j in seq_along(modifiers)) {
            m <- modifiers[j]
            out <- c(out, 
                     paste0("<modifierSpeciesReference species=", '"', p, '"',
                            "/>"))
          }
          
          out <- c(out, "</listOfModifiers>")
        }
        
        # Build <kineticLaw>
        
        # End Reaction
        out <- c(out, "</reaction>")
      }
      out <- c(out, "</listOfReactions>")
    }
    
    
    


  })
  
  out <- c(out, "</model>")
  out <- c(out, "</sbml>")

  out <- paste0(out, collapse = "\n")
  print(out)
  return(out)
}