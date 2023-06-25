

writeSBML <- function(model, filename) {
  # Takes model object of class SBML and converts it to filename.xml
  
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
  cat("<sbml xmlns=\"http://www.sbml.org/sbml/level2\" level=\"2\" 
      version=\"1\">", 
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
    "<sbml xmlns=\"http://www.sbml.org/sbml/level2\" level=\"2\" version=\"1\">")
  out <- c(out, paste0("<model id=", '"', "NAMETOADD", '"', ">"))
  
  tryCatch(expr = {
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
    
    if (n.species > 0) {
      out <- c(out, "<listOfSpecies>")
      for (i in seq_along(species)) {
        entry      <- species[[i]]
        
        id         <- entry$id
        name       <- entry$name
        init.conc  <- entry$initialConcentration
        sub.units  <- entry$size
        compart    <- entry$compartment
        cont       <- entry$constant
        bc         <- entry$spatialDimensions
        
        out <- c(out,
                 paste0("<species id=", '"', id, '" ',
                        "name=", '"', name, '" ',
                        "initialConcentration=", '"', init.conc, '" ',
                        "substanceUnits=", '"', sub.units, '" ',
                        "compartment=", '"', compart, '" ',
                        "constant=", '"', cont, '" ',
                        "boundaryCondition=", '"', bc, '"', "/>")
        )
      }
    }
    out <- c(out, "</listOfSpecies>")

  })
  
  out <- c(out, "</model>")
  out <- c(out, "</sbml>")

  out <- paste0(out, collapse = "\n")
  print(out)
  return(out)
}