OutputSideOfEquation <- function(coefs, vars){
  # Computes string that is equivalent to one side of chemical equation
  #
  # Args:
  #   coefs: vector containing coefficients of species in eqn
  #   vars: vector containing the species in the eqn
  #   
  #   Ex: Eqn: A + 2B, coefs = [1,2], vars = [A,B]
  #
  # Returns:
  #   string version of the equation
  
  
  out <- ""
  #generate eqn when there is only one species
  if (length(coefs) == 1) {
    if (coefs != "1") { #only want to show coefs if they aren't one
      out <- paste0(out, coefs, "*", vars)
    }
    else{
      out <- paste0(out, vars)
    }
  }
  else{#add coefs together when there are multiple
    for (i in seq(length(coefs))) {
      if (i == length(coefs)) { #this is the last vars to add so no "+"
        if (coefs[i] != "1") {
          out <- paste0(out, coefs[i], "*", vars[i])
        }else{
          out <- paste0(out, vars[i])
        }
      }else{#these should all have a plus after them
        if (coefs[i] != "1") {
          out <- paste0(out, coefs[i], "*", vars[i], " + ")
        }else{
          out <- paste0(out[i], vars[i], " + ")
        }
      }
    }
  }
  return(out)
}

OutputArrowType <- function(eqnType, arrowType, kr, kf){
  # determines arrow output type for in latex form
  #
  # Args:
  #   eqnType: Type of equation such as enzyme_rxn or chem_rxn
  #   arrowType: "forward_only" or "both_directions" to generate the eqn arrow
  #   kr: rates that will appear on bottom of arrow in latex
  #   kf: rates that will appear on top of arrow in latex
  #
  # Returns:
  #   string containing latex version for the equation arrow
  if (eqnType == "enzyme_rxn") {
    if (arrowType == "forward_only") {
      out <- paste0("\\xrightleftharpoons", "[", kr, "]", "{", kf, "}")
    }else if (arrowType == "both_directions") {
      out <- paste0("\\xrightleftharpoons", "[", kr, "]", "{", kf, "}")
    }
  }
  else{
    if (arrowType == "forward_only") {
      out <- paste0("\\xrightarrow{", kf, "}")
    }else if (arrowType == "both_directions") {
      out <- paste0("\\xrightleftharpoons", "[", kr, "]", "{", kf, "}")
    }
  }

}


EqnsToLatex <- function(eqnInfo){
  # Writes all eqns out to latex format from the eqn database
  # Args:
  #   eqnInfo: dataframe containing all the eqn information
  #
  # Returns:
  #   string for all latex eqns to be combined with other generated sheets
  #
  # Currently programmed for chemical and enzyme equations.

  out <- "\\section{Equations}\n"
  #find a way to parse equations properly from equations df
  for (row in 1:nrow(eqnInfo)) {
    #unpack df row to get relevant information
    eqn.type = eqnInfo[row, 1]
    LHS.coef <- str_split(eqnInfo[row, 2], " ")[[1]]
    LHS.var <-  str_split(eqnInfo[row, 3], " ")[[1]]
    RHS.coef <- str_split(eqnInfo[row, 4], " ")[[1]]
    RHS.var <-  str_split(eqnInfo[row, 5], " ")[[1]]
    arrow.type <- eqnInfo[row, 6]
    kf <- eqnInfo[row, 7]
    kr <- eqnInfo[row, 8]
    kcat <- eqnInfo[row, 9]
    Vmax <- eqnInfo[row, 10]
    Km <- eqnInfo[row, 11]
    enzyme <- eqnInfo[row, 12]
    FR.bool <- eqnInfo[row, 13]
    forward.regulators <- eqnInfo[row, 14]
    forward.regulators.rate.constants <- eqnInfo[row,15]
    RR.bool <- eqnInfo[row, 16]
    reverse.regulators <- eqnInfo[row, 17]
    reverse.regulators.rate.constants <- eqnInfo[row,18]

    current.latex.eqn <- "\\begin{equation}\n"
    if (eqn.type == "chem_rxn") {
      LHS.of.eqn <- OutputSideOfEquation(LHS.coef, LHS.var)
      RHS.of.eqn <- OutputSideOfEquation(RHS.coef, RHS.var)
      arrow <- OutputArrowType(eqn.type, arrow.type, kr, kf)
      current.latex.eqn <- paste(current.latex.eqn, LHS.of.eqn, arrow, RHS.of.eqn, "\n")
    }
    else if (eqn.type == "enzyme_rxn") {
      if (!is.na(kcat)) {
        #when using kcat, enzyme has to be added to the equation
        enz.LHS.coef <- c(LHS.coef, "1")
        enz.LHS.var <- c(LHS.var, enzyme)
        LHS.of.eqn <- OutputSideOfEquation(enz.LHS.coef, enz.LHS.var)
        RHS.of.eqn <- OutputSideOfEquation(RHS.coef, RHS.var)
        arrow <- OutputArrowType(eqn.type, arrow.type, kcat, Km)
        current.latex.eqn <- paste(current.latex.eqn, LHS.of.eqn, arrow, RHS.of.eqn, "\n")
      }
      else{#enzyme equations used vmax instead of kcat, no enzyme in eqn
        LHS.of.eqn <- OutputSideOfEquation(LHS.coef, LHS.var)
        RHS.of.eqn <- OutputSideOfEquation(RHS.coef, RHS.var)
        arrow <- OutputArrowType(eqn.type, arrow.type, Vmax, Km)
        current.latex.eqn <- paste(current.latex.eqn, LHS.of.eqn, arrow, RHS.of.eqn, "\n")
      }
    }
    current.latex.eqn <- paste0(current.latex.eqn, "\\end{equation}\n")
    out <- paste0(out, current.latex.eqn)
  }
  out <- paste0(out, "\\newpage\n\n")
  return(out)
}

AdditionalEqnsToLatex <- function(additionalEqns){
  # Writes all additional eqns out to latex format from the appropriate vector
  # Args:
  #   additionalEqns: vector of additional eqn strings
  #
  # Returns:
  #   string of all equations parsed into latex format to print

  out <- "\\section{Additional Equations}\n"
  #find a way to parse equations properly from equations df
  for (eqn in additionalEqns) {
    current.latex.eqn <- "\\begin{equation}\n"
    current.latex.eqn <- paste0(current.latex.eqn, eqn)
    current.latex.eqn <- paste0(current.latex.eqn, "\\end{equation}\n")
    out <- paste0(out, current.latex.eqn)
    }
    out <- paste0(out, "\\newpage\n\n")

  return(out)
}

subsetInputOutput <- function(df){
  index_of_rows_with_var <- vector()
  for(row in 1:nrow(myModel)){#search rows of data for the choosen variable and subset them to new df
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

convertVarForLatex <- function(var, inMathModeBool){
  # Converts 
  # Args:
  #   var: variable to change to latex format converting subscripts properly
  #   inMathModeBool: boolean. If true, var takes math mode in latex otherwise
  #                   it uses \textsubscript
  # Returns:
  #   var in latex readable form
  #
  # Ex: var = my_var -> var = my_{var} or my\\textsubscript{var}
  
  # count all "_" in variable
  #if only one then take the start of it to the end of the work and enclose 
  #   in {}
  split.var = strsplit(var, "")[[1]]
  has.underscore = FALSE
  latex.var = ""
  
  if(inMathModeBool){
    latex.var = paste0(latex.var, "$")
    for (i in seq(length(split.var))) {
      if (split.var[i] == "_" && !has.underscore) {
        has.underscore = TRUE
        latex.var = paste0(latex.var, split.var[i], "{")
      }else{
        latex.var = paste0(latex.var, split.var[i])
      }
    }
    if (has.underscore) {
      latex.var = paste0(latex.var, "}")
    }
  }else{
    for (i in seq(length(split.var))) {
      if (split.var[i] == "_" && !has.underscore) {
        has.underscore = TRUE
        latex.var = paste0(latex.var, "\\textsubscript{")
      }else{
        latex.var = paste0(latex.var, split.var[i])
      }
    }
    if (has.underscore) {
      latex.var = paste0(latex.var, "}")
    }
  }
  return(latex.var)
}

InputOutputToLatex <- function(inputOutputDf){
  # Writes all additional eqns out to latex format from the appropriate vector
  # Args:
  #   inputOutputDf: df containing all input/output information
  #
  # Returns:
  #   string of latex form of the inputs and outputs for this equation
  
  out <- "\\section{Inputs \\& Outputs}\n"
  #find all unique species in inputOutput df
  unique.species <- unique(inputOutputDf[ ,3])
  print(unique.species)
  #for loop for each unique species.  Subset IO df for unique species
  for (var in unique.species) {
    index.unique.var <- vector()
    unique.var.found <- FALSE
    latex.line <- paste0("\\noindent \\underline{", var, "} \\\\\n")
    
    for (row in 1:nrow(inputOutputDf)) {
      if(var == inputOutputDf[row, 3]){
        index.unique.var <- c(index.unique.var, row)
        unique.var.found <- TRUE
      }
    }
    if(!unique.var.found){
      latex.line <- paste0(latex.line, "\\tab ", "None \\\\\n\n")
    }else{
      subset.df <- inputOutputDf[index.unique.var, ]
      
      for (new.row in 1:nrow(subset.df)) {
        in.or.out <- subset.df[new.row, 1]
        type <- subset.df[new.row,2]
        rate.constant <- convertVarForLatex(subset.df[new.row,4], FALSE)
        rate.by.species <- subset.df[new.row,5]
        vmax <- ifelse(is.na(subset.df[new.row,6]),
                       subset.df[new.row, 6],
                       convertVarForLatex(subset.df[new.row,6], FALSE))
        kcat <- ifelse(is.na(subset.df[new.row,7]),
                       subset.df[new.row, 7],
                       convertVarForLatex(subset.df[new.row,7], FALSE))
        enzyme <- ifelse(is.na(subset.df[new.row,8]),
                         subset.df[new.row, 8],
                         convertVarForLatex(subset.df[new.row,8], FALSE))
        
        latex.line <- paste0(latex.line, "\\tab ", in.or.out, ": ")
        if (type == "Rate") {
          if(rate.by.species == "FALSE"){
           ifelse(in.or.out == "input",
                  latex.line <-  paste0(latex.line, "Self Synethesis, "),
                  latex.line <-  paste0(latex.line, "Self Degradation, "))
          }
          latex.line <- paste0(latex.line, rate.constant, "\\\\\n")
        }
        else if (type == "Enzyme_Degradation"){
          ifelse(in.or.out == "input",
                 latex.line <- paste0(latex.line, "Enzyme Synthesis, "),
                 latex.line <- paste0(latex.line, "Enzyme Degradation, "))
          if (!is.na(vmax)) {
            latex.line <- paste0(latex.line, "V\\textsubscript{max} = "
                                 , vmax, ", ")
            latex.line <- paste0(latex.line, "K\\textsubscript{m} = ", 
                                 rate.constant, "\\\\\n")
          }else{
            latex.line <- paste0(latex.line, "enzyme = ", enzyme, ", ")
            latex.line <- paste0(latex.line, "k\\textsubscript{cat} = ", 
                                 kcat, ", ")
            latex.line <- paste0(latex.line, "K\\textsubscript{m} = ", 
                                 rate.constant, "\\\\\n")
          }
        }
      }
    }
    out <- paste0(out, latex.line)
  }
  print(out)
  return(out)
  #run latex loop for each row in subsetted df to create latex strings
  #for(row in 1:nrow())
}


GenerateLatexDocument <- function(eqnsLatex, IoLatex, addEqnsLatex){
  # combines eqns, I/O, and additional equations into final latex document
  #
  # Args:
  #   eqnsLatex: string containing all latex eqns
  #   IoLatex: string containing all input/output information in latex form
  #   addEqnsLatex: string containing all additioanl eqns in latex form
  #
  # Returns:
  #   final string eqns for final latex document

  out <- paste0("\\documentclass[12pt]{article}\n",
                "\\usepackage[margin=1in]{geometry}\n",
                "\\usepackage{chemarr}\n",
                "\\newcommand\\tab[1][1cm]{\\hspace*{#1}}\n",
                "\\begin{document}\n",
                "\\tableofcontents\n",
                "\\newpage\n\n")

  out <- paste0(out, eqnsLatex, addEqnsLatex, IoLatex)

  out <- paste0(out, "\\end{document}")

  return(out)
}







