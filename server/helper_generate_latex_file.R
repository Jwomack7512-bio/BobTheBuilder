
#TODO: Determine if parameter needs $param$ or not based on if itll be in math mode of not
VarToLatexForm <- function(variable, mathMode = TRUE, noDollarSign = TRUE) {
  # Takes input variable and changes it to a pretty latex form for latex reader
  # Inputs:
  #   @variable - string variable to be changed to nice formating
  
  # Output: Variable in latex form
  # Example: "My_var" --> "My_{var}
  
  split.var <- str_split(variable, "")[[1]]
  length.of.var <- length(split.var)
  has.underscore = FALSE
  count = 0
  for (letter in split.var) {
    count = count + 1
    if (letter == "_" & count != length.of.var & count != 1) { #prevent splitting if first/last letter is _
      idx <- count
      has.underscore = TRUE
      break
    }
  }
  if (has.underscore) {
    if (mathMode) {
      if (noDollarSign) {
        before <- paste0(split.var[1:idx], collapse = "")
        after <- paste0(split.var[(idx + 1):length.of.var], collapse = "")
        new.var <- paste0(before, "{", after, "}")
      } else{
        before <- paste0(split.var[1:idx], collapse = "")
        before <- paste0("$", before)
        after <- paste0(split.var[(idx + 1):length.of.var], collapse = "")
        new.var <- paste0(before, "{", after, "}$")
      }
    } else {
      #if underscores are to be used in a text phrase
      before <- paste0(split.var[1:(idx - 1)], collapse = "")
      after <- paste0(split.var[(idx + 1):length.of.var], collapse = "")
      new.var <- paste0(before, "\\textsubscript{", after, "}")
    }
  }
  else {
    new.var <- variable
  }
  out <- new.var
  return(out)
}

VarToLatexForComment <- function(string) {
  # Takes sentence and adds appropriate underscores to it
  # Input 
  #   @string - string phrase to be wrapped
  # Output
  #   -string phase with appropriate underscores
  words.in.string <- str_split(string, " ")[[1]]
  num.words <- length(words.in.string)
  new.string <- c()
  for (word in words.in.string) {
    new.word <- VarToLatexForm(word, mathMode = FALSE)
    new.string <- c(new.string, new.word)
  }
  new.string <- paste0(new.string, collapse = " ")
  return(new.string)
}

WrapInText <- function(string){
  #wraps string in \text{}; useful for words in equations otherwise they are too spaced
  #note using text would need mathMode = FALSE
  # Input 
  #   @string - string phrase to be wrapped
  # Output
  #   -string of string phase wrapped in latex text
  out <- paste0("\\text{", string, "}")
  return(out)
}
  

GenerateParameterTable <- function(parameters, values, descriptions) {
  #outputs a table for parameters in latex form for the user
  #inputs:
  # @parameters - vector of parameter names
  # @values - vector of parameter values corresponding to their names
  # @descriptions - vector of parameter descriptions corresponding to their names
  #Outputs:
  # @out - string containing latex table for parameter information
  num.parameters <- length(parameters)
  
  out <- "\n \\section*{\\underline{Parameters}}\n"
  out <- paste0(out, "\\begin{table}[H] \n \\begin{tabular}{111} \n")
  out <- paste0(out, "Parameter & Value & \\multicolumn{1}{c}{Description} \\\\ \\hline \n")
  for (i in seq(num.parameters)) {
    if (i != num.parameters) {
      line.to.add <-
        paste0(VarToLatexForm(parameters[i]),
               " & ",
               values[i],
               " & ",
               VarToLatexForComment(descriptions[i]),
               "\\\\ \n")
    } else {
      line.to.add <-
        paste0(VarToLatexForm(parameters[i]),
               " & ",
               values[i],
               " & ",
               VarToLatexForComment(descriptions[i]),
               "\n"
        )
    }
    out <- paste0(out, line.to.add)
  }
  out <- paste0(out, "\\end{tabular} \n \\end{table} \n \\newpage")
}

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
      out <- paste0(out, coefs, "*", WrapInText(VarToLatexForm(vars, mathMode = FALSE)))
    }
    else{
      out <- paste0(out, WrapInText(VarToLatexForm(vars, mathMode = FALSE)))
    }
  }
  else{#add coefs together when there are multiple
    for (i in seq(length(coefs))) {
      if (i == length(coefs)) { #this is the last vars to add so no "+"
        if (coefs[i] != "1") {
          out <- paste0(out, coefs[i], "*", WrapInText(VarToLatexForm(vars[i], mathMode = FALSE)))
        }else{
          out <- paste0(out, WrapInText(VarToLatexForm(vars[i], mathMode = FALSE)))
        }
      }else{#these should all have a plus after them
        if (coefs[i] != "1") {
          out <- paste0(out, coefs[i], "*", WrapInText(VarToLatexForm(vars[i], mathMode = FALSE)), " + ")
        }else{
          out <- paste0(out[i], WrapInText(VarToLatexForm(vars[i], mathMode = FALSE)), " + ")
        }
      }
    }
  }
  return(out)
}

OutputArrowType <- function(eqnType, arrowType, kr, kf, 
                            frBool = FALSE, #if forward regulator in chem rxn
                            frRC = NULL, #forward regulator rate constant
                            frSpecies = NULL, #forward regulator species
                            rrBool = FALSE, #if reverse regulator in chem rxn
                            rrRC = NULL, #reverse regulator rate constant
                            rrSpecies = NULL){ #reverse regulator species 
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
  frBool = as.logical(frBool)
  rrBool = as.logical(rrBool)
  if (eqnType == "enzyme_rxn") {
    if (arrowType == "forward_only") {
      out <- paste0("\\xrightleftharpoons", "[", VarToLatexForm(kr), "]", "{", VarToLatexForm(kf), "}")
    }else if (arrowType == "both_directions") {
      out <- paste0("\\xrightleftharpoons", "[", VarToLatexForm(kr), "]", "{", VarToLatexForm(kf), "}")
    }
  }
  else{
    if (arrowType == "forward_only") {
      if (frBool) {
        out <-
          paste0(
            "\\xrightarrow{",
            WrapInText(VarToLatexForm(frSpecies, mathMode = FALSE)),
            ", ",
            WrapInText(VarToLatexForm(frRC, mathMode = FALSE)),
            "}"
          )
      }
      else {
        out <- paste0("\\xrightarrow{", VarToLatexForm(kf), "}")
      }
      
    }else if (arrowType == "both_directions") {
      if (frBool & rrBool) {
        out <-
          paste0(
            "\\xrightleftharpoons",
            "[",
            WrapInText(VarToLatexForm(rrSpecies, mathMode = FALSE)),
            ", ",
            VarToLatexForm(rrRC),
            "]",
            "{",
            WrapInText(VarToLatexForm(frSpecies, mathMode = FALSE)),
            ", ",
            VarToLatexForm(frRC),
            "}"
          )
      } else if (frBool) {
        out <-
          paste0("\\xrightleftharpoons",
                 "[",
                 VarToLatexForm(kr),
                 "]",
                 "{",
                 WrapInText(VarToLatexForm(frSpecies, mathMode = FALSE)),
                 ", ",
                 VarToLatexForm(frRC),
                 "}")
      } else if (rrBool) {
        out <-
          paste0("\\xrightleftharpoons",
                 "[",
                 WrapInText(VarToLatexForm(rrSpecies, mathMode = FALSE)),
                 ", ",
                 VarToLatexForm(rrRC),
                 "]",
                 "{",
                 VarToLatexForm(kf),
                 "}")
      } else {
        out <-
          paste0("\\xrightleftharpoons",
                 "[",
                 VarToLatexForm(kr),
                 "]",
                 "{",
                 VarToLatexForm(kf),
                 "}")
      }
    }
  }
}

PrintEquationType <- function(eqnType) {
  
  if (eqnType == "chem_rxn") {
    out <- "Mass Action"
  }
  else if (eqnType == "enzyme_rxn") {
    out <- "Enzyme Reaction"
  }
  else {
    out <- "I don't know what happened, check helper file for errors"
  }
  out <- paste0(out, "\n")
  return(out)
}

SpeciesInModel <- function(variables, descriptions) {
  #input variables - vector of variables in model
  #input: descriptions - vector of variable descriptions
  out <- "\\section*{\\underline{Variables}}\n"
  out <- paste0(out, "\\begin{enumerate}\n")
  for (i in seq(length(variables))) {
    if (length(str_split(descriptions[i], "")[[1]]) > 0) {
      out <-
        paste0(out,
               "\t\\item ",
               VarToLatexForm(variables[i], mathMode = FALSE),
               " - ",
               descriptions[i],
               "\n")
    } else {
      out <- paste0(out, "\t\\item ", VarToLatexForm(variables[i], mathMode = FALSE), "\n")
    }
    
  }
  out <- paste0(out, "\\end{enumerate}\n",  "\\newpage\n\n")
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

  out <- "\\section*{\\underline{Equations}}\n"
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

    current.latex.eqn <- PrintEquationType(eqn.type)
    current.latex.eqn <- paste0(current.latex.eqn, "\\begin{equation}\n")
    if (eqn.type == "chem_rxn") {
      LHS.of.eqn <- OutputSideOfEquation(LHS.coef, LHS.var)
      RHS.of.eqn <- OutputSideOfEquation(RHS.coef, RHS.var)
      arrow <- OutputArrowType(eqn.type, 
                               arrow.type, 
                               kr, 
                               kf, 
                               FR.bool, 
                               forward.regulators.rate.constants,
                               forward.regulators,
                               RR.bool,
                               reverse.regulators.rate.constants,
                               reverse.regulators)
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

  out <- "\\section*{\\underline{Additional Equations}}\n"
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

convertVarForLatex <- function(var, inmathModeBool){
  # Converts 
  # Args:
  #   var: variable to change to latex format converting subscripts properly
  #   inmathModeBool: boolean. If true, var takes math mode in latex otherwise
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
  
  if(inmathModeBool){
    latex.var = paste0(latex.var, "$")
    for (i in seq(length(split.var))) {
      if (split.var[i] == "_" & !has.underscore) {
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
      if (split.var[i] == "_" & !has.underscore) {
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
  
  out <- "\\section*{\\underline{Inputs \\& Outputs}}\n"
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
  out <- paste0(out, "\n", "\\newpage")
  print(out)
  return(out)
  #run latex loop for each row in subsetted df to create latex strings
  #for(row in 1:nrow())
}


GenerateLatexDocument <- function(latexText){
#GenerateLatexDocument <- function(eqnsLatex){
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
                "\\usepackage{float}\n",
                "\\newcommand\\tab[1][1cm]{\\hspace*{#1}}\n",
                "\\begin{document}\n"
                #"\\tableofcontents\n",
                #"\\newpage\n\n"
                )
  #out <- paste0(out, eqnsLatex)
  out <- paste0(out, latexText)

  out <- paste0(out, "\\end{document}")

  return(out)
}







