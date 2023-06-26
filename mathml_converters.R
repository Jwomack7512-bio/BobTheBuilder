library(mathml)

mathml(term=quote((a + b)^2L == a^2L + 2L*a*b + b^2L))
help("katex_mathml")

str <- "\\frac{x+y}{km*s}"

tree <- katex_mathml(str,
                     displayMode = FALSE,
                     include_css = TRUE)

xml.model  <- xmlParse(tree)
xml.model

str <- "x + 2"

tree <- katex_mathml(str,
                     displayMode = FALSE,
                     include_css = FALSE)

xml.model  <- xmlParse(tree)
xml.model

str <- "Vmax*S/(K+S)"
str
s.str <- SplitTerm(str)
s.str

getValueType <- function(value) {
  if (grepl("^[-+*/^%]", value)) {
    return("Operator")
  } else if (grepl("^-?[0-9]+(\\.[0-9]+)?$", value)) {
    return("Number")
  } else if (tolower(value) %in% c("true", "false")) {
    return("Boolean")
  } else if (value == "(") {
    return("Parenthesis_start")
  } else if (value == ")") {
    return("Parenthesis_end")
  } else {
    return("Other")
  }
}

CharTag <- function(charTerm) {
  out <- paste0("<ci> ", charTerm, " </ci>")
  return(out)
}

NumTag <- function(numTerm) {
  out <- paste0("<cn> ", numTerm, " </cn>")
  return(out)
}

operatorTag <- function(operator) {
  tag <- switch(operator,
                "*"   = "<times/>",
                "/"   = "<divide/>",
                "+"   = "<plus/>",
                "-"   = "<minus/>",
                "^"   = "<power/>",
                "exp" = "<exp/>",
                "ln"  = "<ln/>")
  return(tag)
}

toMathMl <- function(str.expression) {
  
  # TODO: create apply list  that will older information about the current apply
  # 
  apply.count <- 1
  apply.list <- list()
  # Each entry will contain information about the apply including
  # type - (+,-, etc)
  # in.par - (bool) true if apply is in parenthesis
  # split expression
  spl <- SplitTerm(str.expression)
  print(spl)
  out <- c()
  num.apply <- 0
  apply.parenthesis <- c()
  apply.par.depth <- c()
  # use this vector to keep track of how many variables that have been counted
  apply.counts <- c()
  # track apply type (+, -, *, etc)
  apply.type <- c()
  for (i in seq_along(spl)) {
    
    val.type <- getValueType(spl[i])
    # check for end apply
    if (apply.count > 1) {
      n.apply <- length(apply.list)
      # Work off the last apply
      last.apply <- apply.list[[n.apply]]
      
      # Check if conditions for end apply are met
      if (last.apply$in.par) {
        if (val.type == ")") {
          out <- c(out, "</apply>")
        }
      }
    }
    
    
    if (val.type == "Other") {
      out <- c(out, CharTag(spl[i]))
    } else if (val.type == "Number") {
      out <- c(out, NumTag(spl[i]))
    } else if (val.type == "Operator") {
      out <- c(out, 
               "<apply>",
               operatorTag(spl[i])
               )
      if (spl[i+1] == "(") {in.par <- TRUE} else {in.par <- FALSE}
      
      apply.list[[apply.count]] <- list("type" = spl[i],
                                        "in.par" = in.par,
                                        "var.count" = 0)
      apply.count <- apply.count + 1
    }
  }
  return(out)
}


toMathMl(str)


getValueType("*")
getValueType("(")
getValueType("2")
getValueType("24365874")
