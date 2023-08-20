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
s.str <- SplitEquationString(str)
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
  spl <- SplitEquationString(str.expression)
  # print(spl)
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

toml <- function(e) {
  if (is.symbol(e)) c("<ci>", as.character(e), "</ci>")
  else if (identical(e[[1]], as.symbol("+")))
    c("<apply>", "<plus/>", Recall(e[[2]]), Recall(e[[3]]), "</apply>")
  else if (identical(e[[1]], as.symbol("-")))
    c("<apply>", "<minus/>", Recall(e[[2]]), Recall(e[[3]]), "</apply>")
  else if (identical(e[[1]], as.symbol("*")))  
    c("<apply>", "<times/>", Recall(e[[2]]), Recall(e[[3]]), "</apply>")
  else if (identical(e[[1]], as.symbol("/")))  
    c("<apply>", "<divide/>", Recall(e[[2]]), Recall(e[[3]]), "</apply>")
  else if (identical(e[[1]], as.symbol("("))) Recall(e[[2]])
}

mathml(str)
y = quote(Vmax*S/(Km+S))
y
xmlParse(mathml(term=y))

help("mathml")


Vmax <- symbol("Vmax")
sympy_func(mathml())

source_python("string2mathml.py")

str <- "Vmax*Sa/(K+Sa)"
test <- py_string2mathml(str)
xmlParse(test)

lat.text <- py_string2latex(str)
lat.text

yu <- quote(Vmax*Sa/(K+Sa))
e <- parse(text = str)[[1]]
e
out <- toml(e)
out <- paste0(out, collapse = "")
# print(out)
kk <- xmlParse(out)
kk
out
yu
e

str <- "Vmax*Sa/(K+Sa)"
test <- py_string2mathml(str)
xmlParse(test)

g <- string2mathml(str)
xmlParse(g)
string2mathml <- function(string_e) {
  # Check expression for S and change
  string_e <- replace_vals(string_e, "S")
  mathml.e <- py_string2mathml(string_e)
  # out      <- replace_vals(mathml.e)
}

replace_vals <- function(string_e, term.in, term.out = "RANDOMVARID4567890") {
  replaced <- FALSE
  terms <- SplitEquationString(string_e)
  if (term.in %in% terms) {
    replaced <- TRUE
    terms[terms==term.in] <- term.out
    
  }
  terms <- paste0(terms, collapse = "")
  # replace(terms, terms=="S", "RANDOMVARID4567890")
  return(terms)
}

a <- replace_vals(str, "S")
a

expToMathML <- function(e) {
  # Recursive function to build content mathml expression from a string
  # expression.
  # @e - string expression, expression (use quote, or parse(text=X)[[1]])
  # Output: 
  # Example: 
  # Input: "Vmax*S/(Km+S)"
  # Output:
  # [1] "<apply>"   "<divide/>" "<apply>"   "<times/>"  "<ci>"      "Vmax"     
  # [7] "</ci>"     "<ci>"      "S"         "</ci>"     "</apply>"  "<apply>"  
  # [13] "<plus/>"   "<ci>"      "Km"        "</ci>"     "<ci>"      "S"        
  # [19] "</ci>"     "</apply>"  "</apply>" 
  
  if (is.symbol(e)) 
    c("<ci> ", as.character(e), " </ci>")
  else if (is.numeric(e))
    c("<cn> ", as.character(e), " </cn>")
  else if (identical(e[[1]], as.symbol("+")))
    c("<apply>", "<plus/>", Recall(e[[2]]), Recall(e[[3]]), "</apply>")
  else if (identical(e[[1]], as.symbol("-")))
    c("<apply>", "<minus/>", Recall(e[[2]]), Recall(e[[3]]), "</apply>")
  else if (identical(e[[1]], as.symbol("*")))  
    c("<apply>", "<times/>", Recall(e[[2]]), Recall(e[[3]]), "</apply>")
  else if (identical(e[[1]], as.symbol("/")))  
    c("<apply>", "<divide/>", Recall(e[[2]]), Recall(e[[3]]), "</apply>")
  else if (identical(e[[1]], as.symbol("^")))
    c("<apply>", "<power/>", Recall(e[[2]]), Recall(e[[3]]), "</apply>")
  else if (identical(e[[1]], as.symbol("("))) Recall(e[[2]])
}

string2mathml <- function(stringExpression) {
  
  # Convert string to expression
  e <- parse(text = stringExpression)[[1]]
  
  # Convert to mathml vector terms
  mathml.terms <- expToMathML(e)
  
  # Collapse to String
  out <- paste0(mathml.terms, collapse = "")
  
  return(out)
}

test <- "bn*km/a+cd*C^3"
test.e <- parse(text = test)[[1]]
test.terms <- expToMathML(test.e)
test.terms
te <- paste0(test.terms, collapse = "")
xmlParse(te)
test <- "bn*km/a+cd*C^3"
out <- string2mathml(test)
out
test <- "Vmax*S/(Km+S)"
e <- parse(text = test)[[1]]
e
is.symbol(e)


out <- toml(e)
# print(out)
out <- paste0(out, collapse = "")
# print(out)
kk <- xmlParse(out)
kk
is.symbol(3)
for (i in seq_along(e)) {
  # print(e[[i]])
  # print(is.symbol(e[[i]]))
}
