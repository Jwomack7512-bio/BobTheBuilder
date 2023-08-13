mj.term <- "-\\left(V_{cell}*(k_{f4}*I*Prot)\\right)"
mj.term <- "+\\left(V_{cell}*(k_{f1}*A*B-k_{r1}*C_{1})\\right)-\\left(V_{cell}*(\\frac{kcat_{2}*Enz*C_{1}}{Km_{2}+C_{1}})\\right)"

term <- "-(V_cell*(k_f4*I*Prot))"

conv <- gsub("\\left(", "", mj.term, fixed = TRUE)
conv <- gsub("\\right)", "", conv, fixed = TRUE)
conv

rmp(parse(text = term))
rmParen(mj.term)

length(mj.term)

gh <- "(V_cell*(k_f1*A*B-k_r1*C_1))-(V_cell*(kcat_2*Enz*C_1/(Km_2+C_1)))"

resutl <- "V_cell*(k_f1*A*B-k_r1*C_1)-(V_cell*kcat_2*Enz*C_1/(Km_2+C_1))"


library(Ryacas)
simplify_expression <- function(expr_str) {
  # Convert the string to a yacas expression
  yacas_expr <- yac(expr_str)
  
  # Simplify the expression
  simplified_expr <- Simplify(yacas_expr)
  
  # Return the simplified expression as a string
  return(toString(simplified_expr))
}

# Test the function
term <- "-(V_cell*(k_f4*I*Prot))"
expr <- "(V_cell*(k_f1*A*B-k_r1*C_1))-(V_cell*(kcat_2*Enz*C_1/(Km_2+C_1)))"
Deriv::Simplify(expr)
t <- unextract_variables(expr)

# Example list
term.vector <- c("V_cell", "k_f4", "I", "Prot")
unit.vector <- c("volume", "volume/(mol*L)", "mol/L", "mol/L")
term <- "-\\left(V_{cell}*(k_{f4}*I*Prot)\\right)"

term <- gsub("{", "", term, fixed = TRUE)
term <- gsub("}", "", term, fixed = TRUE)
term <- gsub("\\left(", "", term, fixed = TRUE)
term <- gsub("\\right)", "", term, fixed = TRUE)

# Split Expression
s.term <- SplitEquationString(term)
s.term

terms <- extract_variables(term)
terms <- terms[!terms %in% c("left", "right")]
# Build Replacement Vector
replace_matching_terms(s.term,
                       terms,
                       unit.vector)

# Use replace_matching_terms
extract_variables(term)



# Simulated data
df <- data.frame(search_column = c("apple", "banana", "cherry", "date", "fig"),
                 return_column = c(1, 2, 3, 4, 5))

terms_vector <- c("cherry", "apple", "fig")
df$search_column %in% terms_vector
# Find matching rows and extract corresponding values
matched_values <- df$return_column[df$search_column %in% terms_vector]

print(matched_values)

df <- data.frame(search_column = c("apple", "banana", "cherry", "date", "fig"),
                 return_column = c(1, 2, 3, 4, 5))

terms_vector <- c("cherry", "apple", "fig")

# Find matching rows while maintaining the order of terms_vector
matched_indices <- match(terms_vector, df$search_column)
matched_values <- df$return_column[matched_indices]

print(matched_values)


term <- "+\\left(V_{cell}*(k_{f1}*A*B-k_{r1}*C_{1})\\right)-\\left(V_{cell}*(\\frac{kcat_{2}*Enz*C_{1}}{Km_{2}+C_{1}})\\right)"

term <- "-\\left(V_{cell}*(k_{f4}*I*Prot)\\right)"
# term <- gsub("\\frac{", "", term,  fixed = TRUE)
# term <- gsub("{", "", term, fixed = TRUE)
# term <- gsub("}", "", term, fixed = TRUE)
term <- remove_braces(term)
term <- gsub("\\left(", "", term, fixed = TRUE)
term <- gsub("\\right)", "", term, fixed = TRUE)
term
# Split Expression
s.term <- SplitEquationString(term)
s.term

terms <- extract_variables(term)
terms
terms <- terms[!terms %in% c("left", "right")]

str <- "c_{12} + \\frac{g_{h} + \\frac{t_{g}}{y_{gh}}}{k}"

remove_braces <- function(input_str) {
  n <- nchar(input_str)
  i <- 1
  result <- character(0)
  
  while (i <= n) {
    char <- substr(input_str, i, i)
    if (char == "_" && substr(input_str, i+1, i+1) == "{") {
      result <- c(result, "_")
      brace_count <- 1
      i <- i + 2
      while (brace_count > 0 && i <= n) {
        if (substr(input_str, i, i) == "{") {
          brace_count <- brace_count + 1
        }
        if (substr(input_str, i, i) == "}") {
          brace_count <- brace_count - 1
        }
        if (brace_count > 0) {
          result <- c(result, substr(input_str, i, i))
        }
        i <- i + 1
      }
    } else {
      result <- c(result, char)
      i <- i + 1
    }
  }
  
  return(paste(result, collapse = ""))
}

cleaned_str <- remove_braces(str)
print(cleaned_str)


# Sample data
vec <- c("-", "V_cell" ,"*", "(", "k_f4", "*", "I", "*", "Prot", ")")
df <- data.frame(term = c("V_cell", "k_f4", "I", "Prot"),
                 type = c("param", "param", "species", "species"))

# Placeholder for indexes that need to be removed later
to_remove <- c()

# Iterate over the terms in vec
for (i in 1:length(vec)) {
  # Check if the term is in df and is of type 'species'
  if (vec[i] %in% df$term[df$type == 'species']) {
    # Check if it is next to a '*'
    if (i > 1 && vec[i - 1] == "*") {
      vec[i] <- paste0("[", vec[i], "]")
      to_remove <- c(to_remove, i - 1)
    } else if (i < length(vec) && vec[i + 1] == "*") {
      vec[i] <- paste0("[", vec[i], "]")
      to_remove <- c(to_remove, i + 1)
    }
  }
}

# Remove the multiplication terms next to the converted values
vec <- vec[-to_remove]

# Collapse the modified vector into a single string
result <- paste(vec, collapse = "")
print(result)


term <- "+\\left(V_{cell}*(k_{f1}*A*B-k_{r1}*C_{1})\\right)-\\left(V_{cell}*(\\frac{kcat_{2}*Enz*C_{1}}{Km_{2}+C_{1}})\\right)"
term <- remove_braces(term)
term <- gsub("\\left(", "", term, fixed = TRUE)
term <- gsub("\\right)", "", term, fixed = TRUE)
vec <- SplitEquationString(term)
vec

df <- data.frame(term = c("V_cell", "k_f1", "C_1", "kcat_2", "Enz", "Km_2", "A", "B"),
                 type = c("param", "param", "species", "param", 
                          "species", "param", "species", "species"))


# Placeholder for indexes that need to be removed later
to_remove <- c()
#This function turns all equations into "pretty" forms.
# Iterate over the terms in vec
for (i in 1:length(vec)) {
  # Check if the term is in df and is of type 'species'
  if (vec[i] %in% df$term[df$type == 'species']) {
    # Check if it is next to a '*'
    vec[i] <- paste0("[", vec[i], "]")
    if (i > 1 && vec[i - 1] == "*") {
      # vec[i] <- paste0("[", vec[i], "]")
      to_remove <- c(to_remove, i - 1)
    } else if (i < length(vec) && vec[i + 1] == "*") {
      # vec[i] <- paste0("[", vec[i], "]")
      to_remove <- c(to_remove, i + 1)
    }
  }
}

# Remove the multiplication terms next to the converted values
vec <- vec[-to_remove]

# Collapse the modified vector into a single string
result <- paste(vec, collapse = "")
print(result)
