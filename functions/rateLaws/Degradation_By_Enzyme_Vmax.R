Degradation_By_Enzyme_Vmax <- function(degradatedVariable, 
                                       Km,
                                       Vmax,
                                       volumeVar) {
  
  # Functional String
  rate.law <- paste0(Vmax, "*", degradatedVariable, "/",
                     "(", Km, "+", degradatedVariable, ")")
  
  # Pretty String
  ps <- paste0(Vmax, "[", degradatedVariable, "]", 
               "/", 
               "(", Km, " + ", "[", degradatedVariable, "]", ")")
  # Latex
  latex.rate.law <- paste0("\\frac{",
                           Var2Latex(Vmax), "*", Var2Latex(degradatedVariable), 
                           "}{",
                           Var2Latex(Km), 
                           "+",
                           Var2Latex(degradatedVariable),
                           "}")
  # MathJax
  mj <- paste0("\\frac{",
               Var2MathJ(Vmax), "*", Var2MathJ(degradatedVariable), 
               "}{",
               Var2MathJ(Km), 
               "+",
               Var2MathJ(degradatedVariable),
               "}")
  
  
  # Add volume to terms
  rate.law <- paste0(volumeVar, "*(", rate.law, ")")
  
  # Mathjax
  mj <- paste0(Var2Latex(volumeVar), "*(", latex.rate.law, ")")
  
  latex.rate.law <- paste0(Var2Latex(volumeVar), "*(", latex.rate.law, ")")
  
  # Mathml
  ml <- katex_mathml(latex.rate.law)
  
  # 
  # Contentml
  content.ml <- 
    paste0("<math xmlns=\"http://www.w3.org/1998/Math/MathML\">",
           string2mathml(rate.law),
           "</math>")
  
  out.list <- list("string" = rate.law,
                   "pretty.string" = ps,
                   "latex" = latex.rate.law,
                   "mj" = mj,
                   "mathml" = ml,
                   "content.ml" = content.ml)
  
  return(out.list)
  
}