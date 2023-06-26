from sympy import symbols, sympify, parse_expr
from sympy.printing.mathml import mathml

def string2mathml(string_expression):
  # Converts string expression to content mathml 
  return(mathml(parse_expr(string_expression)))