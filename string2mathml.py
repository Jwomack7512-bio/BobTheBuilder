from sympy import parse_expr as sympy_parse_expr
from sympy.printing.mathml import mathml as sympy_mathml
from sympy import latex as sympy_latex

def py_string2mathml(string_expression):
  # Converts string expression to content mathml 
  return(sympy_mathml(sympy_parse_expr(string_expression)))

def py_string2latex(string_expression, 
                    mult_symbol = " * "):
  return(sympy_latex(sympy_parse_expr(
  string_expression), mul_symbol = mult_symbol))
