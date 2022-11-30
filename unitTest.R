

val <- 10

u <- "mol / hr"
u.2 <- "mol / min"

conv_unit(val, u, u.2)
conv_multiunit(val, u, u.2)
conv_unit_options
conv_multiunit(x = 10, "ft / hr * F", "m / min * C")


g <- "num <div> time"

# Take term after div and inverse conversion. Use 1's.  Then multiply result by val

val <- 10
a <- "time"
b <- "min"
c <- "hr"
UnitConversion(a, c, b, val)


val <- 10
a <- "time <multiply> conc"
b <- "min*g"
c <- "hr*kg"
UnitConversion(a, c, b, val)


val <- 10
a <- "time <div> conc"
b <- "min/g"
c <- "hr/kg"
UnitConversion(a, c, b, val)
