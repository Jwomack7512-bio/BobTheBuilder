

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

val <- 10
a <- "conc <div> time <power>(2)"
b <- "kg/min^2"
c <- "g/sec^2"
UnitConversion(a, b, c, val)

val <- 10
a <- "conc <power>(2) <div> time <power>(2)"
b <- "kg^2/min^2"
c <- "g^2/sec^2"
UnitConversion(a, b, c, val)

val <- 10
a <- "conc (Mass) <power>(2) <div> <group> volume <power>(2) <multiply> time <power>(2) <endgroup>"
b <- "g^2/(l^2*sec^2)"
c <- "kg/(ml^2*min^2)"
UnitConversion(a, b, c, val)
UnitCompare(a, b, measurements::conv_unit_options$mass, 
            measurements::conv_unit_options$duration)
val <- 10
a <- "conc (Mol) <power>(2) <div> time" 
b <- "mol^2/sec"

UnitCompare(a, b, measurements::conv_unit_options$count, 
            measurements::conv_unit_options$duration)


val <- 10
a <- "conc (Mol) <div> <group> time <power>(2) <multiply> <volume> <endgroup>" 
b <- "mol/(sec^2*l)"
c <- "umol/(sec^2*l)"
UnitConversion(a, b, c, val)
UnitCompare(a, b, measurements::conv_unit_options$count, 
            measurements::conv_unit_options$duration)



a <- data.frame(x1 = 1:5,
           x2 = 6:10,
           x3 = 7)

a

u1 <- "mol"
u2 <- "umol"
b <- data.frame(lapply(a, measurements::conv_unit, u1, u2))
b

d <- a[,2:ncol(a)]
d
d1 <- data.frame(lapply(d, conv_unit, u1, u2))
d2 <- cbind(a[,1], d1)
d2
colnames(d2) <- colnames(a)
d2
