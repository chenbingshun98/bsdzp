# Import lpSolve package
library(lpSolve)

# Set coefficients of the objective function
f.obj <- c(5, 7)

# Set matrix corresponding to coefficients of constraints by rows
# Do not consider the non-negative constraint; it is automatically assumed
f.con <- matrix(c(1, 0,
                  2, 3,
                  1, 1), nrow = 3, byrow = TRUE)

# Set unequality signs
f.dir <- c("<=",
           "<=",
           "<=")

# Set right hand side coefficients
f.rhs <- c(16,
           19,
           8)

# Final value (z)
lp("max", f.obj, f.con, f.dir, f.rhs)
lp("min", f.obj, f.con, f.dir, f.rhs)
#int.vec 	:Numeric vector giving the indices of variables that are required to be integer. The length of this vector will therefore be the number of integer variables.
# Variables final values
lp("max", f.obj, f.con, f.dir, f.rhs)$solution

# Sensitivities
lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$sens.coef.from
lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$sens.coef.to

# Dual Values (first dual of the constraints and then dual of the variables)
# Duals of the constraints and variables are mixed
lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$duals

# Duals lower and upper limits
lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$duals.from
lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$duals.to