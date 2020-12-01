# ====================================================================
# Simulate data to use in subsequent scripts and mixed model analyses
# ====================================================================

set.seed(1234)
nobs = 50
nind = 100
nlevels <- 4
levels.cat <- rnorm(n = nlevels, mean = 0, sd = 2)
indbt <- rnorm(n = nind, mean = 0, sd = 2)
id <- rep(1:nind, each = nobs)
categorique <- sample(x = 1:4, size = nobs*nind, replace = TRUE)
table.cat <- data.frame(categorique = 1:4, val = levels.cat)
x1 <- runif(min = 0, max = 1, n = nobs*nind)
x2 <- runif(min = 0, max = 1, n = nobs*nind)
y <- 2 + x1*3 + x2*(-2) + 0.5*x1*x2 + rep(indbt, each = nobs) + table.cat$val[match(categorique, table.cat$categorique)] + rnorm(sd = 2, n = nobs*nind)

# Create the simulated dataset
data <- data.frame(cbind(id, environnement = categorique, x1, x2, y))
