# ===========================================================================================

# Extract estimates from hierarchical models and compute their credibility intervals

# ===========================================================================================



# ============================================================
# 1. Simulate a dataset
# ============================================================

# Dataset ---------------------------------------------------- 

# 100 ids with 100 reps
# y = continuous dependent variable
# x1 and x2 continuous covariates
# x3 categorical variable with two levels

   nobs = 100
   nind = 100
   indbt <- rnorm(n = nind, mean = 0, sd = 2)
   id <- rep(1:nind, each = nobs)
   x1 <- runif(min = 0, max = 1, n = nobs*nind)
   x2 <- runif(min = 0, max = 1, n = nobs*nind)
   y <- 2 + x1*3 + x2*(-2) + 0.5*x1*x2 + rep(indbt, each = nobs) + rnorm(sd = 2, n = nobs*nind)
   x3 <- sample(c("A", "B"), 10000, replace = TRUE)


# Organizing the dataset -------------------------------------

   data <- cbind(id, x1, x2, x3, y)
   data <- as.data.frame(data)
   str(data)

   data$x1 <- as.numeric(as.character(data$x1))
   data$x2 <- as.numeric(as.character(data$x2))
   data$y <- as.numeric(as.character(data$y))

# ============================================================
# ============================================================



# ============================================================
# 2. Run the model
# ============================================================

# Import librairies ------------------------------------------

  library(lme4)
  library(arm)


# Fit the model ----------------------------------------------

# We use one continuous covariate, one categorical, and fit their interaction
# indvidual as random effect as there are repeated measures

  mod <- lmer(y ~   x1 +  x3  +  x1*x3 +(1|id), data=data)

# We look at the summary
# Here we can extract the estimate of fixed effects
  summary(mod)

# ============================================================
# ============================================================





# ============================================================
# 3. Posterior simulations 
# ============================================================

# We use the function "sim" from the "arm" package:
# https://www.rdocumentation.org/packages/arm/versions/1.11-1/topics/sim

# Run the simulations ----------------------------------------
  nsim <- 1000
  bsim <- arm::sim(mod, n.sim = nsim)


# Extract the credibility intervals --------------------------

# Check the structure of fixed effects simulations
  str(bsim@fixef)

# We have 1000 simulations for each fixed effect coefficient of the model:
# x1 is the slope for the continuous var, at level a
# x3b is the difference in intercept of b to a (reference is a)

# We extract the 95% Credibility interval 
# (we can change the prob if we want the 90% e.g. 0.05, 0.95)
# We can associate these CRi to the estimates from the summary
  apply(bsim@fixef, 2, quantile, prob = c(0.025, 0.975))

# We also need to extract the interaction x1:x3b, to get the slope for the continuous var, at level b
# We need to take the estimate of x1 + x1:x3b from the summary(mod) and make the calculation ourselves
  summary(mod)$coefficients[1,1]+ summary(mod)$coefficients[4,1]

# And here is its associated Cri
# If you have more variables in the model you need to change [,1] and [,3] depending on the fixef stucture
  quantile((bsim@fixef[,1] + bsim@fixef[,3] ),   prob = c(0.025, 0.975))

# Check the structure of random effects simulations
  str(bsim@ranef)

# We have 1000 simulation for each of the 100 individuals
# We extract the 95% credibility intervals, as well as the mean estimate, since we don't have that in the summary output
  quantile(apply(bsim@ranef$id[ , , 1], 1, var), prob = c(0.025, 0.5, 0.975))


# ***You can check the other sections of the GitHub for how to plot the model results in a graph***

# ============================================================
# ============================================================





# ============================================================
# 4. bonus functions for a better display of results
# ============================================================

# You need to have run the model first

# Fixed eff estimate from the summary and associated CRI from bsim

   table <- function() {
   a <- apply(bsim@fixef, 2, quantile, prob = c(0.025, 0.975))
   ta <- t(a)
   ta <- as.data.frame(ta)
   b <- summary(mod)$coefficients
   b <- as.data.frame(b)
   b <- b[ -c(2, 3) ]
   tab <- cbind(b, ta)
   tab <- round(tab, 2)
   tab }

   table()

# Random effect estimate and associated CRI from bsim
# Vhange ranef$id to another variable if you have more random effects
    id <- function(){ id <- as.data.frame(quantile(apply(bsim@ranef$id[ , , 1], 1, var), prob = c(0.025, 0.5, 0.975)))
    id <- t(id)
    round(id, 2)  }

    id()

# CRi for interaction, remember to change fixef [,1] and [,3] depending on your model structure
    CIinter <- function() {
    bdiff <- bsim@fixef[,1] + bsim@fixef[, 3]
    q <- quantile(bdiff, prob = c(0.025, 0.975))
    q <- t(q)
    round(q, 2)
    }
    CIinter()

# ============================================================
# ============================================================
