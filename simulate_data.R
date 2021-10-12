# ===================================================================================

#        Simulate data to use in subsequent scripts for mixed model analyses
#           Simuler des données pour les utiliser dans les modèles mixtes

# ===================================================================================



# =====================================================================
# 1. Preparer les simulations / prepare the simulations
# =====================================================================

set.seed(1234)

# Creation du nombre d'individus et du nombre de mesure repetees.
# Create n individuals with n observations
nobs = 50
nind = 100


# Creation du nombre d'individus et du nombre de mesures repetees. ***(MFF: WHY IS THIS HERE TWO TIMES?)***
nlevels <- 4
levels.cat <- rnorm(n = nlevels, mean = 0, sd = 2)

indbt <- rnorm(n = nind, mean = 0, sd = 2)
id <- rep(1:nind, each = nobs)


# Creation d'une variable categorique
# Create categorical variable
categorique <- sample(x = 1:4, size = nobs*nind, replace = TRUE)

table.cat <- data.frame(categorique = 1:4, val = levels.cat)


# Creation de x1 et x2, des variables categoriques avec un min = 0 et max = 1
# Creation of x1 and x2, two variable with min of 0, max of 1 and variable values
x1 <- runif(min = 0, max = 1, n = nobs*nind)
x2 <- runif(min = 0, max = 1, n = nobs*nind)


# Creation of the response variable y using the model equation
   # here intercept, 
       # coef of partial regression coefficient of x1, x2 and their interaction, 
       # random effect, 
       # categorical variable
       # residual

y <- 2 + x1*3 + x2*(-2) + 12.5*x1*x2 + 
     rep(indbt, each = nobs) + 
     table.cat$val[match(categorique, table.cat$categorique)] +
     rnorm(sd = 2, n = nobs*nind)

# =====================================================================
# =====================================================================





# =====================================================================
# 2. Créer le jeu de données simulé / create the simulated dataset
# =====================================================================

# Create the simulated dataset
data <- data.frame(cbind(id, environnement = categorique, x1, x2, y))


# Check the dataset
head(data)
summary(data)


# Check the dataset with plots
par(mfrow = c(2, 2))
plot(y ~ x1, data)
plot(y ~ x2, data)
plot(y ~ environnement, data)
plot(y ~ id, data)


# Check the dataset via model

# Simple linear model
model1 <- lm(y ~ x1 + x2 + x1:x2 + environnement, data = data)
summary(model1) 

# Mixed model
library(lme4)
model2 <- lmer(y ~ x1 + x2 + x1:x2 + environnement + (1 | id), data = data)
summary(model2) 

# ====================================================================
# ====================================================================
