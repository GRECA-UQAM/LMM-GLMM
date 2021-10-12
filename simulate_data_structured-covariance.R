
# =====================================================================================

#             Simulate a data with specific covariance between variables 
#                           but not repeated measurements

# =====================================================================================



# ===================================================================
# 1. Préparer les données
# ===================================================================

# Loading package 
library(MASS)


# Creation du nombre d individu et du nombre de mesure repetee.
nobs = 50
nind = 100

# Creation du nombre d individu et du nombre de mesure repetee.

indbt <- rnorm(n = nind, mean = 0, sd = 2)
id <- rep(1:nind, each = nobs)


# Creation du vector des moyennes de T1 et T2
temp_mn<-c(1,1)  


# Creation de la matrix T1-T2, avec var =1 and cov = 0.5 
temp_var<-as.matrix(cbind(c(1,0.5), c(0.5,1)))

# Creation du dataset 'trait' avec les deux variables : 
    # Utilisation de la fct mvrnorm (multivariable normal distribution), 
            # n = nb of sample , 
            # mu = vector of mean variables, 
            # sigma = cov matrix, 
            # empirical = T not the pop level values)

traits <- as.data.frame(mvrnorm(n = nobs*nind, mu = temp_mn, Sigma = temp_var, empirical = FALSE))
colnames(traits) <- c("T1", "T2")

# Check la covariance entre les deux traits
cov(traits[, 1:2])

# Creation de la variable categorie 'block' avec 5 niveaux
traits$block <- rep(1:5, 1000)

# Generate les residuals pour l'equation 
temp_res <- rnorm(n = nobs*nind, mean = 0, sd = 1) 

# Fusion des datasets id, traits and residual 
traits <- cbind(id, traits, temp_res)

# Creation de la reponse variable  
y <- 2 + traits$T1*3 + traits$T2*(-2) + 12.5*traits$T1*traits$T2 + rep(indbt, each = nobs) + traits$block*3 + traits$temp_res


# Fusion de la reponse variable et des autre variables pour creer la dataset simule data2
data2 <- cbind(y, traits[, 1:4])

# ===================================================================
# ===================================================================





# ===================================================================
# 2. Vérifier les données
# ===================================================================

# Check the dataset data2
head(data2)
summary(data2)

# Check the dataset via plots
par(mfrow = c(2, 2))
plot(y ~ T1, data2)
plot(y ~ T2, data2)
plot(y ~ block, data2)
plot(y ~ id, data2)

# Check the dataset via model
model1 <- lm(y ~ T1 + T2 + T1:T2 + block, data = data2)
summary(model1)
#
library(lme4)
model2 <- lmer(y ~ T1 + T2 + T1:T2 + block + (1 | id), data = data2)
summary(model2) 

# ===================================================================
# ===================================================================
