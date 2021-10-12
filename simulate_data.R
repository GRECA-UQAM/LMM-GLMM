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



######################################################################
# ====================================================================
# Other way to simulate a data with specific covariance between variables 
# but not repeated measurements
# ====================================================================

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
    #utilisation de la fct mvrnorm (multivariable normal distribution), 
            #n= nb of sample , 
            #mu= vector of mean variables, 
            #sigma= cov matrix, 
            #empirical = T not the pop level values)
traits<-as.data.frame(mvrnorm(n= nobs*nind,mu=temp_mn,Sigma=temp_var,empirical=FALSE))
colnames(traits)<-c("T1", "T2")

# Check la covariance entre les deux traits
cov(traits[,1:2])

# Creation de la variable categorie 'block' avec 5 niveaux
traits$block<-rep(1:5,1000)

# Generate les residuals pour l'equation 
temp_res<-rnorm(n=nobs*nind, mean=0, sd=1) 

# Fusion des datasets id, traits and residual 
traits<-cbind(id,traits, temp_res)    

# Creation de la reponse variable  
y <- 2 + traits$T1*3 + traits$T2*(-2) + 12.5*traits$T1*traits$T2 + rep(indbt, each = nobs) + traits$block*3 + traits$temp_res

# Fusion de la reponse variable et des autre variables pour creer la dataset simule data2
data2<-cbind(y,traits[,1:4])

# Check the dataset data2
head(data2)
summary(data2)

# Check the dataset via plots
par(mfrow=c(2,2))
plot(y~T1, data2)
plot(y~T2, data2)
plot(y~block, data2)
plot(y~id, data2)

# Check the dataset via model
model<-lm(y~T1+T2+T1:T2+block, data=data2)
summary(model) 
#
library(lme4)
model<-lmer(y~T1+T2+T1:T2+block+(1|id), data=data2)
summary(model) 

######################################################################



