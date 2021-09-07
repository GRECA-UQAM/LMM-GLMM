# ====================================================================
# Extracte the BLUP (Best Linear Unbiaised Predictor)
# ====================================================================

# Verifier que la dataset simulee
head(data)

# Loading package 
library(lme4)

# Creer un model mixte
model<-lmer(y~T1+T2+T1:T2+block+(1|id), data=data2)
summary(model) 

# Extraire les BLUPS utilisant la fonction ranef
BLUPS<-ranef(model)$id

# Check the BLUPS
nrow(BLUPS)



# ====================================================================
# Estimation du R2 du model via MuMIn 
# ====================================================================

# Loading packages
library(MuMIn)

# Estimation du R2 du model 
r.squaredGLMM(model)



# ====================================================================
# Estimation de la Repetabilite via lm4 
# ====================================================================

# Extraire les estimes de variance et residus
vc = VarCorr(model)
residual_var = attr(vc,'sc')^2
intercept_var = attr(vc$id,'stddev')[1]^2

# Calculer la repetabilite R
R_lm4 = intercept_var/(intercept_var+residual_var)
R_lm4



# ====================================================================
# Estimation de la Repetabilite via RptR 
# ====================================================================

# Loading packages
library(rptR)

# Estimation de la repetabilite et de son incertitude 

R_rptR <- rpt(y ~ T1+T2+T1:T2+block+(1|id), grname = "id", data = data2, 
    datatype = "Gaussian", nboot = 1000, npermut = 0)
R_rptR