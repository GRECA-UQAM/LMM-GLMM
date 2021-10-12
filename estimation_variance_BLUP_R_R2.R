# ====================================================================

#       Extracte the BLUP (Best Linear Unbiaised Predictor)

# ====================================================================



# ====================================================================
# 1. Importer les données et rouler le modèle
# ====================================================================

# Verifier que la dataset simulee
head(data)

# Loading package 
library(lme4)

# Creer un modèle mixte
model <- lmer(y~ T1 + T2 + T1:T2 + block+ (1 | id), data = data2)
summary(model)

# Extraire les BLUPS utilisant la fonction ranef
BLUPS <- ranef(model)$id

# Check the BLUPS
nrow(BLUPS)

# *** À noter: n'oubliez pas de vérifier que votre modèle est correct ***
# ex. vérifier l'homogénéité des résidus, la variance entre les groupes, etc...

# ====================================================================
# ====================================================================





# ====================================================================
# 2. Estimation du R2 du modèle via MuMIn 
# ====================================================================

# À noter, cette méthode ne fonctionne pas avec toutes les familles
# de distributions.

# Loading packages
library(MuMIn)

# Estimation du R2 du modèle
r.squaredGLMM(model)

# ====================================================================
# ====================================================================





# ====================================================================
# 3. Estimation de la Repetabilite via lme4 
# ====================================================================

# Extraire les estimés de variance et résidus
vc = VarCorr(model)
residual_var = attr(vc,'sc')^2
intercept_var = attr(vc$id,'stddev')[1]^2

# Calculer la repetabilite R
R_lme4 = intercept_var / (intercept_var + residual_var)
R_lme4

# ====================================================================
# ====================================================================





# ====================================================================
# 4. Estimation de la Repetabilite via RptR 
# ====================================================================

# À noter, risque de ne pas fonctionner avec toutes les familles de 
# distributions.

# Ici, c'est une méthode par simulation

# Loading packages
library(rptR)

# Estimation de la repetabilite et de son incertitude 

R_rptR <- rpt(y ~ T1 + T2 + T1:T2 + block + (1 | id), 
              grname = "id", data = data2, 
              datatype = "Gaussian",
              nboot = 1000, npermut = 0)
R_rptR

# ====================================================================
# ====================================================================
