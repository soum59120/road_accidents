##############################################
# MODELISATION PAR REGRESSION LOGISTIQUE
##############################################

# installation des packages
library(ordinal)
library(VGAM)
library(gtsummary)
library(questionr)
library(broom)
library(JLutils)
library(GGally)
library(forestmodel)


###################
## VEHICULES LEGERS
###################

## modelisation
reg_vl <- glm(MAISGLOB ~ ., data = data_vl_f, family = binomial(logit))
summary(reg_vl)
# technique du backward (enlever variables les moins significatives)
reg_vl2 <- step(reg_vl)
summary(reg_vl2)
# odds ratios et intervalles de confiance
odds.ratio(reg_vl2)
# autre représentation des odds ratio des variables
forest_model(reg_vl2)





###################
## 2 RM
###################

## modélisation
reg_2RM <- glm(MAISGLOB ~ ., data = data_2RM_f, family = binomial(logit))
summary(reg_2RM)
# technique du backward (enlever variables les moins significatives)
reg_2RM2 <- step(reg_2RM)
summary(reg_2RM2)
# odds ratios et intervalles de confiance
odds.ratio(reg_2RM2)
# autre représentation des odds ratio des variables
forest_model(reg_2RM2)
