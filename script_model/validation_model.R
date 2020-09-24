##############################################
# VALIDATION DES MODELES
##############################################

# installation des packages
library("pROC")
library("ROCR")
library("caret")
library("performance")
library("prediction")
library("ResourceSelection")
library('car')
library('aod')
library('lmtest')
library('MKmisc')
library('rcompanion')



###################
## VEHICULES LEGERS
###################

# créer une regression où les coefficients sont tous égaux à 0, excepté l'intercept
reg_vl_null <- step(glm(MAISGLOB ~ 1, data = data_vl_f, family = binomial(logit)))
summary(reg_vl_null)

# obtenir les scores pour chaque individus à  partir de la fonction predict()
predict_vl <- predict(reg_vl2, type = "response", newdata = data_vl_f)

# test hosmer et lemeshow
hoslem.test(reg_vl2$y,predict_vl, g=10)

# test du rapport de vraisemblance ou déviance du modèle
lrtest(reg_vl_null,reg_vl2)
# test de vraissemblances pour tous les predictors
Anova(reg_vl2, type=3, test.statistic = "LR")

# critère AIC/BIC
BIC(reg_vl2)
AIC(reg_vl2)

# analyse des effets des variables indépendantes : test WALD
Anova(reg_vl2,test.statistic = "Wald")
# effet globale pour le modèle
wald.test(b=coef(reg_vl2), Sigma=vcov(reg_vl2), Terms=2:17)

# pseudo R²
nagelkerke(reg_vl2)

# analyse des résidus
plot(rstudent(reg_vl2),type="p", cex=.5, ylim=c(-3,3))
abline(h=c(-2,2), col="red")

# matrice de confusion
table(predict_vl > 0.5,data_vl_f$MAISGLOB) # transformer les probabilites predites en variable binaire (> 0.5 ou < 0.5)

# courbe ROC
plot.roc(data_vl_f$MAISGLOB, predict_vl, print.auc = TRUE, legacy.axes = TRUE,
         ci = TRUE)





####################
## 2 ROUES MOTORISES
####################


# créer une regression où les coefficients sont tous égaux à 0, excepté l'intercept
reg_2RM_null <- step(glm(MAISGLOB ~ 1, data = data_2RM_f, family = binomial(logit)))
summary(reg_2RM_null)

# obtenir les scores pour chaque individus à  partir de la fonction predict()
predict_2RM <- predict(reg_2RM2, type = "response", newdata = data_2RM_f)

# test hosmer et lemeshow
hoslem.test(reg_2RM2$y,predict_2RM, g=10)

# test du rapport de vraisemblance ou déviance du modèle
lrtest(reg_2RM_null,reg_2RM2)
# test de vraissemblances pour tous les predictors
Anova(reg_2RM2, type=3, test.statistic = "LR")

# critère AIC/BIC
BIC(reg_2RM2)
AIC(reg_2RM2)

# analyse des effets des variables indépendantes : test WALD
Anova(reg_2RM2,test.statistic = "Wald")
# effet globale pour le modèle
wald.test(b=coef(reg_2RM2), Sigma=vcov(reg_2RM2), Terms=2:12)

# pseudo R²
nagelkerke(reg_2RM2)

# analyse des résidus
plot(rstudent(reg_2RM2),type="p", cex=.5, ylim=c(-3,3))
abline(h=c(-2,2), col="red")

# matrice de confusion
table(predict_2RM > 0.5,data_2RM_f$MAISGLOB) # transformer les probabilites predites en variable binaire (> 0.5 ou < 0.5)

# courbe ROC
plot.roc(data_2RM_f$MAISGLOB, predict_2RM, print.auc = TRUE, legacy.axes = TRUE,
         ci = TRUE)


