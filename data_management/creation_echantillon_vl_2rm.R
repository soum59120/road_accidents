##############################################
# CREATION D'ECHANTILLON D'USAGERS (VL et 2RM) POUR LA MODELISATION 
##############################################

# on garde les usagers transportés à l'hôpital et dont l'indice MAIS > 0
# remarque : on ne garde pas les autres car incohérents (pas transport hôpital et MAIS > 0)


#### VEHICULES LEGERS
table(data_vl$TRHOP, data_vl$MAISGLOB) #croisement variable 'transport hôpital' et indice MAIS
data_vl_f <- data_vl %>% 
  filter(TRHOP == 'Oui' & MAISGLOB > 0)

#### 2 ROUES MOTORISEES
table(data_2RM$TRHOP, data_2RM$MAISGLOB) #croisement variable 'transport hôpital' et indice MAIS
data_2RM_f <- data_2RM %>% 
  filter(TRHOP == 'Oui' & MAISGLOB > 0)



##############################################
# RECODAGE : CHOIX DE FIXER L'INDICE MAIS >=3 et <3 
#(pour modéliser la gravité corporelle des personnes accidentées de la route)
##############################################

####  VEHICULES LEGERS
data_vl_f$MAISGLOB <- mgsub(data_vl_f$MAISGLOB,c(1:2),0) #la modalité "0" représente l'indice MAIS < 3: gravité mineure/modérée
data_vl_f$MAISGLOB <- mgsub(data_vl_f$MAISGLOB,c(3:7),1) #la modalité "1" représente l'indice MAIS >= 3: gravité sévère
table(data_vl_f$MAISGLOB)

####  2 ROUES MOTORISEES
data_2RM_f$MAISGLOB <- mgsub(data_2RM_f$MAISGLOB,c(1:2),0) #la modalité "0" représente l'indice MAIS < 3: gravité mineure/modérée
data_2RM_f$MAISGLOB <- mgsub(data_2RM_f$MAISGLOB,c(3:7),1) #la modalité "1" représente l'indice MAIS >= 3: gravité sévère
table(data_2RM_f$MAISGLOB)



##############################################
# RECODAGE ET CHOIX DE MODALITES DE REFERENCE
##############################################


####  VEHICULES LEGERS

# convertir la variable à prédire en facteur
data_vl_f$MAISGLOB <- as.factor(data_vl_f$MAISGLOB)
# convertir toutes les colonnes en facteur
data_vl_f[,2:length(data_vl_f)] <- lapply(data_vl_f[,2:length(data_vl_f)], 
                                 function(x) as.factor(as.character(x)))

# modification des modalités de référence des variables à utiliser pour la modélisation
data_vl_f$CRANE <- relevel(data_vl_f$CRANE, ref = "Non")
data_vl_f$CLASSE_AGE <- relevel(data_vl_f$CLASSE_AGE, ref = "<19")
data_vl_f$UENFA <- relevel(data_vl_f$UENFA, ref = "Oui")
data_vl_f$APPAR <- relevel(data_vl_f$APPAR, ref = "Autre")
data_vl_f$UVAVPIE <- relevel(data_vl_f$UVAVPIE, ref = "Oui")
data_vl_f$UVPLACE <- relevel(data_vl_f$UVPLACE, ref = "Conducteur")
data_vl_f$UVVPIEG <- relevel(data_vl_f$UVVPIEG, ref = "Oui")
data_vl_f$UVOBJLO <- relevel(data_vl_f$UVOBJLO, ref = "Oui")
data_vl_f$UVINTRU <- relevel(data_vl_f$UVINTRU, ref = "Oui")
data_vl_f$UVLOPDC <- relevel(data_vl_f$UVLOPDC, ref = "Adjacent")
data_vl_f$UVAIRBL <- relevel(data_vl_f$UVAIRBL, ref = "Oui_depl")
data_vl_f$UVAIRBF <- relevel(data_vl_f$UVAIRBF, ref = "Oui_depl")
data_vl_f$UVCEINT <- relevel(data_vl_f$UVCEINT, ref = "Oui")
data_vl_f$VLOUTIL <- relevel(data_vl_f$VLOUTIL, ref = "Oui")
data_vl_f$VLENCAS <- relevel(data_vl_f$VLENCAS, ref = "Oui")
data_vl_f$VLMOTOR <- relevel(data_vl_f$VLMOTOR, ref = "Thermique")
data_vl_f$VLANCIE <- relevel(data_vl_f$VLANCIE, ref = "Ancien")

# garder les variables finales (enlever celles à une modalité)
data_vl_f <- data_vl_f[,!names(data_vl_f) %in% c('ESTGRAV','TRHOP','VLOBSPR','VLCHOCAU','UAGE')] #ESTGRAV, TRHOP, VLOBSPR, VLCHOCAU : modalité unique, VLOBSPR (pas connaissance modalité)
data_vl_f <- na.omit(data_vl_f)




####  VEHICULES 2RM

# convertir la variable à prédire en facteur
data_2RM_f$MAISGLOB <- as.factor(data_2RM_f$MAISGLOB)
## convertir tous les colonnes en numérique
data_2RM_f[,2:length(data_2RM_f)] <- lapply(data_2RM_f[,2:length(data_2RM_f)], 
                                          function(x) as.factor(as.character(x)))

# modification des modalités de référence des variables à utiliser pour la modélisation
data_2RM_f$CRANE <- relevel(data_2RM_f$CRANE, ref = "Non")
data_2RM_f$CLASSE_AGE <- relevel(data_2RM_f$CLASSE_AGE, ref = "<19")
data_2RM_f$APPAR <- relevel(data_2RM_f$APPAR, ref = "Maigre")
data_2RM_f$DRPOSFI <- relevel(data_2RM_f$DRPOSFI, ref = "CG")
data_2RM_f$DRTYPOBS <- relevel(data_2RM_f$DRTYPOBS, ref = "VL")
data_2RM_f$DRTYCHOC <- relevel(data_2RM_f$DRTYCHOC, ref = "Frontal")
data_2RM_f$UDBOTTES <- relevel(data_2RM_f$UDBOTTES, ref = "Oui")
data_2RM_f$UDPANTAL <- relevel(data_2RM_f$UDPANTAL, ref = "Oui")
data_2RM_f$UDBLOUS <- relevel(data_2RM_f$UDBLOUS, ref = "Oui")
data_2RM_f$UDGANTS <- relevel(data_2RM_f$UDGANTS, ref = "Oui")
data_2RM_f$UDCINTEG <- relevel(data_2RM_f$UDCINTEG, ref = "Oui")
data_2RM_f$UDCASQUE <- relevel(data_2RM_f$UDCASQUE, ref = "Oui")
data_2RM_f$UDPLACE <- relevel(data_2RM_f$UDPLACE, ref = "Conducteur")

# garder les variables finales (enlever celles à une modalité)
data_2RM_f <- data_2RM_f[,!names(data_2RM_f) %in% c('TRHOP','ESTGRAV','UENFA','DRCHUTE','UAGE')] #TRHOP, ESTGRAV, UENFAN (modalité unique),DRCHUTE a supprimer
data_2RM_f <- na.omit(data_2RM_f)


