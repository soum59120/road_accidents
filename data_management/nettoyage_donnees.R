
##############################################
# CHARGER DES PACKAGES
##############################################

# ouvrir les packages
library(dplyr)
library(stringi)
library(stringr)
library(textclean)
library(lubridate)
library(varhandle)
library(data.table)



##############################################
##        NETTOYAGE DES DONNEES
##############################################

# ouvrir le fichiers csv
setwd("/data")
initial_data_paris <- read.csv2('data_parisversailles_201712.csv',
                                encoding = 'latin1', 
                                na.strings=c("","NA"))

# variables à garder (pré-sélectionner par les accidentologues)
finaldata_paris <- initial_data_paris %>% 
  select('UNITE','DAACC','UAGE','USEXE','APPAR','TYPUS','NUSAG','UGRAV','TRHOP','UENFA',
         'DREAT','DCD','ESTGRAV','CRANE','MAISTEC','MAISFAC','MAISMEB','MAISTHO',
         'MAISABD','MAISPEAU','MAISGLOB','ISS','RTS','SCGLASGOW','PASSCORE','HEURD',
         'FRSCORE','TRISSF','TRISSP','PIIPARE','PIICAPO','PIITOIT','PIICOFF',
         'PIDISPR','PIPOSPLO','PIPOSPLA','PITOBST','PIAUTOB','PIFRANCH','PIAOBST',
         'PILEQUEL','UDPLACE','UDCASQUE','UDCINTEG','UDGANTS','UDBLOUS','UDPANTAL',
         'UDBOTTES','UDEQAUT','UDSOL','UDTROT','UDVEHIC','UDMUR','UDPOTEAU',
         'UDGLISS','UDANIMAL','UDAUTROB','UDAUTREOB','UDDISPRJ','DRINCEN','DRTYP2R', 
         'DRTYCHOC','DRCHUTE','DRIMMER','DRRAYURE','DRDEFORM','DRTYPOBS','DRAUTOBS',
         'DRPOSFI','DRDISTPDC','VLANCIE','VLMOTOR','VLINCEN','VLOBSPR','VLAUTOB', 
         'VLENCAS','VLOUTIL','VLCHOCFR','VLCHOCLG','VLCHOCLD','VLCHOCAR','VLCHOCTN',
         'VLCHOCAU','VLCHOCNR','UVCEINT','UVAIRBF','UVAIRBL','UVLOPDC','UVINTRU',
         'UVOBJLO','UVVPIEG','UVAVPIE','UVEJECT','UVAPDEC','UVPLACE',"A0","A1","A2",
         "A3","A4","B1","B2","B3","B4","P1","P2","P3","P4","P5","P6","P7","P8","P9"   
  )





##############################################
# 1. POUR USAGER
##############################################

# nettoyer la variable DATE 
finaldata_paris$DAACC <- ymd(finaldata_paris$DAACC)
finaldata_paris$DAACC <- format(as.Date(finaldata_paris$DAACC), "%Y") #garder seulement les années
table(finaldata_paris$DAACC)

# nettoyer la variable HEURE et couper en deux classes : nuit et jour
finaldata_paris$HEURD <- hm(finaldata_paris$HEURD)
finaldata_paris$HEURD <- finaldata_paris$HEURD@hour
finaldata_paris$DAY <- ifelse(finaldata_paris$HEURD<7 | 19<finaldata_paris$HEURD, # heures entre 7h et 19h = jour, et encore 19h et 7h = nuit
                                 "Night","Day")

# nettoyer la variable SEXE avec 1. homme et 2. femme
finaldata_paris$USEXE <- gsub(0,NA,finaldata_paris$USEXE) #remplacer les 0 qui représentent les Non Renseigné en NA
finaldata_paris[is.na(finaldata_paris$USEXE),"USEXE"] <- 'NR' 
finaldata_paris$USEXE[finaldata_paris$USEXE == "1"] <- "Homme"
finaldata_paris$USEXE[finaldata_paris$USEXE == "2"] <- "Femme"
table(finaldata_paris$USEXE)

# nettoyer la variable GRAVITE avec 1. indemne, 2. décédé et 3. blessé
finaldata_paris$UGRAV <- gsub(9,'3',finaldata_paris$UGRAV) #remplacer les 9 représentant les NonRenseigné en 'blessé'
finaldata_paris[is.na(finaldata_paris$UGRAV),"UGRAV"] <- '3' #remplacer les NA en 'blessé' étant la modalité la + représentée 
table(finaldata_paris$UGRAV)

# nettoyer la variable TYPUS avec 1. piéton, 2. vélo, 3. 2rm, 4. VL (voiture légère), 5. PL (poids lourds), 6. Quad
finaldata_paris$TYPUS <- gsub(1,'Pieton',finaldata_paris$TYPUS)
finaldata_paris$TYPUS <- gsub(2,'Velo',finaldata_paris$TYPUS) 
finaldata_paris$TYPUS <- gsub(3,'2RM',finaldata_paris$TYPUS) 
finaldata_paris$TYPUS <- gsub(4,'VL',finaldata_paris$TYPUS) 
finaldata_paris$TYPUS <- gsub(5,'PL',finaldata_paris$TYPUS) 
finaldata_paris$TYPUS <- gsub(6,'Quad',finaldata_paris$TYPUS)
table(finaldata_paris$TYPUS)

# nettoyer la variable UAGE
table(finaldata_paris$UAGE)
finaldata_paris$UAGE <- gsub('NR',NA,finaldata_paris$UAGE) #remplacer les NR en NA
finaldata_paris$UAGE <- as.numeric(finaldata_paris$UAGE) #convertir en chiffre
finaldata_paris[is.na(finaldata_paris$UAGE),]$UAGE <- median(finaldata_paris$UAGE, na.rm = T) # remplacer valeurs manquantes par la médiane
summary(finaldata_paris$UAGE, na.rm = T)

# blessure au crâne
finaldata_paris$CRANE <- gsub(0,'Non',finaldata_paris$CRANE) #remplacer les 0 en NON
finaldata_paris$CRANE <- gsub(1,'Oui',finaldata_paris$CRANE) #remplacer les 1 en OUI
table(finaldata_paris$CRANE)

# transport à l'hôpital
finaldata_paris$TRHOP <- gsub(1,'Oui',finaldata_paris$TRHOP) #remplacer les 0 en OUI
finaldata_paris$TRHOP <- gsub(2,'Non',finaldata_paris$TRHOP) #remplacer les 1 en NON
table(finaldata_paris$TRHOP)

# nettoyer la variable ESTGRAVE (estimation gravité des blessures sur 10 par le 1er médecin) 
# remplacer les notes entre 1 et 10 par la modalités : ">0"
finaldata_paris$ESTGRAV[finaldata_paris$ESTGRAV == "10"] <- ">0"
finaldata_paris$ESTGRAV[finaldata_paris$ESTGRAV == "1"] <- ">0"
finaldata_paris$ESTGRAV[finaldata_paris$ESTGRAV == "5"] <- ">0"
table(finaldata_paris$ESTGRAV)

# si enfant a -12ans 
finaldata_paris$UENFA <- gsub(1,'Oui',finaldata_paris$UENFA) #remplacer les 1 en OUI
finaldata_paris$UENFA <- gsub(2,'Non',finaldata_paris$UENFA) #remplacer les 2 en NON
table(finaldata_paris$UENFA)

# apparence physique : couper en 4 classes = maigre, normale, surpoids et autre
finaldata_paris$APPAR <- gsub(1,'Maigre',finaldata_paris$APPAR) 
finaldata_paris$APPAR <- gsub(2,'Normale',finaldata_paris$APPAR)
finaldata_paris$APPAR <- gsub(3,'Surpoids',finaldata_paris$APPAR)
finaldata_paris$APPAR <- mgsub(finaldata_paris$APPAR,c(4,9),"Autre")
table(finaldata_paris$APPAR)





##############################################
# 2. MAIS
##############################################
# L’indice MAIS (Maximum Abbreviated Injury Scale) représente l’indice AIS maximal, 
#il reprend l’indice AIS pour la blessure la plus grave. 

# MAISTEC (tête + cou)
table(finaldata_paris$MAISTEC)
# MAISFAC (face)
table(finaldata_paris$MAISFAC)
# MAISMEB (membres + bassin)
table(finaldata_paris$MAISMEB)
# MAISTHO (thorax)
table(finaldata_paris$MAISTHO)
# MAISABD (abdomen + pelvis)
table(finaldata_paris$MAISABD)
# MAISPEAU (peau + tissus sous cutanés)
table(finaldata_paris$MAISPEAU)
# MAISGLOB (global)
table(finaldata_paris$MAISGLOB)
# remplacer les valeurs de l'indice MAIS par 7 pour les décédés
finaldata_paris$MAISGLOB[finaldata_paris$DCD == "1"] <- "7"




##############################################
# 3. POUR VEHICULES LEGERS (VL)
##############################################
data_vl <- finaldata_paris %>% filter(TYPUS == 'VL') %>% 
  select('MAISGLOB','CRANE','UAGE','TRHOP','ESTGRAV','UENFA','APPAR','UVAVPIE','UVPLACE',
         'UVVPIEG', 'UVOBJLO', 'UVINTRU', 'UVLOPDC', 'UVAIRBL', 'UVAIRBF', 'UVCEINT','VLOBSPR',
         'VLCHOCAU', 'VLCHOCNR', 'VLCHOCTN', 'VLCHOCAR', 'VLCHOCLD', 'VLCHOCLG', 'VLCHOCFR',
         'VLOUTIL', 'VLENCAS', 'VLMOTOR', 'VLANCIE','DAY')


# information globale concernant l'USAGER
table(data_vl$CRANE)
table(data_vl$TRHOP)
table(data_vl$UENFA)
table(data_vl$APPAR)

# créer des classes d'âges
setDT(data_vl)[UAGE <= 18, CLASSE_AGE := "<19"]
data_vl[UAGE >= 19 & UAGE <= 25, CLASSE_AGE := "[19-25]"]
data_vl[UAGE >= 26 & UAGE <= 35, CLASSE_AGE := "[26-35]"]
data_vl[UAGE >= 36 & UAGE <= 45, CLASSE_AGE := "[36-45]"]
data_vl[UAGE >= 46 & UAGE <= 60, CLASSE_AGE := "[46-60]"]
data_vl[UAGE >= 61, CLASSE_AGE := ">60"]
table(data_vl$CLASSE_AGE)



# VARIABLES LIEES AUX USAGERS VL

#autre victime piégée dans le véhicule
data_vl$UVAVPIE <- mgsub(data_vl$UVAVPIE,c(255,9,'NR'),"Non") #remplacer les 255, NonRenseigné et 9 en Non
data_vl$UVAVPIE <- gsub("1","Oui",data_vl$UVAVPIE)
data_vl$UVAVPIE <- gsub("2","Non",data_vl$UVAVPIE)
table(data_vl$UVAVPIE) 

#place occupée
data_vl$UVPLACE[data_vl$UVPLACE == 1] <- "Conducteur"
data_vl$UVPLACE[data_vl$UVPLACE == 2] <- "Passager_AD" # Passager_AD = passager avant droit
data_vl$UVPLACE <- mgsub(data_vl$UVPLACE,c(3:9,'NR'),"Autre") #remplacer les NonRenseigné et 3:9 en Autre
table(data_vl$UVPLACE) 

#victime piégée dans le véhicule ?
data_vl$UVVPIEG <- mgsub(data_vl$UVVPIEG,c(255,9,'NR'),"Non") #remplacer les 255 et 9 en Non
data_vl$UVVPIEG <- gsub("1","Oui",data_vl$UVVPIEG)
data_vl$UVVPIEG <- gsub("2","Non",data_vl$UVVPIEG)
table(data_vl$UVVPIEG) 

#la victime a-t-elle subi une charge par un objet lourd ?
data_vl$UVOBJLO <- mgsub(data_vl$UVOBJLO,c(255,9,'NR'),"Non") #remplacer les 255, NonRenseigné et 9 en Non
data_vl$UVOBJLO <- gsub("1","Oui",data_vl$UVOBJLO)
data_vl$UVOBJLO <- gsub("2","Non",data_vl$UVOBJLO)
table(data_vl$UVOBJLO) 

#intrusion sur occupant
data_vl$UVINTRU <- mgsub(data_vl$UVINTRU,c(255,9,'NR'),"Non") 
data_vl$UVINTRU <- gsub("1","Oui",data_vl$UVINTRU)
data_vl$UVINTRU <- gsub("2","Non",data_vl$UVINTRU)
table(data_vl$UVINTRU) 

#localisation du choc/occupant
data_vl$UVLOPDC <- mgsub(data_vl$UVLOPDC,c(255,9,'NR'),"Autre") 
data_vl$UVLOPDC <- gsub("1","Adjacent",data_vl$UVLOPDC)
data_vl$UVLOPDC <- gsub("2","Distant",data_vl$UVLOPDC)
table(data_vl$UVLOPDC) 

#airbag latéral
data_vl$UVAIRBL <- mgsub(data_vl$UVAIRBL,c(255,9,'-1','NR'),"Autre") #remplacer les 255,-1,9 en Autre
data_vl$UVAIRBL <- gsub("1","Oui_depl",data_vl$UVAIRBL) # depl = deployé
data_vl$UVAIRBL <- gsub("2","Oui_ndepl",data_vl$UVAIRBL) # ndepl = non_déployé
data_vl$UVAIRBL <- gsub("3","Non",data_vl$UVAIRBL)
table(data_vl$UVAIRBL) 

#airbag frontal
data_vl$UVAIRBF <- mgsub(data_vl$UVAIRBF,c(255,9,'-1','NR'),"Autre") #remplacer les 255,-1,9 en Autre
data_vl$UVAIRBF <- gsub("1","Oui_depl",data_vl$UVAIRBF)
data_vl$UVAIRBF <- gsub("2","Oui_ndepl",data_vl$UVAIRBF)
data_vl$UVAIRBF <- gsub("3","Non",data_vl$UVAIRBF)
table(data_vl$UVAIRBF) 

#personne ceinturée
data_vl$UVCEINT <- mgsub(data_vl$UVCEINT,c(255,9,'NR'),"Oui") #remplacer les 255, NonRenseigné et 9 en Autre
data_vl$UVCEINT <- gsub("1","Oui",data_vl$UVCEINT)
data_vl$UVCEINT <- gsub("2","Non",data_vl$UVCEINT)
table(data_vl$UVCEINT) 



# VARIABLES LIEES AUX VL

#aucune déformation
data_vl$VLCHOCAU <- gsub("NR","Non",data_vl$VLCHOCAU)
data_vl$VLCHOCAU <- gsub("0","Non",data_vl$VLCHOCAU)
data_vl$VLCHOCAU <- gsub("1","Oui",data_vl$VLCHOCAU)
table(data_vl$VLCHOCAU) 

#choc non renseigné
data_vl$VLCHOCNR <- gsub("NR","Non",data_vl$VLCHOCNR)
data_vl$VLCHOCNR <- gsub("0","Non",data_vl$VLCHOCNR)
data_vl$VLCHOCNR <- gsub("1","Oui",data_vl$VLCHOCNR)
table(data_vl$VLCHOCNR)

#tonneau ou renversement
data_vl$VLCHOCTN <- gsub("NR","Non",data_vl$VLCHOCTN)
data_vl$VLCHOCTN <- gsub("0","Non",data_vl$VLCHOCTN)
data_vl$VLCHOCTN <- gsub("1","Oui",data_vl$VLCHOCTN)
table(data_vl$VLCHOCTN) 

#choc arrière
data_vl$VLCHOCAR <- gsub("NR","Non",data_vl$VLCHOCAR)
data_vl$VLCHOCAR <- gsub("0","Non",data_vl$VLCHOCAR)
data_vl$VLCHOCAR <- gsub("1","Oui",data_vl$VLCHOCAR)
table(data_vl$VLCHOCAR)

#choc latéral droit
data_vl$VLCHOCLD <- gsub("NR","Non",data_vl$VLCHOCLD)
data_vl$VLCHOCLD <- gsub("0","Non",data_vl$VLCHOCLD)
data_vl$VLCHOCLD <- gsub("1","Oui",data_vl$VLCHOCLD)
table(data_vl$VLCHOCLD) 

#choc latéral gauche
data_vl$VLCHOCLG <- gsub("NR","Non",data_vl$VLCHOCLG)
data_vl$VLCHOCLG <- gsub("0","Non",data_vl$VLCHOCLG)
data_vl$VLCHOCLG <- gsub("1","Oui",data_vl$VLCHOCLG)
table(data_vl$VLCHOCLG)

#choc frontal
data_vl$VLCHOCFR <- gsub("NR","Non",data_vl$VLCHOCFR)
data_vl$VLCHOCFR <- gsub("0","Non",data_vl$VLCHOCFR)
data_vl$VLCHOCFR <- gsub("1","Oui",data_vl$VLCHOCFR)
table(data_vl$VLCHOCFR) 

#utilisation d'outils?
data_vl$VLOUTIL <- mgsub(data_vl$VLOUTIL,c(255,9,'NR'),"Non") #remplacer les 255, NonRenseigné et 9 en Non car modalité plus représentée
data_vl$VLOUTIL <- gsub("1","Oui",data_vl$VLOUTIL)
data_vl$VLOUTIL <- gsub("2","Non",data_vl$VLOUTIL)
table(data_vl$VLOUTIL) 

# présence d'encastrement?
data_vl$VLENCAS <- mgsub(data_vl$VLENCAS,c(255,9,'NR'),"Non") #remplacer les 255, NonRenseigné et 9 en Non car modalité plus représentée
data_vl$VLENCAS <- gsub("1","Oui",data_vl$VLENCAS)
data_vl$VLENCAS <- gsub("2","Non",data_vl$VLENCAS)
table(data_vl$VLENCAS) 

#type de motorisation
data_vl$VLMOTOR <- mgsub(data_vl$VLMOTOR,c(255,9,2,3,'NR'),"Autre") #remplacer les 255, 2,3,9 et NonRenseigné en Autre
data_vl$VLMOTOR <- gsub("1","Thermique",data_vl$VLMOTOR)
table(data_vl$VLMOTOR) 

#ancienneté
data_vl$VLANCIE <- mgsub(data_vl$VLANCIE,c(255,9,'NR'),"Autre") #remplacer les 255, NonRenseigné et 9 en Autre
data_vl$VLANCIE <- gsub("1","Ancien",data_vl$VLANCIE)
data_vl$VLANCIE <- gsub("2","Recent",data_vl$VLANCIE)
table(data_vl$VLANCIE)





##############################################
# 4. POUR 2 ROUES MOTORISES
##############################################
data_2RM <- finaldata_paris %>% filter(TYPUS == '2RM') %>%
  select('MAISGLOB','UAGE','CRANE','TRHOP','ESTGRAV','APPAR','UENFA','UDPOTEAU',
         'UDVEHIC','UDTROT','UDSOL','UDBOTTES','UDPANTAL','UDBLOUS','UDGANTS',
         'UDCINTEG','UDCASQUE','UDPLACE','DRPOSFI', 'DRTYPOBS', 'DRDEFORM', 'DRRAYURE', 
         'DRCHUTE','DRTYCHOC')

# information globale concernant l'USAGER
table(data_2RM$CRANE)
table(data_2RM$TRHOP)
table(data_2RM$APPAR)

# créer des classes d'âges
setDT(data_2RM)[UAGE <= 18, CLASSE_AGE := "<19"] #moinsde18
data_2RM[UAGE >= 19 & UAGE <= 25, CLASSE_AGE := "[19-25]"] #19-25ans
data_2RM[UAGE >= 26 & UAGE <= 35, CLASSE_AGE := "[26-35]"] #26-35ans
data_2RM[UAGE >= 36 & UAGE <= 45, CLASSE_AGE := "[36-45]"] #36-45ans
data_2RM[UAGE >= 46, CLASSE_AGE := ">45"] #+45ans
table(data_2RM$CLASSE_AGE)


# VARIABLES LIEES AUX USAGERS 2RM

#heurt sur poteau/arbre ?
data_2RM$UDPOTEAU <- gsub('0',"Non",data_2RM$UDPOTEAU)
data_2RM$UDPOTEAU <- gsub("1","Oui",data_2RM$UDPOTEAU)
data_2RM$UDPOTEAU <- gsub('NR',"Non",data_2RM$UDPOTEAU)
table(data_2RM$UDPOTEAU) 

#choc avec un vehicule ?
data_2RM$UDVEHIC <- gsub('0',"Non",data_2RM$UDVEHIC)
data_2RM$UDVEHIC <- gsub("1","Oui",data_2RM$UDVEHIC)
data_2RM$UDVEHIC <- gsub('NR',"Oui",data_2RM$UDVEHIC)
table(data_2RM$UDVEHIC) 
 
#choc avec le trottoir?
data_2RM$UDTROT <- gsub('0',"Non",data_2RM$UDTROT)
data_2RM$UDTROT <- gsub("1","Oui",data_2RM$UDTROT)
data_2RM$UDTROT <- gsub('NR',"Oui",data_2RM$UDTROT)
table(data_2RM$UDTROT) 

#heurt le sol ?
data_2RM$UDSOL <- gsub('0',"Non",data_2RM$UDSOL)
data_2RM$UDSOL <- gsub("1","Oui",data_2RM$UDSOL)
data_2RM$UDSOL <- gsub('NR',"Oui",data_2RM$UDSOL)
table(data_2RM$UDSOL) 

#port de bottes moto
data_2RM$UDBOTTES <- mgsub(data_2RM$UDBOTTES,c(9,'NR'),"Non") 
data_2RM$UDBOTTES <- gsub('2',"Non",data_2RM$UDBOTTES)
data_2RM$UDBOTTES <- gsub("1","Oui",data_2RM$UDBOTTES)
table(data_2RM$UDBOTTES) 

#port de pantalon moto
data_2RM$UDPANTAL <- mgsub(data_2RM$UDPANTAL,c(9,'NR'),"Non")
data_2RM$UDPANTAL <- gsub('2',"Non",data_2RM$UDPANTAL)
data_2RM$UDPANTAL <- gsub("1","Oui",data_2RM$UDPANTAL)
table(data_2RM$UDPANTAL)

#port d'un blouson moto
data_2RM$UDBLOUS <- mgsub(data_2RM$UDBLOUS,c(9,'NR'),"Non")
data_2RM$UDBLOUS <- gsub('2',"Non",data_2RM$UDBLOUS)
data_2RM$UDBLOUS <- gsub("1","Oui",data_2RM$UDBLOUS)
table(data_2RM$UDBLOUS) 

#port de gants
data_2RM$UDGANTS <- mgsub(data_2RM$UDGANTS,c(9,'NR'),"Oui") 
data_2RM$UDGANTS <- gsub('2',"Non",data_2RM$UDGANTS)
data_2RM$UDGANTS <- gsub("1","Oui",data_2RM$UDGANTS) 
table(data_2RM$UDGANTS) 

#casque intégral
data_2RM$UDCINTEG <- mgsub(data_2RM$UDCINTEG,c(9,'NR'),"Oui") 
data_2RM$UDCINTEG <- mgsub(data_2RM$UDCINTEG,c("2","0"),"Non") 
data_2RM$UDCINTEG <- gsub("1","Oui",data_2RM$UDCINTEG) 
table(data_2RM$UDCINTEG) 

#port d'un casque
data_2RM$UDCASQUE <- mgsub(data_2RM$UDCASQUE,c(3,9,'NR'),"Oui") 
data_2RM$UDCASQUE <- gsub("2","Non",data_2RM$UDCASQUE)
data_2RM$UDCASQUE <- gsub("1","Oui",data_2RM$UDCASQUE)
table(data_2RM$UDCASQUE)

#place occupée
data_2RM$UDPLACE <- mgsub(data_2RM$UDPLACE,c('-1',4,'NR'),"Conducteur") 
data_2RM$UDPLACE <- gsub("1","Conducteur",data_2RM$UDPLACE)
data_2RM$UDPLACE <- gsub("2","Autre",data_2RM$UDPLACE)
table(data_2RM$UDPLACE) 



# VARIABLES LIEES AUX 2RM 

#position finale du véhicule
data_2RM$DRPOSFI <- mgsub(data_2RM$DRPOSFI,c(3,4,9,'-1','NR'),"Autre") 
data_2RM$DRPOSFI <- gsub("1","CG",data_2RM$DRPOSFI) # CG = côté gauche
data_2RM$DRPOSFI <- gsub("2","CD",data_2RM$DRPOSFI) # CD = côté droit 
table(data_2RM$DRPOSFI) 

#type d'obstacle principal
data_2RM$DRTYPOBS <- mgsub(data_2RM$DRTYPOBS,c(1:2,4:10,'-1','NR'),"Autre") 
data_2RM$DRTYPOBS <- gsub("3","VL",data_2RM$DRTYPOBS) # 3 = véhicule léger
table(data_2RM$DRTYPOBS) 

#déformation
data_2RM$DRDEFORM <- mgsub(data_2RM$DRDEFORM,c(9,'-1','NR'),'Autre') 
data_2RM$DRDEFORM <- gsub("0","Aucune",data_2RM$DRDEFORM) 
data_2RM$DRDEFORM <- gsub("1","Avant",data_2RM$DRDEFORM) 
data_2RM$DRDEFORM <- mgsub(data_2RM$DRDEFORM,c(2:4),'Partout') 
table(data_2RM$DRDEFORM) 

#présence de rayures ?
data_2RM$DRRAYURE <- gsub("NR","Autre",data_2RM$DRRAYURE)
data_2RM$DRRAYURE <- gsub("0","Non",data_2RM$DRRAYURE)
data_2RM$DRRAYURE <- gsub("1","Oui",data_2RM$DRRAYURE)
table(data_2RM$DRRAYURE) 

#type de choc
data_2RM$DRTYCHOC <- mgsub(data_2RM$DRTYCHOC,c(9,255,'NR'),'Autre') 
data_2RM$DRTYCHOC <- gsub("1","Frontal",data_2RM$DRTYCHOC) 
data_2RM$DRTYCHOC <- gsub("2","Lateral",data_2RM$DRTYCHOC) 
data_2RM$DRTYCHOC <- gsub("3","Arriere",data_2RM$DRTYCHOC) 
table(data_2RM$DRTYCHOC) 

