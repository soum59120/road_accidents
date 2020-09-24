##############################################
# PREPARATION DES DONNEES POUR LA CREATION D'UNE MAP
# (utilisation API GOOGLE MAPS)
##############################################

# importer les packages
library(ggmap)
library(sp)
library(leaflet)
library(dplyr)
library(stringi)
library(stringr)
library(textclean)
library(lubridate)
library(varhandle)
library(data.table)
library(reshape2)
library(shiny)
library(shinyBS)
library(DT)

##############################################
# Extraire les adresses dans notre base de données
##############################################

# Ouvrir les données et nettoyer
initial_data_paris <- read.csv2('/data/data_parisversailles_201712.csv',
                                encoding = 'latin1', 
                                na.strings=c("","NA"))
origAddress <- initial_data_paris[,c('NUMUSA.x','LIACC')] # garder les variables d'intérêt : NUMUSA (code unique) et les adresses (LIACC)
names(origAddress)[2] <- 'addresses' # renommer la colonne
origAddress$addresses <- as.character(origAddress$addresses) # transformer la colonne en charactère
origAddress <- na.omit(origAddress) # supprimer les lignes avec des NA (sans adresses)
origAddress$addresses <- paste(origAddress$addresses, "france", sep=" ") # préciser le lieu pour que l'API géolocalise + facilement



##############################################
# Création d'une boucle à partir des adresses pour 
# avoir les latitudes et longitudes de chaque adresse
# puis les ajouter dans notre dataframe origAddress
##############################################

# clé de l'API google maps et boucle. Table finale contiendra : addresses, longitude, latitude, geoAdress (adresse reconnue par l'API)
register_google (key = "AIzaSyDgP9t9uoSly8zXzx3B2mCmK7f0p_XvAHs") 
for(i in 1:nrow(origAddress)){
  result <- geocode(origAddress$addresses[i], output = "latlona", source = "google")
  origAddress$lon[i] <- ifelse(is.na(as.numeric(result[1])), NA, as.numeric(result[1]))
  origAddress$lat[i] <- ifelse(is.na(as.numeric(result[2])), NA, as.numeric(result[2]))
  origAddress$geoAddress[i] <- ifelse("address" %in% colnames(result), as.character(result[3]),NA)
}



##############################################
# Joindre les données pour avoir la gravité 
# de l'accident à partir de l'indice MAIS
##############################################

# Grouper l'indice MAIS en 3 classes : indemne (MAIS = 0), faible/modérée (MAIS = 1 et 2), grave/mortelle (MAIS = [3:7])
initial_data_paris$MAISGLOB <- mgsub(initial_data_paris$MAISGLOB,0,"indemne")
initial_data_paris$MAISGLOB <- mgsub(initial_data_paris$MAISGLOB,c(1:2),"faible/modérée")
initial_data_paris$MAISGLOB <- mgsub(initial_data_paris$MAISGLOB,c(3:7),"grave/mortelle") 
initial_data_paris <- initial_data_paris[,c('NUMUSA.x','MAISGLOB')] # garder seulement la variable MAISGLOB et la colonne unique 'NUMUSA.x'

# Joindre les deux tables contenant les adresses avec latitudes/longitudes ainsi que la table contenant l'indice MAIS recodé
origAddress <- left_join(origAddress, initial_data_paris, by = "NUMUSA.x")



##############################################
# Nettoyer notre dataframe pour créer la MAP
##############################################

# Créer une colonne pour avoir de façon unique les adresses et voir la fréquence des adresses
origAddress$geoAddress1 <- paste(origAddress$geoAddress, origAddress$lon, origAddress$lat, sep=" ")
t1 <- data.frame(table(origAddress$geoAddress1))
colnames(t1)[1] <- 'geoAddress1'

# Transformer la colonne 'MAISGLOB' contenant les 3 classes en colonne. 
# Ainsi, pour chaque adresse et classe, nous comptons le nombre de cas
t2 <- reshape2::dcast(reshape2::melt(origAddress), geoAddress1 ~ MAISGLOB)
t2[,c("faible/modérée","grave/mortelle","indemne")] <- t2[,c("faible/modérée","grave/mortelle","indemne")]/2
colnames(t2)[1] <- 'geoAddress1'

# Joindre les deux tables : une contenant la fréquence d'accident et l'autre le nombre pour chaque classe de gravité corporelle par lieu
t1 <- left_join(t1,t2, by = "geoAddress1")
t1 <- left_join(t1,origAddress[,c('geoAddress','geoAddress1','lon','lat')], by = "geoAddress1")
t1 <- t1[!duplicated(t1),] # enlever les doublons
data_map <- t1 %>% select(geoAddress,Freq,"faible/modérée","grave/mortelle","indemne",lon,lat) # garder les variables voulues
rm(t1,t2,result,i) # supprimer les variables inutilisables

# Recoder la fréquence d'accident en 4 classes : rare/faible, peu fréquent, très fréquent et important
data_map$Freq1[data_map$Freq %in% c(1:2)] <- 'rare/faible'
data_map$Freq1[data_map$Freq %in% c(3:9)] <- 'peu fréquent'
data_map$Freq1[data_map$Freq %in% c(10:30)] <- 'très fréquent'
data_map$Freq1[data_map$Freq %in% c(31:110)] <- 'important'
data_map$Freq1 <- as.factor(data_map$Freq1) # transformer en facteur
data_map$Freq1 <- ordered(data_map$Freq1, levels = c("important", "très fréquent", "peu fréquent", "rare/faible")) # ré ordonner les facteurs

# Avoir le taux de gravité corporelle par adresse en pourcentage
data_map$`faible/modérée` <- paste(round(data_map$`faible/modérée`/data_map$Freq, 2)*100,'%')
data_map$`grave/mortelle` <- paste(round(data_map$`grave/mortelle`/data_map$Freq, 2)*100,'%')
data_map$indemne <- paste(round(data_map$indemne/data_map$Freq, 2)*100,'%')

# Renommer les colonnes
names(data_map)[names(data_map) == "geoAddress"] <- "adresse"
names(data_map)[names(data_map) == "Freq"] <- "nombre accident"
names(data_map)[names(data_map) == "Freq1"] <- "fréquence accident"
data_map <- data_map %>% select(adresses, `nombre accident`, `fréquence accident`, indemne, `faible/modérée`, `grave/mortelle`, lon,lat)
rownames(data_map) <- c(1:543) # rechiffrer les lignes

# Sauvegarder les résultats
save(data_map, file = "data_map.RData")

