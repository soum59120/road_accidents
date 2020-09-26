##  GRAVITE CORPORELLE DES ACCIDENTS DE LA ROUTE A PARIS/VERSAILLES (2015-2016) 
* Données disponibles dans le dossier "/data"
* Packages utiles à importer lister dans le fichier: "requirements.txt" 


#### **1. Nettoyage des données (/data_management)**
    - Lancer le script "nettoyage_donnees.py" pour le nettoyage des données
    - Lancer le script "creation_echantillon_vl_2rm.py" pour créer deux échantillons pour la modélisation : Véhicules Légers et 2 Roues Motorisées.


<!-- blank line -->
----
<!-- blank line -->


#### **2. Création des Modèles Mogistiques des Deux Echantillons: Véhicules Légers et 2 Roues Motorisées (/scrip_model)**
    - Lancer le script "/modelisation_gravite_corporelle.py" pour lancer les modèles logistiques
    - Lancer le script "/validation_model.py" pour lancer les critères d'évaluation de modèle 


<!-- blank line -->
----
<!-- blank line -->


#### **3. Déployement (sur Shiny) d'une Cartographie recensant la Gravité Corporelle lors d'un Accidents (/maps)**
    - Lancer le script "/preparation_data.py" pour préparer nos données (extraction des longitudes/latitudes) 
    - Lancer le script "/construction_map.py" pour la construction de l'application


## Disponible sur le lien suivant : https://elabboutisoumaya.shinyapps.io/MapTrafficAccidents/ 

Aperçu de l'application ci-joint : 
![Capture d’écran 2020-09-26 à 15 09 23](https://user-images.githubusercontent.com/47501144/94341578-b9b12780-000a-11eb-8e15-b79f11fb64f7.png)
![Capture d’écran 2020-09-24 à 17 48 28](https://user-images.githubusercontent.com/47501144/94169361-feb04f00-fe8e-11ea-82b4-233b2652f2f5.png)
