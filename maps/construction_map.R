##############################################
# CREATION D'UNE APPLICATION DEPLOYE SUR SHINY CONTENANT LA MAP
##############################################

#importer les packages
library(ggmap)
library(sp)
library(leaflet)
library(dplyr)
library(data.table)
library(reshape2)
library(shiny)
library(shinyBS)
library(DT)
library(shinydashboard)
library(leaflet)
library(shiny)


# Définir UI pour designer l'application
ui <- dashboardPage(skin = "black", # Haut de page de couleur noir
                    dashboardHeader(title = "Accidents de la Route à Paris/Versailles (2015-2016)",
                                    titleWidth = 600), # Choisir le titre de l'application

                                        dashboardSidebar(sidebarMenu( # Choix des onglets et définition des titres et icones pour les représenter
                      menuItem("Carte", tabName = "map_road", icon = icon("map")),
                      menuItem("Données utilisées", tabName = "tab_road", icon = icon("table"))
                    )),
                    
                    dashboardBody(
                      # Choix des couleurs et polices du titre de la page de l'application
                      tags$head(tags$style(HTML('
                                                  .main-header .logo {
                                                  font-family: "Georgia", Times, "Times New Roman", serif;
                                                  font-weight: bold;
                                                  font-size: 18px;}
                                                  '))),
                      
                      # Inclure les differents formats souhaites (box, fluidpage, etc) dans chaque onglet pré-défini
                      tabItems( #TabItems est un tableau qui rassemble tous les tabitem (onglet)
                        tabItem(tabName = "map_road", #clé qui relie au menuItem (onglet)
                                fluidRow(
                                  titlePanel(h4("Cartographique de la Gravité Corporelle des Accidents de la Route", align = "center")),
                                  leafletOutput("mymap", height = "600"),
                                )),
                        
                        tabItem(tabName = "tab_road",
                                fluidRow(
                                  titlePanel(h4("Données utilisées pour la construction de la carte (N = 543, données aggrégées)", align = "center")),
                                  DTOutput('table')
                                ))
                      )))



# Définir SERVER pour intégrer les données
server <- function(input, output, session) {
  load("/data/mapsfin.RData")
  
  # On initialise une palette de color, ici le choix = "magma"
  pal <- colorFactor(palette = "magma", domain = NULL)
  
  # Texte qui ira dans les étiquettes popup! L'argument popup accepte les entrées html. "<br>" désigne les sauts de ligne. 
  popup_label <- paste(
    "Adresse:", data_map$adresse, "<br>",
    "Victimes indemnes:", data_map$indemne, "<br>",
    "Victimes ayant des légères blessures:", data_map$`faible/modérée`, "<br>",
    "Victimes ayant de graves blessures/Décès:", data_map$`grave/mortelle`
  )
  
  # Creation de la map à partir de Leaflet
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      # You're adding circle markers here. In addition, we're specifying the color and popup labels for the markers. The tilde (~) is telling the color argument to accept the output of the pal object, which is now returning colors mapped to each species. Other plotting software like ggplot don't require a tilde to map to factors or numbers, so this may look foreign, but leaflet does and it just takes a little getting used to.
      addCircleMarkers(data = data_map,
                       color = ~pal(`fréquence accident`),
                       popup = popup_label) %>%
      # We're specifying the legend to be in the bottom left of the map. Colors are specified a little differently here. The "pal" argument specifies the palette being used and the "values" argument specifies the values to map to. We're using the tilde here so it knows that you're mapping to a factor.
      addLegend(
        data = data_map,
        "bottomleft",
        pal = pal,
        values = ~`fréquence accident`,
        opacity = .9,
        title = "Occurrence des Accidents"
      )
  })
  
  # Creation de la table qu'on projetera sur l'application
  output$table = renderDT(
    data_map, options = list(lengthChange = FALSE)
  )
}



# Deployement de l'application
shinyApp(ui, server)