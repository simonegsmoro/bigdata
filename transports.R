library(leaflet)
library(magrittr)
library(sp)
library(leaflet.extras)
library(leaflet.extras2)
library(htmlwidgets)

bcn <- read.csv(file = 'TRANSPORTS.csv')

newcol <- c()
for(i in 1:dim(bcn)[1]){
  if(grepl('Estació marítima', bcn$NOM_CAPA[i], fixed = TRUE)){
    newcol <- c(newcol,'Estació marítima')
  }
  else if(grepl('Ferrocarrils Generalitat (FGC)', bcn$NOM_CAPA[i], fixed = TRUE)){
    newcol <- c(newcol,'Ferrocarrils Generalitat (FGC)')
  }
  else if(grepl('Funicular', bcn$NOM_CAPA[i], fixed = TRUE)){
    newcol <- c(newcol,'Funicular')
  }
  else if(grepl('Metro i línies urbanes FGC', bcn$NOM_CAPA[i], fixed = TRUE)){
    newcol <- c(newcol,'Metro i línies urbanes FGC')
  }
  else if(grepl('RENFE', bcn$NOM_CAPA[i], fixed = TRUE)){
    newcol <- c(newcol,'RENFE')
  }
  else if(grepl('Telefèric', bcn$NOM_CAPA[i], fixed = TRUE)){
    newcol <- c(newcol,'Telefèric')
  }
  else if(grepl('Tramvia', bcn$NOM_CAPA[i], fixed = TRUE)){
    newcol <- c(newcol,'Tramvia')
  }
  else if(grepl("Tren a l'aeroport", bcn$NOM_CAPA[i], fixed = TRUE)){
    newcol <- c(newcol,"Tren a l'aeroport")
  }
}

bcn$transporte <- newcol
    
pal <- colorFactor(
  palette = c('red', 'blue', 'black', 'orange','green','purple','yellow','hotpink'),
  domain = bcn$transporte
)

m <- leaflet(data = bcn) %>% addTiles() %>% 
  addCircleMarkers(~LONGITUD, ~LATITUD, 
              popup = ~as.character(EQUIPAMENT), 
              label = ~as.character(EQUIPAMENT),
              color = ~pal(transporte),
              clusterOptions = markerClusterOptions()) %>%
  addLegend(pal = pal,values = ~NOM_CAPA) %>%
  addMiniMap(tiles = providers$Esri.WorldStreetMap,toggleDisplay = T) %>%  # Añade un minimapa en la parte inferior derecha
  addDrawToolbar(editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) %>% # Al costado se añade una barra de herramientas
  addEasyprint(options = easyprintOptions(
    title = 'Print map',
    position = 'bottomleft',
    exportOnly = TRUE))
    
saveWidget(m, file="transports.html")
