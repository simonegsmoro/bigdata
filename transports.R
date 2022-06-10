library(leaflet)
library(magrittr)
library(sp)
library(leaflet.extras)
library(leaflet.extras2)
library(htmlwidgets)

bcn <- read.csv(file = 'TRANSPORTS.csv')

newcol <- c()
for(i in 1:dim(bcn)[1]){
  if(grepl('METRO ', bcn$EQUIPAMENT[i], fixed = TRUE)){
    newcol <- c(newcol,'METRO')
  }
  else if(grepl('RENFE ', bcn$EQUIPAMENT[i], fixed = TRUE)){
    newcol <- c(newcol,'RENFE')
  }
  else if(grepl('TRAMVIA ', bcn$EQUIPAMENT[i], fixed = TRUE)){
    newcol <- c(newcol,'TRAMVIA')
  }
  else if(grepl('FGC ', bcn$EQUIPAMENT[i], fixed = TRUE)){
    newcol <- c(newcol,'FGC')
  }
  else{
    newcol <- c(newcol,'Other')
  }
}

bcn$transporte <- newcol

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
