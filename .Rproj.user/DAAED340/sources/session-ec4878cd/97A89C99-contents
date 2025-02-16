# Funcion Agregatte
# Agrega un raster de datos por su valor más frecuente
# Hecha en Rcpp para velocidad máxima

library(geosphere)

# Obtiene las coordenadas del pais seleccionado, a partir del vector de centroides (ojo:ya creado!!)
GetCoords <- function(pais){
  x <- st_coordinates(st_geometry(centroides[centroides$ISO_A2 == pais,]))[1]
  y <- st_coordinates(st_geometry(centroides[centroides$ISO_A2 == pais,]))[2]
  return(list(x = x ,y = y))
}


# arma un vector de puntos intermedios entre las dos coordenadas basado en
# https://www.r-graph-gallery.com/how-to-draw-connecting-routes-on-map-with-r-and-great-circles/
catenaria <- function( origen, destino, originproy = "+proj=robin", destinyproy = "+proj=robin", breaks = FALSE , ...){
  # convertir de Robinson a WGS84
  origen <- spTransform(SpatialPoints(origen,CRS(originproy)), CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  destino <- spTransform(SpatialPoints(destino,CRS(originproy)), CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  inter <- gcIntermediate(origen, destino, n=40, addStartEnd=TRUE, breakAtDateLine=F)             
  inter <- data.frame(inter)
  diff_of_lon=abs(origen$x) + abs(destino$x)
  inter$part <- 0
  if(breaks == TRUE & diff_of_lon > 180){
    inter <- inter %>% mutate(part = ifelse(lon>=0, 1, 0))
  }
  inter <- spTransform(SpatialPoints(inter,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")),CRS(destinyproy))
  return(inter)
}

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = font_tw, color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}
