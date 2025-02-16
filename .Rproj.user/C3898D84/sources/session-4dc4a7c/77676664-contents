# *************************************************
# Modulo de Insercion Exportadora - CEPAL 2021
# Fernando García Díaz 
# Gráfico de distribución global de la población
# *************************************************

library(terra)
library(raster)
library(ggspatial)
library(ggplot2)
library(ggthemes)
library(viridis)
library(sf)
library(dplyr)
library(svglite)
library(exactextractr)

# 0 Definiciones
"%+%" <- function(x,y) paste(x,y,sep="") # se define %+% como concatenacion
data_dir       <- getwd()%+%"/data/"
results_dir    <- getwd()%+%"/results/"



# 1 Load Data
Population     <- geodata::population(year = 2020, path = data_dir, resolution = 10)
Population     <- raster(Population)

PROY           <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # Proyeccion Seleccionada para MAPAS ROBINSON
Population.df  <- raster::as.data.frame(raster::aggregate(Population, fact = 15, fun = sum, na.rm = TRUE), xy = TRUE) # Pasar a data.frame para hacer el analisis, reduciendo detalle
Population     <- projectRaster(Population, crs = (PROY) , over = TRUE) # cambiar la proyeccion
#Population    <- as.data.frame(Population, xy = TRUE)

# Vectores
graticula      <- rnaturalearth::ne_download(destdir = data_dir, scale = 110, type = "graticules_15", category = "physical")
bounding       <- rnaturalearth::ne_download(destdir = data_dir, scale = 110, type = "wgs84_bounding_box", category = "physical")
ocean          <- rnaturalearth::ne_download(destdir = data_dir, scale = 110, type = "ocean", category = "physical")
wmap_countries <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")

ggplot() +  
            geom_sf(data = wmap_countries, fill = "gray", color ="white", size=0.2, alpha= 1) +  # ideal para indices
            layer_spatial(Population,  aes(fill = after_stat(band1)), alpha = 0.8) +
            geom_sf(data = ocean, fill ="darkgray", color = "black", alpha = 1, size = 0.1) +
            geom_sf(data = graticula, color = "white", alpha = 0.4, size = 0.05) +
            scale_fill_viridis(limits = c(0,100000), na.value = NA, "Densidad") +
            #geom_path(data = test, aes(lon, lat, group = part), color ="white", size= 0.5, alpha= 0.6,
            #          arrow = arrow(type = "open", angle = 30, length = unit(0.1, "inches"))) +  # ideal para indices
            #coord_sf(ylim = c(-50, 90)) +
            hrbrthemes::theme_ipsum() +
            labs(title = "Población mundial", caption = "Fuente: F.García Díaz")

# 3 Geo-Analisis
# Para cada punto del mapa, armar la distancia media de cada persona a ese punto
# Es ultra ineficiente, pero ya está
Population.distance <- matrix(ncol = 1, nrow = length(Population.df$population_density))
pb = txtProgressBar(min = 0, max = length(Population.df$population_density), initial = 0, style = 3) 
for (i in 1:length(Population.df$population_density)){
  setTxtProgressBar(pb,i)
  Population.df$distances  <- distGeo( Population.df[, 1:2], Population.df[i, 1:2]  )/1000 
  Population.df$weighted   <- Population.df$distances * Population.df$population_density
  Population.distance[i] <- sum(Population.df$weighted, na.rm = TRUE)/sum(Population.df$population_density, na.rm = TRUE)
}

Population.distance   <- as.data.frame(Population.distance)
Population.distance$x <- Population.df$x
Population.distance$y <- Population.df$y
# buscar el "centro de masa"
centro <- Population.distance[which(Population.distance$V1 == min(Population.distance$V1, na.rm = TRUE)),2:3]
# Transformar en raster layer
coordinates(Population.distance) <- ~ x + y
gridded(Population.distance)     <- TRUE
# coerce to raster
Population.distance              <- raster(Population.distance)
crs(Population.distance)         <- crs(wmap_countries)
# crs(Population.distance)  "+proj=longlat +datum=WGS84 +no_defs"

shading      <- ggplot() +  
              #geom_sf(data = wmap_countries, fill = "gray", color ="white", size=0.2, alpha= 1) +  # ideal para indices
              layer_spatial(data = Population.distance,  aes(fill = stat(band1)), alpha = 0.7) +
              scale_fill_viridis_c(option = "magma", direction = -1) +
              coord_sf(ylim = c(-50, 90)) 
shading.Grob <- ggplotGrob(shading)

# graficar
# Obtener las distancias medias, con la libreria exact extract, que es rapidísima!
wmap_countries <- wmap_countries |> mutate(distancia_media = exact_extract(Population.distance, wmap_countries, 'mean'))
g <- ggplot() +  
      geom_sf(data = wmap_countries, aes(fill = distancia_media), color ="#f5f5f2", size = 0.1, alpha = 1) +  # ideal para indices
#      annotation_custom(grob = shading.Grob$grobs[[6]]$children[[3]]) +
      #layer_spatial(data = Population.distance,  aes(fill = stat(band1)), alpha = 0.8) +
#      layer_spatial(data = Population,  aes(alpha = stat(band1)), fill = "black") +
      scale_fill_viridis_c(option = "magma", direction = -1) +
      geom_sf(data = ocean, fill ="#f5f5f2", color = "#f5f5f2", alpha = 1, size = 1) +
      geom_sf(data = wmap_countries, fill = NA, color ="#f5f5f2", size = 0.1, alpha = 1) +  # ideal para indices
      #geom_sf(data = graticula, color = "black", alpha = 1, size = 0.1) +
      scale_alpha_continuous(limits = c(0, 300000), na.value = 0) +
      geom_spatial_point(data = centro, aes(x,y)) +
      #scale_fill_viridis_c(option = "magma", direction = -1) +
      coord_sf(ylim = c(-50, 90)) +
      hrbrthemes::theme_ipsum() +
      labs(title = "Distribución global de la población",  x= NULL, y = NULL, 
           subtitle = "Distancia media a cada ser humano", caption = "Fuente: F.García Díaz en base a WorldPop") +
      theme(plot.title = element_text(face = "bold"))
#svglite(results_dir %+% "Distribucion Poblacion-discreto.svg" , pointsize = 11 , width = 11 , height = 6.89)
g
#dev.off()


# Hacer mapa de distancias promedio
g <- ggplot() +  
  geom_sf(data = wmap_countries, fill = "white", color ="black", size = 0.1, alpha = 1) +  # ideal para indices
  annotation_custom(grob = shading.Grob$grobs[[6]]$children[[3]]) +
  layer_spatial(data = Population,  aes(alpha = stat(band1)), fill = "white") +
  geom_sf(data = ocean, fill ="#f5f5f2", color = "#f5f5f2", alpha = 1, size = 1) +
  geom_sf(data = wmap_countries, fill = NA, color ="#f5f5f2", size = 0.1, alpha = 1) +  # ideal para indices
  #geom_sf(data = graticula, color = "black", alpha = 1, size = 0.1) +
  scale_alpha_continuous(limits = c(0, 100000), na.value = 0) +
  geom_spatial_point(data = centro, aes(x,y)) +
  #scale_fill_viridis_c(option = "magma", direction = -1) +
  coord_sf(ylim = c(-50, 90)) +
  hrbrthemes::theme_ipsum() +
  theme(legend.position = "none") +
  labs(title = "Distribución global de la población", x = NULL, y = NULL, 
       subtitle = "Distancia media a cada ser humano", caption = "Fuente: F.García Díaz en base a WorldPop")
#svglite(results_dir %+% "Distribucion Poblacion-continuo.svg" , pointsize = 11 , width = 11 , height = 6.89)
g
#dev.off()
