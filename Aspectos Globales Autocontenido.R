# *************************************************
# Modulo de Insercion Exportadora - CEPAL 2021
# Fernando García Díaz 
# Gráfico de distribución global de la población
# *************************************************

library(terra)
library(ggplot2)
library(viridis)
library(sf)
library(dplyr)
library(data.table)
library(rnaturalearth)
library(geodata)
library(tidyterra)
library(geosphere)
library(progressr)
library(pbapply)  # Para la barra de progreso
library(exactextractr)
library(ggspatial)
library(hrbrthemes)
library(crayon)
library(geodist)

# 0 Definiciones
"%+%" <- function(x,y) base::paste(x,y,sep="") # se define %+% como concatenacion
# Definir las rutas de las carpetas
data_dir    <- paste0(getwd(), "/data/")    # carpeta de descarga de datos
results_dir <- paste0(getwd(), "/results/") # carpeta de resultados
# Crear las carpetas si no existen
dir.create(data_dir,    showWarnings = FALSE, recursive = TRUE)
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

# Parametri de resolucion del rester (trade off calidad velocidad) 
resoluciones     <- c(alta = 12, media = 18 , baja = 45)  # elegir resolucion. baja para tests. alta tarda infinito. media ok.
resolucion       <- "media"

# 1 Load Data
cat(yellow("[INFO] Cargando datos\n"))
Population     <- geodata::population(year = 2020, path = data_dir, resolution = 10)
Population     <- terra::aggregate(Population, fact = resoluciones[resolucion]) 

# Completar el raster para que ocupe todo el planeta
# Crear un raster vacío con la misma extensión, resolución y proyección
r_base <- rast(ext(Population), resolution = res(Population), crs = crs(Population))
# Llenar el raster vacío con ceros
values(r_base) <- 0
# Fusionar los dos raster (se mantiene el valor del raster original donde hay datos)
Population <- cover(Population, r_base)
# Pasar a Data.frame
Population.df  <- data.table::as.data.table(terra::as.data.frame(Population, xy = TRUE)) #obtener el dataframe de datos
# Proyectar los datos para visualizar
PROY <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # Proyeccion Seleccionada para MAPAS ROBINSON
Population     <- terra::project(Population, PROY)
                    
# Vectores
ocean          <- rnaturalearth::ne_download(destdir = data_dir, scale = 110, type = "ocean", category = "physical")
wmap_countries <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")

# 2 World Population Plot
ggplot2::ggplot() +  
  ggplot2::geom_sf(data = wmap_countries, fill = "gray", color ="white", size=0.2, alpha= 1) +  
  tidyterra::geom_spatraster(data = Population, alpha = 0.8) + # `lyr.1` es la primera banda del raster
  ggplot2::geom_sf(data = ocean, fill ="darkgray", color = "black", alpha = 1, size = 0.1) +
  scale_fill_viridis(limits = c(0,1000), na.value = NA, "Densidad") +
  hrbrthemes::theme_ipsum() +
  ggplot2::labs(title = "Población mundial", caption = "Fuente: F.García Díaz")

# 3 Geo-Analisis
# Para cada punto del mapa, armar la distancia media de cada persona a ese punto
cat(yellow("[INFO] Calculando distancias ponderadas\n"))
setDT(Population.df)  # Asegurar que Population.df es un data.table
Population.df[, Population.distance := NA_real_]  # Inicializar columna
Population.df[, Population.distance := pbsapply(.I, function(i) {
  #distances <- geosphere::distGeo(.SD[i, .(x, y)], .SD[, .(x, y)]) / 1000
  distances <- geodist::geodist(.SD[i, .(x, y)], .SD[, .(x, y)], measure = "geodesic") / 1000
  weighted <- distances * Population.df$population_density
  sum(weighted, na.rm = TRUE) / sum(Population.df$population_density, na.rm = TRUE)
})]


# buscar el "centro de masa"
centro <- Population.df[which(Population.df$Population.distance == min(Population.df$Population.distance, na.rm = TRUE)),1:2]
# Armar el raster final con las distancias ponderadas
# 1 Convertir Population.df en un SpatVector (puntos)
Population_vect <- terra::vect(Population.df, geom = c("x", "y"), crs = "EPSG:4326")
# 2 Crear un raster con la resolución deseada
res_x <- min(diff(sort(unique(Population.df$x))))  # Resolución en X
res_y <- min(diff(sort(unique(Population.df$y))))  # Resolución en Y
ext   <- terra::ext(min(Population.df$x), max(Population.df$x), min(Population.df$y), max(Population.df$y))
Population_rast <- terra::rast(ext = ext, res = c(res_x, res_y), crs = "EPSG:4326")
# Rellenar el raster con los valores de Population.distance
Population_rast <- terra::rasterize(Population_vect, Population_rast, field = "Population.distance")
# Armar el raster de distancias ponderadas
shading  <- ggplot() +  
            tidyterra::geom_spatraster(data = Population_rast, alpha = 0.8) + # `lyr.1` es la primera banda del raster
            scale_fill_viridis_c(option = "magma", direction = -1) +
            coord_sf(ylim = c(-50, 90)) 
shading.Grob <- ggplotGrob(shading)

# graficar
# Obtener las distancias medias, con la libreria exact extract, que es rapidísima!
cat(yellow("[INFO] Guardando gráficos\n"))
wmap_countries <- wmap_countries |> mutate(distancia_media = exact_extract(Population_rast, wmap_countries, 'mean'))
g <- ggplot() +  
  geom_sf(data = graticula, color = "white", alpha = 0.5, size = 0.1) +
  geom_sf(data = wmap_countries, aes(fill = distancia_media), color ="#f5f5f2", size = 0.1, alpha = 1) +  # ideal para indices
  geom_sf(data = ocean, fill ="#f5f5f2", color = "#f5f5f2", alpha = 1, size = 1) +
  geom_sf(data = wmap_countries, fill = NA, color ="#f5f5f2", size = 0.1, alpha = 1) +  # ideal para indices
  scale_alpha_continuous(limits = c(0, 300000), na.value = 0) +
  geom_spatial_point(data = centro, aes(x,y), crs = 4326) +
  scale_fill_viridis_c("Distancia media", option = "magma", direction = -1) +
  coord_sf( crs = sf::st_crs(PROY), default_crs = sf::st_crs(PROY)) +
  theme_ipsum_rc() +
  labs(title = "Distribución global de la población",  x= NULL, y = NULL, 
       subtitle = "Distancia media a cada ser humano", caption = "Fuente: F.García Díaz en base a WorldPop") +
  theme(plot.title = element_text(face = "bold"))

svglite::svglite(results_dir %+% "Distribucion Poblacion-discreto.svg" , pointsize = 11 , width = 11 , height = 6.89)
print(g)
dev.off()

# Hacer mapa de distancias promedio
g <- ggplot() +  
  geom_sf(data = wmap_countries, fill = "white", color ="black", size = 0.1, alpha = 1) +  
  annotation_custom(grob = shading.Grob$grobs[[6]]$children[[3]]) +
  geom_sf(data = ocean, fill ="#f5f5f2", color = "#f5f5f2", alpha = 1, size = 1) +
  geom_sf(data = wmap_countries, fill = NA, color ="#f5f5f2", size = 0.1, alpha = 1) +  
  geom_spatial_point(data = centro, aes(x,y), crs = 4326) +
  coord_sf(ylim = c(-50, 90)) +
  hrbrthemes::theme_ipsum_rc() +
  theme(legend.position = "none") +
  labs(title = "Distribución global de la población", x = NULL, y = NULL, 
       subtitle = "Distancia media a cada ser humano", caption = "Fuente: F.García Díaz en base a WorldPop")
svglite::svglite(results_dir %+% "Distribucion Poblacion-continuo.svg" , pointsize = 11 , width = 11 , height = 6.89)
print(g)
dev.off()

