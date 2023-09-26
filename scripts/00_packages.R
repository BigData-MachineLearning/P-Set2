# Empezamos con OSM
rm(list = ls())
if(require("pacman")==F){install.packages("pacman")}
# Cargar pacman (contiene la función p_load)
require(pacman) 

# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(tidyverse, # Manipular dataframes
       rio, # Import data easily
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       rgeos, # Calcular centroides de un poligono
       tmaptools, # geocode_OSM()
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Get OSM's data 
       tidymodels,
       stringi,
       skimr) #para modelos de ML
