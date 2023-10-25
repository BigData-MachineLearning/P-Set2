# Data_base_creation

rm(list = ls())

        #============================#
        ##### === 0.PACKAGES === #####
        #============================#

# Esto es un script que carga los paquetes
source("scripts/00_packages.R")

#importo los datos

train <-   as.data.frame(import("stores/train.csv")) 
template <- as.data.frame(import("stores/submission_template.csv")) 
test <- as.data.frame(import("stores/test.csv")) 

        #==============================#
        ##### === 1.FIRST LOOK === #####
        #==============================#

colnames(train)
skim(train)

# property_type numeric

train <- train %>%
  mutate(property_type2 = ifelse(property_type == "Apartamento", 0, 1))

# Assuming you have a 'test' data frame
test <- test %>%
  mutate(property_type2 = ifelse(property_type == "Apartamento", 0, 1))
        #=============================#
        ##### === 2.TEXT VARS === #####
        #=============================#



# Este es el script que limpia inicialmente los datos de texto

source("scripts/01_clean_desc.R")

# Variables externas texto descripcion

          ### === Dummy Parqueadero === ###

test <- test |> 
  mutate(parqueadero = as.numeric(grepl("\\b(parqueadero|garaje|parking)\\b", test$description)))

train <- train |> 
  mutate(parqueadero = as.numeric(grepl("\\b(parqueadero|garaje|parking)\\b", train$description)))

          ### === En que piso es el apto === ###

train <- train |>
  mutate(piso_info = str_extract(description, "(\\w+|\\d+) piso (\\w+|\\d+)")) #palbra o numero que antecede o

test <- test |>
  mutate(piso_info = str_extract(description, "(\\w+|\\d+) piso (\\w+|\\d+)")) #palbra o numero que antecede o

# va despues

numeros_escritos <- c("uno|primero|primer", "dos|segundo|segund", "tres|tercero|tercer", "cuatro|cuarto", "cinco|quinto", "seis|sexto", "siete|septimo", "ocho|octavo", "nueve|noveno", "diez|decimo|dei")
numeros_numericos <- as.character(1:10)

train <- train %>%
  mutate(piso_info = str_replace_all(piso_info, setNames(numeros_numericos,numeros_escritos))) # set names empareja

test <- test %>%
  mutate(piso_info = str_replace_all(piso_info, setNames(numeros_numericos,numeros_escritos))) # set names empareja

# ahora me quedo con el numero

train <- train |>
  mutate(piso_numerico = as.integer(str_extract(piso_info, "\\d+")))

test <- test |>
  mutate(piso_numerico = as.integer(str_extract(piso_info, "\\d+")))

# Medida de proteccion

train <- train |>
  mutate(piso_numerico = ifelse(piso_numerico > 20, NA, piso_numerico))

test <- test |>
  mutate(piso_numerico = ifelse(piso_numerico > 20, NA, piso_numerico))

### === Dummy Penthouse === ###

train <- train |> 
  mutate(pent_house = as.numeric(grepl("\\b(penthouse|pent house|penthause|pent hause)\\b", train$description)))

test <- test |> 
  mutate(pent_house = as.numeric(grepl("\\b(penthouse|pent house|penthause|pent hause)\\b", test$description)))

      #=================================#
      ##### === 3.External VARS === #####
      #=================================#
      
#script que jala info antes de sacar variables

source("scripts/02_variables_externas.R")
# Distancia a chapinero o al centro


# Distancia transmi

# Calculamos las distancias al paradero mas cercano

#Distancia ciclovias

#Distancia Centros comerciales

#Distacia parques

#Estrato si es posible

      #================================#
      ##### === 4.Imputaciones === #####
      #================================#


source("scripts/03_imputation.R")
sf_use_s2(TRUE)
# Termino de corregir los cuartos/habitaciones

train$bedrooms <- ifelse(!is.na(train$bedrooms) & train$bedrooms != train$rooms, train$rooms, train$bedrooms)
test$bedrooms <- ifelse(!is.na(test$bedrooms) & test$bedrooms != test$rooms, test$rooms, test$bedrooms)


#Export
#rio::export(train, "db_tandas/tanda1/train_1.csv")
#rio::export(test, "db_tandas/tanda1/test_1.csv")

#Vamos a hacer la tanda 2 a partir de la tanda 1.

# train2 <- train
# test2 <- test

names(train2)
names(test2)

summary(train2$price) %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  mutate(V1=scales::dollar(V1))


summary(train2$pmt2) %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  mutate(V1=scales::dollar(V1))

train2 <- train2 %>% 
  filter(between(pmt2,100000,12e6)) #Antes estabamos con 38644 observaciones.
#Ahora estamos con 37488 observaciones. 

#Ahora miremos como están los datos despúes de ese filtro:

summary(train2) #Vemos que surface_total sigue teniendo outliers, entonces
# intentemos filtrar a ver cuantas observaciones perdemos.

train2 <- train2 %>% 
  filter(between(surface_total,0,500)) #Recordemos que teniamos 37488 observaciones antes.
# ahora quedamos con 36629

#Organizamos test2
summary(test2)

# #
# train1 <- import("db_tandas/tanda1/train_1.csv")
# test1 <- import("db_tandas/tanda1/test_1.csv")
# 
# train2 <- train
# test2 <- test
# 
# 
# 
# # Replace missing values in train2
# train2 <- train2 %>%
#   left_join(train1 %>% select(property_id, bedrooms), by = "property_id") %>%
#   mutate(bedrooms = ifelse(is.na(bedrooms.x), bedrooms.y, bedrooms.x)) %>%
#   select(-bedrooms.x, -bedrooms.y)
# 
# # Replace missing values in test2
# test2 <- test2 %>%
#   left_join(test1 %>% select(property_id, bedrooms), by = "property_id") %>%
#   mutate(bedrooms = ifelse(is.na(bedrooms.x), bedrooms.y, bedrooms.x)) %>%
#   select(-bedrooms.x, -bedrooms.y)
# 
# train2 <- train2 %>% filter(!is.na(estrato))
# #

# rio::export(train2, "db_tandas/tanda2/train_2.csv")
# rio::export(test2, "db_tandas/tanda2/test_2.csv")

# =============================================================================#
############################ === Tanda 3 === ###################################
# =============================================================================#

 
 train <- import("db_tandas/tanda2/train_2.csv")
 test <- import("db_tandas/tanda2/test_2.csv")
 
 
 # train <- train |>
 #   filter(UPL %in% c("Usaquén", "Niza", "Barrios Unidos", "Teusaquillo", "Chapinero"))
 
 # train <- train |>
 #   filter(UPL == "Chapinero")
 
 train$estrato <- as.factor(train$estrato)
 
 test$estrato <- as.factor(test$estrato)
 
 
  # rio::export(train, "db_tandas/tanda3/train_3.csv")
  # rio::export(test, "db_tandas/tanda3/test_3.csv")
  # 

  # =============================================================================#
  ############################ === Tanda 4 === ###################################
  # =============================================================================#
  train <- import("db_tandas/tanda2/train_2.csv")
  test <- import("db_tandas/tanda2/test_2.csv")

  # Determinamos el centro del mapa 
  latitud_central <- mean(train$lat)
  longitud_central <- mean(train$lon)
  
  latitud_central <- mean(test$lat)
  longitud_central <- mean(test$lon)
  
  # Volvemos la base un objeto espacial
  
  train_sf <- st_as_sf(train, coords = c("lon", "lat") , crs = 4326)
  test_sf <- st_as_sf(test, coords = c("lon", "lat") , crs = 4326)
  
  #============================#
  ##### === Add Police === #####
  #============================#
  
  # Extraemos la info de las estaciones del Transmi
  police <- opq(bbox = getbb("Bogotá Colombia")) %>%
    add_osm_feature(key ='amenity' , value = 'police') 
  # Cambiamos el formato para que sea un objeto sf (simple features)
  
  police_sf <- osmdata_sf(police)
  
  # De las features del parque nos interesa su geomoetría y donde están ubicados 
  police_sf_geometria <- police_sf$osm_polygons %>% 
    select(osm_id, name)
  
  # Calculamos el centroide de cada parque para aproximar su ubicación como un solo punto 
  #centroides <- gCentroid(as(parada_de_bus_sf_geometria$geometry, "Spatial"), byid = T)
  centroides_police <-st_centroid(police_sf_geometria$geometry)
  
  # centroides de distancia a transmi
  
  centroides_police_sf <- do.call(rbind, st_geometry(centroides_police)) |>
    as_tibble() |> setNames(c("lon", "lat"))
  
  # centroides coords y crs
  centroides_police_sf <- st_as_sf(centroides_police_sf, coords = c("lon", "lat"), crs=4326)
  
  nearest_police <- st_nearest_feature(train_sf,centroides_police_sf)
  
  train<- train %>% mutate(distancia_police=st_distance(x = train_sf, y = centroides_police_sf[nearest_police,], by_element=TRUE))
  
  nearest_police <- st_nearest_feature(test_sf,centroides_police_sf)
  test<- test %>% mutate(distancia_police=st_distance(x = test_sf, y = centroides_police_sf[nearest_police,], by_element=TRUE))
  
  train$distancia_police <- as.numeric(train$distancia_police)
  test$distancia_police <- as.numeric(test$distancia_police)
  
  
  rio::export(train, "db_tandas/tanda4/train_4.csv")
  rio::export(test, "db_tandas/tanda4/test_4.csv")