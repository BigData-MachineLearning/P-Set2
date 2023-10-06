

#===========================#
##### === 0.Set up  === #####
#===========================#

# Determinamos el centro del mapa 
latitud_central <- mean(train$lat)
longitud_central <- mean(train$lon)

latitud_central <- mean(test$lat)
longitud_central <- mean(test$lon)

train_sf <- st_as_sf(train, coords = c("lon", "lat") , crs = 4326)
test_sf <- st_as_sf(test, coords = c("lon", "lat") , crs = 4326)


#===========================#
##### === 1.Transmi === #####
#===========================#

# Extraemos la info de las estaciones del Transmi
parada_de_bus <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key ='amenity' , value = 'bus_station') 
# Cambiamos el formato para que sea un objeto sf (simple features)

parada_de_bus_sf <- osmdata_sf(parada_de_bus)

# De las features del parque nos interesa su geomoetría y donde están ubicados 
parada_de_bus_sf_geometria <- parada_de_bus_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada parque para aproximar su ubicación como un solo punto 
#centroides <- gCentroid(as(parada_de_bus_sf_geometria$geometry, "Spatial"), byid = T)
centroides_bus <-st_centroid(parada_de_bus_sf_geometria$geometry)

# centroides de distancia a transmi

centroides_bus_sf <- do.call(rbind, st_geometry(centroides_bus)) |>
  as_tibble() |> setNames(c("lon", "lat"))

# centroides coords y crs
centroides_bus_sf <- st_as_sf(centroides_bus_sf, coords = c("lon", "lat"), crs=4326)

nearest_bus <- st_nearest_feature(train_sf,centroides_bus_sf)

train<- train %>% mutate(distancia_bus=st_distance(x = train_sf, y = centroides_bus_sf[nearest_bus,], by_element=TRUE))

nearest_bus <- st_nearest_feature(test_sf,centroides_bus_sf)
test<- test %>% mutate(distancia_bus=st_distance(x = test_sf, y = centroides_bus_sf[nearest_bus,], by_element=TRUE))

#==============================#
##### === 2.ciclovias  === #####
#==============================#
# Distancia ciclovias

train_sf <- st_as_sf(train, coords = c("lon", "lat") , crs = 4326)
test_sf <- st_as_sf(test, coords = c("lon", "lat") , crs = 4326)

ciclovias <-st_read("stores/ciclovias")

ciclovias<-st_transform(ciclovias,4326)


distances_train <- st_distance(train_sf, ciclovias)
distances_test <- st_distance(test_sf, ciclovias)

# Find the minimum distance for each apartment
min_distances_train <- apply(distances_train, 1, min)
train$ciclovia_near <- min_distances_train

min_distances_test <- apply(distances_test, 1, min)
test$ciclovia_near <- min_distances_test


#============================#
##### === 3.Parques  === #####
#============================#

#parques
# Extraemos la info de todos los parques de Bogota
parques <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "park") 
# Cambiamos el formato para que sea un objeto sf (simple features)
parques_sf <- osmdata_sf(parques)

# De las features del parque nos interesa su geomoetría y donde estan ubicados 
parques_geometria <- parques_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada parque para aproximar s ubciacion como un solo punto 
centroides <- gCentroid(as(parques_geometria$geometry, "Spatial"), byid = T)

# convertimos los scontroides a formato sf(simple features)
centroides_sf <- st_as_sf(centroides, coords = c("x", "y"))
# Esto va a ser demorado!
# Calculamos las diatnacias para cada combinacion immueble - parque
dist_matrix_train <- st_distance(x = train_sf, y = centroides_sf)
dist_matrix_test <- st_distance(x = test_sf, y = centroides_sf)

# Encontramos la distancia mínima a un parque
dist_min <- apply(dist_matrix_train, 1, min)

# La agregamos como variablea nuestra base de datos original 
train <- train %>% mutate(distancia_parque = dist_min)

dist_min <- apply(dist_matrix_test, 1, min)
test <- test %>% mutate(distancia_parque = dist_min)


#=======================================#
##### === 4.Centros comerciales === #####
#=======================================#

# Extraemos la info de todos los CCs de Bogota
centros_com <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "shop" , value = "mall") 
# Cambiamos el formato para que sea un objeto sf (simple features)
centros_com_sf <- osmdata_sf(centros_com)

# De las features del CC nos interesa su geomoetría y donde estan ubicados 
centros_com_geometria <- centros_com_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada CC para aproximar la ubciacion como un solo punto 
centroides <- gCentroid(as(centros_com_geometria$geometry, "Spatial"), byid = T)

# convertimos los scontroides a formato sf(simple features)
centroides_sf <- st_as_sf(centroides, coords = c("x", "y"))
# Esto va a ser demorado!
# Calculamos las diatnacias para cada combinacion immueble - CC
dist_matrix_train <- st_distance(x = train_sf, y = centroides_sf)
dist_matrix_test <- st_distance(x = test_sf, y = centroides_sf)

# Encontramos la distancia mínima a un CC
dist_min <- apply(dist_matrix_train, 1, min)

# La agregamos como variable nuestra base de datos original 
train <- train %>% mutate(distancia_cc = dist_min)

dist_min <- apply(dist_matrix_test, 1, min)
test <- test %>% mutate(distancia_cc = dist_min)
