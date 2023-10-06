##

# Determinamos el centro del mapa 
latitud_central <- mean(train$lat)
longitud_central <- mean(train$lon)

latitud_central <- mean(test$lat)
longitud_central <- mean(test$lon)




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


# variable distanmcia a transmi

train_sf <- st_as_sf(train, coords = c("lon", "lat") , crs = 4326)
test_sf <- st_as_sf(test, coords = c("lon", "lat") , crs = 4326)
# centroides de distancia a transmi

centroides_bus_sf <- do.call(rbind, st_geometry(centroides_bus)) |>
  as_tibble() |> setNames(c("lon", "lat"))

# centroides coords y crs
centroides_bus_sf <- st_as_sf(centroides_bus_sf, coords = c("lon", "lat"), crs=4326)

# Distancia ciclovias

train_sf <- st_as_sf(train, coords = c("lon", "lat") , crs = 4326)
test_sf <- st_as_sf(test, coords = c("lon", "lat") , crs = 4326)

ciclovias <-st_read("stores/ciclovias")

ciclovias<-st_transform(ciclovias,4326)


distances <- st_distance(train_sf, ciclovias)

# Find the minimum distance for each apartment
min_distances <- apply(distances, 1, min)
train$ciclovia_near <- min_distances
