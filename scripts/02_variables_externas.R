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
centroides <-st_centroid(parada_de_bus_sf_geometria$geometry)


# variable distanmcia a transmi

train_sf <- st_as_sf(train, coords = c("lon", "lat") , crs = 4326)
test_sf <- st_as_sf(test, coords = c("lon", "lat") , crs = 4326)
# centroides de distancia a transmi

centroides_sf <- do.call(rbind, st_geometry(centroides)) |>
  as_tibble() |> setNames(c("lon", "lat"))

# centroides coords y crs
centroides_sf <- st_as_sf(centroides_sf, coords = c("lon", "lat"), crs=4326)

