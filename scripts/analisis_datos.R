#Pruebas Y Análisis de Datos:

source("scripts/00_packages.R")
datos <- import("db_tandas/tanda1/train_1.csv")
summary(datos)
colSums(is.na(datos))#Vemos Missing values

summary(datos$price) %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  mutate(V1=scales::dollar(V1))

datos <- datos %>% 
  mutate(precio_por_mt2=round(price/surface_total),0)

summary(datos$pmt2) %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  mutate(V1=scales::dollar(V1))

# =============================================================================#
############################ === Análisis Tanda 2=== ###########################
# =============================================================================#

tanda2 <- import("db_tandas/tanda2/train_2.csv")
tanda2 <- tanda2 %>% 
  mutate(logp=log(price),data=tanda2)

#Precio
p <-ggplot(tanda2,aes(x=price))+
  geom_histogram(fill="darkblue",alpha=0.4)+
  labs(x="Valor de venta (log-scale)",y="Cantidad")+
  scale_x_log10(labels=scales::dollar)+
  theme_bw()

ggplotly(p)

#Surface_total:
s <-ggplot(tanda2,aes(x=surface_total))+
  geom_histogram(fill="darkblue",alpha=0.4)+
  labs(x="Surface_total",y="Cantidad")+
  theme_bw()

ggplotly(s)

# Crear el gráfico de dispersión usando ggplot2
ggplot(tanda2, aes(x = surface_total, y = price)) +
  geom_point(shape = 1, color = "blue") +
  labs(title = "Gráfico de Dispersión",
       x = "Surface_total",
       y = "Precio")


ggplot(tanda2, aes(x = bedrooms, y = price)) +
  geom_point(shape = 1, color = "blue") +
  labs(title = "Gráfico de Dispersión",
       x = "Surface_total",
       y = "Precio")


######################dddd
#========================#
#### Datos originales ####
#========================#

# 

train <-   as.data.frame(import("stores/train.csv")) 


# Create a data frame with your variable names
variable_names <- c("Variable1", "Variable2", "Variable3", "Variable4")

# Create a data frame with blank spaces
blank_spaces <- rep("", length(variable_names))

# Combine the variable names and blank spaces
data <- data.frame(Variable = blank_spaces, Description = variable_names)

# Create an HTML table with borders between rows
html_table <- htmlTable(data, rnames = FALSE, header = c("Variable Name", "Description"), border = 1)


# Missings

missing_data <- data.frame(
  Variables = names(train),
  Missing = sapply(train, function(x) sum(is.na(x)))
)


stargazer(as.data.frame(missing_data), summary = FALSE,
          type = "text",title="Missing values Analysis", 
          digits=1, out="views/missings_1.htm")


# Summary tables

train2 <-   as.data.frame(import("db_tandas/tanda4/train_4.csv")) 

categoricas <- train2 |> select(price, bathrooms, bedrooms, property_type2, parqueadero, pent_house, UPL, estrato)

numericas <- train2 |> select(price, distancia_bus, distancia_parque, distancia_cc, distancia_police, surface_total)

stargazer(as.data.frame(numericas), summary = T,
          type = "text",title="Numeric variables analysis", 
          digits=1, out="views/desc_num.htm")


#  Relaciones 

#Numericas 

#Numericas 

# price vs distancia bus, distancia ciclovia

scatt_price_bus <- ggplot(train2, aes(x = distancia_bus, y = price)) +
  geom_point(shape=1, color = "#FF6666", alpha = 0.5) +   
  geom_smooth(method=lm,  
              se=FALSE, colour="black") + 
  labs(title = "Distancia TM vs price", 
       subtitle = "COP") +
  ylab("price") +
  xlab("Distancia TM") +
  scale_y_continuous(labels = scales::label_number_si())+
  theme_bw() +
  theme(plot.title = element_text(size = 20) , 
        plot.subtitle = element_text(size = 12, color = "#a0a0a0")) 

scatt_price_ciclo <- ggplot(train2, aes(x = ciclovia_near, y = price)) +
  geom_point(shape=1, color = "#FF6666", alpha = 0.5) +   
  geom_smooth(method=lm,  
              se=FALSE, colour="black") + 
  labs(title = "Distancia Cicloruta vs price", 
       subtitle = "COP") +
  ylab("price") +
  xlab("Distancia Cicloruta") +
  scale_y_continuous(labels = scales::label_number_si())+
  theme_bw() +
  theme(plot.title = element_text(size = 20) , 
        plot.subtitle = element_text(size = 12, color = "#a0a0a0")) 

TM_ciclo <- grid.arrange(scatt_price_bus, scatt_price_ciclo) 

ggsave("views/TM_ciclo.png")
#price vs distancia paruqe distancia cc


#price vs distancia paruqe distancia cc

scatt_price_park <- ggplot(train2, aes(x = distancia_parque, y = price)) +
  geom_point(shape=1, color = "#FF6666", alpha = 0.5) +   
  geom_smooth(method=lm,  
              se=FALSE, colour="black") + 
  labs(title = "Distancia parque vs price", 
       subtitle = "COP") +
  ylab("price") +
  xlab("Distancia parque") +
  scale_y_continuous(labels = scales::label_number_si())+
  theme_bw() +
  theme(plot.title = element_text(size = 20) , 
        plot.subtitle = element_text(size = 12, color = "#a0a0a0")) 


scatt_price_cc <- ggplot(train2, aes(x = distancia_cc, y = price)) +
  geom_point(shape=1, color = "#FF6666", alpha = 0.5) +   
  geom_smooth(method=lm,  
              se=FALSE, colour="black") + 
  labs(title = "Distancia CC vs price", 
       subtitle = "COP") +
  ylab("price") +
  xlab("Distancia Centro Comercial") +
  scale_y_continuous(labels = scales::label_number_si())+
  theme_bw() +
  theme(plot.title = element_text(size = 20) , 
        plot.subtitle = element_text(size = 12, color = "#a0a0a0")) 

park_cc <-grid.arrange(scatt_price_park, scatt_price_cc) 
ggsave("views/park_cc.png")

# price vs distancia policia

scatt_police_price <- ggplot(train2, aes(x = distancia_police, y = price)) +
  geom_point(shape=1, color = "#FF6666", alpha = 0.5) +   
  geom_smooth(method=lm,  
              se=FALSE, colour="black") + 
  labs(title = "Distancia CAI vs price", 
       subtitle = "COP") +
  ylab("price") +
  xlab("Distancia CAI") +
  scale_y_continuous(labels = scales::label_number_si())+
  theme_bw() +
  theme(plot.title = element_text(size = 20) , 
        plot.subtitle = element_text(size = 12, color = "#a0a0a0")) 

scatt_police_price

ggsave("views/scatt_police_price.png")


# price vs surface total

scatt_price_surface <- ggplot(train2, aes(x = surface_total, y = price)) +
  geom_point(shape=1, color = "#FF6666", alpha = 0.5) +   
  geom_smooth(method=lm,  
              se=FALSE, colour="black") + 
  labs(title = "surface (mt2) vs price", 
       subtitle = "COP") +
  ylab("price") +
  xlab("surface area") +
  scale_y_continuous(labels = scales::label_number_si()) +
  theme_bw() +
  theme(plot.title = element_text(size = 20) , 
        plot.subtitle = element_text(size = 12, color = "#a0a0a0")) 

scatt_price_surface

ggsave("views/scatt_price_surface.png")

#### Boxplots

#### Boxplots

# price vs bathrooms, bedrooms

box_bathrooms_price <- train2 |> 
  mutate(bathrooms = as.factor(bathrooms)) |>
  ggplot( aes(x=bathrooms, y=price, fill = bathrooms)) + 
  geom_boxplot() +
  labs(title = "bathrooms vs price", 
       subtitle = "COP") +
  xlab("Bathrooms") +
  ylab("Price") +
  scale_y_continuous(labels = scales::label_number_si()) +
  theme_bw() +
  theme(plot.title = element_text(size = 20) , 
        plot.subtitle = element_text(size = 12, color = "#a0a0a0")) 

box_bathrooms_price

ggsave("views/box_bathrooms_price.png")


box_bedrooms_price <- train2 |> 
  mutate(bedrooms = as.factor(bedrooms)) |>
  ggplot( aes(x=bedrooms, y=price, fill = bedrooms)) + 
  geom_boxplot() +
  labs(title = "bedrooms vs price", 
       subtitle = "COP") +
  xlab("bedrooms") +
  ylab("Price") +
  scale_y_continuous(labels = scales::label_number_si()) +
  theme_bw() +
  theme(plot.title = element_text(size = 20) , 
        plot.subtitle = element_text(size = 12, color = "#a0a0a0")) 

box_bedrooms_price
ggsave("views/box_bedrooms_price.png")


# price vs property type

box_property_price <- train2 |> 
  mutate(property_type2 = as.character(property_type2)) |>
  ggplot( aes(x=property_type2, y=price, fill = property_type2)) + 
  geom_boxplot() +
  labs(title = "Property type vs price", 
       subtitle = "COP - 0 = Apartment & 1 = House") +
  xlab("Property type") +
  ylab("price") +
  scale_y_continuous(labels = scales::label_number_si()) +
  theme_bw() +
  theme(plot.title = element_text(size = 20) , 
        plot.subtitle = element_text(size = 12, color = "#a0a0a0")) 

box_property_price
ggsave("views/box_property_price.png")


# price vs parqueadero

box_parking_price <- train2 |> 
  mutate(parqueadero = as.character(parqueadero)) |>
  ggplot( aes(x=parqueadero, y=price, fill = parqueadero)) + 
  geom_boxplot() +
  labs(title = "Parqueadero vs Price", 
       subtitle = "COP") +
  xlab("Parqueadero") +
  ylab("price") +
  scale_y_continuous(labels = scales::label_number_si()) +
  theme_bw() +
  theme(plot.title = element_text(size = 20) , 
        plot.subtitle = element_text(size = 12, color = "#a0a0a0")) 

box_parking_price
ggsave("views/box_parking_price.png")


# price vs penthouse

box_penthouse_price <- train2 |> 
  mutate(pent_house = as.character(pent_house)) |>
  ggplot( aes(x=pent_house, y=price, fill = pent_house)) + 
  geom_boxplot() +
  labs(title = "Penthouse vs price", 
       subtitle = "COP") +
  xlab("Penthouse") +
  ylab("price") +
  scale_y_continuous(labels = scales::label_number_si()) +
  theme_bw() +
  theme(plot.title = element_text(size = 20) , 
        plot.subtitle = element_text(size = 12, color = "#a0a0a0")) 

box_penthouse_price
ggsave("views/box_penthouse_price.png")


# Distribuciones de UPL



# mapas

# Ciclovias y transmi

latitud_central <- mean(train2$lat)
longitud_central <- mean(train2$lon)

# Extraemos la info de las estaciones del Transmi
parada_de_bus <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key ='amenity' , value = 'bus_station') 
# Cambiamos el formato para que sea un objeto sf (simple features)

parada_de_bus_sf <- osmdata_sf(parada_de_bus)

ciclovias <-st_read("stores/ciclovias")

ciclovias<-st_transform(ciclovias,4326)

bbox <- getbb("Bogotá Colombia")
map <- leaflet() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 11) %>%
  addTiles() %>%
  addCircleMarkers(data = parada_de_bus_sf$osm_points, radius = 1, color = "black", fill = TRUE) %>%
  addPolylines(data = ciclovias, color = "blue", weight = 2)

map

