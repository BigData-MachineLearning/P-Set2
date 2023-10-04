# Data_base_creation

rm(list = ls())

# Esto es un script que carga los paquetes
source("scripts/00_packages.R")

#importo los datos

train <-   as.data.frame(import("stores/train.csv")) 
template <- as.data.frame(import("stores/submission_template.csv")) 
test <- as.data.frame(import("stores/test.csv")) 

# Que tenemos

colnames(train)
skim(train)

# Anlisis missings


# Variables texto

# Este es el script que limpia inicialmente los datos de texto

source("scripts/01_clean_desc.R")

# Variables externas texto descripcion

# Agrego las variables basado en lo que vi en mi nube de palabras
test <- test |> 
  mutate(parqueadero = as.numeric(grepl("\\b(parqueadero|garaje|parking)\\b", test$description)))

train <- train |> 
  mutate(parqueadero = as.numeric(grepl("\\b(parqueadero|garaje|parking)\\b", train$description)))

# En que piso es el apto

### variable de piso

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

# Dummy de penthouse

# Agrego las variables basado en lo que vi en mi nube de palabras
test <- test |> 
  mutate(pent_house = as.numeric(grepl("\\b(penthouse|pent house|penthause|pent hause)\\b", test$description)))

train <- train |> 
  mutate(pent_house = as.numeric(grepl("\\b(parqueadero|garaje|parking)\\b", train$description)))






# Variables externas de mapas

# Determinamos el centro del mapa 
latitud_central <- mean(train$lat)
longitud_central <- mean(train$lon)

latitud_central <- mean(test$lat)
longitud_central <- mean(test$lon)

# Distancia a chapinero o al centro

# Distancia transmi


#Distancia Centros comerciales



#Distancia ciclovias



#Distacia parques


#Estrato si es posible


# imputaciones









