# Data_base_creation

rm(list = ls())

# Esto es un script que carga los paquetes
source("scripts/00_packages.R")

#importo los datos

train <-   as.data.frame(import("stores/train.csv")) 
template <- as.data.frame(import("stores/submission_template.csv")) 
test <- as.data.frame(import("stores/test.csv")) 

#


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
  mutate(parqueadero = as.numeric(grepl("\b(parqueadero|garaje|parking)\b", test$description)))

train <- train |> 
  mutate(parqueadero = as.numeric(grepl("\b(parqueadero|garaje|parking)\b", train$description)))

# Dummy de penthouse


# Distancia a chapinero o al centro

# Imputaciones



# Variables externas de mapas


# Distancia transmi


#Distancia Centros comerciales



#Distancia ciclovias



#Distacia parques

#estrato si es posible












