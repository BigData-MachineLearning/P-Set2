# Data_base_creation

rm(list = ls())

source("scripts/00_packages.R")



train <-   as.data.frame(import("stores/train.csv")) 
template <- as.data.frame(import("stores/submission_template.csv")) 
test <- as.data.frame(import("stores/test.csv")) 

#


# Que tenemos

colnames(train)
skim(train)

# Anlisis missings


# Variables texto

# script de limpieza 

source("scripts/01_clean_desc.R")

# Variables externas texto descripcion

# Agrego las variables basado en lo que vi en mi nube de palabras
test <- test |> 
  mutate(parqueadero = as.numeric(grepl("\b(parqueadero|garaje|parking)\b", test$description)))

train <- train |> 
  mutate(parqueadero = as.numeric(grepl("\b(parqueadero|garaje|parking)\b", train$description)))

# Dummy de penthouse



# Imputaciones



# Variables externas de mapas


# Distancia transmi


#Distancia Centros comerciales



#Distancia ciclovias



#Distacia parques

#estrato si es posible












