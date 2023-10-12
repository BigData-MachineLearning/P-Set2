#Pruebas Y Análisis de Datos:

source("scripts/00_packages.R")
datos <- import("db_tandas/tanda1/train_1.csv")

colSums(is.na(datos))#Vemos Missing values

summary(datos$price) %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  mutate(V1=scales::dollar(V1))

datos <- datos %>% 
  mutate(precio_por_mt2=round(price/surface_total),0)

summary(datos$precio_por_mt2) %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  mutate(V1=scales::dollar(V1))
