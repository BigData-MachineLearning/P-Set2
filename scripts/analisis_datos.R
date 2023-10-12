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


