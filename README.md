# P-Set2: Making Money with ML?

## Autores

- Sebastián Marín Lombo
- Daniel Felipe Cortes
- Jorge Gómez
- Nicolás González Gort

## Nuestro reto:
Una nueva start-up dedicada a la compra y venta de propiedades acaba de contratarte a ti y a tu equipo para desarrollar un modelo predictivo. Su objetivo es comprar la mayor cantidad de propiedades en el barrio de Chapinero en Bogotá, Colombia gastando lo menos posible.

La compañía tiene una muestra de datos de propiedades individuales en Bogotá de https://www.properati.com.co. Sin embargo, la información sobre propiedades en Chapinero falta en su mayoría.

La empresa quiere evitar el fiasco de Zillow. Zillow desarrolló algoritmos para comprar casas. Sin embargo, sus modelos sobrestimaron considerablemente el precio de las viviendas. Esta sobreestimación supuso unas pérdidas de unos 500 millones de dólares para la empresa y una reducción aproximada del 25% de su plantilla.

## ¿Cómo se conseguirá?
El objetivo principal es construir un modelo predictivo de los precios de venta. A partir del de Rosen "Hedonic Prices and Implicit Markets: Product Differentiation in Pure Competition" (1974), sabemos que un vector de sus características, C = (c1, c2, . . . , cn) describe un bien diferenciado.

En el caso de una vivienda, estas características pueden incluir atributos estructurales (por ejemplo número de dormitorios), los servicios públicos del barrio (por ejemplo, la calidad de la escuela local) y los locales (delincuencia, calidad del aire, etc.). 

## Estructura del repo

`db_tandas`:
Contiene todas las bases modificadas para la estimacion de los modelos.
Ademas tiene el archivo de excel models_registry.xlsx el cual sirve para registrar el desempeño de distintos modelos e informacion de las especificaciones usadas. 

`views`:
Contiene las imagenes, los mapas, gráicos y demas vizualizaciones usadas en el documento final. 


`stores`:
Contiene todas las bases de datos, las originales de train y test, y un template para las submissions.

`scripts`:
Contiene todos los scrips del taller que se corren para hacer las predicciones. Estos son los siguientes: 
- `00_packages.R` <- Corre los paquetes que se usarán en el proyecto.
- `01_clean_desc.R` <- Limpieza del texto de la descripción (eliminacion caracxteres especiales, espacios dobles, entre otros)
- `01.2_desc_vars.R` <- Análisis del texto de la descripción de la propiedad procesado para ver qué variables se pueden obtener.
- `02_variables_externas.R` <- Se incluyen variables externas geoespaciales de Open Map Tools y de datos abiertos de la alcaldía de Bogotá. 
- `03_imputation.R` <- Imputaciones de missings en bases de training y testing.
- `04_database_setup.R` <- Se corren los scripts del 0 al 3 y se agregan variables del terxto de la descripcion, luego exporta la base de datos que se usara para la tanada de modelos.
- `05_models.R` <- Ejecuta modelos para predecir los precios y guarda los resultados.

`document`:
Documento de trabajo final.