### Limpieza datos textos


source("scripts/00_packages.R")


db <- db |> 
  mutate(description = str_to_lower(description))

# cambiar de utf 8 a ascii para evitar tildes. 

db <- db |> 
  mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))

# #liminamos caracteres especiales

db <- db |>
  mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))

# quitar espacios dobles y mas

db <- db |>
  mutate(description = str_trim(gsub("\\s+", " ", description)))

db$description[1]

# viendo el parqueadero

db <- db |> 
  mutate(parqueadero = as.numeric(grepl("parqueadero", db$description)))