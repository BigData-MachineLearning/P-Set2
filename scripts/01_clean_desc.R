### Limpieza datos textos

# Todo a minuscula

train <- train |> 
  mutate(description = str_to_lower(description))

test <- test |> 
  mutate(description = str_to_lower(description))

# cambiar de utf 8 a ascii para evitar tildes. 

train <- train |>  
  mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))

test <- test |>  
  mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))

# Limitamos caracteres especiales

train <- train |>
  mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))

test <- test |>
  mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))

# quitar espacios dobles y mas

train <- train |>
  mutate(description = str_trim(gsub("\\s+", " ", description)))


test <- test |>
  mutate(description = str_trim(gsub("\\s+", " ", description)))

# Como queda
test$description[1]




