### Limpieza datos textos

#====================================#
##### === 1.Todo a minuscula === #####
#====================================#

train <- train |> 
  mutate(description = str_to_lower(description))

test <- test |> 
  mutate(description = str_to_lower(description))

#======================================#
##### === 2.Encoding set ASCII === #####
#======================================#

# cambiar de utf 8 a ascii para evitar tildes. 


train <- train |>  
  mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))

test <- test |>  
  mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))

#======================================#
##### === 3.Special characters === #####
#======================================#

# Limitamos caracteres especiales

train <- train |>
  mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))

test <- test |>
  mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))

#======================================#
##### === 4.Special characters === #####
#======================================#
# quitar espacios dobles y entrelineados dobles

train <- train |>
  mutate(description = str_trim(gsub("\\s+", " ", description)))


test <- test |>
  mutate(description = str_trim(gsub("\\s+", " ", description)))

# Como queda
test$description[1]




