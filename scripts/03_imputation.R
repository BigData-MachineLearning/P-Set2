### imputacion

# Hay missings considerables en:

# bathrooms, rooms, surface_total, surface_covered
#    10071   18260      # 30079     # 30079

# Imputacion mts2

train <- train |>
  mutate(surface_extr = str_extract(description, "\\d+\\s*(?:mts2|mt2|mtrs2|m2|ms2|metros\\s*cuadrados)")) #palbra o numero que antecede o

test <- test |>
  mutate(surface_extr = str_extract(description, "\\d+\\s*(?:mts2|mt2|mtrs2|m2|ms2|metros\\s*cuadrados)")) #palbra o numero que antecede o

# ahora me quedo con el numero

train <- train |>
  mutate(surface_nums = as.integer(str_extract(surface_extr, "\\d+")))


test <- test |>
  mutate(surface_nums = as.integer(str_extract(surface_extr, "\\d+")))


# Medida de proteccion
train <- train |>
  mutate(surface_nums = ifelse(surface_nums > 1000, NA, surface_nums))

test <- test |>
  mutate(surface_nums = ifelse(surface_nums > 1000, NA, surface_nums))

# Imputo

train$surface_total <- ifelse(is.na(train$surface_total), train$surface_nums, train$surface_total)

test$surface_total <- ifelse(is.na(test$surface_total), test$surface_nums, test$surface_total)

# result = 6353 imputados, podría mejorar 
30079 - sum(is.na(train$surface_total))
train <- train |>
  select(-c(surface_extr, surface_nums))

test <- test |>
  select(-c(surface_extr, surface_nums))

#
train$surface_total <- ifelse(is.na(train$surface_covered), train$surface_total, train$surface_covered)

test$surface_total <- ifelse(is.na(test$surface_covered), test$surface_total, test$surface_covered)

# Imputacion rooms

train <- train |>
  mutate(rooms_extr = str_extract(description, 
                                  "\\b(\\d+|uno|dos|tres|cuatro|cinco|seis|siete|ocho|nueve|diez)\\s*(?:habitacion|habitaciones|cuarto|cuartos|alcoba|alcobas)\\b"
  )) #palbra o numero que antecede o

test <- test |>
  mutate(rooms_extr = str_extract(description, 
                                  "\\b(\\d+|uno|dos|tres|cuatro|cinco|seis|siete|ocho|nueve|diez)\\s*(?:habitacion|habitaciones|cuarto|cuartos|alcoba|alcobas)\\b"
  )) #palbra o numero que antecede o

numeros_escritos <- c("uno|primero|primer", "dos|segundo|segund", "tres|tercero|tercer", "cuatro|cuarto", "cinco|quinto", "seis|sexto", "siete|septimo", "ocho|octavo", "nueve|noveno", "diez|decimo|dei")
numeros_numericos <- as.character(1:10)

train <- train %>%
  mutate(piso_info = str_replace_all(rooms_extr, setNames(numeros_numericos,numeros_escritos))) # set names empareja

test <- test %>%
  mutate(piso_info = str_replace_all(rooms_extr, setNames(numeros_numericos,numeros_escritos))) # set names empareja

# ahora me quedo con el numero

train <- train |>
  mutate(rooms_nums = as.integer(str_extract(rooms_extr, "\\d+")))

test <- test |>
  mutate(rooms_nums = as.integer(str_extract(rooms_extr, "\\d+")))


# Medida de proteccion
train <- train |>
  mutate(rooms_nums = ifelse(rooms_nums > 6, NA, rooms_nums))

test <- test |>
  mutate(rooms_nums = ifelse(rooms_nums > 6, NA, rooms_nums))

# Imputo

train$rooms <- ifelse(is.na(train$rooms), train$rooms_nums, train$)

test$rooms <- ifelse(is.na(test$rooms), test$rooms_nums, test$rooms)

# result =
18260 - sum(is.na(train$rooms))
train <- train |>
  select(-c(rooms_extr, rooms_nums))

test <- test |>
  select(-c(rooms_extr, rooms_nums))

