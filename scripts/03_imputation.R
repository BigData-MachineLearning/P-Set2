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

# result = 7601 imputados, podría mejorar con info de 
30079 - sum(is.na(train$surface_total))







