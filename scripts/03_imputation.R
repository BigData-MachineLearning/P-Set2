### imputacion


skim(train)


# Hay missings considerables en:

# bathrooms, rooms, surface_total, surface_covered
#    10071   18260      # 30079     # 30079

# Imputacion batrooms

### variable de piso

train <- train |>
  mutate(surface_extr = str_extract(description, "\\d+\\s*(?:mts2|mt2|mtrs2|m2|ms2|metros\\s*cuadrados)")) #palbra o numero que antecede o

# ahora me quedo con el numero

train <- train |>
  mutate(surface_nums = as.integer(str_extract(surface_extr, "\\d+")))

#

train |> 
  filter(surface_nums>= 1000) |>
  
  train$description[114]


# Medida de proteccion
train <- train |>
  mutate(surface_nums = ifelse(surface_nums > 1000, NA, surface_nums))

# Imputo


train$surface_total <- ifelse(is.na(train$surface_total), train$surface_nums, train$surface_total)

sum(is.na(train$surface_total))

