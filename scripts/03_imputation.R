
#===========================#
##### === 0.Set up  === #####
#===========================#

colSums(is.na(train))
# Hay missings considerables en:

# bathrooms, rooms, surface_total, surface_covered
#    10071   18260      # 30079     # 30079

#===================================#
##### === 1.Imputacion mts2 === #####
#===================================#
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

#
train$surface_total <- ifelse(is.na(train$surface_covered), train$surface_total, train$surface_covered)

test$surface_total <- ifelse(is.na(test$surface_covered), test$surface_total, test$surface_covered)

train <- train |>
  mutate(surface_total = ifelse(surface_total > 1000, NA, surface_total))

test <- test |>
  mutate(surface_total = ifelse(surface_total > 1000, NA, surface_total))


# result = 6353 imputados, podría mejorar 
30079 - sum(is.na(train$surface_total))
train <- train |>
  select(-c(surface_extr, surface_nums))

test <- test |>
  select(-c(surface_extr, surface_nums))



#====================================#
##### === 2.Imputacion rooms === #####
#====================================#

train <- train |>
  mutate(rooms_extr = str_extract(description, 
                                  "\\b(\\d+|uno|una|un|dos|tres|cuatro|cinco|seis|siete|ocho|nueve|diez)\\s*(?:habitacion|habitaciones|cuarto|cuartos|alcoba|alcobas)\\b"
  )) #palbra o numero que antecede o

test <- test |>
  mutate(rooms_extr = str_extract(description, 
                                  "\\b(\\d+|uno|una|un|dos|tres|cuatro|cinco|seis|siete|ocho|nueve|diez)\\s*(?:habitacion|habitaciones|cuarto|cuartos|alcoba|alcobas)\\b"
  )) #palbra o numero que antecede o

numeros_escritos <- c("uno|primero|primer|un|una", "dos|segundo|segund", "tres|tercero|tercer", "cuatro|cuarto", "cinco|quinto", "seis|sexto", "siete|septimo", "ocho|octavo", "nueve|noveno", "diez|decimo|dei")
numeros_numericos <- as.character(1:10)

train <- train %>%
  mutate(rooms_extr = str_replace_all(rooms_extr, setNames(numeros_numericos,numeros_escritos))) # set names empareja

test <- test %>%
  mutate(rooms_extr = str_replace_all(rooms_extr, setNames(numeros_numericos,numeros_escritos))) # set names empareja

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

train$rooms <- ifelse(is.na(train$rooms), train$rooms_nums, train$rooms)

test$rooms <- ifelse(is.na(test$rooms), test$rooms_nums, test$rooms)

# result = 12738 imputados, bien
18260 - sum(is.na(train$rooms))
train <- train |>
  select(-c(rooms_extr, rooms_nums))

test <- test |>
  select(-c(rooms_extr, rooms_nums))

# Imputacion bathrooms

#========================================#
##### === 3.Imputacion bathrooms === #####
#========================================#

train <- train |>
  mutate(bathrooms_extr = str_extract(description, 
                                      "\\b(\\d+|uno|dos|tres|cuatro|cinco|seis|siete|ocho|nueve|diez)\\s*(?:bano|banos|sanitario|sanitarios)\\b"
  )) #palbra o numero que antecede o

test <- test |>
  mutate(bathrooms_extr = str_extract(description, 
                                      "\\b(\\d+|uno|una|un|dos|tres|cuatro|cinco|seis|siete|ocho|nueve|diez)\\s*(?:bano|banos|sanitario|sanitarios)\\b"
  )) #palbra o numero que antecede o

numeros_escritos <- c("uno|primero|primer|un|una", "dos|segundo|segund", "tres|tercero|tercer", "cuatro|cuarto", "cinco|quinto", "seis|sexto", "siete|septimo", "ocho|octavo", "nueve|noveno", "diez|decimo|dei")
numeros_numericos <- as.character(1:10)

train <- train %>%
  mutate(bathrooms_extr = str_replace_all(bathrooms_extr, setNames(numeros_numericos,numeros_escritos))) # set names empareja

test <- test %>%
  mutate(bathrooms_extr = str_replace_all(bathrooms_extr, setNames(numeros_numericos,numeros_escritos))) # set names empareja

# ahora me quedo con el numero

train <- train |>
  mutate(bathrooms_nums = as.integer(str_extract(bathrooms_extr, "\\d+")))

test <- test |>
  mutate(bathrooms_nums = as.integer(str_extract(bathrooms_extr, "\\d+")))


# Medida de proteccion
train <- train |>
  mutate(bathrooms_nums = ifelse(bathrooms_nums > 10, NA, bathrooms_nums))

test <- test |>
  mutate(bathrooms_nums = ifelse(bathrooms_nums > 10, NA, bathrooms_nums))

# Imputo

train$bathrooms <- ifelse(is.na(train$bathrooms), train$bathrooms_nums, train$bathrooms)

test$bathrooms <- ifelse(is.na(test$bathrooms), test$bathrooms_nums, test$bathrooms)

# Correccion adicional

# Define a function to update 'bathrooms' column
update_bathrooms <- function(data) {
  data$bathrooms <- ifelse(is.na(data$bathrooms), 
                           str_count(data$description, "\\bbano\\b|\\bbanos\\b"), 
                           data$bathrooms)
  return(data)
}

# Update 'bathrooms' column in train and test data frames
train <- update_bathrooms(train)
test <- update_bathrooms(test)

# result = 3930 imputados, bien
10071 - sum(is.na(train$bathrooms))
train <- train |>
  select(-c(bathrooms_extr, bathrooms_nums))

test <- test |>
  select(-c(bathrooms_extr, bathrooms_nums))


#================================================#
##### === 4.Imputando por el más cercano === #####
#================================================#
# 

# variable de costo metro cuadrado 
train <- train |>
  mutate(pmt2 = price/surface_total)


# Filter out appartments where pmt2 is empty
train_nonempty <- train[!is.na(train$pmt2),]

# Calculate distance matrix between all appartments
dist_matrix <- distm(train[,c("lon", "lat")], train_nonempty[,c("lon", "lat")])

# Find index of closest appartment where pmt2 is not empty
closest_index <- apply(dist_matrix, 1, which.min)

# Find value of closest appartment where pmt2 is not empty
closest_value <- train_nonempty$pmt2[closest_index]

# Replace NA values in pmt2 with closest values
train$pmt2 <- replace(train$pmt2, is.na(train$pmt2), closest_value)

# Calculamos el area en los missings

train <- train |>
  mutate(surface_total = ifelse(is.na(surface_total), price/pmt2, surface_total))

# En test no puedo hacer ese proceso ya que no hay variable de precio, utilizamos la variable de media de area por UPL.
neighborhood_means_test <- test %>%
  group_by(UPL) %>%
  summarize(mean_surface = mean(surface_total, na.rm = TRUE)) %>%
  ungroup()



# Step 2: Replace missing values in surface_total with the mean of the group they belong to in the test dataframe
test <- test %>%
  left_join(neighborhood_means_test, by = "UPL") %>%
  mutate(surface_total = ifelse(is.na(surface_total), mean_surface, surface_total)) %>%
  select(-mean_surface)

# with new data

train <- train %>%
  mutate(band = cut(surface_total, breaks = c(0, 100, 200, 300, Inf),
                    labels = c("<100", "100-200", "200-300", "300+"),
                    right = FALSE))

test <- test %>%
  mutate(band = cut(surface_total, breaks = c(0, 100, 200, 300, Inf),
                    labels = c("<100", "100-200", "200-300", "300+"),
                    right = FALSE))

# Replace 0 in bedrooms as missing (NA)
train$bedrooms[train$bedrooms == 0] <- NA

test$bedrooms[test$bedrooms == 0] <- NA

#Mode function

Mode <- function(x) {
  x <- x[!is.na(x)]  # Remove missing values
  if (length(x) == 0) {
    return(NA)       # Return NA if all values are missing
  }
  unique_values <- unique(x)
  counts <- table(x)
  max_count <- max(counts)
  mode_values <- unique_values[counts == max_count]
  return(mode_values)
}

# Impute bathrooms according to bands with the mode within its band
train <- train %>%
  group_by(band) %>%
  mutate(bathrooms = ifelse(is.na(bathrooms), Mode(bathrooms), bathrooms)) %>%
  ungroup()

test <- test %>%
  group_by(band) %>%
  mutate(bathrooms = ifelse(is.na(bathrooms), Mode(bathrooms), bathrooms)) %>%
  ungroup()

#  Impute bedrooms according to bands with the mode within its band
train <- train %>%
  group_by(band) %>%
  mutate(bedrooms = ifelse(is.na(bedrooms), Mode(bedrooms), bedrooms)) %>%
  ungroup()

test <- test %>%
  group_by(band) %>%
  mutate(bedrooms = ifelse(is.na(bedrooms), Mode(bedrooms), bedrooms)) %>%
  ungroup()

test <- test |>
  mutate(surface_total = ifelse(is.na(surface_total), mean(surface_total, na.rm = TRUE), surface_total)) 

