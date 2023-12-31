
#Bases de datos:

rm(list = ls())
train <- import("db_tandas/tanda2/train_2.csv")
test <- import("db_tandas/tanda2/test_2.csv")

# =============================================================================#
############################ === Forest 13 === ##################################
# =============================================================================#

# In Excel: NO

# Subitted (10/27/2023)
# Jorge
train <- train %>% mutate(pt_bedrooms=property_type2*bedrooms, pt_bathrooms=property_type2*bathrooms,
                          pt_surface=property_type2*surface_total,pt_est=property_type2*estrato)
test <- test %>%  mutate(pt_bedrooms=property_type2*bedrooms, pt_bathrooms=property_type2*bathrooms,
                         pt_surface=property_type2*surface_total,pt_est=property_type2*estrato)
### specifico engine Arbol

forest_spec <- rand_forest(
  mtry = tune(),              # Hiperparámetro: Número de variables a considerar en cada división
  min_n = tune(),             # Hiperparámetro: Profundidad mínima del árbol
  trees = tune(),
) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("regression")       # Cambiar a modo de regresión

#grilla de parametros



tune_grid_forest <- grid_regular(
  mtry(range = c(1, 10)),
  min_n(range = c(1, 20)),
  trees(range = c(5, 50)),
  levels = 5
)

forest_recipe <- 
  recipe(formula = price ~ year + surface_total + bedrooms + bathrooms + property_type2 + parqueadero  + pent_house + distancia_bus + ciclovia_near + 
           distancia_parque + distancia_cc + estrato + pt_bedrooms + pt_bathrooms+
           pt_surface+pt_est+UPL, data = train) %>%
  step_interact(terms = ~ bathrooms:bedrooms + distancia_bus:ciclovia_near) %>% 
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())

# Workflow

forest_workflow <- workflow() %>% 
  add_recipe(forest_recipe) %>% 
  add_model(forest_spec)

# Validación Cruzada Espacial en Bloques

train_sf <- st_as_sf(
  train,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("lon", "lat"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)
# aplicamos la funcion spatial_block_cv

doParallel::registerDoParallel(cores = 4)
tictoc::tic()

set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 10)

#autoplot(block_folds)

#Entreno

tune_forest <- tune_grid(
  forest_workflow,
  resamples = block_folds, 
  grid = tune_grid_forest,
  metrics = metric_set(mae)
)

tictoc::toc()

# Escojo

best_parms_forest <- select_best(tune_forest, metric = "mae")
best_parms_forest

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
forest_final <- finalize_workflow(forest_workflow, best_parms_forest)

forest_final_fit <- fit(forest_final, data = train)
importance2 <- vip::vi(forest_final_fit)
importance2 <- importance2 |>
  mutate(Importance = Importance/1000000)

test$pred1 <- predict(forest_final_fit, test)[[1]]

# Guardar datos

submission_forest_3 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_forest_3, "results/tanda2_forest13.csv")

# =============================================================================#
############################ === Forest 12 === ##################################
# =============================================================================#

# In Excel: NO

# Subitted (10/27/2023)
# Jorge
train <- train %>% mutate(pt_bedrooms=property_type2*bedrooms, pt_bathrooms=property_type2*bathrooms,
                          pt_surface=property_type2*surface_total,pt_est=property_type2*estrato)
test <- test %>%  mutate(pt_bedrooms=property_type2*bedrooms, pt_bathrooms=property_type2*bathrooms,
                    pt_surface=property_type2*surface_total,pt_est=property_type2*estrato)
### specifico engine Arbol

forest_spec <- rand_forest(
  mtry = tune(),              # Hiperparámetro: Número de variables a considerar en cada división
  min_n = tune(),             # Hiperparámetro: Profundidad mínima del árbol
  trees = tune(),
) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("regression")       # Cambiar a modo de regresión

#grilla de parametros



tune_grid_forest <- grid_regular(
  mtry(range = c(1, 10)),
  min_n(range = c(1, 20)),
  trees(range = c(5, 50)),
  levels = 5
)

forest_recipe <- 
  recipe(formula = price ~ year + surface_total + bedrooms + bathrooms + property_type2 + parqueadero  + pent_house + distancia_bus + ciclovia_near + 
           distancia_parque + distancia_cc + estrato + pt_bedrooms + pt_bathrooms+
           pt_surface+pt_est, data = train) %>%
  step_interact(terms = ~ bathrooms:bedrooms + distancia_bus:ciclovia_near) %>% 
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())

# Workflow

forest_workflow <- workflow() %>% 
  add_recipe(forest_recipe) %>% 
  add_model(forest_spec)

# Validación Cruzada Espacial en Bloques

train_sf <- st_as_sf(
  train,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("lon", "lat"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)
# aplicamos la funcion spatial_block_cv

doParallel::registerDoParallel(cores = 4)
tictoc::tic()

set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 10)

#autoplot(block_folds)

#Entreno

tune_forest <- tune_grid(
  forest_workflow,
  resamples = block_folds, 
  grid = tune_grid_forest,
  metrics = metric_set(mae)
)

tictoc::toc()

# Escojo

best_parms_forest <- select_best(tune_forest, metric = "mae")
best_parms_forest

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
forest_final <- finalize_workflow(forest_workflow, best_parms_forest)

forest_final_fit <- fit(forest_final, data = train)
importance2 <- vip::vi(forest_final_fit)
importance2 <- importance2 |>
  mutate(Importance = Importance/1000000)

test$pred1 <- predict(forest_final_fit, test)[[1]]

# Guardar datos

submission_forest_3 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_forest_3, "results/tanda2_forest12.csv")

# =============================================================================#
############################ === Forest 11 === #################################
# =============================================================================#

# In Excel: YES

# Subitted (10/25/2023)
# Jorge

forest_spec <- rand_forest(
  mtry = tune(),              # Hiperparámetro: Número de variables a considerar en cada división
  min_n = tune(),             # Hiperparámetro: Profundidad mínima del árbol
  trees = tune(),
) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("regression")       # Cambiar a modo de regresión

#grilla de parametros



tune_grid_forest <- grid_regular(
  mtry(range = c(4, 10)),
  min_n(range = c(5, 30)),
  trees(range = c(50, 500)),
  levels = c(7, 6, 10)
)

forest_recipe <- 
  recipe(formula = price ~ year + surface_total + bedrooms + bathrooms + property_type2 + parqueadero + pent_house + distancia_bus + ciclovia_near + distancia_parque + distancia_cc + UPL + estrato, data = train) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(terms = ~ bathrooms:bedrooms + distancia_bus:ciclovia_near + starts_with("estrato"):distancia_parque +
                  starts_with("estrato"):distancia_cc)


# Workflow

forest_workflow <- workflow() %>% 
  add_recipe(forest_recipe) %>% 
  add_model(forest_spec)

# Validación Cruzada Espacial en Bloques

train_sf <- st_as_sf(
  train,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("lon", "lat"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)
# aplicamos la funcion spatial_block_cv

doParallel::registerDoParallel(cores = 4)
tictoc::tic()

set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 10)

#autoplot(block_folds)

#Entreno

tune_forest <- tune_grid(
  forest_workflow,
  resamples = block_folds, 
  grid = tune_grid_forest,
  metrics = metric_set(mae)
)

tictoc::toc()

# Escojo

best_parms_forest <- select_best(tune_forest, metric = "mae")
best_parms_forest

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
forest_final <- finalize_workflow(forest_workflow, best_parms_forest)

forest_final_fit <- fit(forest_final, data = train)
importance2 <- vip::vi(forest_final_fit)
importance2 <- importance2 |>
  mutate(Importance = Importance/1000000)

test$pred1 <- predict(forest_final_fit, test)[[1]]

# Guardar datos

submission_forest_11 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_forest_11, "results/tanda2_forest11.csv")


# =============================================================================#
############################ === Forest 10 === ##################################
# =============================================================================#

# In Excel: YES

# Subitted (10/25/2023)
# Jorge

forest_spec <- rand_forest(
  mtry = tune(),              # Hiperparámetro: Número de variables a considerar en cada división
  min_n = tune(),             # Hiperparámetro: Profundidad mínima del árbol
  trees = tune(),
) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("regression")       # Cambiar a modo de regresión

#grilla de parametros



tune_grid_forest <- grid_regular(
  mtry(range = c(10,10)),
  min_n(range = c(20, 20)),
  trees(range = c(450, 450)),
  levels = c(1, 1, 1)
)

forest_recipe <- 
  recipe(formula = price ~ year + surface_total + bedrooms + bathrooms + property_type2 + parqueadero + pent_house + distancia_bus + ciclovia_near + distancia_parque + distancia_cc + UPL + estrato, data = train) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(terms = ~ bathrooms:bedrooms + distancia_bus:ciclovia_near + starts_with("estrato"):distancia_parque)


# Workflow

forest_workflow <- workflow() %>% 
  add_recipe(forest_recipe) %>% 
  add_model(forest_spec)

# Validación Cruzada Espacial en Bloques

train_sf <- st_as_sf(
  train,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("lon", "lat"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)
# aplicamos la funcion spatial_block_cv

doParallel::registerDoParallel(cores = 4)
tictoc::tic()

set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 10)

#autoplot(block_folds)

#Entreno

tune_forest <- tune_grid(
  forest_workflow,
  resamples = block_folds, 
  grid = tune_grid_forest,
  metrics = metric_set(mae)
)

tictoc::toc()

# Escojo

best_parms_forest <- select_best(tune_forest, metric = "mae")
best_parms_forest

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
forest_final <- finalize_workflow(forest_workflow, best_parms_forest)

forest_final_fit <- fit(forest_final, data = train)
importance2 <- vip::vi(forest_final_fit)
importance2 <- importance2 |>
  mutate(Importance = Importance/1000000)

test$pred1 <- predict(forest_final_fit, test)[[1]]

# Guardar datos

submission_forest_10 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_forest_10, "results/tanda2_forest10.csv")

# =============================================================================#
############################ === Forest 9 === ##################################
# =============================================================================#

# In Excel: NO

# Subitted (10/25/2023)
# Jorge

### specifico engine Arbol
train$estrato <- as.factor(train$estrato)
test$estrato <- as.factor(test$estrato)


forest_spec <- rand_forest(
  mtry = tune(),              # Hiperparámetro: Número de variables a considerar en cada división
  min_n = tune(),             # Hiperparámetro: Profundidad mínima del árbol
  trees = tune(),
) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("regression")       # Cambiar a modo de regresión

#grilla de parametros



tune_grid_forest <- grid_regular(
  mtry(range = c(5,5)),
  min_n(range = c(20, 20)),
  trees(range = c(450, 450)),
  levels = c(1, 1, 1)
)

forest_recipe <- 
  recipe(formula = price ~ year + surface_total + bedrooms + bathrooms + property_type2 + parqueadero  + pent_house + distancia_bus + ciclovia_near + 
           distancia_parque + distancia_cc + UPL + estrato, data = train) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(terms = ~ bathrooms:bedrooms + distancia_bus:ciclovia_near)


# Workflow

forest_workflow <- workflow() %>% 
  add_recipe(forest_recipe) %>% 
  add_model(forest_spec)

# Validación Cruzada Espacial en Bloques

train_sf <- st_as_sf(
  train,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("lon", "lat"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)
# aplicamos la funcion spatial_block_cv

doParallel::registerDoParallel(cores = 4)
tictoc::tic()

set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 10)

#autoplot(block_folds)

#Entreno

tune_forest <- tune_grid(
  forest_workflow,
  resamples = block_folds, 
  grid = tune_grid_forest,
  metrics = metric_set(mae)
)

tictoc::toc()

# Escojo

best_parms_forest <- select_best(tune_forest, metric = "mae")
best_parms_forest

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
forest_final <- finalize_workflow(forest_workflow, best_parms_forest)

forest_final_fit <- fit(forest_final, data = train)
importance2 <- vip::vi(forest_final_fit)
importance2 <- importance2 |>
  mutate(Importance = Importance/1000000)

test$pred1 <- predict(forest_final_fit, test)[[1]]

# Guardar datos

submission_forest_9 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_forest_9, "results/tanda2_forest9.csv")



# =============================================================================#
############################ === Forest 8 === ##################################
# =============================================================================#

# In Excel: YES

# Subitted (10/25/2023)
# Jorge

### specifico engine Arbol
train$estrato <- as.factor(train$estrato)
test$estrato <- as.factor(test$estrato)


forest_spec <- rand_forest(
  mtry = tune(),              # Hiperparámetro: Número de variables a considerar en cada división
  min_n = tune(),             # Hiperparámetro: Profundidad mínima del árbol
  trees = tune(),
) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("regression")       # Cambiar a modo de regresión

#grilla de parametros



tune_grid_forest <- grid_regular(
  mtry(range = c(10,10)),
  min_n(range = c(20, 20)),
  trees(range = c(450, 450)),
  levels = c(1, 1, 1)
)

forest_recipe <- 
  recipe(formula = price ~ year + surface_total + bedrooms + bathrooms + property_type2 + parqueadero  + pent_house + distancia_bus + ciclovia_near + 
           distancia_parque + distancia_cc + UPL + estrato, data = train) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(terms = ~ bathrooms:bedrooms + distancia_bus:ciclovia_near)


# Workflow

forest_workflow <- workflow() %>% 
  add_recipe(forest_recipe) %>% 
  add_model(forest_spec)

# Validación Cruzada Espacial en Bloques

train_sf <- st_as_sf(
  train,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("lon", "lat"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)
# aplicamos la funcion spatial_block_cv

doParallel::registerDoParallel(cores = 4)
tictoc::tic()

set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 10)

#autoplot(block_folds)

#Entreno

tune_forest <- tune_grid(
  forest_workflow,
  resamples = block_folds, 
  grid = tune_grid_forest,
  metrics = metric_set(mae)
)

tictoc::toc()

# Escojo

best_parms_forest <- select_best(tune_forest, metric = "mae")
best_parms_forest

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
forest_final <- finalize_workflow(forest_workflow, best_parms_forest)

forest_final_fit <- fit(forest_final, data = train)
importance2 <- vip::vi(forest_final_fit)
importance2 <- importance2 |>
  mutate(Importance = Importance/1000000)

test$pred1 <- predict(forest_final_fit, test)[[1]]

# Guardar datos

submission_forest_8 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_forest_8, "results/tanda2_forest8.csv")


# =============================================================================#
############################ === Forest 7 === ##################################
# =============================================================================#

# In Excel: YES

# Subitted (10/25/2023)
# JORGE-

### specifico engine Arbol
train$estrato <- as.factor(train$estrato)
test$estrato <- as.factor(test$estrato)


forest_spec <- rand_forest(
  mtry = tune(),              # Hiperparámetro: Número de variables a considerar en cada división
  min_n = tune(),             # Hiperparámetro: Profundidad mínima del árbol
  trees = tune(),
) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("regression")       # Cambiar a modo de regresión

#grilla de parametros



tune_grid_forest <- grid_regular(
  mtry(range = c(10,10)),
  min_n(range = c(50, 50)),
  trees(range = c(450, 450)),
  levels = c(1, 1, 1)
)

forest_recipe <- 
  recipe(formula = price ~ year + surface_total + bedrooms + bathrooms + property_type2 + parqueadero  + pent_house + distancia_bus + ciclovia_near + 
           distancia_parque + distancia_cc + UPL + estrato, data = train) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(terms = ~ bathrooms:bedrooms + distancia_bus:ciclovia_near)


# Workflow

forest_workflow <- workflow() %>% 
  add_recipe(forest_recipe) %>% 
  add_model(forest_spec)

# Validación Cruzada Espacial en Bloques

train_sf <- st_as_sf(
  train,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("lon", "lat"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)
# aplicamos la funcion spatial_block_cv

doParallel::registerDoParallel(cores = 4)
tictoc::tic()

set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 10)

#autoplot(block_folds)

#Entreno

tune_forest <- tune_grid(
  forest_workflow,
  resamples = block_folds, 
  grid = tune_grid_forest,
  metrics = metric_set(mae)
)

tictoc::toc()

# Escojo

best_parms_forest <- select_best(tune_forest, metric = "mae")
best_parms_forest

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
forest_final <- finalize_workflow(forest_workflow, best_parms_forest)

forest_final_fit <- fit(forest_final, data = train)
importance2 <- vip::vi(forest_final_fit)
importance2 <- importance2 |>
  mutate(Importance = Importance/1000000)

test$pred1 <- predict(forest_final_fit, test)[[1]]

# Guardar datos

submission_forest_7 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_forest_7, "results/tanda2_forest7.csv")

# =============================================================================#
############################ === Forest 6 === ##################################
# =============================================================================#

# In Excel: YES

# Subitted (10/20/2023)
# Jorge

### specifico engine Arbol

forest_spec <- rand_forest(
  mtry = tune(),              # Hiperparámetro: Número de variables a considerar en cada división
  min_n = tune(),             # Hiperparámetro: Profundidad mínima del árbol
  trees = tune(),
) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("regression")       # Cambiar a modo de regresión

#grilla de parametros



tune_grid_forest <- grid_regular(
  mtry(range = c(5, 5)),
  min_n(range = c(50, 50)),
  trees(range = c(300, 300)),
  levels = c(1, 1, 1)
)

forest_recipe <- 
  recipe(formula = price ~ year + surface_total + bedrooms + bathrooms + property_type2 + parqueadero  + pent_house + distancia_bus + ciclovia_near + 
           distancia_parque + distancia_cc + UPL , data = train) %>%
  step_interact(terms = ~ bathrooms:bedrooms + distancia_bus:ciclovia_near) %>% 
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())

# Workflow

forest_workflow <- workflow() %>% 
  add_recipe(forest_recipe) %>% 
  add_model(forest_spec)

# Validación Cruzada Espacial en Bloques

train_sf <- st_as_sf(
  train,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("lon", "lat"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)
# aplicamos la funcion spatial_block_cv

doParallel::registerDoParallel(cores = 4)
tictoc::tic()

set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 10)

#autoplot(block_folds)

#Entreno

tune_forest <- tune_grid(
  forest_workflow,
  resamples = block_folds, 
  grid = tune_grid_forest,
  metrics = metric_set(mae)
)

tictoc::toc()

# Escojo

best_parms_forest <- select_best(tune_forest, metric = "mae")
best_parms_forest

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
forest_final <- finalize_workflow(forest_workflow, best_parms_forest)

forest_final_fit <- fit(forest_final, data = train)
importance2 <- vip::vi(forest_final_fit)
importance2 <- importance2 |>
  mutate(Importance = Importance/1000000)

test$pred1 <- predict(forest_final_fit, test)[[1]]

# Guardar datos

submission_forest_6 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_forest_6, "results/tanda2_forest6.csv")


# =============================================================================#
############################ === Forest 5 === ##################################
# =============================================================================#

# In Excel: YES

# Subitted (10/17/2023)
# Jorge

### specifico engine Arbol

forest_spec <- rand_forest(
  mtry = tune(),              # Hiperparámetro: Número de variables a considerar en cada división
  min_n = tune(),             # Hiperparámetro: Profundidad mínima del árbol
  trees = tune(),
) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("regression")       # Cambiar a modo de regresión

#grilla de parametros



tune_grid_forest <- grid_regular(
  mtry(range = c(4, 10)),
  min_n(range = c(5, 50)),
  trees(range = c(300, 300)),
  levels = c(7, 10, 1)
  )
  
  forest_recipe <- 
    recipe(formula = price ~ year + surface_total + bedrooms + bathrooms + property_type2 + parqueadero  + pent_house + distancia_bus + ciclovia_near + 
             distancia_parque + distancia_cc + estrato , data = train) %>%
    step_interact(terms = ~ bathrooms:bedrooms + distancia_bus:ciclovia_near) %>% 
    step_novel(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors())
  
  # Workflow
  
  forest_workflow <- workflow() %>% 
    add_recipe(forest_recipe) %>% 
    add_model(forest_spec)
  
  # Validación Cruzada Espacial en Bloques
  
  train_sf <- st_as_sf(
    train,
    # "coords" is in x/y order -- so longitude goes first!
    coords = c("lon", "lat"),
    # Set our coordinate reference system to EPSG:4326,
    # the standard WGS84 geodetic coordinate reference system
    crs = 4326
  )
  # aplicamos la funcion spatial_block_cv
  
  doParallel::registerDoParallel(cores = 4)
  tictoc::tic()
  
  set.seed(123)
  block_folds <- spatial_block_cv(train_sf, v = 10)
  
  #autoplot(block_folds)
  
  #Entreno
  
  tune_forest <- tune_grid(
    forest_workflow,
    resamples = block_folds, 
    grid = tune_grid_forest,
    metrics = metric_set(mae)
  )
  
  tictoc::toc()
  
  # Escojo
  
  best_parms_forest <- select_best(tune_forest, metric = "mae")
  best_parms_forest
  
  # Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
  forest_final <- finalize_workflow(forest_workflow, best_parms_forest)
  
  forest_final_fit <- fit(forest_final, data = train)
  importance2 <- vip::vi(forest_final_fit)
  importance2 <- importance2 |>
    mutate(Importance = Importance/1000000)
  
  test$pred1 <- predict(forest_final_fit, test)[[1]]
  
  # Guardar datos
  
  submission_forest_5 <- test |> select(property_id, pred1) |>
    rename(price = pred1) |>
    mutate(price = round(price))
  
  
  rio::export(submission_forest_5, "results/tanda2_forest5.csv")
  
  

# =============================================================================#
############################ === Forest 4 === ##################################
# =============================================================================#

# In Excel: YES

# Subitted (10/17/2023)
# Jorge

### specifico engine Arbol

forest_spec <- rand_forest(
  mtry = tune(),              # Hiperparámetro: Número de variables a considerar en cada división
  min_n = tune(),             # Hiperparámetro: Profundidad mínima del árbol
  trees = tune(),
) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("regression")       # Cambiar a modo de regresión

#grilla de parametros



tune_grid_forest <- grid_regular(
  mtry(range = c(4, 6)),
  min_n(range = c(5, 30)),
  trees(range = c(50, 500)),
  levels = c(3, 6, 10)
)

forest_recipe <- 
  recipe(formula = price ~ year + surface_total + bedrooms + bathrooms + property_type2 + parqueadero  + pent_house + distancia_bus + ciclovia_near + 
           distancia_parque + distancia_cc + UPL + estrato , data = train) %>%
  step_interact(terms = ~ bathrooms:bedrooms + distancia_bus:ciclovia_near) %>% 
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())

# Workflow

forest_workflow <- workflow() %>% 
  add_recipe(forest_recipe) %>% 
  add_model(forest_spec)

# Validación Cruzada Espacial en Bloques

train_sf <- st_as_sf(
  train,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("lon", "lat"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)
# aplicamos la funcion spatial_block_cv

doParallel::registerDoParallel(cores = 4)
tictoc::tic()

set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 10)

#autoplot(block_folds)

#Entreno

tune_forest <- tune_grid(
  forest_workflow,
  resamples = block_folds, 
  grid = tune_grid_forest,
  metrics = metric_set(mae)
)

tictoc::toc()

# Escojo

best_parms_forest <- select_best(tune_forest, metric = "mae")
best_parms_forest

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
forest_final <- finalize_workflow(forest_workflow, best_parms_forest)

forest_final_fit <- fit(forest_final, data = train)
importance2 <- vip::vi(forest_final_fit)
importance2 <- importance2 |>
  mutate(Importance = Importance/1000000)

test$pred1 <- predict(forest_final_fit, test)[[1]]

# Guardar datos

submission_forest_4 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_forest_4, "results/tanda2_forest4.csv")






# =============================================================================#
############################ === Forest 3 === ##################################
# =============================================================================#

# In Excel: YES

# Subitted (10/16/2023)
# Jorge

### specifico engine Arbol

forest_spec <- rand_forest(
  mtry = tune(),              # Hiperparámetro: Número de variables a considerar en cada división
  min_n = tune(),             # Hiperparámetro: Profundidad mínima del árbol
  trees = tune(),
) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("regression")       # Cambiar a modo de regresión

#grilla de parametros



tune_grid_forest <- grid_regular(
  mtry(range = c(1, 10)),
  min_n(range = c(1, 20)),
  trees(range = c(5, 50)),
  levels = 5
)

forest_recipe <- 
  recipe(formula = price ~ year + surface_total + bedrooms + bathrooms + property_type2 + parqueadero  + pent_house + distancia_bus + ciclovia_near + 
           distancia_parque + distancia_cc + UPL + estrato , data = train) %>%
  step_interact(terms = ~ bathrooms:bedrooms + distancia_bus:ciclovia_near) %>% 
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())

# Workflow

forest_workflow <- workflow() %>% 
  add_recipe(forest_recipe) %>% 
  add_model(forest_spec)

# Validación Cruzada Espacial en Bloques

train_sf <- st_as_sf(
  train,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("lon", "lat"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)
# aplicamos la funcion spatial_block_cv

doParallel::registerDoParallel(cores = 4)
tictoc::tic()

set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 10)

#autoplot(block_folds)

#Entreno

tune_forest <- tune_grid(
  forest_workflow,
  resamples = block_folds, 
  grid = tune_grid_forest,
  metrics = metric_set(mae)
)

tictoc::toc()

# Escojo

best_parms_forest <- select_best(tune_forest, metric = "mae")
best_parms_forest

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
forest_final <- finalize_workflow(forest_workflow, best_parms_forest)

forest_final_fit <- fit(forest_final, data = train)
importance2 <- vip::vi(forest_final_fit)
importance2 <- importance2 |>
  mutate(Importance = Importance/1000000)

test$pred1 <- predict(forest_final_fit, test)[[1]]

# Guardar datos

submission_forest_3 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_forest_3, "results/tanda2_forest3.csv")


# =============================================================================#
############################ === Forest 2 === ##################################
# =============================================================================#

# In Excel: YES

# Subitted (10/15/2023)
# Jorge

### specifico engine Arbol

forest_spec <- rand_forest(
  mtry = tune(),              # Hiperparámetro: Número de variables a considerar en cada división
  min_n = tune(),             # Hiperparámetro: Profundidad mínima del árbol
  trees = tune(),
) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("regression")       # Cambiar a modo de regresión

#grilla de parametros



tune_grid_forest <- grid_regular(
  mtry(range = c(1, 10)),
  min_n(range = c(1, 20)),
  trees(range = c(5, 50)),
  levels = 5
)

forest_recipe <- 
  recipe(formula = price ~ year + surface_total + bedrooms + bathrooms + property_type2 + parqueadero  + pent_house + distancia_bus + ciclovia_near + 
           distancia_parque + distancia_cc + UPL , data = train) %>%
  step_interact(terms = ~ bathrooms:bedrooms + distancia_bus:ciclovia_near) %>% 
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())

# Workflow

forest_workflow <- workflow() %>% 
  add_recipe(forest_recipe) %>% 
  add_model(forest_spec)

# Validación Cruzada Espacial en Bloques

train_sf <- st_as_sf(
  train,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("lon", "lat"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)
# aplicamos la funcion spatial_block_cv

doParallel::registerDoParallel(cores = 4)
tictoc::tic()

set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 5)

#autoplot(block_folds)

#Entreno

tune_forest <- tune_grid(
  forest_workflow,
  resamples = block_folds, 
  grid = tune_grid_forest,
  metrics = metric_set(mae)
)

tictoc::toc()

# Escojo

best_parms_forest <- select_best(tune_forest, metric = "mae")
best_parms_forest

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
forest_final <- finalize_workflow(forest_workflow, best_parms_forest)

forest_final_fit <- fit(forest_final, data = train)
importance2 <- vip::vi(forest_final_fit)


test$pred1 <- predict(forest_final_fit, test)[[1]]

# Guardar datos

submission_forest_2 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_forest_2, "results/tanda2_forest2.csv")



# =============================================================================#
############################ === Forest 1 === ##################################
# =============================================================================#

# In Excel: YES

# Subitted (10/14/2023)
# Jorge

### specifico engine Arbol

forest_spec <- rand_forest(
  mtry = tune(),              # Hiperparámetro: Número de variables a considerar en cada división
  min_n = tune(),             # Hiperparámetro: Profundidad mínima del árbol
  trees = tune(),
) %>%
  set_engine("ranger") %>%
  set_mode("regression")       # Cambiar a modo de regresión

#grilla de parametros



tune_grid_forest <- grid_regular(
  mtry(range = c(1, 10)),
  min_n(range = c(1, 20)),
  trees(range = c(5, 50)),
  levels = 5
)

forest_recipe <- 
  recipe(formula = price ~ year + surface_total + bedrooms + bathrooms + property_type2 + parqueadero  + pent_house + distancia_bus + ciclovia_near + 
           distancia_parque + distancia_cc , data = train) %>%
  step_interact(terms = ~ bathrooms:bedrooms + distancia_bus:ciclovia_near) %>% 
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())

# Workflow

forest_workflow <- workflow() %>% 
  add_recipe(forest_recipe) %>% 
  add_model(forest_spec)

# Validación Cruzada Espacial en Bloques

train_sf <- st_as_sf(
  train,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("lon", "lat"),
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)
# aplicamos la funcion spatial_block_cv

doParallel::registerDoParallel(cores = 4)
tictoc::tic()

set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 5)

#autoplot(block_folds)

#Entreno

tune_forest <- tune_grid(
  forest_workflow,
  resamples = block_folds, 
  grid = tune_grid_forest,
  metrics = metric_set(mae)
)

tictoc::toc()

# Escojo

best_parms_forest <- select_best(tune_forest, metric = "mae")
best_parms_forest

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
forest_final <- finalize_workflow(forest_workflow, best_parms_forest)

forest_final_fit <- fit(forest_final, data = train)

test$pred1 <- predict(forest_final_fit, test)[[1]]

# Guardar datos

submission_forest_1 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_forest_1, "results/tanda2_forest1.csv")

 
