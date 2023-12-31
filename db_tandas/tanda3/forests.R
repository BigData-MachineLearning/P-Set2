
#Bases de datos:


train <- import("db_tandas/tanda3/train_3.csv")
test <- import("db_tandas/tanda3/test_3.csv")


# =============================================================================#
############################ === Forest 2 === ##################################
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

submission_forest_3 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_forest_2, "results/tanda3_forest3.csv")


# =============================================================================#
############################ === Forest 2 === ##################################
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
  recipe(formula = price ~ year + surface_total + property_type2 + parqueadero  + pent_house + distancia_bus + ciclovia_near + 
           distancia_parque + distancia_cc , data = train) %>%
  step_interact(terms = ~ distancia_bus:ciclovia_near) %>% 
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

submission_forest_2 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_forest_2, "results/tanda3_forest3.csv")





# =============================================================================#
############################ === Forest 1 === ##################################
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
  set_engine("ranger") %>%
  set_mode("regression")       # Cambiar a modo de regresión

#grilla de parametros



tune_grid_forest <- grid_regular(
  mtry(range = c(1, 10)),
  min_n(range = c(10, 50)),
  trees(range = c(5, 100)),
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

test$pred1 <- predict(forest_final_fit, test)[[1]]

# Guardar datos

submission_forest_1 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_forest_1, "results/tanda3_forest1.csv")

 
