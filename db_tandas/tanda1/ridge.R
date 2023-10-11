#Bases de datos:


train <- import("db_tandas/tanda1/train_1.csv")
test <- import("db_tandas/tanda1/test_1.csv")

# =============================================================================#
############################ === Ridge Uno=== ##################################
# =============================================================================#

# In Excel: YES

# Subitted (10/10/2023)
# Jorge

# grid para cuadrar el lambda


penalty_grid <- grid_regular(penalty(range = c(0, 10)), levels = 50)


# Especifico el modelo
ridge_spec <- 
  linear_reg(penalty = tune(), mixture = 0) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet")


# Declaro mi receta

ridge_recipe <- 
  recipe(formula = price ~ year + surface_total + bedrooms + bathrooms + property_type2 + parqueadero  + pent_house + distancia_bus + ciclovia_near + distancia_parque + distancia_cc 
, data = train) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())


# Workflow

ridge_workflow <- workflow() %>% 
  add_recipe(ridge_recipe) %>% 
  add_model(ridge_spec)

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
set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 5)

#autoplot(block_folds)

#Entreno

tune_ridge <- tune_grid(
  ridge_workflow,
  resamples = block_folds, 
  grid = penalty_grid,
  metrics = metric_set(mae)
)

# Escojo

best_parms_ridge <- select_best(tune_ridge, metric = "mae")

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
ridge_final <- finalize_workflow(ridge_workflow, best_parms_ridge)

ridge_final_fit <- fit(ridge_final, data = train)

test$pred1 <- predict(ridge_final_fit, test)[[1]]

# Guardar datos

submission_ridge_1 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_ridge_1, "results/tanda1_modelo1.csv")

# =============================================================================#
############################ === Ridge Dos=== ##################################
# =============================================================================#

# In Excel: YES

# Subitted (10/11/2023)
# Nicolas

# grid para cuadrar el lambda


penalty_grid <- grid_regular(penalty(range = c(0, 10)), levels = 50)


# Especifico el modelo
ridge_spec <- 
  linear_reg(penalty = tune(), mixture = 0) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet")


# Declaro mi receta

ridge_recipe <- 
  recipe(formula = price ~ year + surface_total + bedrooms + bathrooms + property_type2 + parqueadero  + pent_house + distancia_bus + ciclovia_near + distancia_parque + distancia_cc 
         , data = train) %>%
  step_interact(terms = ~ surface_total:bedrooms+bedrooms:bathrooms) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())


# Workflow

ridge_workflow <- workflow() %>% 
  add_recipe(ridge_recipe) %>% 
  add_model(ridge_spec)

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
set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 5)

#autoplot(block_folds)

#Entreno

tune_ridge <- tune_grid(
  ridge_workflow,
  resamples = block_folds, 
  grid = penalty_grid,
  metrics = metric_set(mae)
)

# Escojo

best_parms_ridge <- select_best(tune_ridge, metric = "mae")

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
ridge_final <- finalize_workflow(ridge_workflow, best_parms_ridge)

ridge_final_fit <- fit(ridge_final, data = train)

test$pred1 <- predict(ridge_final_fit, test)[[1]]

# Guardar datos

submission_ridge_1 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_ridge_1, "results/tanda1_modelo2.csv")


# =============================================================================#
############################ === Ridge tres === ##################################
# =============================================================================#

# In Excel: YES

# Subitted (10/11/2023)
# Nicolas

# grid para cuadrar el lambda


penalty_grid <- grid_regular(penalty(range = c(0, 10)), levels = 50)


# Especifico el modelo
ridge_spec <- 
  linear_reg(penalty = tune(), mixture = 0) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet")


# Declaro mi receta

ridge_recipe <- 
  recipe(formula = price ~ year + surface_total + bedrooms + bathrooms + property_type2 + parqueadero  + pent_house + distancia_bus + ciclovia_near + distancia_parque + distancia_cc 
         , data = train) %>%
  step_interact(terms = ~ distancia_bus:ciclovia_near + distancia_cc :distancia_parque) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())


# Workflow

ridge_workflow <- workflow() %>% 
  add_recipe(ridge_recipe) %>% 
  add_model(ridge_spec)

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
set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 5)

#autoplot(block_folds)

#Entreno

tune_ridge <- tune_grid(
  ridge_workflow,
  resamples = block_folds, 
  grid = penalty_grid,
  metrics = metric_set(mae)
)

# Escojo

best_parms_ridge <- select_best(tune_ridge, metric = "mae")

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
ridge_final <- finalize_workflow(ridge_workflow, best_parms_ridge)

ridge_final_fit <- fit(ridge_final, data = train)

test$pred1 <- predict(ridge_final_fit, test)[[1]]

# Guardar datos

submission_ridge_1 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_ridge_1, "results/tanda1_modelo3.csv")

# =============================================================================#
############################ === Ridge 4 === ##################################
# =============================================================================#

# In Excel: YES

# Subitted (10/11/2023)
# Nicolas

# grid para cuadrar el lambda


penalty_grid <- grid_regular(penalty(range = c(0, 10)), levels = 50)


# Especifico el modelo
ridge_spec <- 
  linear_reg(penalty = tune(), mixture = 0) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet")


# Declaro mi receta

ridge_recipe <- 
  recipe(formula = price ~ year + surface_total + bedrooms + bathrooms + property_type2 + parqueadero  + pent_house + distancia_bus + ciclovia_near + distancia_parque + distancia_cc 
         , data = train) %>%
  step_interact(terms = ~ distancia_cc:distancia_parque + bathrooms:bedrooms + surface_total:bedrooms) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())


# Workflow

ridge_workflow <- workflow() %>% 
  add_recipe(ridge_recipe) %>% 
  add_model(ridge_spec)

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
set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 5)

#autoplot(block_folds)

#Entreno

tune_ridge <- tune_grid(
  ridge_workflow,
  resamples = block_folds, 
  grid = penalty_grid,
  metrics = metric_set(mae)
)

# Escojo

best_parms_ridge <- select_best(tune_ridge, metric = "mae")

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
ridge_final <- finalize_workflow(ridge_workflow, best_parms_ridge)

ridge_final_fit <- fit(ridge_final, data = train)

test$pred1 <- predict(ridge_final_fit, test)[[1]]

# Guardar datos

submission_ridge_1 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_ridge_1, "results/tanda1_modelo4.csv")

# =============================================================================#
############################ === Ridge 5 === ##################################
# =============================================================================#

# In Excel: YES

# Subitted (10/11/2023)
# Nicolas

# grid para cuadrar el lambda


penalty_grid <- grid_regular(penalty(range = c(0, 10)), levels = 50)


# Especifico el modelo
ridge_spec <- 
  linear_reg(penalty = tune(), mixture = 0) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet")


# Declaro mi receta

ridge_recipe <- 
  recipe(formula = price ~ year + surface_total + bedrooms + bathrooms + property_type2 + parqueadero  + pent_house + distancia_bus + ciclovia_near + distancia_parque + distancia_cc 
         , data = train) %>%
  step_interact(terms = ~ bathrooms:bedrooms + surface_total:bedrooms + distancia_bus:ciclovia_near) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())


# Workflow

ridge_workflow <- workflow() %>% 
  add_recipe(ridge_recipe) %>% 
  add_model(ridge_spec)

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
set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 5)

#autoplot(block_folds)

#Entreno

tune_ridge <- tune_grid(
  ridge_workflow,
  resamples = block_folds, 
  grid = penalty_grid,
  metrics = metric_set(mae)
)

# Escojo

best_parms_ridge <- select_best(tune_ridge, metric = "mae")

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
ridge_final <- finalize_workflow(ridge_workflow, best_parms_ridge)

ridge_final_fit <- fit(ridge_final, data = train)

test$pred1 <- predict(ridge_final_fit, test)[[1]]

# Guardar datos

submission_ridge_1 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_ridge_1, "results/tanda1_modelo5.csv")