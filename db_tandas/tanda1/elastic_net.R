#Bases de datos:


train <- import("db_tandas/tanda1/train_1.csv")
test <- import("db_tandas/tanda1/test_1.csv")


# =============================================================================#
############################ === E-NET 1 === ##################################
# =============================================================================#

# In Excel: no

# Subitted (10/()/2023)
# Jorge

# grid para cuadrar el lambda


penalty_grid <- grid_regular(penalty(range = c(0, 10)), mixture(), levels = c(penalty = 50, mixture = 50))


# Especifico el modelo
enet_spec <- 
  linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet")


# Declaro mi receta

enet_recipe <- 
  recipe(formula = price ~ year + surface_total + bedrooms + bathrooms + property_type2 + parqueadero  + pent_house + distancia_bus + ciclovia_near + 
           distancia_parque + distancia_cc 
         , data = train) %>%
  step_interact(terms = ~ bathrooms:bedrooms + surface_total:bedrooms + distancia_bus:ciclovia_near) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())


# Workflow

enet_workflow <- workflow() %>% 
  add_recipe(enet_recipe) %>% 
  add_model(enet_spec)

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
block_folds <- spatial_block_cv(train_sf, v = 10)

#autoplot(block_folds)

#Entreno

tune_enet <- tune_grid(
  enet_workflow,
  resamples = block_folds, 
  grid = penalty_grid,
  metrics = metric_set(mae)
)

# Escojo

best_parms_enet <- select_best(tune_enet, metric = "mae")
best_parms_enet

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
enet_final <- finalize_workflow(enet_workflow, best_parms_enet)

enet_final_fit <- fit(enet_final, data = train)

test$pred1 <- predict(enet_final_fit, test)[[1]]

# Guardar datos

submission_enet_1 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_enet_1, "results/tanda1_enet5.csv")