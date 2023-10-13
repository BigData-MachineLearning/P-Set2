
#Bases de datos:


train <- import("db_tandas/tanda1/train_1.csv")
test <- import("db_tandas/tanda1/test_1.csv")


# =============================================================================#
############################# === Tree 2 === ###################################
# =============================================================================#


### specifico engine Arbol

tree_spec <- decision_tree(
  
  tree_depth = tune(),
  min_n = tune()
  
) |>
  set_mode("regression")

#grilla de parametros


tune_grid_tree <- grid_regular(
  tree_depth(range = c(1,10)),
  min_n(range = c(1,20)),
  levels = c(tree_depth = 10, min_n = 20)
)

tree_recipe <- 
  recipe(formula = price ~ year + surface_total + bedrooms + bathrooms + property_type2 + parqueadero  + pent_house + distancia_bus + ciclovia_near + 
           distancia_parque + distancia_cc , data = train) %>%
  step_interact(terms = ~ bathrooms:bedrooms + distancia_bus:ciclovia_near) %>% 
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())

# Workflow

tree_workflow <- workflow() %>% 
  add_recipe(tree_recipe) %>% 
  add_model(tree_spec)

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

tune_tree <- tune_grid(
  tree_workflow,
  resamples = block_folds, 
  grid = tune_grid_tree,
  metrics = metric_set(mae)
)

# Escojo

best_parms_tree <- select_best(tune_tree, metric = "mae")
best_parms_tree

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
tree_final <- finalize_workflow(tree_workflow, best_parms_tree)

tree_final_fit <- fit(tree_final, data = train)

test$pred1 <- predict(tree_final_fit, test)[[1]]

# Guardar datos

submission_tree_2 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_tree_2, "results/tanda1_tree2.csv")




# =============================================================================#
############################# === Tree 1 === ###################################
# =============================================================================#


### specifico engine Arbol

tree_spec <- decision_tree(
  
  tree_depth = tune(),
  min_n = tune()
  
) |>
  set_mode("regression")

#grilla de parametros


tune_grid_tree <- grid_regular(
  tree_depth(range = c(1,10)),
  min_n(range = c(1,20)),
  levels = c(tree_depth = 50, min_n = 50)
)

tree_recipe <- 
  recipe(formula = price ~ year + surface_total + bedrooms + bathrooms + property_type2 + parqueadero  + pent_house + distancia_bus + ciclovia_near + 
           distancia_parque + distancia_cc , data = train) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors())

# Workflow

tree_workflow <- workflow() %>% 
  add_recipe(tree_recipe) %>% 
  add_model(tree_spec)

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

tune_tree <- tune_grid(
  tree_workflow,
  resamples = block_folds, 
  grid = tune_grid_tree,
  metrics = metric_set(mae)
)

# Escojo

best_parms_tree <- select_best(tune_tree, metric = "mae")
best_parms_tree

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
tree_final <- finalize_workflow(tree_workflow, best_parms_tree)

tree_final_fit <- fit(tree_final, data = train)

test$pred1 <- predict(tree_final_fit, test)[[1]]

# Guardar datos

submission_tree_1 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_tree_1, "results/tanda1_tree1.csv")



