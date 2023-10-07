
# first model

rm(list = ls())
train <-as.data.frame(import("db_tandas/train_1.csv"))
test <- as.data.frame(import("db_tandas/test_1.csv"))

train$price <- as.numeric(train$price)
test$price <- as.numeric(test$price)


train_sf <- st_as_sf(train,
                     coords = c("lon", "lat"),
                     crs = 4326)

test_sf <- st_as_sf(test,
                    coords = c("lon", "lat"),
                    crs = 4326)
# Modelos


### specifico engine Arbol

tree_spec <- decision_tree(tree_depth = tune(), min_n = tune()) %>% 
  set_mode("regression")

#grilla de parametros

tune_grid_tree <- grid_regular(
  tree_depth(range = c(1,15)),
  min_n(range = c(1,200)),
  levels = 5
)

# Mi primera receta

rec_1 <- recipe(price ~ year + surface_total + bedrooms + bathrooms + property_type2 + parqueadero + piso_numerico + pent_house + distancia_bus + ciclovia_near + distancia_parque + distancia_cc
, data = train) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) 

# Mi workflow

workflow_1.1 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(tree_spec)

# En bloques

#Creo los folds espaciales
set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 5)

autoplot(block_folds)
#walk(block_folds$splits, function(x) print(autoplot(x)))

tune_tree <- tune_grid(
  workflow_1.1,
  resamples = block_folds, 
  grid = tune_grid_tree,
  metrics = metric_set(mae)
)

# Utilizar 'select_best' para seleccionar el mejor valor.
best_parms_tree <- select_best(tune_tree, metric = "mae")
best_parms_tree

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
tree_final <- finalize_workflow(workflow_1.1, best_parms_tree)

# Ajustar el modelo  utilizando los datos de entrenamiento
tree_final_fit <- fit(tree_final, data = test)

predictions_1.1 <- predict(tree_final_fit, new_data = test)