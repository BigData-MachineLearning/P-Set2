

# Modelos


### specifico engine Arbol

tree_spec <- decision_tree(
  
  tree_depth = tune(),
  min_n = tune()
  
) |>
  set_mode("regression")

#grilla de parametros

tune_grid_tree <- grid_regular(
  tree_depth(range = c(1,15)),
  min_n(range = c(1,200)),
  levels = 10
)


## mi bosque

# Tune grid  para el modelo de rf
rf_grid <- grid_regular(mtry(range = c(2, 4)),
                                min_n(range = c(1, 10)),
                                trees(range = c(100, 300)), levels = 10)

# Agregar modelos basados en árboles
# Random Forest

## Modelo de rf
rf_spec<- rand_forest(
  mtry = tune(),              # Hiperparámetro: Número de variables a considerar en cada división
  min_n = tune(),             # Hiperparámetro: Profundidad mínima del árbol
  trees = tune(),
) %>%
  set_engine("randomForest") %>%
  set_mode("regression")       # Cambiar a modo de regresión


# boosting tree

# Tune grid aleatorio para el modelo de boost
tune_grid_boost <- grid_regular(
  trees(range = c(400, 600)),
  min_n(range = c(1, 3)),
  learn_rate(range = c(0.001, 0.01)), levels = 10
)

# Especificación del modelo boost_tree en tidymodels
boost_spec <- boost_tree(
  trees = tune(),
  min_n = tune(),
  learn_rate = tune()
) %>%
  set_mode("regression")  # Cambiar a modo de regresión

# Recetas


# Primera receta
rec_1 <- recipe(price ~ distancia_parque + rooms + bathrooms + property_type_2 +parqueadero + distancia_bus + piso_numerico, data = db) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) 


# Creo los workflows

# Crear un flujo de trabajo que incluye la receta de preprocesamiento y el modelo
## para el caso de los arboles incorpora no linealidades.
workflow_1.1 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(tree_spec)

workflow_1.2 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(rf_spec)

workflow_1.3 <- workflow() %>%
  add_recipe(rec_1) %>%
  add_model(boost_spec)


# Cross validation espaciales Tunear mis parametros


# En bloques


train_sf <- st_as_sf(train,
                     coords = c("lon", "lat"),
                     crs = 4326)


#Creo los folds espaciales
set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 5)

autoplot(block_folds)

p_load("purrr")

walk(block_folds$splits, function(x) print(autoplot(x)))

# Esto se utilizará para evaluar el rendimiento del modelo en diferentes subconjuntos de  datos durante la validación cruzada.
df_fold <- vfold_cv(train, v = 3)

tune_tree <- tune_grid(
  workflow_1.1,
  resamples = block_folds, 
  grid = tune_grid_tree,
  metrics = metric_set(mae)
)


tune_rf <- tune_grid(
  workflow_1.2,
  resamples = block_folds, 
  grid = rf_grid_random,
  metrics = metric_set(mae)
)



tune_boost <- tune_grid(
  workflow_1.3,
  resamples = block_folds, 
  grid = tune_grid_boost,
  metrics = metric_set(mae)
)

# Utilizar 'select_best' para seleccionar el mejor valor.
best_parms_tree <- select_best(tune_tree, metric = "mae")
best_parms_tree

## # A tibble: 1 × 3
##   tree_depth min_n .config             
##        <int> <int> <chr>               
## 1          6    18 Preprocessor1_Model3

# Utilizar 'select_best' para seleccionar el mejor valor.
best_parms_rf<- select_best(tune_rf, metric = "mae")
best_parms_rf

## # A tibble: 1 × 4
##    mtry trees min_n .config             
##   <int> <int> <int> <chr>               
## 1     3   252     8 Preprocessor1_Model4

# Utilizar 'select_best' para seleccionar el mejor valor.
best_parms_boost <- select_best(tune_boost, metric = "mae")
best_parms_boost

## # A tibble: 1 × 4
##   trees min_n learn_rate .config             
##   <int> <int>      <dbl> <chr>               
## 1   524     1       1.02 Preprocessor1_Model1

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
tree_final <- finalize_workflow(workflow_1.1, best_parms_tree)

# Ajustar el modelo  utilizando los datos de entrenamiento
tree_final_fit <- fit(tree_final, data = test)


# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
rf_final <- finalize_workflow(workflow_1.2, best_parms_rf)

# Ajustar el modelo utilizando los datos de entrenamiento
rf_final_fit <- fit(rf_final, data = test)


# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
boost_final <- finalize_workflow(workflow_1.3, best_parms_boost)

# Ajustar el modelo  utilizando los datos de entrenamiento
boost_final_fit <- fit(boost_final, data = test)

augment(tree_final_fit, new_data = test) %>%
  mae(truth = price, estimate = .pred)