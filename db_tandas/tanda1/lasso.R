#Bases de datos:
rm(list = ls())

train <- import("db_tandas/tanda1/train_1.csv")
test <- import("db_tandas/tanda1/test_1.csv")

# =============================================================================#
############################ === Lasso Uno=== ##################################
# =============================================================================#

# In Excel: NO- HAY QUE PONERLO; NO SE PUDO SUBIR TAMPOCO POR FALTA DE INTENTOS

# Subitted (10/11/2023)
# Nicolas

#Esto se utilizará para evaluar el rendimiento del modelo en diferentes subconjuntos de  datos durante la validación cruzada.
df_fold <- vfold_cv(train, v = 5)

lasso_recipe <- 
  recipe(formula = price ~ year + surface_total + bedrooms + bathrooms + property_type2 + parqueadero  + pent_house + distancia_bus + ciclovia_near + distancia_parque + distancia_cc 
         , data = train) %>%
  step_interact(terms = ~ surface_total:bedrooms+bedrooms:bathrooms+ciclovia_near:distancia_bus) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# Especificar y configurar un modelo de regresión Lasso

# Definir las especificaciones del modelo Lasso
lasso_spec <- 
  linear_reg(penalty = tune(), mixture = 1) %>%
  set_mode("regression") %>%
  set_engine("glmnet") 

# Crear un flujo de trabajo que incluye la receta de preprocesamiento y el modelo Lasso
lasso_workflow <- workflow() %>%
  add_recipe(lasso_recipe) %>%
  add_model(lasso_spec)

# Utilizar 'grid_regular' para generar una cuadrícula de valores de penalización
penalty_grid <- grid_regular(penalty(range = c(-2, 2)), levels = 50)

# Realizar una búsqueda de hiperparámetros en un flujo de trabajo Lasso 
#y visualizar los resultados

# Realizar la búsqueda de hiperparámetros utilizando tune_grid
tune_res <- tune_grid(
  lasso_workflow,
  resamples = df_fold, 
  grid = penalty_grid,
  metrics = metric_set(rmse)
)


# Utilizar 'select_best' para seleccionar el mejor valor de penalización
best_penalty <- select_best(tune_res, metric = "rmse")

# Finalizar el flujo de trabajo Lasso con el mejor valor de penalización
lasso_final <- finalize_workflow(lasso_workflow, best_penalty)

# Ajustar el modelo de regresión Lasso utilizando los datos de entrenamiento
lasso_final_fit <- fit(lasso_final, data = train)

test$pred1 <- predict(lasso_final_fit, test)[[1]]

# Guardar datos
submission_lasso_1 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_lasso_1, "results/tanda1_lassomodelo1.csv")
