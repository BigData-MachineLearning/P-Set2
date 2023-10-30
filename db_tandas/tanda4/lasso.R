#Bases de datos:


rm(list = ls())


train2 <- import("db_tandas/tanda4/train_4.csv")
test2 <- import("db_tandas/tanda4/test_4.csv")
train2 <- train2 %>% mutate(surface_total2=surface_total^2,distancia_cc2=distancia_cc^2,
                            distancia_bus2=distancia_bus^2, distancia_parque2=distancia_parque^2,
                            distancia_police2=distancia_police^2)

test2 <- test2 %>% mutate(surface_total2=surface_total^2,distancia_cc2=distancia_cc^2,
                          distancia_bus2=distancia_bus^2, distancia_parque2=distancia_parque^2,
                          distancia_police2=distancia_police^2)
train2_final <- train2
test2_final  <- test2
names(train2)


# =============================================================================#
############################ === Lasso Uno=== ##################################
# =============================================================================#

# In Excel: NO

# Subitted : 10/26/2023
# Nicolas

#Esto se utilizará para evaluar el rendimiento del modelo en diferentes subconjuntos de  datos durante la validación cruzada.
df_fold <- vfold_cv(train2_final, v = 5)

lasso_recipe <- 
  recipe(formula = price ~ year + surface_total + estrato + bedrooms + bathrooms + parqueadero +
           distancia_bus + ciclovia_near + distancia_parque + distancia_cc + distancia_cc2 + 
           surface_total2+distancia_bus2+distancia_parque2, data = train2_final) %>%
  step_interact(terms = ~ bedrooms:bathrooms + ciclovia_near:distancia_bus) %>% 
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
lasso_final_fit <- fit(lasso_final, data = train2_final)

tidy(lasso_final_fit)
test2_final$pred1 <- predict(lasso_final_fit, test2_final)[[1]]

# Guardar datos
submission_lasso_1 <-test2_final |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_lasso_1, "results/tanda4_lasso1.csv")

