#Bases de datos:


train <- import("db_tandas/tanda2/train_2.csv")
test <- import("db_tandas/tanda2/test_2.csv")


# ===============================================================================#
############################ === Boosting 1 === ##################################
# ===============================================================================#

# Especificaciones modelo boosting

boost_spec <- boost_tree(
  trees = tune(),
  learn_rate = tune(),
  min_n = tune()
) %>% 
  set_mode("regression")

#Pongo mi grilla de prueba

tune_grid_boost <-
  grid_regular( trees(range = c(400, 600)),
                min_n(range = c(1, 3)),
                learn_rate(range = c(0.001, 0.01)), levels = 5)


# Creo la receta


boost_recipe <- 
  recipe(formula = price ~ year + surface_total + bedrooms + bathrooms + property_type2 + parqueadero  + pent_house + distancia_bus + ciclovia_near + 
           distancia_parque + distancia_cc , data = train) %>%
  step_interact(terms = ~ bathrooms:bedrooms + distancia_bus:ciclovia_near) %>% 
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())

# Creo el workflow

boost_workflow <- workflow() |>
  add_model(boost_spec) |>
  add_recipe(boost_recipe)


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

tune_boost <-
  tune_grid(boost_workflow,
            resamples = block_folds,
            grid = tune_grid_boost,
            metrics = metric_set(mae))



tictoc::toc()

# Escojo

best_parms_boost <- select_best(tune_boost, metric = "mae")
best_parms_boost

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
boost_final <- finalize_workflow(boost_workflow, best_parms_boost)

boost_final_fit <- fit(boost_final, data = train)

test$pred1 <- predict(boost_final_fit, test)[[1]]

# Guardar datos

submission_boost_1 <- test |> select(property_id, pred1) |>
  rename(price = pred1) |>
  mutate(price = round(price))


rio::export(submission_boost_2, "results/tanda2_boost1.csv")







