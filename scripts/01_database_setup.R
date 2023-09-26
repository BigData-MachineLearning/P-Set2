# Data_base_creation

rm(list = ls())
source("scripts/00_packages.R")


train <- import("stores/train.csv")
template <-import("stores/submission_template.csv")
test <- import("stores/test.csv")


# At least 4 predictors coming from external sources


# At least 2 predictors coming from the title or description 
# of the properties.

