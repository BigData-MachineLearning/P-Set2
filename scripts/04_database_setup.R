# Data_base_creation

rm(list = ls())
source("scripts/00_packages.R")


train <- import("stores/train.csv")
template <-import("stores/submission_template.csv")
test <- import("stores/test.csv")

#
colnames(train)

# Que tenemos

# Anlisis missings

