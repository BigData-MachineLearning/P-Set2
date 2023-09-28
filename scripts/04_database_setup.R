# Data_base_creation

rm(list = ls())
source("scripts/00_packages.R")


train <-   as.data.frame(import("stores/train.csv")) 
template <- as.data.frame(import("stores/submission_template.csv")) 
test <- as.data.frame(import("stores/test.csv")) 

#


# Que tenemos

colnames(train)
skim(train)

# Anlisis missings

