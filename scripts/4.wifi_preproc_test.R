# --------------------------------------------------------------------------- #
# Wifi Fingerprinting 
# Data preprocessing (for test)
# Author: Guillem Perdigó
# Date: November 2019
# --------------------------------------------------------------------------- #

library(tidyverse)

# read ----
test <- read_csv("data/testdata.csv")
train <- readRDS("data/train_preproc.RDS")

# Rename cols & transform data types ----
# shorter feature names
newColnames <- c('lon', 'lat', 'floor', 'build', 'space', 'rel_pos', 'user', 'phone', 'time')
colnames(test)[521:529] <- newColnames
rm(newColnames)

# categorical features to factors
test <- 
  test %>% 
  select(floor, build, space, rel_pos, user, phone) %>% 
  apply(2, as.factor) %>% 
  cbind(select(test, lon, lat, time, 1:520))

test$time <- as.POSIXct(test$time, origin = '1970-01-01', tz = 'GMT')

# RSSI 100 to -110 ----
test[test==100] = -110

# filter out WAP's not present in test ----
test <-
  test %>% 
  select(names(train))

# pca to test
pca <- readRDS("models/pca.RDS")

test_pca <-
  as_tibble(predict(
    pca,
    newdata = test %>% select(5:ncol(test))
  )) %>% select(PC1:PC100)

test_pca <- 
  test %>% 
  select(floor, lon, lat) %>% 
  cbind(test_pca)

# save preprocessed files
saveRDS(test_pca, "data/test_preproc_pca.RDS")




