# --------------------------------------------------------------------------- #
# Wifi Fingerprinting 
# Data preprocessing
# Author: Guillem Perdigó
# Date: November 2019
# --------------------------------------------------------------------------- #

library(tidyverse)

# read ----
train <- read_csv("data/trainingData.csv")
valid <- read_csv("data/validationData.csv")

# Rename cols & transform data types ----
# shorter feature names
newColnames <- c('lon', 'lat', 'floor', 'build', 'space', 'rel_pos', 'user', 'phone', 'time')
colnames(train)[521:529] <- newColnames
colnames(valid)[521:529] <- newColnames
rm(newColnames)

# categorical features to factors
train <- 
  train %>% 
  select(floor, build, space, rel_pos, user, phone) %>% 
  apply(2, as.factor) %>% 
  cbind(select(train, lon, lat, time, 1:520))

train$time <- as.POSIXct(train$time, origin = '1970-01-01', tz = 'GMT')

valid <- 
  valid %>% 
  select(floor, build, space, rel_pos, user, phone) %>% 
  apply(2, as.factor) %>% 
  cbind(select(valid, lon, lat, time, 1:520))

valid$time <- as.POSIXct(valid$time, origin = '1970-01-01', tz = 'GMT')

# remove duplicate rows ----
train <- unique(train)

# RSSI 100 to -110 ----
train[train==100] = -110
valid[valid==100] = -110

# filter out WAP's with maximum strength = -110 ----
maxStr_train <- train %>% 
  select(10:529) %>% 
  summarise_all(max) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column('WAP')

maxStr_valid <- valid %>% 
  select(10:529) %>% 
  summarise_all(max) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column('WAP')

max110_tr <-
  maxStr_train %>% 
  filter(V1 == -110)

max110_val <-
  maxStr_valid %>% 
  filter(V1 == -110)

max110 <- rbind(max110_tr, max110_val)
max110 <- max110[,1]

train <-
  train %>% 
  select(-max110)

valid <-
  valid %>% 
  select(-max110)

# filter out from "train" rows with max RSSI value > -34 ----
waps <- names(train[,11:ncol(train)])
train$maxRSSI <-
  train %>% 
  select(waps) %>% 
  apply(1, max)

train <- 
  train %>% 
  filter(maxRSSI < -34) %>% 
  select(-maxRSSI)

# save preprocessed files
saveRDS(train, "data/train_preproc.RDS")
saveRDS(valid, "data/valid_preproc.RDS")





