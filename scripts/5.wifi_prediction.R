# --------------------------------------------------------------------------- #
# Wifi Fingerprinting
# Final predictions (for test)
# Author: Guillem Perdigó
# Date: November 2019
# --------------------------------------------------------------------------- #

# load data & models
test_preproc_pca <- readRDS("data/test_preproc_pca.RDS")
floor_model <- readRDS("models/pca_floor_ranger.RDS")
lon_model <- readRDS("models/pca_lon_knn.RDS")
lat_model <- readRDS("models/pca_lat_knn.RDS")

# predictions
test_preproc_pca$floor <-
  predict(floor_model,
          test_preproc_pca[, 4:ncol(test_preproc_pca)])

test_preproc_pca$lon <-
  predict(lon_model,
          test_preproc_pca[, 4:ncol(test_preproc_pca)])

test_preproc_pca$lat <-
  predict(lat_model,
          test_preproc_pca[, 4:ncol(test_preproc_pca)])

# write file
test_preproc_pca %>% 
  select(floor, lon, lat) %>% 
  write_csv2("data/guillem_perdigo_results")

