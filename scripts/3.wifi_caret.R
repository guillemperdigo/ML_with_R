# modeling --------------------------------------------------------------------

# read data
train_pca <- readRDS("data/train_preproc_pca.RDS")
valid_pca <-readRDS("data/valid_preproc_pca.RDS")


# sample data (to iterate faster)
set.seed(8888)
train_pca_Sample_indices <- createDataPartition(y = train_pca$floor,
                                                p = 1/20,
                                                list = F)
train_pca_sample <- train_pca[train_pca_Sample_indices, ]
rm(train_pca_Sample_indices)

# define cross validation
fitControl <-
  trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 1,
    allowParallel = TRUE
  )

# Predict floor ----
methods <- c("ranger", "gbm", "knn", "svmLinear")

comp_model_pca_floor <- c()

for(i in methods) {
  
  set.seed(8888)
  
  model <- train(
    floor ~ .,
    data = train_pca_sample[, c(1, 4:ncol(train_pca_sample))],
    method = i,
    metric = "Accuracy"
    #preProcess = c("center", "scale")
  )
  
  saveRDS(model, file = paste0("models/pca_floor_", i, ".RDS"))
  
  pred <- predict(model, newdata = valid_pca)
  
  pred_metric <- postResample(valid_pca$floor, pred)
  
  comp_model_pca_floor <- cbind(comp_model_pca_floor , pred_metric)
  
}

colnames(comp_model_pca_floor) <- methods

comp_model_pca_floor
#              ranger       gbm       knn svmLinear
# Accuracy 0.8739874 0.8334833 0.8136814 0.8289829
# Kappa    0.8240943 0.7684162 0.7454075 0.7640430

# tune ranger model for floor ----

# create a bigger sample
set.seed(8888)
train_pca_Sample_indices <- createDataPartition(y = train_pca$floor,
                                                p = 1/10,
                                                list = F)
train_pca_sample <- train_pca[train_pca_Sample_indices, ]
rm(train_pca_Sample_indices)

set.seed(8888)

rangerFloor <- readRDS("models/pca_floor_ranger.RDS")

rangerGrid <- expand.grid(
  .mtry = c(40, 60),
  .splitrule = "extratrees",
  .min.node.size = c(1, 5)
)

rangerFloor_tuned <- train(
  floor ~ .,
  data = train_pca_sample[, c(1, 4:ncol(train_pca_sample))],
  method = "ranger",
  tuneGrid = rangerGrid,
  metric = "Accuracy",
  num.trees = 50
)
rangerFloor_tuned

saveRDS(rangerFloor_tuned, file = paste0("models/rangerFloor_tuned.RDS"))

pred <- predict(rangerFloor_tuned, newdata = valid_pca)

pred_metric <- postResample(valid_pca$floor, pred)

comp_model_pca_floor <- cbind(comp_model_pca_floor , pred_metric)

colnames(comp_model_pca_floor) <- append(methods, "rangerFloor_tuned")

comp_model_pca_floor

# Predict longitude ----
methods <- c("knn")

comp_model_pca_lon <- c()

for(i in methods) {
  
  set.seed(8888)
  
  model <- train(
    lon ~ .,
    data = train_pca_sample[, c(2, 4:ncol(train_pca_sample))],
    method = i,
    metric = "MAE"
    #preProcess = c("center", "scale")
  )
  
  saveRDS(model, file = paste0("models/pca_lon_", i, ".RDS"))
  
  pred <- predict(model, newdata = valid_pca)
  
  pred_metric <- postResample(valid_pca$lon, pred)
  
  comp_model_pca_lon <- cbind(comp_model_pca_lon, pred_metric)
  
}

colnames(comp_model_pca_lon) <- methods

comp_model_pca_lon
#           ranger        gbm        knn  svmLinear
#RMSE     12.892098 15.5268125 17.7915658 57.2038427
#Rsquared  0.988866  0.9836978  0.9781445  0.8021643
#MAE       7.686468 10.4520144  7.4501969 41.1311201

# longitude tuning
# Your turn! Write your code here!


# Predict latitude ----
methods <- c("knn")

comp_model_pca_lat <- c()

for(i in methods) {
  
  set.seed(8888)
  
  model <- train(
    lat ~ .,
    data = train_pca_sample[, c(3, 4:ncol(train_pca_sample))],
    method = i,
    metric = "MAE"
    #preProcess = c("center", "scale")
  )
  
  saveRDS(model, file = paste0("models/pca_lat_", i, ".RDS"))
  
  pred <- predict(model, newdata = valid_pca)
  
  pred_metric <- postResample(valid_pca$lat, pred)
  
  comp_model_pca_lat <- cbind(comp_model_pca_lat, pred_metric)
  
}


colnames(comp_model_pca_lat) <- methods

comp_model_pca_lat
#           ranger        gbm        knn  svmLinear
# RMSE     10.3275966 13.5036298 13.9959584 30.428919
# Rsquared  0.9791253  0.9641142  0.9610209  0.815186
# MAE       6.6928284  9.2345750  6.9345844 22.389828

# latitude tuning

# Your turn! Write your code here
