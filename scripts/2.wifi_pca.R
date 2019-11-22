# --------------------------------------------------------------------------- #
# Wifi Fingerprinting with Caret 
#    - dimensionality reduction with PCA (95% variance)
#    - using for loops to select model
# Author: Guillem Perdigó
# Date: November 2019
# --------------------------------------------------------------------------- #

library(caret)
library(foreach)
library(tidyverse)
#library(animation)
#library(devtools)
#library(ggmap)
#library(factoextra)

# read data
train <- readRDS("data/train_preproc.RDS")
valid <-readRDS("data/valid_preproc.RDS")

# Preprocess (train & valid) --------------------------------------------------

# remove columns with no/low variance
lowVar <- train %>% nearZeroVar(freqCut = 3500/1, uniqueCut = .1)
train <- select(train, -lowVar)
valid <- valid %>% select(names(train)) # keep in valid only columns in train
rm(lowVar)

# PCA ----
pca_scaled <- prcomp(train[10:ncol(train)], scale = TRUE) # scaled
pca <- prcomp(train[10:ncol(train)], scale = FALSE) # not scaled

# percent variation explained by component

# scaled
# pca_scaled.var <- pca_scaled$sdev^2
# pca_scaled.var.per <- round(pca_scaled.var/sum(pca_scaled.var)*100, 2)
# sum(pca_scaled.var.per[1:150]) # 150 comp. explain 95% variance

# not scaled
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 2)
sum(pca.var.per[1:100]) # 117 comp. explain 95% variance

# explore PCA
barplot(pca.var.per,
        main = "Scree Plot",
        xlab = "Principal Component",
        ylab = "Percent Variation")

pcaCharts <- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var / sum(x.var)
  print("proportions of variance:")
  print(x.pvar)
  
  par(mfrow = c(2, 2))
  plot(
    x.pvar,
    xlab = "Principal component",
    ylab = "Proportion of variance explained",
    ylim = c(0, 1),
    type = 'b'
  )
  plot(
    cumsum(x.pvar),
    xlab = "Principal component",
    ylab = "Cumulative Proportion of variance explained",
    ylim = c(0, 1),
    type = 'b'
  )
  screeplot(x)
  screeplot(x, type = "l")
  par(mfrow = c(1, 1))
}
pcaCharts(pca)

# keep only first 100 comp. in train
train_pca <- 
  as_tibble(pca$x) %>% 
  select(PC1:PC100)

# apply PCA to valid and keep first 117 comp.
valid_pca <-
  as_tibble(predict(
    pca,
    newdata = valid %>% select(5:ncol(valid))
  )) %>% select(PC1:PC100)

# adding target cols to pca df's
train_pca <- 
  train %>% 
  select(floor, lon, lat) %>% 
  cbind(train_pca)

valid_pca <- 
  valid %>% 
  select(floor, lon, lat) %>% 
  cbind(valid_pca)

rm(pca.var, pca.var.per, pred, pred_metric, pcaCharts, pca_scaled)

# save PCA & PCA datasets
saveRDS(pca, "models/pca.RDS")
saveRDS(train_pca, "data/train_preproc_pca.RDS")
saveRDS(valid_pca, "data/valid_preproc_pca.RDS")





# pca with caret ----

# train sample to iterate faster
trainSample_indices <- createDataPartition(y = train$BUILDINGID,
                                           p = .8,
                                           list = F)
trainSample <- train[trainSample_indices, ]
rm(trainSample_indices)

# define cross validation & PCA threshold
fitControl <-
  trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 1,
    allowParallel = TRUE,
    preProcOptions = list(tresh = 0.9)
  )

gbmFloor <- train(
  FLOOR ~ .,
  data = trainSample[, c(3, 5:ncol(trainSample))],
  method = "gbm",
  metric = "Accuracy",
  trControl = fitControl,
  preProcess = c("center", "scale", "pca"),
  pcaComp = 120
)
gbmFloor
#Accuracy  Kappa   
# 0.9692756  0.9597727

# valid accuracy
gbmFloorPred <- predict(gbmFloor, valid)
postResample(gbmFloorPred, valid$FLOOR)
#Accuracy     Kappa 
#0.8721872 0.8200494


