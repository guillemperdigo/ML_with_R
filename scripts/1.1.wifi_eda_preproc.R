# --------------------------------------------------------------------------- #
# Wifi Fingerprinting 
# EDA & Data preprocessing
# Author: Guillem Perdigó
# Date: November 2019
# --------------------------------------------------------------------------- #

# Load libraries
library(tidyverse)
library(plotly)

# Read data
train <- read_csv("data/trainingData.csv")
valid <- read_csv("data/validationData.csv")


# Summary of non-WAPs features
summary(train[521:529])
summary(valid[521:529])


# Rename cols & transform data types
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



# How many unique lon-lat-floor points do we have?
train %>% 
  select(lon, lat, floor) %>% 
  unique() %>% 
  nrow()

valid %>% 
  select(lon, lat, floor) %>% 
  unique() %>% 
  nrow()


# Let's plot unique points
train %>%
  select(lon, lat, floor) %>%
  unique() %>%
  plot_ly(
    x = train$lon,
    y = train$lat,
    z = train$floor,
    type = "scatter3d",
    mode = "markers"
  )

valid %>%
  select(lon, lat, floor) %>%
  unique() %>%
  plot_ly(
    x = valid$lon,
    y = valid$lat,
    z = valid$floor,
    type = "scatter3d",
    mode = "markers"
  )


# Let's explore both datasets combined
train$dataset <- "train"
valid$dataset <- "valid"
all <- rbind(train, valid)

all %>%
  select(lon, lat, dataset) %>%
  unique() %>%
  plot_ly(
    x = all$lon,
    y = all$lat,
    z = all$floor,
    type = "scatter3d",
    mode = "markers",
    color = all$dataset
  )



# Remove dataset column
train$dataset <- NULL
valid$dataset <- NULL



# Find and remove duplicate rows
train %>% unique() %>% nrow()  # 637 duplicate rows
train <- unique(train)

valid %>% unique() %>% nrow() # 0 duplicate rows


# Distribution of RSSI values across all WAPs (excluding 100s)
train %>% 
  select(10:529) %>% 
  as.matrix() %>% 
  as.vector() %>% 
  as_tibble() %>% 
  filter(value!=100) %>% 
  ggplot(aes(value)) +
  geom_density()

valid %>% 
  select(10:529) %>% 
  as.matrix() %>% 
  as.vector() %>% 
  as_tibble() %>% 
  filter(value!=100) %>% 
  ggplot(aes(value)) +
  geom_density()


# We will convert 100's to -110's (-110 is arbitrary, but makes some sense)
train[train==100] = -110
valid[valid==100] = -110

# We also saw that in "train" there's a group of outliers at around RSSI = -20,
# not present in "valid"
train %>%
select(10:529) %>%
max()

valid %>%
select(10:529) %>%
max()


# Let's visualize RSSI values for each WAP in "train"
train %>% 
  select(WAP001:WAP520) %>% 
  gather(key = 'WAP', value = 'RSSI') %>% 
  filter(RSSI != -110) %>% 
  ggplot(aes(x = WAP, y = RSSI)) +
  geom_point(size = 0.1, shape = 3)


# Maximum strength of each WAP
maxStr_train <- train %>% 
  select(10:529) %>% 
  summarise_all(max) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column('WAP')

head(maxStr_train)

maxStr_valid <- valid %>% 
  select(10:529) %>% 
  summarise_all(max) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column('WAP')

head(maxStr_valid)


# Visualize maximum strenght of WAPs
maxStr_train %>% 
  ggplot(aes(V1)) +
  geom_histogram(bins = 60) +
  xlim(-120, 10) +
  ylim(-10, 160)

maxStr_valid %>% 
  ggplot(aes(V1)) +
  geom_histogram() +
  xlim(-120, 10) +
  ylim(-10, 160)



# We'll filter out WAP's with maximum strength = -110 (it means we never receive
# any signal from them)
max110_tr <-
  maxStr_train %>% 
  filter(V1 == -110)

max110_val <-
  maxStr_valid %>% 
  filter(V1 == -110)

inner_join(max110_tr, max110_val, by = 'WAP') %>% 
  nrow()


# There are 55 WAP's without reception (all values = -110) in train and 153 
# WAP's without reception in valid. Interestingly, they do not overlap. 
# Let's exclude them all.
max110 <- rbind(max110_tr, max110_val)
max110 <- max110[,1]

train <-
train %>% 
select(-max110)

valid <-
valid %>% 
select(-max110)


# Let's go back to those strange RSSI values in "train", between -34 and 0
max34_tr <-
  maxStr_train %>% 
  filter(V1 >= -34)

max34_val <-
  maxStr_valid %>% 
  filter(V1 != -110)

inner_join(max34_tr, max34_val, by = 'WAP') %>% 
  nrow()


There are 81 WAPs that sometimes give signals "too strong" in "train", but they are present in "valid", without any inconsistencies. We will approach this problem by looking at the rows.
{r}
waps <- names(train[,11:ncol(train)])
train$maxRSSI <-
  train %>% 
  select(waps) %>% 
  apply(1, max)

rows34 <-
  train %>% 
  filter(maxRSSI > -34)

rows34 %>% 
  select(phone) %>% 
  table()



Turns out it was mostly fault of 1-3 phones. Overall, there are "only" 527 rows with such values. By deleting them, we're losing ~10 times less information than by deleting 81 columns.

{r}
train <- 
train %>% 
filter(maxRSSI < -34) %>% 
select(-maxRSSI)


# Let's explore phones
{r}
train %>% 
  ggplot(aes(phone, fill = build)) +
  geom_bar()


# Let's explore users and buildings
{r}
train %>% 
ggplot(aes(user, fill = build)) +
geom_bar()









