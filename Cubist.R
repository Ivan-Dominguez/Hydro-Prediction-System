# Cubist Algorithm

library(Cubist)
library(caTools)

library(rpart)
library (dplyr)
library(ggplot2)
library(caret)
library(rpart.plot)

setwd("~/Google Drive/Degree Project/Repository/Hydro-Prediction-System/training_data/Scaled Training Files")

#load data
x_vars <- read.csv("scaled_training_fwts_XVars.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"))
y_var <- read.csv("scaled_training_fwts_YVar.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"))

#merge files
x_vars$y <- as.numeric(paste(y_var$fwts))

#split in training and test sets
split <- sample.split(x_vars$y, SplitRatio = 0.8)
trainning_set <- subset(x_vars, split == TRUE)
test_set <- subset(x_vars, split == FALSE)

#train model
model_tree <- cubist(x = trainning_set[, -16], y = trainning_set$y)

#predictions
Prediction <- predict(model_tree, test_set)
