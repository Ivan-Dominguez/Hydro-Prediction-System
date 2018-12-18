library(rpart)
library(caTools)
library (dplyr)

setwd("~/Google Drive/Degree Project/Repository/Hydro-Prediction-System/training_data/Scaled Training Files")

#load data
x_vars <- read.csv("scaled_training_fwts_XVars.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"))
y_var <- read.csv("scaled_training_fwts_YVar.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"))

#merge files
x_vars$y <- paste(y_var$V1)

#split in training and test sets
split <- sample.split(x_vars$y, SplitRatio = 0.8)
trainning_set <- subset(x_vars, split == TRUE)
test_set <- subset(x_vars, split == FALSE)
  
#create tree
tree <- rpart(y ~., data = x_vars)

#prediction
y_predict <- predict(tree, newdata = test_set)

summary(tree)
rpart.plot(tree)
