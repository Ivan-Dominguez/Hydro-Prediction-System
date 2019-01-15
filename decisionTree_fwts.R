library(rpart)
library(caTools)
library (dplyr)
library(ggplot2)
library(caret)
library(rpart.plot)


setwd("~/Google Drive/Degree Project/Repository/Hydro-Prediction-System/training_data/Scaled Training Files")

#load data
x_vars <- read.csv("scaled_training_fwts_XVars.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"))
y_var <- read.csv("scaled_training_fwts_YVar.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"))

#merge files
x_vars$y <- as.numeric(paste(y_var$V1))

#split in training and test sets
split <- sample.split(x_vars$y, SplitRatio = 0.8)
trainning_set <- subset(x_vars, split == TRUE)
test_set <- subset(x_vars, split == FALSE)
  
#create tree
tree <- rpart(formula= y ~ .,
              data = trainning_set,
              minsplit = 2,
              minbucket = 2)

rpart.plot(tree)

#predictions
y_pred = predict(tree, newdata = test_set)

#visualize
ggplot() +
  geom_line(aes(x=1:576, y = test_set$y[1:576]),
             colour = 'red')+
  geom_line(aes(x=1:576, y = as.vector(y_pred[1:576])),
             colour = 'blue') +
  ggtitle('Decision Tree') +
  xlab('Time') +
  ylab('Fwts')
