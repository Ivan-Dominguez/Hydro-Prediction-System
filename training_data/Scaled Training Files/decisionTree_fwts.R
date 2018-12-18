library(rpart)
library(caTools)
library(plyr)


setwd("~/Google Drive/Degree Project/training_data/Scaled Training Files")

#load data
x_vars <- read.csv("scaled_training_fwts_XVars.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"))
y_var <- read.csv("scaled_training_fwts_YVar.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"))


#merge files
x_vars$y <- paste(y_var$V1)

#split in training and test sets
sets_x <- sample.split(x_vars, Splitratio = 0.8)
sets_y <- sample.split(y_var, Splitratio = 0.8)

train_x <- subset(x_vars, sets_x == TRUE)
test_x <- subset(x_vars, sets_x == FALSE)

train_y <- subset(y_var, sets_y == TRUE)
test_y <- subset(y_vars, sets_y == FALSE)

  

#create tree
tree <- rpart(Play ~.,
              method="class", data = DTdata, minsplit = 1)

summary(tree)
rpart.plot(tree)
