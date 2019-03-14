library(Cubist)
library(caTools)
library(rpart)
library (dplyr)
library(caret)
library(rpart.plot)
library(reticulate)
library(keras)
library(readr)
library(plotly)
library(lubridate)
library(stringr)
library(recipes)
library(h2o)
library(xgboost)

setwd("~/Google Drive/Degree Project/Repository/Hydro-prediction-System/training_data")

#load data
data <- read.csv("trainingFile_fwts.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"),stringsAsFactors=FALSE)
data$pres = as.numeric(data$pres)
data$pres[is.na(data$pres)] <- 0

#scale data for LSTM
receipe_object_fwts <- recipe(data) %>%
  step_sqrt("fwts") %>%
  step_center("fwts") %>%
  step_scale("fwts") %>%
  prep()

scaled_data <- bake(receipe_object_fwts,data)

#initialize H2O
h2o.init()

#load function
source('make_predictions_fcn.R')

#open dashboard in browser
runApp('Dashboard.R', launch.browser = TRUE)
