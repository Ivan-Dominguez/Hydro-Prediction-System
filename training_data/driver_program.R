library (dplyr)
library(shiny)

#setwd("~/Google Drive/Degree Project/Repository/Hydro-prediction-System/training_data")
setwd("C:/Users/Administrator/Documents/GitHub/Hydro-Prediction-System/training_data")
#set time zone
Sys.setenv(TZ='UTC')

#load data
data <- read.csv("trainingFile_fwts.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"),stringsAsFactors=FALSE)
data$pres = as.numeric(data$pres)
data$pres[is.na(data$pres)] <- 0


#load functions
source('make_predictions_fcn.R')
#source('daily_plot_fcn.R')
source('plot2.R')
source("get_mean_values_fcn.R")
source("charge_control.R")

#open dashboard in browser
runApp('Dashboard.R', launch.browser = TRUE)
