# Cubist Algorithm

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
Prediction_cubist <- predict(model_tree, test_set)

#********************************************************#

#Load FWTS Model
fwts_lstm_model_lag288_mv <-
  load_model_hdf5(
    "/Users/ivan/Google Drive/Degree Project/Repository/Hydro-Prediction-System/training_data/saved_models/Stateful_Lag288_FWTS_MV16_MODEL",
    compile = TRUE
  )

first_point<-as.numeric(289)
last_point<-as.numeric(576)

#create arrays
testX_sint_vector<- test_set$sint[first_point:last_point]
testX_sint_array <- array(data = testX_sint_vector,dim = c(length(testX_sint_vector),1,1))

testX_cost_vector<-test_set$cost[first_point:last_point]
testX_cost_array <- array(data = testX_cost_vector,dim = c(length(testX_cost_vector),1,1))

testX_temp_vector<-test_set$temp[first_point:last_point]
testX_temp_array <- array(data = testX_temp_vector,dim = c(length(testX_temp_vector),1,1))

testX_hum_vector<-test_set$hum[first_point:last_point]
testX_hum_array <- array(data = testX_hum_vector,dim = c(length(testX_hum_vector),1,1))

testX_dew_vector<-test_set$dew[first_point:last_point]
testX_dew_array <- array(data = testX_dew_vector,dim = c(length(testX_dew_vector),1,1))

testX_wspd_vector<-test_set$wspd[first_point:last_point]
testX_wspd_array <- array(data = testX_wspd_vector,dim = c(length(testX_wspd_vector),1,1))

testX_vis_vector<-test_set$vis[first_point:last_point]
testX_vis_array <- array(data = testX_vis_vector,dim = c(length(testX_vis_vector),1,1))

testX_pres_vector<-test_set$pres[first_point:last_point]
testX_pres_array <- array(data = testX_pres_vector,dim = c(length(testX_pres_vector),1,1))


testX_mon_vector<-test_set$mon[first_point:last_point]
testX_mon_array <- array(data = testX_mon_vector,dim = c(length(testX_mon_vector),1,1))

testX_tue_vector<-test_set$tue[first_point:last_point]
testX_tue_array <- array(data = testX_tue_vector,dim = c(length(testX_tue_vector),1,1))

testX_wed_vector<-test_set$wed[first_point:last_point]
testX_wed_array <- array(data = testX_wed_vector,dim = c(length(testX_wed_vector),1,1))

testX_thu_vector<-test_set$thu[first_point:last_point]
testX_thu_array <- array(data = testX_thu_vector,dim = c(length(testX_thu_vector),1,1))

testX_fri_vector<-test_set$fri[first_point:last_point]
testX_fri_array <- array(data = testX_fri_vector,dim = c(length(testX_fri_vector),1,1))

testX_sat_vector<-test_set$sat[first_point:last_point]
testX_sat_array <- array(data = testX_sat_vector,dim = c(length(testX_sat_vector),1,1))

testX_sun_vector<-test_set$sun[first_point:last_point]
testX_sun_array <- array(data = testX_sun_vector,dim = c(length(testX_sun_vector),1,1))

testX_fwts_vector<-test_set$y[first_point:last_point]
testX_fwts_array <- array(data = testX_fwts_vector,dim = c(length(testX_fwts_vector),1,1))

fwts_testX_input_list <- list(
  testX_fwts_array,testX_temp_array,testX_dew_array,testX_hum_array,testX_wspd_array,testX_vis_array,testX_pres_array,testX_mon_array,testX_tue_array,testX_wed_array,testX_thu_array,testX_fri_array,testX_sat_array,testX_sun_array,testX_sint_array,testX_cost_array
)

#predictions FWTS Model
predictions_fwts <-
  fwts_lstm_model_lag288_mv %>% predict(fwts_testX_input_list, batch_size = 72) %>% .[, 1]
pred_array_fwts <-
  array(data = predictions_fwts, dim = c(length(predictions_fwts), 1, 1))

predictions_df <-
  data.frame(matrix(ncol = 4, nrow = 576))
columns = c("datetime",
            "brts_pred",
            "fwts_pred",
            "pats_pred")

colnames(predictions_df) <- columns

predictions_df$fwts_pred <- predictions_fwts

# format date and time
predictionDate <- as.Date(x = "2014-01-02")
predictions_df$datetime <-
  seq(ymd_hm(paste(predictionDate, "00:00")), ymd_hm(paste(predictionDate, "23:55")), by =
        "5 min")

predictions_df$date<-as.Date(predictions_df$datetime)
predictions_df$time<-strftime(predictions_df$datetime,format="%H:%M:%S",tz="UTC")


#read attributes needed to reverse scaling process
attribute_centre_fwts <-
  as.numeric(
    read_lines(
      "/Users/ivan/Google Drive/Degree Project/Repository/Hydro-Prediction-System/training_data/saved_attributes/attribute_centre_fwts_stateful_lag288.txt"
    )
  )

attribute_scale_fwts <-
  as.numeric(
    read_lines(
      "/Users/ivan/Google Drive/Degree Project/Repository/Hydro-Prediction-System/training_data/saved_attributes/attribute_scale_fwts_stateful_lag288.txt"
    )
  )

#Rescale Predictions using Saved Attributes

test_scaled_back<-
  (test_set$y * attribute_scale_fwts + attribute_centre_fwts) ^ 2


Prediction_cubist_scaled_back<-
  (Prediction_cubist * attribute_scale_fwts + attribute_centre_fwts) ^ 2

predictions_df$fwts_pred <-
  (predictions_df$fwts_pred * attribute_scale_fwts + attribute_centre_fwts) ^ 2


#LSTM Predicted Daily Peak
ymax_fwts_pred = max(predictions_df$fwts_pred)
xcoord_fwts_pred <-
  predictions_df$datetime[which.max(predictions_df$fwts_pred)]

#Cubist Predicted Daily Peak
ymax_cubist_pred = max(Prediction_cubist_scaled_back)
xcoord_cubist_pred <-
  predictions_df$datetime[which.max(Prediction_cubist_scaled_back)]

#Test data peak
ymax_test_pred = max(test_scaled_back[first_point:last_point])
xcoord_test_pred <-
  predictions_df$datetime[which.max(test_scaled_back[first_point:last_point])]


#**********************************************************************#

#visualize
pl <-
  plot_ly(
    mode = 'lines+markers'
  ) %>%
  add_trace(
    y =  ~ test_scaled_back[first_point:last_point],
    x =  ~ predictions_df$datetime[first_point:last_point],
    mode = 'lines',
    type = 'scatter',
    name = "Test Data",
    line = list(color = ("red"))
  ) %>%
  add_trace(
    y =  ~ predictions_df$fwts_pred[first_point:last_point],
    x =  ~ predictions_df$datetime[first_point:last_point],
    mode = 'lines',
    type = 'scatter',
    name = "LSTM",
    line = list(color = ("green"))
  ) %>%
  add_trace(
    y =  ~ Prediction_cubist_scaled_back[first_point:last_point],
    x =  ~ predictions_df$datetime[first_point:last_point],
    mode = 'lines',
    type = 'scatter',
    name = "Cubist",
    line = list(color = ("blue"))
  ) %>%
  add_trace(
    x =  ~ xcoord_test_pred,
    y =  ~ ymax_test_pred,
    mode = 'markers',
    type = 'scatter',
    name = paste("Real daily Peak",xcoord_test_pred),
    marker = list(color = ("black"),size=9,symbol="triangle-up")
  ) %>%
  add_trace(
    x =  ~ xcoord_fwts_pred,
    y =  ~ ymax_fwts_pred,
    mode = 'markers',
    type = 'scatter',
    name = paste("LSTM Predicted Daily Peak",xcoord_fwts_pred),
    marker = list(color = ("yellow"),size=9,symbol="circle")
  ) %>%
  add_trace(
    x =  ~ xcoord_cubist_pred,
    y =  ~ ymax_cubist_pred,
    mode = 'markers',
    type = 'scatter',
    name = paste("Cubist Predicted Daily Peak",xcoord_cubist_pred),
    marker = list(color = ("orange"),size=9,symbol="square")
  ) %>%
  layout(
    title = paste('Prediction for', predictionDate),
    xaxis = list(
      title = 'Time',
      autotick = TRUE,
      showticklabels = TRUE
    ),
    yaxis = list(title = "Power Consumption")
  )

pl
