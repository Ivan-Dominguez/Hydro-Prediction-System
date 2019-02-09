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
library(neuralnet)
library(xgboost)

setwd("~/Google Drive/Degree Project/Repository/Hydro-Prediction-System/training_data")

#load data
data <- read.csv("trainingFile_fwts.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"),stringsAsFactors=FALSE)
data$pres = as.numeric(data$pres)

#scale data
receipe_object_fwts <- recipe(data) %>%
  step_sqrt("fwts") %>%
  step_center("fwts") %>%
  step_scale("fwts") %>%
  prep()

data <- bake(receipe_object_fwts,data)

######################################### set dates ############################################
prediction_date_str<-"2018-02-14"

prediction_date <- as.Date(prediction_date_str)
day_before<-prediction_date - 1
last_30days_date <- prediction_date - 365

#dates to string
day_before_str<-as.character.Date(day_before)
last_30days_date_str<-as.character.Date(last_30days_date)

#boundaries
training_beginning<-data %>% filter(str_detect(datetime, last_30days_date_str))
training_end<-data %>% filter(str_detect(datetime, day_before_str))

start<-training_beginning$X[1]
end<-training_end$X[288]

######################################### train/test sets  AND Model ############################################
training_set<-data[start:end,]
training_set<-training_set[,c(-1,-2,-19,-20)]

test_set<-data %>% filter(str_detect(datetime, prediction_date_str))

cubist_model<-cubist(x = training_set[,-1], y = training_set$fwts, committees = 20, neighbors = 5)
Prediction_cubist <- predict(cubist_model, test_set)


# neural_net_model<-neuralnet(form = fwts ~.,
#                             data = training_set$fwts
#                             )

xgboost_model <- xgboost(data = test_set[,-1], label = training_set$fwts, nrounds = 150, max_depth = 3,
                         eta = 04, gamma = 0, subsample = 0.75, colsample_bytree = 0.8, rate_drop = 0.01, 
                         skip_drop = 0.95, min_child_weight = 1)

Prediction_xgboost <- predict(xgboost_model, test_set)


#********************************************************#

#Load FWTS Model
fwts_lstm_model_lag288_mv <-
  load_model_hdf5(
    "/Users/ivan/Google Drive/Degree Project/Repository/Hydro-Prediction-System/training_data/saved_models/Stateful_Lag288_FWTS_MV16_MODEL",
    compile = TRUE
  )

#create arrays
testX_sint_vector<- test_set$sint
testX_sint_array <- array(data = testX_sint_vector,dim = c(length(testX_sint_vector),1,1))

testX_cost_vector<-test_set$cost
testX_cost_array <- array(data = testX_cost_vector,dim = c(length(testX_cost_vector),1,1))

testX_temp_vector<-test_set$temp
testX_temp_array <- array(data = testX_temp_vector,dim = c(length(testX_temp_vector),1,1))

testX_hum_vector<-test_set$hum
testX_hum_array <- array(data = testX_hum_vector,dim = c(length(testX_hum_vector),1,1))

testX_dew_vector<-test_set$dew
testX_dew_array <- array(data = testX_dew_vector,dim = c(length(testX_dew_vector),1,1))

testX_wspd_vector<-test_set$wspd
testX_wspd_array <- array(data = testX_wspd_vector,dim = c(length(testX_wspd_vector),1,1))

testX_vis_vector<-test_set$vis
testX_vis_array <- array(data = testX_vis_vector,dim = c(length(testX_vis_vector),1,1))

testX_pres_vector<-test_set$pres
testX_pres_array <- array(data = testX_pres_vector,dim = c(length(testX_pres_vector),1,1))


testX_mon_vector<-test_set$mon
testX_mon_array <- array(data = testX_mon_vector,dim = c(length(testX_mon_vector),1,1))

testX_tue_vector<-test_set$tue
testX_tue_array <- array(data = testX_tue_vector,dim = c(length(testX_tue_vector),1,1))

testX_wed_vector<-test_set$wed
testX_wed_array <- array(data = testX_wed_vector,dim = c(length(testX_wed_vector),1,1))

testX_thu_vector<-test_set$thu
testX_thu_array <- array(data = testX_thu_vector,dim = c(length(testX_thu_vector),1,1))

testX_fri_vector<-test_set$fri
testX_fri_array <- array(data = testX_fri_vector,dim = c(length(testX_fri_vector),1,1))

testX_sat_vector<-test_set$sat
testX_sat_array <- array(data = testX_sat_vector,dim = c(length(testX_sat_vector),1,1))

testX_sun_vector<-test_set$sun
testX_sun_array <- array(data = testX_sun_vector,dim = c(length(testX_sun_vector),1,1))

testX_fwts_vector<-test_set$fwts
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
  data.frame(matrix(ncol = 4, nrow = 288))
columns = c("datetime",
            "brts_pred",
            "fwts_pred",
            "pats_pred")

colnames(predictions_df) <- columns

predictions_df$fwts_pred <- predictions_fwts

# format date and time
predictionDate <- as.Date(prediction_date)
predictions_df$datetime <-
  seq(ymd_hm(paste(predictionDate, "00:00")), ymd_hm(paste(predictionDate, "23:55")), by =
        "5 min")

#split date and time in two columns
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
  (test_set$fwts * attribute_scale_fwts + attribute_centre_fwts) ^ 2


Prediction_cubist_scaled_back<-
  (Prediction_cubist * attribute_scale_fwts + attribute_centre_fwts) ^ 2

Prediction_xgboost_scaled_back<-
  (Prediction_xgboost * attribute_scale_fwts + attribute_centre_fwts) ^ 2


predictions_df$fwts_pred <-
  (predictions_df$fwts_pred * attribute_scale_fwts + attribute_centre_fwts) ^ 2


#LSTM Predicted Daily Peak
ymax_fwts_pred = max(predictions_df$fwts_pred)
xcoord_fwts_pred <-
  predictions_df$datetime[which.max(predictions_df$fwts_pred)]

#Cubist Predicted Daily Peak
ymax_cubist_pred = max(Prediction_cubist_scaled_back[144:288])
xcoord_cubist_pred <-
  predictions_df$datetime[which.max(Prediction_cubist_scaled_back[144:288]) + 143]

#XGBoost Predicted Daily Peak
ymax_xgboost_pred = max(Prediction_xgboost_scaled_back[144:288])
xcoord_xgboostt_pred <-
  predictions_df$datetime[which.max(Prediction_xgboost_scaled_back[144:288]) + 143]


#Test data peak
ymax_test_pred = max(test_scaled_back)
xcoord_test_pred <-
  predictions_df$datetime[which.max(test_scaled_back)]

#**********************************************************************#

#visualize
pl <-
  plot_ly(
    mode = 'lines+markers'
  ) %>%
  add_trace(
    y =  ~ test_scaled_back,
    x =  ~ predictions_df$datetime,
    mode = 'lines',
    type = 'scatter',
    name = "Test Data",
    line = list(color = ("red"))
  ) %>%
  add_trace(
    y =  ~ predictions_df$fwts_pred,
    x =  ~ predictions_df$datetime,
    mode = 'lines',
    type = 'scatter',
    name = "LSTM",
    line = list(color = ("green"))
  ) %>%
  add_trace(
    y =  ~ Prediction_cubist_scaled_back,
    x =  ~ predictions_df$datetime,
    mode = 'lines',
    type = 'scatter',
    name = "Cubist",
    line = list(color = ("blue"))
  ) %>%
  add_trace(
    y =  ~ Prediction_cubist_scaled_back,
    x =  ~ predictions_df$datetime,
    mode = 'lines',
    type = 'scatter',
    name = "XGBoost",
    line = list(color = ("orange"))
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
  add_trace(
    x =  ~ xcoord_cubist_pred,
    y =  ~ ymax_cubist_pred,
    mode = 'markers',
    type = 'scatter',
    name = paste("XGBoost Predicted Daily Peak",xcoord_cubist_pred),
    marker = list(color = ("black"),size=9,symbol="square")
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
