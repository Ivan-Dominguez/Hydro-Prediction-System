

h2o.init()


######################################### set dates ############################################

h2o.removeAll()
k_clear_session()

prediction_date_str<-"2017-01-17"

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
end<-training_end$X[nrow(training_end)]

######################################### train & test sets ###################################
training_set<-data[start:end,]
training_set<-training_set[,c(-1,-2,-19,-20)]

test_set<-data %>% filter(str_detect(datetime, prediction_date_str))
test_set<-test_set[,c(-1,-2,-19,-20)]

scaled_test_set<-scaled_data %>% filter(str_detect(datetime, prediction_date_str))
scaled_test_set<-scaled_test_set[,c(-1,-2,-19,-20)]

#fill missing rows copies of the last row
if(nrow(test_set) < 288){
  rows_needed<-288 - nrow(test_set)
  last_row<-test_set[nrow(test_set),]
  new_row<-data.frame(last_row)
  #fwts=0,sint=0,cost=0,temp=0,dew=0,hum=0,wspd=0,vis=0,pres=0,mon=0,tue=wed   thu   fri   sat   sun
  for(i in seq(1:rows_needed)){
    
    test_set <- rbind(test_set,new_row)
  }
  
}

######################################### Models ############################################

#Cubist
cubist_model<-cubist(x = training_set[,-1], y = training_set$fwts, committees = 20, neighbors = 5)
prediction_cubist <- predict(cubist_model, test_set)

#xgboost
xgboost_model <- h2o.xgboost(y='fwts',
                             x=c('temp','dew','hum','wspd','vis','pres','mon','tue','wed','thu','fri','sat','sun','sint','cost'),
                             training_frame=as.h2o(training_set)
                             )

prediction_xgboost <-as.vector(predict(xgboost_model, newdata = as.h2o(test_set[-1])))

#Deep learning
deepLearning_model <- h2o.deeplearning(y='fwts',
                                       x=c('temp','dew','hum','wspd','vis','pres','mon','tue','wed','thu','fri','sat','sun','sint','cost'),
                                       activation = 'Rectifier',
                                       training_frame=as.h2o(training_set),
                                       hidden=c(10,10),
                                       epochs=100,
                                       train_samples_per_iteration=-2
)
prediction_deepLearning <- as.vector(predict(deepLearning_model, newdata=as.h2o(test_set[-1])))

#Random Forest
variables=c('temp','dew','hum','wspd','vis','pres','mon','tue','wed','thu','fri','sat','sun','sint','cost')
RF_model<-h2o.randomForest(x=variables,
                           y="fwts",
                           ntrees=500,
                           max_depth=10,
                           training_frame=as.h2o(training_set),
                           seed=1242525
)

prediction_RF = as.vector(predict(RF_model, newdata = as.h2o(test_set[-1])))


#Load FWTS Model
fwts_lstm_model_lag288_mv <-
  load_model_hdf5(
    "/Users/ivan/Google Drive/Degree Project/Repository/Hydro-prediction-System/training_data/saved_models/Stateful_Lag288_FWTS_MV16_MODEL",
    compile = TRUE
  )

#********************************************************#
#LSTM

#create arrays
testX_sint_vector<- scaled_test_set$sint
testX_sint_array <- array(data = testX_sint_vector,dim = c(length(testX_sint_vector),1,1))

testX_cost_vector<-scaled_test_set$cost
testX_cost_array <- array(data = testX_cost_vector,dim = c(length(testX_cost_vector),1,1))

testX_temp_vector<-scaled_test_set$temp
testX_temp_array <- array(data = testX_temp_vector,dim = c(length(testX_temp_vector),1,1))

testX_hum_vector<-scaled_test_set$hum
testX_hum_array <- array(data = testX_hum_vector,dim = c(length(testX_hum_vector),1,1))

testX_dew_vector<-scaled_test_set$dew
testX_dew_array <- array(data = testX_dew_vector,dim = c(length(testX_dew_vector),1,1))

testX_wspd_vector<-scaled_test_set$wspd
testX_wspd_array <- array(data = testX_wspd_vector,dim = c(length(testX_wspd_vector),1,1))

testX_vis_vector<-scaled_test_set$vis
testX_vis_array <- array(data = testX_vis_vector,dim = c(length(testX_vis_vector),1,1))

testX_pres_vector<-scaled_test_set$pres
testX_pres_array <- array(data = testX_pres_vector,dim = c(length(testX_pres_vector),1,1))


testX_mon_vector<-scaled_test_set$mon
testX_mon_array <- array(data = testX_mon_vector,dim = c(length(testX_mon_vector),1,1))

testX_tue_vector<-scaled_test_set$tue
testX_tue_array <- array(data = testX_tue_vector,dim = c(length(testX_tue_vector),1,1))

testX_wed_vector<-scaled_test_set$wed
testX_wed_array <- array(data = testX_wed_vector,dim = c(length(testX_wed_vector),1,1))

testX_thu_vector<-scaled_test_set$thu
testX_thu_array <- array(data = testX_thu_vector,dim = c(length(testX_thu_vector),1,1))

testX_fri_vector<-scaled_test_set$fri
testX_fri_array <- array(data = testX_fri_vector,dim = c(length(testX_fri_vector),1,1))

testX_sat_vector<-scaled_test_set$sat
testX_sat_array <- array(data = testX_sat_vector,dim = c(length(testX_sat_vector),1,1))

testX_sun_vector<-scaled_test_set$sun
testX_sun_array <- array(data = testX_sun_vector,dim = c(length(testX_sun_vector),1,1))

testX_fwts_vector<-scaled_test_set$fwts
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
      "/Users/ivan/Google Drive/Degree Project/Repository/Hydro-prediction-System/training_data/saved_attributes/attribute_centre_fwts_stateful_lag288.txt"
    )
  )

attribute_scale_fwts <-
  as.numeric(
    read_lines(
      "/Users/ivan/Google Drive/Degree Project/Repository/Hydro-prediction-System/training_data/saved_attributes/attribute_scale_fwts_stateful_lag288.txt"
    )
  )

#******************************* Rescale LSTM predictions ***************************************#

predictions_df$fwts_pred <-
  (predictions_df$fwts_pred * attribute_scale_fwts + attribute_centre_fwts) ^ 2

#******************************* find peak values ***************************************#
#LSTM Predicted Daily Peak
ymax_fwts_pred = max(predictions_df$fwts_pred)
xcoord_fwts_pred <-
  predictions_df$datetime[which.max(predictions_df$fwts_pred)]

#Cubist Predicted Daily Peak
ymax_cubist_pred = max(prediction_cubist[144:288])
xcoord_cubist_pred <-
  predictions_df$datetime[which.max(prediction_cubist[144:288]) + 143]

#XGBoost Predicted Daily Peak
ymax_xgboost_pred = max(prediction_xgboost[144:288])
xcoord_xgboost_pred <-
  predictions_df$datetime[which.max(as.vector(prediction_xgboost[144:288])) + 143]

#Deep learning Predicted Daily Peak
ymax_deepLearning_pred = max(prediction_deepLearning[144:288])
xcoord_deepLearning_pred <-
  predictions_df$datetime[which.max(as.vector(prediction_deepLearning[144:288])) + 143]

#Random Forest Predicted Daily Peak
ymax_RF_pred = max(prediction_RF[144:288])
xcoord_RF_pred <-
  predictions_df$datetime[which.max(prediction_RF[144:288]) + 143]

#Avg of all peak times
prediction_avg = (prediction_xgboost + prediction_cubist + prediction_deepLearning +
                    prediction_RF + predictions_df$fwts_pred) / 5

ymax_avg_pred = max(prediction_avg[144:288])
xcoord_avg_pred <-
  predictions_df$datetime[which.max(prediction_avg[144:288]) + 143]

#Median of peak time
xcoord_median_pred = median(c(xcoord_xgboost_pred, xcoord_cubist_pred, xcoord_deepLearning_pred,
                              xcoord_RF_pred, xcoord_fwts_pred))

#Test data peak
ymax_test_pred = max(test_set$fwts[144:288])
xcoord_test_pred <-
  predictions_df$datetime[which.max(test_set$fwts[144:288]) + 143]

peak_times_list<-c(xcoord_xgboost_pred, xcoord_cubist_pred, xcoord_deepLearning_pred, xcoord_RF_pred,
                   xcoord_avg_pred, xcoord_median_pred, xcoord_fwts_pred, xcoord_test_pred)


#******************************* visualize ***************************************#
pl <-
  plot_ly(
    mode = 'lines+markers'
  ) %>%
  add_trace(
    y =  ~ test_set$fwts,
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
    y =  ~ prediction_cubist,
    x =  ~ predictions_df$datetime,
    mode = 'lines',
    type = 'scatter',
    name = "Cubist",
    line = list(color = ("blue"))
  ) %>%
  add_trace(
    y =  ~ prediction_xgboost,
    x =  ~ predictions_df$datetime,
    mode = 'lines',
    type = 'scatter',
    name = "XGBoost",
    line = list(color = ("orange"))
  ) %>%
  add_trace(
    y =  ~ prediction_deepLearning,
    x =  ~ predictions_df$datetime,
    mode = 'lines',
    type = 'scatter',
    name = "Deep Learning",
    line = list(color = ("purple"))
  ) %>%
  add_trace(
    y =  ~ prediction_RF,
    x =  ~ predictions_df$datetime,
    mode = 'lines',
    type = 'scatter',
    name = "Random Forest",
    line = list(color = ("yellow"))
  ) %>%
  add_trace(
    x =  ~ xcoord_test_pred,
    y =  ~ ymax_test_pred,
    mode = 'markers',
    type = 'scatter',
    name = paste("Real daily Peak", strftime(xcoord_test_pred,format="%H:%M:%S",tz="UTC")),
    marker = list(color = ("black"),size=9,symbol="triangle-up")
  ) %>%
  add_trace(
    x =  ~ xcoord_fwts_pred,
    y =  ~ ymax_fwts_pred,
    mode = 'markers',
    type = 'scatter',
    name = paste("LSTM Predicted Daily Peak", strftime(xcoord_fwts_pred,format="%H:%M:%S",tz="UTC")),
    marker = list(color = ("yellow"),size=9,symbol="circle")
  ) %>%
  add_trace(
    x =  ~ xcoord_cubist_pred,
    y =  ~ ymax_cubist_pred,
    mode = 'markers',
    type = 'scatter',
    name = paste("Cubist Predicted Daily Peak", strftime(xcoord_cubist_pred,format="%H:%M:%S",tz="UTC")),
    marker = list(color = ("orange"),size=9,symbol="square")
  ) %>%
  add_trace(
    x =  ~ xcoord_xgboost_pred,
    y =  ~ ymax_xgboost_pred,
    mode = 'markers',
    type = 'scatter',
    name = paste("XGBoost Predicted Daily Peak",strftime(xcoord_xgboost_pred,format="%H:%M:%S",tz="UTC")),
    marker = list(color = ("black"),size=9,symbol="square")
  ) %>%
  add_trace(
    x =  ~ xcoord_deepLearning_pred,
    y =  ~ ymax_deepLearning_pred,
    mode = 'markers',
    type = 'scatter',
    name = paste("Deep Learning Predicted Daily Peak",strftime(xcoord_deepLearning_pred,format="%H:%M:%S",tz="UTC")),
    marker = list(color = ("black"),size=9,symbol="square")
  ) %>%
  add_trace(
    x =  ~ xcoord_RF_pred,
    y =  ~ ymax_RF_pred,
    mode = 'markers',
    type = 'scatter',
    name = paste("Random Forest Predicted Daily Peak",strftime(xcoord_RF_pred,format="%H:%M:%S",tz="UTC")),
    marker = list(color = ("black"),size=9,symbol="square")
  ) %>%
  layout(
    title = paste('prediction for', predictionDate),
    xaxis = list(
      title = 'Time',
      autotick = TRUE,
      showticklabels = TRUE
    ),
    yaxis = list(title = "Power Consumption")
  )

pl
