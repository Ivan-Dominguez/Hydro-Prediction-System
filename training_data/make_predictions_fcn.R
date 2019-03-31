library(Cubist)
library(caTools)
library(rpart)
library (dplyr)
library(caret)
library(rpart.plot)
library(reticulate)
library(readr)
library(plotly)
library(lubridate)
library(stringr)
library(h2o)
library(xgboost)

#initialize H2O
h2o.init()

make_predictions<-function(date_str){
  
  ######################################### set dates ############################################
  
  date_str<-as.character(date_str)
  prediction_date <- as.Date(date_str)
  day_before<-prediction_date - 1
  last_60days_date <- prediction_date - 60
  
  #dates to string
  day_before_str<-as.character.Date(day_before)
  last_60days_date_str<-as.character.Date(last_60days_date)
  
  #boundaries
  training_beginning<-data %>% filter(str_detect(datetime, last_60days_date_str))
  training_end<-data %>% filter(str_detect(datetime, day_before_str))
  
  start<-training_beginning$X[1]
  end<-training_end$X[nrow(training_end)]
  
  ######################################### train & test sets ###################################
  training_set<-data[start:end,]
  training_set<-training_set[,c(-1,-2,-19,-20)]
  
  test_set<-data %>% filter(str_detect(datetime, date_str))
  test_set<-test_set[,c(-1,-2,-19,-20)]
  
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
  xgboost_model <- xgboost(data = as.matrix(training_set[-1]), label = training_set$fwts, nrounds = 150, max_depth = 3,
                           eta = 0.4, gamma = 0, subsample = 0.75, colsample_bytree = 0.8, rate_drop = 0.01,
                           skip_drop = 0.95, min_child_weight = 1)
  
  prediction_xgboost <-as.vector(predict(xgboost_model, newdata = as.matrix(test_set[-1])))
  
  #Deep learning
  scale_training_set<-training_set
  scale_training_set[1]<-scale(training_set[1])
  scale_scale=attr(scale(training_set[1]), 'scaled:scale')
  scale_center=attr(scale(training_set[1]), 'scaled:center')
  
  deepLearning_model<-h2o.deeplearning(y='fwts',
                                       x=c('temp','dew','hum','wspd','vis','pres','mon','tue','wed','thu','fri','sat','sun','sint','cost'),
                                       activation = 'Rectifier',
                                       training_frame=as.h2o(as.matrix(scale_training_set)),
                                       hidden=c(10,10),
                                       epochs=100,
                                       train_samples_per_iteration=-2)
  
  prediction_deepLearning <- as.vector(predict(deepLearning_model, newdata=as.h2o(test_set[-1])))
  prediction_deepLearning <-prediction_deepLearning * scale_scale+scale_center
  
  #Random Forest
  variables=c('temp','dew','hum','wspd','vis','pres','mon','tue','wed','thu','fri','sat','sun','sint','cost')
  RF_model<-h2o.randomForest(x=variables,
                             y="fwts",
                             ntrees=500,
                             max_depth=10,
                             training_frame=as.h2o(training_set),
                             seed=1242525)
  
  prediction_RF = as.vector(predict(RF_model, newdata = as.h2o(test_set[-1])))
  
  #free memory
  h2o.removeAll()
  
  ######################################### times of the day #############################################
  
  #format date and time
  predictionDate <- as.Date(prediction_date)
  
  datetimes <-seq(ymd_hm(paste(predictionDate, "00:00")),
                  ymd_hm(paste(predictionDate, "23:55")), 
                  by = "5 min")
  
  ##################################### Find peak values  ######################################
  
  
  #Cubist Predicted Daily Peak
  ymax_cubist_pred = max(prediction_cubist[144:288])
  xcoord_cubist_pred <-
    datetimes[which.max(prediction_cubist[144:288]) + 143]
  
  #XGBoost Predicted Daily Peak
  ymax_xgboost_pred = max(prediction_xgboost[144:288])
  xcoord_xgboost_pred <-
    datetimes[which.max(as.vector(prediction_xgboost[144:288])) + 143]
  
  #Deep learning Predicted Daily Peak
  ymax_deepLearning_pred = max(prediction_deepLearning[144:288])
  xcoord_deepLearning_pred <-
    datetimes[which.max(as.vector(prediction_deepLearning[144:288])) + 143]
  
  #Random Forest Predicted Daily Peak
  ymax_RF_pred = max(prediction_RF[144:288])
  xcoord_RF_pred <-
    datetimes[which.max(prediction_RF[144:288]) + 143]
  
  #Test data peak
  ymax_test_pred = max(test_set$fwts[144:288])
  xcoord_test_pred <-
    datetimes[which.max(test_set$fwts[144:288]) + 143]
  
  
  #lists
  xcoord_list<-list("test"=xcoord_test_pred, "cubist"=xcoord_cubist_pred, "xgboost"=xcoord_xgboost_pred, 
                 "DL"=xcoord_deepLearning_pred, "RF"=xcoord_RF_pred)
  
  ymax_list<-list("test"=ymax_test_pred, "cubist"=ymax_cubist_pred, "xgboost"=ymax_xgboost_pred,
               "DL"=ymax_deepLearning_pred, "RF"=ymax_RF_pred)
  
  list_of_preditcions<-list("hours"=datetimes,"test"=test_set$fwts,"cubist"= prediction_cubist,
                            "xgboost"=prediction_xgboost,"DL"=prediction_deepLearning,
                            "RF"=prediction_RF,"xcoord_list"=xcoord_list, "ymax_list"=ymax_list)
  
  
  return(list_of_preditcions)
}