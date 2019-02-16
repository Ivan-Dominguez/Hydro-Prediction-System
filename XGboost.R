#install.packages('xgboost')
#install.packages('Cubist')
library(Cubist)
library(xgboost)
library(h2o)
library(Metrics)
library(dplyr)
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
#install.packages('Metrics')
h2o.init()
x_vars <- read.csv("G:/degree project/Scaled Training Files/scaled_training_fwts_XVars.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"))
y_var <- read.csv("G:/degree project/Scaled Training Files/scaled_training_fwts_YVar.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"))
x_vars$y <- paste(y_var$V1)
x_vars$y=as.numeric(x_vars$y)
training_set=x_vars[1:347039,]
test_set=x_vars[347040:347328,]

training_set=x_vars[-(326893:327181),]
test_set=x_vars[326893:327181,]
##############################################33
data <- read.csv("G:/degree project/trainingFile_fwts.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"),stringsAsFactors=FALSE)
data$pres = as.numeric(data$pres)
######################################### set dates ############################################
prediction_date_str<-"2018-01-01"

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

test_set_real<-data %>% filter(str_detect(datetime, prediction_date_str))
test_set<-test_set_real[,c(-1,-2,-19,-20)]
#########################################################

variables=c('temp','dew','hum','wspd','vis','pres','mon','tue','wed','thu','fri','sat','sun','sint','cost')
model1<-h2o.randomForest(x=variables,
                        y="fwts",
                        ntrees=500,
                        max_depth=10,
                        training_frame=as.h2o(training_set),
                        seed=1242525
)
model2 = xgboost(data = as.matrix(training_set[-1]), label = training_set$fwts, nrounds = 100)
y_pred2 = predict(model2, newdata = as.matrix(test_set[-1]))
y_pred1 = predict(model1, newdata = as.h2o(test_set[-1]))
y_total=(as.vector(y_pred1)+as.vector(y_pred2))/2
ggplot() +
  geom_point(aes(x=1:576, y = test_set$y),
             colour = 'red')+
  geom_point(aes(x=1:576, y =as.vector(y_total)),
             colour = 'blue') +
  ggtitle('Combine') +
  xlab('Time') +
  ylab('Fwts')
library(caret)
classifier=train(form= y ~.,data=training_set,method='ranger')
classifier


#TESTING:XGboost
##############################################
i=1;
berrors=0;
berror<-c(1:51)
berror<-NULL
bRMSE<-c(1:51)
bRMSE<-NULL
#1206 days in total,test after 700days,test 15hours interval
test=sample(200000:347328,50,replace=FALSE)
while(i<=50) {
testing_sub_set=x_vars[(test[i]):(test[i]+179),]
testing_train=x_vars[1:(test[i]),]
test_model= xgboost(data = as.matrix(testing_train[-16]), label = testing_train$y, nrounds = 100)
pred=predict(test_model, newdata = as.matrix(testing_sub_set[-16]))
bRMSE[i]=rmse(testing_sub_set$y,pred)
berror[i]=abs(which.max(testing_sub_set$y)-which.max(pred))*5
berrors=berrors+berror[i];
i=i+1;}

ggplot() +
  geom_point(aes(x=1:288, y = testing_sub_set$y),
             colour = 'red')+
  geom_point(aes(x=1:288, y =as.vector(pred)),
             colour = 'blue') +
  ggtitle('XGboost') +
  xlab('Time') +
  ylab('Fwts')

which.max(testing_sub_set$y)
which.max(pred)
#TESTING:RF
##############################################
i=1;
rerrors=0;
rerror<-c(1:51)
rerror<-NULL
rRMSE<-c(1:51)
rRMSE<-NULL
#1206 days in total,test after 700days
while(i<=50) {
  testing_sub_set=x_vars[(test[i]):(test[i]+287),]
  testing_train=x_vars[1:(test[i]),]
  test_model<-h2o.randomForest(x=variables,
                                 y="y",
                                 ntrees=500,
                                 max_depth=10,
                                 training_frame=as.h2o(testing_train),
                                 seed=1242525
  )
  pred=as.vector(predict(test_model, newdata = as.h2o(testing_sub_set[-16])))
  rRMSE[i]=rmse(testing_sub_set$y,pred)
  #ggplot() +
  #  geom_point(aes(x=1:288, y = testing_sub_set$y),..
  #             colour = 'red')+
  #  geom_point(aes(x=1:288, y =as.vector(pred)),
  #             colour = 'blue') +
  #  ggtitle('XGboost') +
  #  xlab('Time') +
  #  ylab('Fwts')
  
  #which.max(testing_sub_set$y)
  #which.max(pred)
  rerror[i]=abs(which.max(testing_sub_set$y)-which.max(pred))*5
  rerrors=rerrors+rerror[i];
  i=i+1;}

print(error)
test[9]
install.packages('DMwR')



#visualize

#RF Predicted Daily Peaky
RFmax_fwts_pred = max(as.vector(y_pred1))
RFcoord_fwts_pred <-
  test_set_real$datetime[which.max(as.vector(y_pred1))]

#XG Predicted Daily Peak
XGmax_pred = max(as.vector(y_pred2))
XGcoord_pred <-
  test_set_real$datetime[which.max(as.vector(y_pred1))]

#Test data peak
ymax_test_pred = max(test_set$fwts)
xcoord_test_pred <-
  test_set_real$datetime[which.max(test_set$fwts)]




pl <-
  plot_ly(
    mode = 'lines+markers'
  ) %>%
  add_trace(
    y =  ~ test_set$fwts,
    x =  ~ test_set_real$datetime,
    mode = 'lines',
    type = 'scatter',
    name = "Test Data",
    line = list(color = ("red"))
  ) %>%
  add_trace(
    y =  ~ as.vector(y_pred1),
    x =  ~ test_set_real$datetime,
    mode = 'lines',
    type = 'scatter',
    name = "RF",
    line = list(color = ("green"))
  ) %>%
  add_trace(
    y =  ~ as.vector(y_pred2),
    x =  ~ test_set_real$datetime,
    mode = 'lines',
    type = 'scatter',
    name = "XGboost",
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
    x =  ~ RFcoord_fwts_pred,
    y =  ~ RFmax_fwts_pred,
    mode = 'markers',
    type = 'scatter',
    name = paste("RF Predicted Daily Peak",RFcoord_fwts_pred),
    marker = list(color = ("yellow"),size=9,symbol="circle")
  ) %>%
  add_trace(
    x =  ~ XGcoord_pred,
    y =  ~ XGmax_pred,
    mode = 'markers',
    type = 'scatter',
    name = paste("XGboost Predicted Daily Peak",XGcoord_pred),
    marker = list(color = ("orange"),size=9,symbol="square")
  ) %>%
  layout(
    title = paste('Prediction for',as.Date(prediction_date)),
    xaxis = list(
      title = 'Time',
      autotick = TRUE,
      showticklabels = TRUE
    ),
    yaxis = list(title = "Power Consumption")
  )
pl
