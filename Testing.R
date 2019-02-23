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
library(Cubist)
library(R2HTML)
library(reporteRs)
h2o.init()
##############################################33
data <- read.csv("G:/degree project/trainingFile_fwts.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"),stringsAsFactors=FALSE)
data$pres = as.numeric(data$pres)
peak_times_list<-c(RFcoord_fwts_pred,XGcoord_pred,CUcoord_pred,DLcoord_pred,AVEcoord_pred,MEDIAN_pred,xcoord_test_pred)
pred_times <-data.frame(matrix(nrow = 30, ncol = 8))
columns = c("Date","xgboost", "cubist","deepLearning","rf", "avg", "median", "real_peak")
colnames(pred_times) <- columns
######################################### set dates ############################################
prediction_date_str<-"2018-02-28"
prediction_date <- as.Date(prediction_date_str)
i=0
while(i<9){
prediction_date=prediction_date+1;
day_before<-prediction_date - 1
last_30days_date <- prediction_date - 60

#dates to string
day_before_str<-as.character.Date(day_before)
last_30days_date_str<-as.character.Date(last_30days_date)

#boundaries
training_beginning<-data %>% filter(str_detect(datetime, last_30days_date_str))
training_end<-data %>% filter(str_detect(datetime, day_before_str))

start<-training_beginning$X[1]
end<-training_end$X[288]

######################################### train/test sets  AND Model ############################################
training_set_real<-data[start:end,]
training_set<-training_set_real[,c(-1,-2,-19,-20)]

test_set_real<-data %>% filter(str_detect(datetime, toString(prediction_date)))
test_set<-test_set_real[,c(-1,-2,-19,-20)]
#########################################################
model3<-cubist(x = training_set[-1], y = training_set$fwts, committees = 20, neighbors = 5)
scale_training_set<-training_set
scale_training_set[1]<-scale(training_set[1])
scale_scale=attr(scale(training_set[1]), 'scaled:scale')
scale_center=attr(scale(training_set[1]), 'scaled:center')
model4<-h2o.deeplearning(y='fwts',
                        x=c('temp','dew','hum','wspd','vis','pres','mon','tue','wed','thu','fri','sat','sun','sint','cost'),
                        activation = 'Rectifier',
                        training_frame=as.h2o(as.matrix(scale_training_set)),
                        hidden=c(10,10),
                        epochs=100,
                        train_samples_per_iteration=-2
)

variables=c('temp','dew','hum','wspd','vis','pres','mon','tue','wed','thu','fri','sat','sun','sint','cost')
model1<-h2o.randomForest(x=variables,
                         y="fwts",
                         ntrees=500,
                         max_depth=10,
                         training_frame=as.h2o(training_set),
                         seed=1242525
)
model2 = xgboost(data = as.matrix(training_set[-1]), label = training_set$fwts,  nrounds = 100)
mat<-xgb.importance (feature_names = colnames(training_set[-1]),model = model2)
y_pred2 = predict(model2, newdata = as.matrix(test_set[-1]))
y_pred1 = predict(model1, newdata = as.h2o(test_set[-1]))
y_pred3 = predict(model3, (test_set[-1]))
y_pred4 = predict(model4,newdata=as.h2o(test_set[-1]))
y_pred4 = y_pred4*scale_scale+scale_center
y_pred5 = (as.vector(y_pred1)+y_pred2+y_pred3+as.vector(y_pred4))/4
#visualize


#RF Predicted Daily Peaky
RFmax_fwts_pred = max(as.vector(y_pred1)[144:288])
RFcoord_fwts_pred <-
  test_set_real$datetime[which.max(as.vector(y_pred1)[144:288])+143]

#XG Predicted Daily Peak
XGmax_pred = max(as.vector(y_pred2)[144:288])
XGcoord_pred <-
  test_set_real$datetime[which.max(as.vector(y_pred2)[144:288])+143]

#Test data peak
ymax_test_pred = max(test_set$fwts[144:288])
xcoord_test_pred <-
  test_set_real$datetime[which.max(test_set$fwts[144:288])+143]

#Cubist Predicted Daily Peak
CUmax_pred = max(as.vector(y_pred3)[144:288])
CUcoord_pred <-
  test_set_real$datetime[which.max(as.vector(y_pred3)[144:288])+143]

#DeepLearning Predicted Daily Peak
DLmax_pred = max(as.vector(y_pred4)[144:288])
DLcoord_pred <-
  test_set_real$datetime[which.max(as.vector(y_pred4)[144:288])+143]

#Average of 4 models
AVEmax_pred = max(as.vector(y_pred5)[144:288])
AVEcoord_pred <-
  test_set_real$datetime[which.max(as.vector(y_pred5)[144:288])+143]

#Median of peak time
MEDIAN_pred <- strptime(prediction_date, "%Y-%m-%d")-mean(difftime(
  paste(prediction_date, "00:00:00", sep=" "),
  c(RFcoord_fwts_pred,DLcoord_pred,XGcoord_pred,CUcoord_pred),
  units = "secs"
))
MEDIAN_pred <-as.character(MEDIAN_pred)
#store difference

pred_times$xgboost[i]=XGcoord_pred
pred_times$cubist[i]=CUcoord_pred
pred_times$deepLearning[i]=DLcoord_pred
pred_times$rf[i]=RFcoord_fwts_pred
pred_times$avg[i]=AVEcoord_pred
pred_times$median[i]=MEDIAN_pred
pred_times$real_peak[i]=xcoord_test_pred
pred_times$Date[i]=as.character(prediction_date)
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
    line = list(color = ("black"))
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
    marker = list(color = ("black"),size=9,symbol="circle")
  ) %>%
  add_trace(
    x =  ~ RFcoord_fwts_pred,
    y =  ~ RFmax_fwts_pred,
    mode = 'markers',
    type = 'scatter',
    name = paste("RF Predicted Daily Peak",RFcoord_fwts_pred),
    marker = list(color = ("green"),size=9,symbol="circle")
  ) %>%
  add_trace(
    x =  ~ XGcoord_pred,
    y =  ~ XGmax_pred,
    mode = 'markers',
    type = 'scatter',
    name = paste("XGboost Predicted Daily Peak",XGcoord_pred),
    marker = list(color = ("blue"),size=9,symbol="circle")
  ) %>%
  add_trace(
    y =  ~ as.vector(y_pred3),
    x =  ~ test_set_real$datetime,
    mode = 'lines',
    type = 'scatter',
    name = "Cubist",
    line = list(color = ("pink"))
  )%>%
  add_trace(
    x =  ~ CUcoord_pred,
    y =  ~ CUmax_pred,
    mode = 'markers',
    type = 'scatter',
    name = paste("Cubist Predicted Daily Peak",CUcoord_pred),
    marker = list(color = ("pink"),size=9,symbol="circle")
  )%>%
  add_trace(
    y =  ~ as.vector(y_pred4),
    x =  ~ test_set_real$datetime,
    mode = 'lines',
    type = 'scatter',
    name = "Deep Learning",
    line = list(color = ("red"))
  )%>%
  add_trace(
    x =  ~ DLcoord_pred,
    y =  ~ DLmax_pred,
    mode = 'markers',
    type = 'scatter',
    name = paste("Deep Learning Predicted Daily Peak",DLcoord_pred),
    marker = list(color = ("red"),size=9,symbol="circle")
  )%>%
  add_trace(
    y =  ~ as.vector(y_pred5),
    x =  ~ test_set_real$datetime,
    mode = 'lines',
    type = 'scatter',
    name = "Average of models",
    line = list(color = ("yellow"))
  )%>%
  add_trace(
    x =  ~ MEDIAN_pred,
    y =  ~ 58000,
    mode = 'markers',
    type = 'scatter',
    name = paste("Mean of peak time of models Predicted Daily Peak",MEDIAN_pred),
    marker = list(color = ("orange"),size=9,symbol="circle")
  )%>%
  add_trace(
    x =  ~ AVEcoord_pred,
    y =  ~ AVEmax_pred,
    mode = 'markers',
    type = 'scatter',
    name = paste("AVERAGE of models Predicted Daily Peak",AVEcoord_pred),
    marker = list(color = ("yellow"),size=9,symbol="circle")
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
output=paste("G:/degree project/OUTPUT/test2/",prediction_date,".html", sep="")
htmlwidgets::saveWidget(pl, output)
i=i+1;
}
