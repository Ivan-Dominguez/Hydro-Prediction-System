#install.packages('xgboost')
library(xgboost)
library(h2o)
library(Metrics)
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

variables=c('temp','dew','hum','wspd','vis','pres','mon','tue','wed','thu','fri','sat','sun','sint','cost')
model1<-h2o.randomForest(x=variables,
                        y="y",
                        ntrees=500,
                        max_depth=10,
                        training_frame=as.h2o(training_set),
                        seed=1242525
)
model2 = xgboost(data = as.matrix(training_set[-16]), label = training_set$y, nrounds = 100)
y_pred2 = predict(model2, newdata = as.matrix(test_set[-16]))
y_pred1 = predict(model1,newdata = as.h2o(test_set[-16]))
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
