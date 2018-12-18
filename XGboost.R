#install.packages('xgboost')
library(xgboost)

x_vars <- read.csv("G:/degree project/Scaled Training Files/scaled_training_fwts_XVars.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"))
y_var <- read.csv("G:/degree project/Scaled Training Files/scaled_training_fwts_YVar.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"))
x_vars$y <- paste(y_var$V1)
x_vars$y=as.numeric(x_vars$y)
training_set=x_vars[1:346752,]
test_set=x_vars[346753:347328,]

training_set=x_vars[200000:347328,]
test_set=x_vars[346753:347328,]

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
  geom_point(aes(x=1:289, y = test_set$y),
             colour = 'red')+
  geom_point(aes(x=1:289, y =as.vector(y_total)),
             colour = 'blue') +
  ggtitle('XGboost') +
  xlab('Time') +
  ylab('Fwts')
library(caret)
classifier=train(form= y ~.,data=training_set,method='rf')
classifier