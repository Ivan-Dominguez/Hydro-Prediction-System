#install.packages('caTools')
library(caTools)
#install.packages('h2o')
library(h2o)
h2o.init()
MyData <- read.csv(file="G:/degree project/trainingFile_fwts.csv")
set.seed(213)
training_set=MyData[-(326893:327181),]
test_set=MyData[326893:327181,]
#split = sample.split(MyData$fwts, SplitRatio = 0.999)
#training_set = subset(MyData, split == TRUE)
#test_set = subset(MyData, split == FALSE)


x_vars <- read.csv("G:/degree project/Scaled Training Files/scaled_training_fwts_XVars.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"))
y_var <- read.csv("G:/degree project/Scaled Training Files/scaled_training_fwts_YVar.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"))
x_vars$y <- paste(y_var$V1)
x_vars$y=as.numeric(x_vars$y)
split = sample.split(MyData$fwts, SplitRatio = 0.9995)
training_set = subset(x_vars, split == TRUE)
test_set = subset(x_vars, split == FALSE)

training_set=x_vars[-(326893:327181),]
test_set=x_vars[326893:327181,]


training_set=x_vars[1:346752,]
test_set=x_vars[346753:347328,]

variables=c('temp','dew','hum','wspd','vis','pres','mon','tue','wed','thu','fri','sat','sun','sint','cost')
model<-h2o.randomForest(x=variables,
                        y="y",
                        ntrees=500,
                        max_depth=10,
                        training_frame=as.h2o(training_set),
                        seed=1242525
)
summary(model)
#install.packages('caret')
library(caret)
library(ggplot2)
y_pred = predict(model,newdata = as.h2o(test_set[-16]))
ggplot() +
  geom_point(aes(x=1:289, y = test_set$y),
             colour = 'red')+
  geom_point(aes(x=1:289, y = as.vector(y_pred)),
             colour = 'blue') +
  ggtitle('Random Forest Regression(random day)') +
  xlab('Time') +
  ylab('Fwts')
