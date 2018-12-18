install.packages('caTools')
library(caTools)
install.packages('h2o')
library(h2o)
h2o.init()
MyData <- read.csv(file="G:/degree project/trainingFile_fwts.csv")
set.seed(213)
split = sample.split(MyData$fwts, SplitRatio = 0.8)
training_set = subset(MyData, split == TRUE)
test_set = subset(MyData, split == FALSE)
variables=c('sint','temp','dew','hum','wspd','vis','pres','mon','tue','wed','thu','fri','sat','sun','ssm')
model<-h2o.randomForest(x=variables,
                        y="fwts",
                        ntrees=500,
                        training_frame=as.h2o(training_set),
                        validation_frame=as.h2o(test_set),
                        seed=1242525
)
summary(model)
install.packages('caret')
library(caret)
folds = createFolds(training_set$fwts, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = h2o.randomForest(x=variables,
                                y="fwts",
                                ntrees=500,
                                training_frame=as.h2o(training_set),
                                seed=1242525)
  y_pred = predict(classifier, newdata = as.h2o(test_fold[-3]))
  cm = table(test_fold[,3], as.vector(y_pred))
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})

accuracy = mean(as.numeric(cv))
# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)