library(h2o)
install.packages('ggplot2')
install.packages('colorspace')
library(ggplot2)
h2o.init(nthreads=-1)
remove.packages(c("ggplot2", "data.table"))
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)
MyData <- read.csv(file="G:/degree project/trainingFile_fwts.csv")

Data<-MyData[(210373:347328),(1:18)]
Data[,(6:18)]=scale(Data[,(6:18)])
Data[,3]=scale(Data[,3])
trainData<- Data[1:(136956-288),(1:18)]
testData<- Data[(136956-287):136956,(1:18)]
classifier=h2o.deeplearning(y='fwts',
                            x=c('temp','dew','hum','wspd','vis','pres','mon','tue','wed','thu','fri','sat','sun','sint','cost'),
                            activation = 'Rectifier',
                            training_frame=as.h2o(trainData),
                            hidden=c(10,10),
                            epochs=100,
                            train_samples_per_iteration=-2
)

pred=h2o.predict(classifier,newdata=as.h2o(testData[-3]))
pred
ggplot() +
  geom_point(aes(x=1:288, y = testData$fwts),
             colour = 'red')+
  geom_point(aes(x=1:288, y =as.vector(pred)),
             colour = 'blue') +
  ggtitle('Deeplearning') +
  xlab('Time') +
  ylab('Fwts')
which.max(as.vector(pred))
which.max(testData$fwts)
