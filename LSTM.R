install.packages('tidyverse')
install.packages('tidytext')
install.packages('keras')
install.packages('data.table')
library(keras)
library(dplyr)
library(ggplot2)
install.packages('ggthemes')
library(ggthemes)
library(lubridate)
library(tidyverse) # importing, cleaning, visualising 
library(tidytext) # working with text
library(keras) # deep learning with keras
library(data.table) # fast csv reading


library(keras)
system("conda config --set ssl_verify false")
 install_keras(tensorflow = "gpu")
install_keras()

set.seed(7)

MyData <- read.csv(file="G:/degree project/trainingFile_fwts.csv")

trainData<- MyData[(210373:347040),]
testData<- MyData[347041:347328,]
ggplot() +
  geom_point(aes(x=1:289, y = MyData[210373:(210373+288),3]),
             colour = 'red')+
  geom_point(aes(x=1:289, y = MyData[315661:(315661+288),3]),
             colour = 'blue') +
  ggtitle('Random Forest Regression(random day)') +
  xlab('Time') +
  ylab('Fwts')
MyData[211225:211225+which.max(MyData[211225:(211225+288)-1,3]),2]
MyData[316237:316237+which.max(MyData[316237:(316237+288)-1,3]),2]



max_value <- max(trainData$fwts)
min_value <- min(trainData$fwts)
spread <- max_value - min_value

dataset <- (Data$fwts - min_value) / spread

create_dataset <- function(dataset,
                           look_back = 1)
{
  l <- length(dataset)
  dataX <- array(dim = c(l - look_back, look_back))
  
  for (i in 1:ncol(dataX))
  {
    dataX[, i] <- dataset[i:(l - look_back + i - 1)]
  }
  
  dataY <- array(
    data = dataset[(look_back + 1):l],
    dim = c(l - look_back, 1))
  
  return(
    list(
      dataX = dataX,
      dataY = dataY))
}

look_back <- 1
trainXY <- create_dataset(trainData, look_back)
testXY <-  create_dataset(testData, look_back)

dim_train <- length(trainXY$dataX)

dim_test <- length(testXY$dataX)
dim(trainXY$dataX) <- c(dim_train[1], 1, dim_train[2])
dim(testXY$dataX) <- c(dim_test[1], 1, dim_test[2])

model <- keras_model_sequential()

model %>%
  layer_lstm(
    units = 4,
    input_shape = c(1, look_back)) %>%
  layer_dense(
    units = 1) %>%
  compile(
    loss = 'mean_squared_error',
    optimizer = 'adam') %>%
  fit(trainXY$dataX,
      trainXY$dataY,
      epochs = 100,
      batch_size = 1,
      verbose = 2)
