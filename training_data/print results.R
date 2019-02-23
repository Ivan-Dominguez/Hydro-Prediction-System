

setwd("~/Google Drive/Degree Project/Repository/Hydro-prediction-System/training_data")

#load data
results_file_6days <- read.csv("pred_times_2017_6days.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"),stringsAsFactors=FALSE)
results_file_7days <- read.csv("pred_times_2017_7days.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"),stringsAsFactors=FALSE)
results_file_1year <- read.csv("pred_times_2017_1year.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"),stringsAsFactors=FALSE)

#compute time differences
colNumber<-ncol(pred_times)-1
result_list <-data.frame(matrix(nrow = 365, ncol = 7))
columns = c("xgboost", "cubist","deepLearning","rf", "LSTM", "avg", "median")
colnames(result_list) <- columns

limit_in_minutes <-25

pred_times<-results_file_6days[-1]

for(row in 1:365){
  for (column in 1:7){
    
    pred_peak<-pred_times[row, column]
    real_peak<-pred_times$real_peak[row]
    
    time_diff<-difftime(pred_times$real_peak[row], pred_times[row, column], units="mins" )
    time_diff<- as.numeric(time_diff)
    
   if(abs(time_diff) <= limit_in_minutes){
      result_list[row, column]<-time_diff
    }
  }
}

#write.csv(result_list, file = "result_list_2017_6days.csv")

na_count <-sapply(result_list, function(x) sum(length(which(is.na(x)))))
na_count
