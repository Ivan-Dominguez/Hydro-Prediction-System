

setwd("~/Google Drive/Degree Project/Repository/Hydro-prediction-System/training_data")

#load data
as.data.frame(results_file)

results_fileT2days <- read.csv("pred_times_2017_2days.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"),stringsAsFactors=FALSE)
results_fileT3days <- read.csv("pred_times_2017_3days.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"),stringsAsFactors=FALSE)
results_fileT4days <- read.csv("pred_times_2017_4days.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"),stringsAsFactors=FALSE)
results_fileT5days <- read.csv("pred_times_2017_5days.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"),stringsAsFactors=FALSE)
results_fileT6days <- read.csv("pred_times_2017_6days.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"),stringsAsFactors=FALSE)
week <- read.csv("pred_times_2017_7days.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"),stringsAsFactors=FALSE)
month <- read.csv("pred_times_2017_30days.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"),stringsAsFactors=FALSE)

results_fileT1year <- read.csv("pred_times_2017_1year.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"),stringsAsFactors=FALSE)



#correct real peak column
results_fileT2days$real_peak <-results_fileT6days$real_peak
results_fileT3days$real_peak <-results_fileT6days$real_peak
results_fileT4days$real_peak <-results_fileT6days$real_peak
results_fileT5days$real_peak <-results_fileT6days$real_peak
results_fileT6days$real_peak <-results_fileT6days$real_peak
results_fileT7days$real_peak<-results_fileT6days$real_peak
results_fileT1year$real_peak <-results_fileT6days$real_peak
  
#compute time differences
colNumber<-ncol(pred_times)-1
result_list <-data.frame(matrix(nrow = 365, ncol = 7))
columns = c("xgboost", "cubist","deepLearning","rf", "LSTM", "avg", "median")
colnames(result_list) <- columns

limit_in_minutes <-35

pred_times<-results_fileT6days[-1]

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


na_count_1year <-sapply(result_list, function(x) sum(length(which(is.na(x)))))
na_count_1year
