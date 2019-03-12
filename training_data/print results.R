
setwd("~/Google Drive/Degree Project/Repository/Hydro-prediction-System/training_data")

#load data
week <- read.csv("week.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"),stringsAsFactors=FALSE)
month <- read.csv("month.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"),stringsAsFactors=FALSE)

count_errors<-function(times_list){

  pred_times<-times_list[-1]
  
  
    #compute time differences
    colNumber<-ncol(pred_times)-1
    result_list <-data.frame(matrix(nrow = 365, ncol = 7))
    columns = c("xgboost", "cubist","deepLearning","rf", "LSTM", "avg", "median")
    colnames(result_list) <- columns
    
    limit_in_minutes <-30
    
    
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
    
    return(result_list)
}


week_results<-count_errors(week)
month_results<-count_errors(month)

week_na_count <-sapply(week_results, function(x) sum(length(which(is.na(x)))))
week_na_count

month_na_count <-sapply(month_results, function(x) sum(length(which(is.na(x)))))
month_na_count
