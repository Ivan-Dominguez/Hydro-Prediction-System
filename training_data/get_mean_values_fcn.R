get_mean_values<-function(pred_list, checkbox_list,predictionDate){
  
    sum_pred<-0
    divisor<-0
    
    if(checkbox_list$cubist_chk){
      sum_pred<-sum_pred + pred_list$cubist
      divisor<-divisor + 1
    }
    if(checkbox_list$xgb_chk){
      sum_pred<-sum_pred + pred_list$xgboost
      divisor<-divisor + 1
    }
    if(checkbox_list$dl_chk){
      sum_pred<-sum_pred + pred_list$DL
      divisor<-divisor + 1
    }
    if(checkbox_list$rf_chk){
      sum_pred<-sum_pred + pred_list$RF
      divisor<-divisor + 1
    }
    
    predictions_avg <- sum_pred/divisor
    
    #avg peak
    avg_peak = max(predictions_avg[144:288])
    avg_peak_time <-
      pred_list$hours[which.max(predictions_avg[144:288]) + 143]
 
  
    #Mean
    pred_vector<-as_datetime(NA)
    
    if(checkbox_list$cubist_chk){
      pred_vector<-c(pred_vector, pred_list$xcoord_list$cubist)
    }
    if(checkbox_list$xgb_chk){
      pred_vector<-c(pred_vector, pred_list$xcoord_list$xgboost)
    }
    if(checkbox_list$dl_chk){
      pred_vector<-c(pred_vector, pred_list$xcoord_list$DL)
    }
    if(checkbox_list$rf_chk){
      pred_vector<-c(pred_vector, pred_list$xcoord_list$Rf)
    }
    if(checkbox_list$avg_chk){
      pred_vector<-c(pred_vector,  pred_list$xcoord_list$avg)
    }
    
    #remove 1st element(empty)
    pred_vector<-pred_vector[2:length( pred_vector)]
    
    #mean of peak times
    mean_peak_time <- strptime(predictionDate, "%Y-%m-%d")-mean(difftime(
      paste(predictionDate, "00:00:00", sep=" "),
      pred_vector,
      units = "secs"))
    
  
  mean_list<-list("predictions_avg"=predictions_avg, "avg_peak"=avg_peak,
                  "avg_peak_time"=avg_peak_time,"mean_peak_time"=mean_peak_time )
  
  return(mean_list)
}