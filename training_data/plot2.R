plot2<-function(pred_list, predictionDate, checkbox_list){
  
  pl <-
    plot_ly(
      mode = 'lines+markers'
    ) %>%
    layout(
      title = paste(predictionDate),
      xaxis = list(
        title = 'Time',
        autotick = TRUE,
        showticklabels = TRUE
      ),
      yaxis = list(title = "Power Consumption")
    )
 
  
  ######################## curves #####################
  
 #cubist  
 if(checkbox_list$cubist_chk){ 
   pl<- add_trace(
      pl,
      y =  ~ pred_list$cubist,
      x =  ~ pred_list$hours,
      mode = 'lines',
      type = 'scatter',
      name = "Cubist",
      line = list(color = 'rgb(255, 0, 255)')
    )
 } 
 
  #XGBoost
  if(checkbox_list$xgb_chk){ 
    
    pl<-add_trace(
      pl,
      y =  ~ pred_list$xgboost,
      x =  ~ pred_list$hours,
      mode = 'lines',
      type = 'scatter',
      name = "XGBoost",
      line = list(color = ("blue"))
    )
  }
  
  #Deep Learning
  if(checkbox_list$dl_chk){ 
    
    pl<-add_trace(
      pl,
      y =  ~ pred_list$DL,
      x =  ~ pred_list$hours,
      mode = 'lines',
      type = 'scatter',
      name = "Deep Learning",
      line = list(color = 'rgb(255, 128, 0)')
    )
  }
  
  #Random Forest
  if(checkbox_list$rf_chk){ 
    
    pl<-add_trace(
      pl,
      y =  ~ pred_list$RF,
      x =  ~ pred_list$hours,
      mode = 'lines',
      type = 'scatter',
      name = "Random Forest",
      line = list(color = ("green"))
    )
  }
  
  #AVG
  if(checkbox_list$avg_chk){ 
    
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
    
    prediction_avg = sum_pred/divisor

    pl<-add_trace(
      pl,
      y =  ~ prediction_avg,
      x =  ~ pred_list$hours,
      mode = 'lines',
      type = 'scatter',
      name = "AVG",
      line = list(color = 'rgb(255, 215, 0)')
    )
  }
  
  #Test data
  if(checkbox_list$test_chk){ 
    
    pl<-add_trace(
      pl,
      y =  ~ pred_list$test,
      x =  ~ pred_list$hours,
      mode = 'lines',
      type = 'scatter',
      name = "Test Data",
      line = list(color = ("black"))
    )
  }
  
  
  ###################################### peaks ################################################
  
  #cubist
  if(checkbox_list$cubist_chk){ 
    pl<- add_trace(
      pl,
      x =  ~ pred_list$xcoord_list$cubist,
      y =  ~ pred_list$ymax_list$cubist,
      mode = 'markers',
      type = 'scatter',
      name = paste("Cubist Predicted Peak", strftime(pred_list$xcoord_list$cubist,format="%H:%M",tz="UTC")),
      marker = list(color = ("black"),size=9,symbol="circle")
    ) 
  }
    
  #XGBoost
  if(checkbox_list$xgb_chk){ 
    pl<- add_trace(
      pl,
      x =  ~ pred_list$xcoord_list$xgboost,
      y =  ~ pred_list$ymax_list$xgboost,
      mode = 'markers',
      type = 'scatter',
      name = paste("XGBoost Predicted Peak",strftime(pred_list$xcoord_list$xgboost,format="%H:%M",tz="UTC")),
      marker = list(color = ("black"),size=9,symbol="circle")
    )
  }
  
  #Deep Learning
  if(checkbox_list$dl_chk){ 
    pl<- add_trace(
      pl,
      x =  ~ pred_list$xcoord_list$DL,
      y =  ~ pred_list$ymax_list$DL,
      mode = 'markers',
      type = 'scatter',
      name = paste("Deep Learning Predicted Peak",strftime(pred_list$xcoord_list$DL,format="%H:%M",tz="UTC")),
      marker = list(color = ("black"),size=9,symbol="circle")
    )
  }
  
  #Random Forest
  if(checkbox_list$rf_chk){ 
    pl<- add_trace(
      pl,
      x =  ~ pred_list$xcoord_list$RF,
      y =  ~ pred_list$ymax_list$RF,
      mode = 'markers',
      type = 'scatter',
      name = paste("Random Forest Predicted Peak",strftime(pred_list$xcoord_list$RF,format="%H:%M",tz="UTC")),
      marker = list(color = ("black"),size=9,symbol="circle")
    )
  }
  
  #AVG
  if(checkbox_list$avg_chk){ 
    
    
    avg_peak = max(prediction_avg[144:288])
    avg_peak_time <-
      pred_list$hours[which.max(prediction_avg[144:288]) + 143]
    
    pl<- add_trace(
      pl,
      y =  ~ avg_peak,
      x =  ~ avg_peak_time,
      mode = 'markers',
      type = 'scatter',
      name = paste("AVG Predicted Peak", strftime(avg_peak_time,format="%H:%M",tz="UTC")),
      marker = list(color = ("black"),size=9,symbol="circle")
    )
  } 
  
  #Test Data
  if(checkbox_list$test_chk){ 
    pl<- add_trace(
      pl,
      y =  ~ pred_list$ymax_list$test,
      x =  ~ pred_list$xcoord_list$test,
      mode = 'markers',
      type = 'scatter',
      name = paste("Real Peak", strftime(pred_list$xcoord_list$test,format="%H:%M",tz="UTC")),
      marker = list(color = ("red"),size=13,symbol="triangle-up")
    )
  }  
  
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
  mean_time <- strptime(predictionDate, "%Y-%m-%d")-mean(difftime(
    paste(predictionDate, "00:00:00", sep=" "),
    pred_vector,
    units = "secs"))
    
    
  pl<- add_trace(
    pl,
    y =  ~ pred_list$ymax_list$test + 2000,
    x =  ~ mean_time,
    mode = 'markers',
    type = 'scatter',
    name = paste("Mean",strftime(mean_time,format="%H:%M",tz="UTC")),
    marker = list(color = 'rgb(0, 191, 255)',size=9,symbol="square")
  )
  
  return(pl) 
}