plot2<-function(pred_list, predictionDate, checkbox_list, mean_values){
  
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
    
    pl<-add_trace(
      pl,
      y =  ~ mean_values$predictions_avg,
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
    
    
    # avg_peak = max(prediction_avg[144:288])
    # avg_peak_time <-
    #   pred_list$hours[which.max(prediction_avg[144:288]) + 143]
    
    pl<- add_trace(
      pl,
      y =  ~ mean_values$avg_peak,
      x =  ~ mean_values$avg_peak_time,
      mode = 'markers',
      type = 'scatter',
      name = paste("AVG Predicted Peak", strftime(mean_values$avg_peak_time,format="%H:%M",tz="UTC")),
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
  
  pl<- add_trace(
    pl,
    y =  ~ pred_list$ymax_list$test + 2000,
    x =  ~ mean_values$mean_peak_time,
    mode = 'markers',
    type = 'scatter',
    name = paste("Mean",strftime(mean_values$mean_peak_time,format="%H:%M",tz="UTC")),
    marker = list(color = 'rgb(0, 191, 255)',size=9,symbol="square")
  )
  
  return(pl) 
}