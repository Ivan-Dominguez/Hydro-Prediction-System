daily_plot<-function(pred_list, predictionDate){

  pl <-
    plot_ly(
      mode = 'lines+markers'
    ) %>%
    add_trace(
      y =  ~ pred_list$cubist,
      x =  ~ pred_list$hours,
      mode = 'lines',
      type = 'scatter',
      name = "Cubist",
      line = list(color = ("pink"))
    ) %>%
    add_trace(
      y =  ~ pred_list$xgboost,
      x =  ~ pred_list$hours,
      mode = 'lines',
      type = 'scatter',
      name = "XGBoost",
      line = list(color = ("blue"))
    ) %>%
    add_trace(
      y =  ~ pred_list$DL,
      x =  ~ pred_list$hours,
      mode = 'lines',
      type = 'scatter',
      name = "Deep Learning",
      line = list(color = ("orange"))
    ) %>%
    add_trace(
      y =  ~ pred_list$RF,
      x =  ~ pred_list$hours,
      mode = 'lines',
      type = 'scatter',
      name = "Random Forest",
      line = list(color = ("green"))
    ) %>%
  add_trace(
    y =  ~ pred_list$AVG,
    x =  ~ pred_list$hours,
    mode = 'lines',
    type = 'scatter',
    name = "AVG",
    line = list(color = ("yellow"))
    ) %>%
  add_trace(
    y =  ~ pred_list$test,
    x =  ~ pred_list$hours,
    mode = 'lines',
    type = 'scatter',
    name = "Test Data",
    line = list(color = ("black"))
  ) %>%
    add_trace(
      x =  ~ pred_list$xcoord_list$cubist,
      y =  ~ pred_list$ymax_list$cubist,
      mode = 'markers',
      type = 'scatter',
      name = paste("Cubist Predicted Peak", strftime(pred_list$xcoord_list$cubist,format="%H:%M:%S",tz="UTC")),
      marker = list(color = ("black"),size=9,symbol="circle")
    ) %>%
    add_trace(
      x =  ~ pred_list$xcoord_list$xgboost,
      y =  ~ pred_list$ymax_list$xgboost,
      mode = 'markers',
      type = 'scatter',
      name = paste("XGBoost Predicted Peak",strftime(pred_list$xcoord_list$xgboost,format="%H:%M:%S",tz="UTC")),
      marker = list(color = ("black"),size=9,symbol="circle")
    ) %>%
    add_trace(
      x =  ~ pred_list$xcoord_list$DL,
      y =  ~ pred_list$ymax_list$DL,
      mode = 'markers',
      type = 'scatter',
      name = paste("Deep Learning Predicted Peak",strftime(pred_list$xcoord_list$DL,format="%H:%M:%S",tz="UTC")),
      marker = list(color = ("black"),size=9,symbol="circle")
    ) %>%
    add_trace(
      x =  ~ pred_list$xcoord_list$RF,
      y =  ~ pred_list$ymax_list$RF,
      mode = 'markers',
      type = 'scatter',
      name = paste("Random Forest Predicted Peak",strftime(pred_list$xcoord_list$RF,format="%H:%M:%S",tz="UTC")),
      marker = list(color = ("black"),size=9,symbol="circle")
    ) %>%
    add_trace(
      x =  ~ pred_list$xcoord_list$avg,
      y =  ~ pred_list$ymax_list$avg,
      mode = 'markers',
      type = 'scatter',
      name = paste("AVG Predicted Peak", strftime(pred_list$xcoord_list$avg,format="%H:%M:%S",tz="UTC")),
      marker = list(color = ("black"),size=9,symbol="circle")
    ) %>%
  add_trace(
    x =  ~ pred_list$xcoord_list$test,
    y =  ~ pred_list$ymax_list$test,
    mode = 'markers',
    type = 'scatter',
    name = paste("Real Peak", strftime(pred_list$xcoord_list$test,format="%H:%M:%S",tz="UTC")),
    marker = list(color = ("red"),size=13,symbol="triangle-up")
  ) %>%
    add_trace(
      x =  ~ pred_list$xcoord_list$mean,
      y =  ~ 63000,
      mode = 'markers',
      type = 'scatter',
      name = paste("Mean",strftime(pred_list$xcoord_list$mean,format="%H:%M:%S",tz="UTC")),
      marker = list(color = ("red"),size=9,symbol="square")
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
  
  pl
  
  
  return(pl)
}

