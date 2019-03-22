daily_plot<-function(pred_list, predictionDate){

  pl <-
    plot_ly(
      mode = 'lines+markers'
    ) %>%
    add_trace(
      y =  ~ pred_list$test,
      x =  ~ pred_list$hours,
      mode = 'lines',
      type = 'scatter',
      name = "Test Data",
      line = list(color = ("red"))
    ) %>%
    add_trace(
      y =  ~ pred_list$LSTM,
      x =  ~ pred_list$hours,
      mode = 'lines',
      type = 'scatter',
      name = "LSTM",
      line = list(color = ("green"))
    ) %>%
    add_trace(
      y =  ~ pred_list$cubist,
      x =  ~ pred_list$hours,
      mode = 'lines',
      type = 'scatter',
      name = "Cubist",
      line = list(color = ("blue"))
    ) %>%
    add_trace(
      y =  ~ pred_list$xgboost,
      x =  ~ pred_list$hours,
      mode = 'lines',
      type = 'scatter',
      name = "XGBoost",
      line = list(color = ("orange"))
    ) %>%
    add_trace(
      y =  ~ pred_list$DL,
      x =  ~ pred_list$hours,
      mode = 'lines',
      type = 'scatter',
      name = "Deep Learning",
      line = list(color = ("purple"))
    ) %>%
    add_trace(
      y =  ~ pred_list$RF,
      x =  ~ pred_list$hours,
      mode = 'lines',
      type = 'scatter',
      name = "Random Forest",
      line = list(color = ("yellow"))
    ) %>%
    add_trace(
      x =  ~ pred_list$xcoord_list[1],
      y =  ~ pred_list$ymax_list[1],
      mode = 'markers',
      type = 'scatter',
      name = paste("Real daily Peak", strftime(pred_list$xcoord_list[1],format="%H:%M:%S",tz="UTC")),
      marker = list(color = ("black"),size=11,symbol="triangle-up")
    ) %>%
    add_trace(
      x =  ~ pred_list$xcoord_list[2],
      y =  ~ pred_list$ymax_list[2],
      mode = 'markers',
      type = 'scatter',
      name = paste("LSTM Predicted Daily Peak", strftime(pred_list$xcoord_list[2],format="%H:%M:%S",tz="UTC")),
      marker = list(color = ("black"),size=9,symbol="square")
    ) %>%
    add_trace(
      x =  ~ pred_list$xcoord_list[3],
      y =  ~ pred_list$ymax_list[3],
      mode = 'markers',
      type = 'scatter',
      name = paste("Cubist Predicted Daily Peak", strftime(pred_list$xcoord_list[3],format="%H:%M:%S",tz="UTC")),
      marker = list(color = ("black"),size=9,symbol="square")
    ) %>%
    add_trace(
      x =  ~ pred_list$xcoord_list[4],
      y =  ~ pred_list$ymax_list[4],
      mode = 'markers',
      type = 'scatter',
      name = paste("XGBoost Predicted Daily Peak",strftime(pred_list$xcoord_list[4],format="%H:%M:%S",tz="UTC")),
      marker = list(color = ("black"),size=9,symbol="square")
    ) %>%
    add_trace(
      x =  ~ pred_list$xcoord_list[5],
      y =  ~ pred_list$ymax_list[5],
      mode = 'markers',
      type = 'scatter',
      name = paste("Deep Learning Predicted Daily Peak",strftime(pred_list$xcoord_list[5],format="%H:%M:%S",tz="UTC")),
      marker = list(color = ("black"),size=9,symbol="square")
    ) %>%
    add_trace(
      x =  ~ pred_list$xcoord_list[6],
      y =  ~ pred_list$ymax_list[6],
      mode = 'markers',
      type = 'scatter',
      name = paste("Random Forest Predicted Daily Peak",strftime(pred_list$xcoord_list[6],format="%H:%M:%S",tz="UTC")),
      marker = list(color = ("black"),size=9,symbol="square")
    ) %>%
    add_trace(
      x =  ~ pred_list$xcoord_list[7],
      y =  ~ 67000,
      mode = 'markers',
      type = 'scatter',
      name = paste("AVG",strftime(pred_list$xcoord_list[7],format="%H:%M:%S",tz="UTC")),
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

