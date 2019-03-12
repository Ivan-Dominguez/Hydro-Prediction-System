library(shiny)
library(shinydashboard)

runApp('30_days.R')


ui <- dashboardPage(
  
  dashboardHeader(title = "HYDRO", titleWidth = 100),
  
  dashboardSidebar(
    
    width = 150,
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
  ),
  
  dashboardBody(
   tabItem(tabName = "first",
           h2("Daily Prediction")),
   
   fluidRow(
     box(plotlyOutput("plot"), width=15, height=500)

   )
  )
  
)


#SERVER
server <- function(input, output) { 
  
  output$plot <- renderPlotly({
    plot_ly(
      mode = 'lines+markers'
    ) %>%
    add_trace(
      y =  ~ test_set$fwts,
      x =  ~ predictions_df$datetime,
      mode = 'lines',
      type = 'scatter',
      name = "Test Data",
      line = list(color = ("red"))
    ) %>%
    add_trace(
      y =  ~ predictions_df$fwts_pred,
      x =  ~ predictions_df$datetime,
      mode = 'lines',
      type = 'scatter',
      name = "LSTM",
      line = list(color = ("green"))
    ) %>%
    add_trace(
      y =  ~ prediction_cubist,
      x =  ~ predictions_df$datetime,
      mode = 'lines',
      type = 'scatter',
      name = "Cubist",
      line = list(color = ("blue"))
    ) %>%
    add_trace(
      y =  ~ prediction_xgboost,
      x =  ~ predictions_df$datetime,
      mode = 'lines',
      type = 'scatter',
      name = "XGBoost",
      line = list(color = ("orange"))
    ) %>%
    add_trace(
      y =  ~ prediction_deepLearning,
      x =  ~ predictions_df$datetime,
      mode = 'lines',
      type = 'scatter',
      name = "Deep Learning",
      line = list(color = ("purple"))
    ) %>%
    add_trace(
      y =  ~ prediction_RF,
      x =  ~ predictions_df$datetime,
      mode = 'lines',
      type = 'scatter',
      name = "Random Forest",
      line = list(color = ("yellow"))
    ) %>%
    add_trace(
      x =  ~ xcoord_test_pred,
      y =  ~ ymax_test_pred,
      mode = 'markers',
      type = 'scatter',
      name = paste("Real daily Peak", strftime(xcoord_test_pred,format="%H:%M:%S",tz="UTC")),
      marker = list(color = ("black"),size=9,symbol="triangle-up")
    ) %>%
    add_trace(
      x =  ~ xcoord_fwts_pred,
      y =  ~ ymax_fwts_pred,
      mode = 'markers',
      type = 'scatter',
      name = paste("LSTM Predicted Daily Peak", strftime(xcoord_fwts_pred,format="%H:%M:%S",tz="UTC")),
      marker = list(color = ("yellow"),size=9,symbol="circle")
    ) %>%
    add_trace(
      x =  ~ xcoord_cubist_pred,
      y =  ~ ymax_cubist_pred,
      mode = 'markers',
      type = 'scatter',
      name = paste("Cubist Predicted Daily Peak", strftime(xcoord_cubist_pred,format="%H:%M:%S",tz="UTC")),
      marker = list(color = ("orange"),size=9,symbol="square")
    ) %>%
    add_trace(
      x =  ~ xcoord_xgboost_pred,
      y =  ~ ymax_xgboost_pred,
      mode = 'markers',
      type = 'scatter',
      name = paste("XGBoost Predicted Daily Peak",strftime(xcoord_xgboost_pred,format="%H:%M:%S",tz="UTC")),
      marker = list(color = ("black"),size=9,symbol="square")
    ) %>%
    add_trace(
      x =  ~ xcoord_deepLearning_pred,
      y =  ~ ymax_deepLearning_pred,
      mode = 'markers',
      type = 'scatter',
      name = paste("Deep Learning Predicted Daily Peak",strftime(xcoord_deepLearning_pred,format="%H:%M:%S",tz="UTC")),
      marker = list(color = ("black"),size=9,symbol="square")
    ) %>%
    add_trace(
      x =  ~ xcoord_RF_pred,
      y =  ~ ymax_RF_pred,
      mode = 'markers',
      type = 'scatter',
      name = paste("Random Forest Predicted Daily Peak",strftime(xcoord_RF_pred,format="%H:%M:%S",tz="UTC")),
      marker = list(color = ("black"),size=9,symbol="square")
    ) %>%
    add_trace(
      x =  ~ xcoord_median_pred,
      y =  ~ 58000,
      mode = 'markers',
      type = 'scatter',
      name = paste("MEDIAN",strftime(xcoord_median_pred,format="%H:%M:%S",tz="UTC")),
      marker = list(color = ("red"),size=9,symbol="square")
    ) %>%
    layout(
      title = paste('prediction for', predictionDate),
      xaxis = list(
        title = 'Time',
        autotick = TRUE,
        showticklabels = TRUE
      ),
      yaxis = list(title = "Power Consumption")
    )#end plotly
    
  })
  
  }

shinyApp(ui, server)

