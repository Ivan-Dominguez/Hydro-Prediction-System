
require("keras")
require("jsonlite")
require("lubridate")
require("recipes")
require("dplyr")
require("recipes")
require("curl")
require("tidyr")
require("readr")
require("plotly")
require("htmlwidgets")
require("shiny")






dataset_fwts <- read.csv("/Users/ivan/Google Drive/Degree Project/Repository/Hydro-Prediction-System/training_data/trainingFile_fwts.csv", header=TRUE, sep=",", na.strings=c("NA", "NULL"))


#the part in quotations is the column name you are scaling
receipe_object_fwts <- recipe(dataset_fwts) %>%
  step_sqrt("fwts") %>%
  step_center("fwts") %>%
  step_scale("fwts") %>%
  prep()

data <- bake(receipe_object_fwts,dataset_fwts)


#****************Variables**************#
predictionDate <- as.Date(x = "2014-01-02")
yesterdayDate <- predictionDate-1
  

#****************TODAY's WEATHER Dark Sky API CALL**************#
  
  str_todayDate <- paste(predictionDate, "T", "00:00:00", sep = "")
url <-
  paste(
    "https://api.darksky.net/forecast/bc758d0b80bd80c01ee2e038288dd440/48.3809,-89.2477,",
    str_todayDate,
    "?units=ca&exclude=alerts,minutely,daily,flags,currently",
    sep = ""
  )
darkSky_df <-data.frame(fromJSON(url, simplifyDataFrame = TRUE))



#*****************Create dataframe to use as model input**********
input <-data.frame(matrix(ncol = 19, nrow = (nrow(darkSky_df))))
columns = c(
  "datetime",
  "time",
  "fwts",
  "ssm",
  "sint",
  "cost",
  "temp",
  "dew",
  "hum",
  "wspd",
  "pres",
  "vis",
  "mon",
  "tue",
  "wed",
  "thu",
  "fri",
  "sat",
  "sun"
)
colnames(input) <- columns




#********************Insert Data into Data Frame***********************#
input$datetime <-
  as.POSIXct(darkSky_df$hourly.data.time,
             origin = "1970-01-01")

input$datetime<-input$datetime-(300*12*5)

input$temp <-
  darkSky_df$hourly.data.temperature #as degress celcius
input$dew <-
  darkSky_df$hourly.data.dewPoint #as degrees celcius
input$hum <-
  darkSky_df$hourly.data.humidity #as decimal fraction
input$pres <-
  darkSky_df$hourly.data.pressure/10 #as hectopascals
input$vis <- darkSky_df$hourly.data.visibility #in kilometers
input$wspd <-
  darkSky_df$hourly.data.windSpeed #as kilometers per hour



#****Create a dataframe with all possible date/times at interval of 5 mins*****#
dateRange <-
  data.frame(datetime = seq(min(input$datetime), max(input$datetime) + 3540, by = 5 *
                              60))

#*******************Right Join dateRange dataframe********************#
input <- input %>%
  right_join(dateRange, by = "datetime") %>%
  fill(temp, dew, hum, pres, wspd, vis)

#*********************Fill Time Column******************************#
input$time <- strftime(input$datetime, format = "%H:%M:%S")



#Transform Time into Sine & Cosine
seconds_in_day <- 24 * 60 * 60
input$ssm <-
  (as.numeric(as.POSIXct(strptime(input$time, format = "%H:%M:%S"))) -  as.numeric(as.POSIXct(strptime("0", format = "%S"))))
input$sint <- sin(2 * pi * input$ssm / seconds_in_day)
input$cost <- cos(2 * pi * input$ssm / seconds_in_day)

#insert date column
dataset_fwts$date<-strftime(dataset_fwts$datetime, format="%Y-%m-%d")



#Prefill Days of Week Columns to 0
input$mon <- rep(0, nrow(input))
input$tue <- rep(0, nrow(input))
input$wed <- rep(0, nrow(input))
input$thu <- rep(0, nrow(input))
input$fri <- rep(0, nrow(input))
input$sat <- rep(0, nrow(input))
input$sun <- rep(0, nrow(input))



# #Iterate through all rows setting appropriate weekday to 1
for (i in 1:nrow(input)) {
  weekday <- weekdays(input$datetime[i])
  if (weekday == "Monday") {
    input$mon[i] <- 1
  }
  if (weekday == "Tuesday") {
    input$tue[i] <- 1
  }
  if (weekday == "Wednesday") {
    input$wed[i] <- 1
  }
  if (weekday == "Thursday") {
    input$thu[i] <- 1
  }
  if (weekday == "Friday") {
    input$fri[i] <- 1
  }
  if (weekday == "Saturday") {
    input$sat[i] <- 1
  }
  if (weekday == "Sunday") {
    input$sun[i] <- 1
  }
}


fwts_model <- load_model_hdf5("/Users/ivan/Google Drive/Degree Project/Repository/Hydro-Prediction-System/training_data/saved_models/Stateful_Lag288_FWTS_MV16_MODEL", compile = TRUE)




testX_sint_vector<-input$sint
testX_sint_array <- array(dim = c(length(testX_sint_vector),1,1))


testX_cost_vector<-input$cost
testX_cost_array <- array(dim = c(length(testX_cost_vector),1,1))

testX_temp_vector<-input$temp
testX_temp_array <- array(dim = c(length(testX_temp_vector),1,1))

testX_hum_vector<-input$hum
testX_hum_array <- array(dim = c(length(testX_hum_vector),1,1))

testX_dew_vector<-input$dew
testX_dew_array <- array(dim = c(length(testX_dew_vector),1,1))

testX_wspd_vector<-input$wspd
testX_wspd_array <- array(dim = c(length(testX_wspd_vector),1,1))

testX_vis_vector<-input$vis
testX_vis_array <- array(dim = c(length(testX_vis_vector),1,1))

testX_pres_vector<-input$pres
testX_pres_array <- array(dim = c(length(testX_pres_vector),1,1))


testX_mon_vector<-input$mon
testX_mon_array <- array(dim = c(length(testX_mon_vector),1,1))

testX_tue_vector<-input$tue
testX_tue_array <- array(dim = c(length(testX_tue_vector),1,1))

testX_wed_vector<-input$wed
testX_wed_array <- array(dim = c(length(testX_wed_vector),1,1))

testX_thu_vector<-input$thu
testX_thu_array <- array(dim = c(length(testX_thu_vector),1,1))

testX_fri_vector<-input$fri
testX_fri_array <- array(dim = c(length(testX_fri_vector),1,1))

testX_sat_vector<-input$sat
testX_sat_array <- array(dim = c(length(testX_sat_vector),1,1))

testX_sun_vector<-input$sun
testX_sun_array <- array(dim = c(length(testX_sun_vector),1,1))


testX_fwts_vector<-dataset_fwts$fwts[1:288]
testX_fwts_array <- array(dim = c(length(testX_fwts_vector),1,1))


fwts_testX_input_list <- list(
  testX_fwts_array,testX_temp_array,testX_dew_array,testX_hum_array,testX_wspd_array,testX_vis_array,testX_pres_array,testX_mon_array,testX_tue_array,testX_wed_array,testX_thu_array,testX_fri_array,testX_sat_array,testX_sun_array,testX_sint_array,testX_cost_array
)

predictions_fwts <-
  fwts_model %>% predict(fwts_testX_input_list, batch_size = 72) %>% .[, 1]

pred_array_fwts <-
  array(dim = c(length(predictions_fwts), 1, 1))


predictions_df <-
  data.frame(matrix(ncol = 2, nrow = 288))
columns = c("datetime",
            "fwts_pred")
colnames(predictions_df) <- columns

predictions_df$datetime <-
  seq(ymd_hm(paste(predictionDate, "00:00")), ymd_hm(paste(predictionDate, "23:55")), by =
        "5 min")

predictions_df$fwts_pred <- predictions_fwts




attribute_centre_fwts <-
  as.numeric(
    read_lines(
      "/Users/ivan/Google Drive/Degree Project/Repository/Hydro-Prediction-System/training_data/saved_attributes/attribute_centre_fwts_stateful_lag288.txt"
    )
  )
attribute_scale_fwts <-
  as.numeric(
    read_lines(
      "/Users/ivan/Google Drive/Degree Project/Repository/Hydro-Prediction-System/training_data/saved_attributes/attribute_scale_fwts_stateful_lag288.txt"
    )
  )



#Rescale Predictions using Saved Attributes for Each Feed

predictions_df$fwts_pred <-
  (predictions_df$fwts_pred * attribute_scale_fwts + attribute_centre_fwts) ^ 2




predictions_df$date<-as.Date(predictions_df$datetime)
predictions_df$time<-strftime(predictions_df$datetime,format="%H:%M:%S",tz="UTC")

##***************************Calculations*******************************#




#FWTS Predicted Daily Peak
ymax_fwts_pred = max(predictions_df$fwts_pred)
xcoord_fwts_pred <-
  predictions_df$datetime[which.max(predictions_df$fwts_pred)]





#FWTS Predicted Daily Min
ymin_fwts_pred = min(predictions_df$fwts_pred)
xcoord_fwts_min_pred <-
  predictions_df$datetime[which.min(predictions_df$fwts_pred)]



pl <-
  plot_ly(
    mode = 'lines+markers'
  ) %>%
  add_trace(
    y =  ~ predictions_df$fwts_pred,
    x =  ~ predictions_df$datetime,
    mode = 'lines',
    type = 'scatter',
    name = "FWTS P.C Prediction",
    line = list(color = ("green"))
  ) %>%
  add_trace(
    x =  ~ xcoord_fwts_pred,
    y =  ~ ymax_fwts_pred,
    mode = 'markers',
    type = 'scatter',
    name = paste("FWTS Predicted Daily Peak",xcoord_fwts_pred),
    marker = list(color = ("red"),size=9,symbol="triangle-up")
  ) %>%
  add_trace(
    x =  ~ xcoord_fwts_min_pred,
    y =  ~ ymin_fwts_pred,
    mode = 'markers',
    type = 'scatter',
    name = paste("FWTS Predicted Daily Min",xcoord_fwts_min_pred),
    marker = list(color = ("orange"),size=9,symbol="triangle-down")
  ) %>%
  layout(
    title = paste('Weather Model Predicted Power Consumption for', predictionDate),
    xaxis = list(
      title = 'Time',
      autotick = TRUE,
      showticklabels = TRUE
      
    ),
    yaxis = list(title = "Power Consumption")
  )

pl

# graphfileName <-
#   paste(
#     format(
#       Sys.time(),
#       "C:/Users/developer/Documents/prediction/predictionGraphs/weatherModel_predictions/prediction_weatherModel_%Y-%m-%d_%H-%M"
#     ),
#     "html",
#     sep = "."
#   )
# saveWidget(as_widget(pl), graphfileName, selfcontained = FALSE)

