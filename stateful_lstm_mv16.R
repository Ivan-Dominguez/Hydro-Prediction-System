generatePredictions_mv<-function(){

setwd("C:/Users/developer/Documents/prediction")
require("keras")
require("jsonlite")
require("lubridate")
require("htmlwidgets")
require("plotly")
require("RMySQL")
require("RODBC")
require("tidyverse")
require("DMwR")
require("zoo")
require("ggplot2")
require("mailR")
require("recipes")
require("dplyr")


#****************Variables**************#
predictionDate <- Sys.Date()
todayWeekday<-weekdays(predictionDate)
yesterdayDate <- predictionDate - 1
twoDaysAgoDate <- predictionDate - 2
threeDaysAgoDate <- predictionDate - 3

#Load BRTS Model
brts_lstm_model_lag288_mv <-
  load_model_hdf5(
    "C:/Users/developer/Documents/prediction/saved_models/Stateful_Lag288_BRTS_MV16_MODEL",
    compile = TRUE
  )

#Load FWTS Model
fwts_lstm_model_lag288_mv <-
  load_model_hdf5(
    "C:/Users/developer/Documents/prediction/saved_models/Stateful_Lag288_FWTS_MV16_MODEL",
    compile = TRUE
  )

#Load PATS Model
pats_lstm_model_lag288_mv <-
  load_model_hdf5(
    "C:/Users/developer/Documents/prediction/saved_models/Stateful_Lag288_PATS_MV16_MODEL",
    compile = TRUE
  )

#****************TODAY's WEATHER Dark Sky API CALL**************#

str_todayDate <- paste(predictionDate, "T", "00:00:00", sep = "")
url <-
  paste(
    "https://api.darksky.net/forecast/4d35c6b380b2884f29fc75db08ae7e83/48.3809,-89.2477,",
    str_todayDate,
    "?units=ca&exclude=alerts,minutely,daily,flags,currently",
    sep = ""
  )
today_darksky_df <-
  data.frame(fromJSON(url, simplifyDataFrame = TRUE))

today_darksky_df$hourly.data.precipAccumulation<-NULL


#****************YESTERDAY's WEATHER Dark Sky API CALL**************#

str_yesterdayDate <-
  paste(yesterdayDate, "T", "00:00:00", sep = "")
url <-
  paste(
    "https://api.darksky.net/forecast/4d35c6b380b2884f29fc75db08ae7e83/48.3809,-89.2477,",
    str_yesterdayDate,
    "?units=ca&exclude=alerts,minutely,daily,flags,currently",
    sep = ""
  )
yesterday_darksky_df <-
  data.frame(fromJSON(url, simplifyDataFrame = TRUE))

yesterday_darksky_df$hourly.data.precipAccumulation<-NULL



#******************************#

myConnection <-
  odbcDriverConnect(
    "Driver={SQL Server}; Server=pcvscadadb,1433\\ARCHIVEDB;Database=ARCHIVEDB; UID=developer; Pwd=PeakShave2018"
  )

str_sql_4Days <-
  paste(
    "SELECT * FROM ARCHIVE_HISTSAMPLES78 WHERE TIME LIKE '",
    threeDaysAgoDate,
    " %' OR",
    " TIME LIKE '",
    twoDaysAgoDate,
    " %' OR",
    " TIME LIKE '",
    yesterdayDate,
    " %' OR",
    " TIME LIKE '",
    predictionDate,
    " %'",
    sep = ''
  )

combined_df <- as.data.frame(sqlQuery(myConnection, str_sql_4Days))

odbcCloseAll()

combined_df$TIME <- ymd_hms(combined_df$TIME, tz = 'UTC')
attributes(combined_df$TIME)$tzone <- "America/Thunder_Bay"
combined_df <-
  combined_df[order(combined_df$TIME , decreasing = FALSE), ]
combined_df$date <- strftime(combined_df$TIME, format = "%Y-%m-%d")
combined_df$date <- ymd(combined_df$date)
combined_df$time <- strftime(combined_df$TIME, format = "%H:%M:%S")

combined_df$`MISC.P10_KW-VAL-Q` <- NULL
combined_df$`MISC.P17_KW-VAL-Q` <- NULL
combined_df$`MISC.P02_KW-VAL-Q` <- NULL
colnames(combined_df)[colnames(combined_df) == "MISC.P10_KW-VAL"] <-
  "fwts"
colnames(combined_df)[colnames(combined_df) == "MISC.P17_KW-VAL"] <-
  "brts"
colnames(combined_df)[colnames(combined_df) == "MISC.P02_KW-VAL"] <-
  "pats"
colnames(combined_df)[colnames(combined_df) == "TIME"] <- "datetime"

today_df <- combined_df[grepl(predictionDate, combined_df$date), ]
yesterday_df <- combined_df[grepl(yesterdayDate, combined_df$date), ]
twoDaysAgo_df <- combined_df[grepl(twoDaysAgoDate, combined_df$date), ]
threeDaysAgo_df <-
  combined_df[grepl(threeDaysAgoDate, combined_df$date), ]


odbcCloseAll()


#****************CREATE TRAINING DATAFRAME************************************#
data <-
  data.frame(matrix(ncol = 24, nrow = (
   nrow(yesterday_darksky_df) + nrow(today_darksky_df)
  )))
columns = c(
  "datetime",
  "time",
  "fwts",
  "fwts_lag",
  "brts",
  "brts_lag",
  "pats",
  "pats_lag",
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
colnames(data) <- columns

#********************Combine Yesterday's DarkSky Dataframe & Today DarkSky Dataframe**************************#
darkSky_df <- rbind(yesterday_darksky_df,today_darksky_df)


#********************Insert Data into Data Frame***********************#
data$datetime <-
  as.POSIXct(darkSky_df$hourly.data.time,
             origin = "1970-01-01")
data$temp <-
  darkSky_df$hourly.data.temperature #as degress celcius
data$dew <-
  darkSky_df$hourly.data.dewPoint #as degrees celcius
data$hum <-
  darkSky_df$hourly.data.humidity #as decimal fraction
data$pres <-
  darkSky_df$hourly.data.pressure/10 #as hectopascals
data$vis <- darkSky_df$hourly.data.visibility #in kilometers
data$wspd <-
  darkSky_df$hourly.data.windSpeed #as kilometers per hour



#****Create a dataframe with all possible date/times at interval of 5 mins*****#
dateRange <-
  data.frame(datetime = seq(min(data$datetime), max(data$datetime) + 3540, by = 5 *
                              60))

#*******************Right Join dateRange dataframe********************#
data <- data %>%
  right_join(dateRange, by = "datetime") %>%
  fill(temp, dew, hum, pres, wspd, vis)

#*********************Fill Time Column******************************#
data$time <- strftime(data$datetime, format = "%H:%M:%S")



#Transform Time into Sine & Cosine
seconds_in_day <- 24 * 60 * 60
data$ssm <-
  (as.numeric(as.POSIXct(strptime(data$time, format = "%H:%M:%S"))) -  as.numeric(as.POSIXct(strptime("0", format = "%S"))))
data$sint <- sin(2 * pi * data$ssm / seconds_in_day)
data$cost <- cos(2 * pi * data$ssm / seconds_in_day)



#Prefill Days of Week Columns to 0
data$mon <- rep(0, nrow(data))
data$tue <- rep(0, nrow(data))
data$wed <- rep(0, nrow(data))
data$thu <- rep(0, nrow(data))
data$fri <- rep(0, nrow(data))
data$sat <- rep(0, nrow(data))
data$sun <- rep(0, nrow(data))



# #Iterate through all rows setting appropriate weekday to 1
for (i in 1:nrow(data)) {
  weekday <- weekdays(data$datetime[i])
  if (weekday == "Monday") {
    data$mon[i] <- 1
  }
  if (weekday == "Tuesday") {
    data$tue[i] <- 1
  }
  if (weekday == "Wednesday") {
    data$wed[i] <- 1
  }
  if (weekday == "Thursday") {
    data$thu[i] <- 1
  }
  if (weekday == "Friday") {
    data$fri[i] <- 1
  }
  if (weekday == "Saturday") {
    data$sat[i] <- 1
  }
  if (weekday == "Sunday") {
    data$sun[i] <- 1
  }
}

#****************************FILL POWER CONSUMPTION FOR FWTS,BRTS,PATS INTO DATAFRAME****************#
data$fwts[1:nrow(yesterday_df)] <- yesterday_df$fwts
data$brts[1:nrow(yesterday_df)] <- yesterday_df$brts
data$pats[1:nrow(yesterday_df)] <- yesterday_df$pats

data$fwts_lag[1:nrow(twoDaysAgo_df)] <- twoDaysAgo_df$fwts
data$brts_lag[1:nrow(twoDaysAgo_df)] <- twoDaysAgo_df$brts
data$pats_lag[1:nrow(twoDaysAgo_df)] <- twoDaysAgo_df$pats


receipe_object_fwts <- recipe(data) %>%
  step_sqrt("fwts") %>%
  step_center("fwts") %>%
  step_scale("fwts") %>%
  prep()

data <- bake(receipe_object_fwts,data)

receipe_object_fwts_lag <- recipe(data) %>%
  step_sqrt("fwts_lag") %>%
  step_center("fwts_lag") %>%
  step_scale("fwts_lag") %>%
  prep()

data <- bake(receipe_object_fwts_lag,data)



receipe_object_brts <- recipe(data) %>%
  step_sqrt("brts") %>%
  step_center("brts") %>%
  step_scale("brts") %>%
  prep()

data <- bake(receipe_object_brts,data)

receipe_object_brts_lag <- recipe(data) %>%
  step_sqrt("brts_lag") %>%
  step_center("brts_lag") %>%
  step_scale("brts_lag") %>%
  prep()

data <- bake(receipe_object_brts_lag,data)

receipe_object_pats <- recipe(data) %>%
  step_sqrt("pats") %>%
  step_center("pats") %>%
  step_scale("pats") %>%
  prep()

data <- bake(receipe_object_pats,data)

receipe_object_pats_lag <- recipe(data) %>%
  step_sqrt("pats_lag") %>%
  step_center("pats_lag") %>%
  step_scale("pats_lag") %>%
  prep()

data <- bake(receipe_object_pats_lag,data)



lag_fwts_trainX_vector<- data$fwts_lag[1:nrow(twoDaysAgo_df)]
lag_brts_trainX_vector<-data$brts_lag[1:nrow(twoDaysAgo_df)]
lag_pats_trainX_vector<-data$pats_lag[1:nrow(twoDaysAgo_df)]
temp_trainX_vector<- data$temp[1:nrow(twoDaysAgo_df)]
dew_trainX_vector<- data$dew[1:nrow(twoDaysAgo_df)]
hum_trainX_vector<- data$hum[1:nrow(twoDaysAgo_df)]
wspd_trainX_vector<- data$wspd[1:nrow(twoDaysAgo_df)]
vis_trainX_vector<- data$vis[1:nrow(twoDaysAgo_df)]
pres_trainX_vector<- data$pres[1:nrow(twoDaysAgo_df)]
mon_trainX_vector<- data$mon[1:nrow(twoDaysAgo_df)]
tue_trainX_vector<- data$tue[1:nrow(twoDaysAgo_df)]
wed_trainX_vector<- data$wed[1:nrow(twoDaysAgo_df)]
thu_trainX_vector<- data$thu[1:nrow(twoDaysAgo_df)]
fri_trainX_vector<- data$fri[1:nrow(twoDaysAgo_df)]
sat_trainX_vector<- data$sat[1:nrow(twoDaysAgo_df)]
sun_trainX_vector<- data$sun[1:nrow(twoDaysAgo_df)]
sint_trainX_vector<-data$sint[1:nrow(twoDaysAgo_df)]
cost_trainX_vector<-data$cost[1:nrow(twoDaysAgo_df)]


lag_fwts_trainX_array <- array(data = lag_fwts_trainX_vector,dim = c(length(lag_fwts_trainX_vector),1,1) )
lag_brts_trainX_array <- array(data = lag_brts_trainX_vector,dim = c(length(lag_brts_trainX_vector),1,1) )
lag_pats_trainX_array <- array(data = lag_pats_trainX_vector,dim = c(length(lag_pats_trainX_vector),1,1) )
temp_trainX_array <- array(data = temp_trainX_vector,dim = c(length(temp_trainX_vector),1,1) )
dew_trainX_array <- array(data = dew_trainX_vector,dim = c(length(dew_trainX_vector),1,1) )
hum_trainX_array <- array(data = hum_trainX_vector,dim = c(length(hum_trainX_vector),1,1) )
wspd_trainX_array <- array(data = wspd_trainX_vector,dim = c(length(wspd_trainX_vector),1,1) )
vis_trainX_array <- array(data = vis_trainX_vector,dim = c(length(vis_trainX_vector),1,1) )
pres_trainX_array <- array(data = pres_trainX_vector,dim = c(length(pres_trainX_vector),1,1) )
mon_trainX_array <- array(data = mon_trainX_vector,dim = c(length(mon_trainX_vector),1,1) )
tue_trainX_array <- array(data = tue_trainX_vector,dim = c(length(tue_trainX_vector),1,1) )
wed_trainX_array <- array(data = wed_trainX_vector,dim = c(length(wed_trainX_vector),1,1) )
thu_trainX_array <- array(data = thu_trainX_vector,dim = c(length(thu_trainX_vector),1,1) )
fri_trainX_array <- array(data = fri_trainX_vector,dim = c(length(fri_trainX_vector),1,1) )
sat_trainX_array <- array(data = sat_trainX_vector,dim = c(length(sat_trainX_vector),1,1) )
sun_trainX_array <- array(data = sun_trainX_vector,dim = c(length(sun_trainX_vector),1,1) )
sint_trainX_array <- array(data = sint_trainX_vector,dim = c(length(sint_trainX_vector),1,1) )
cost_trainX_array <- array(data = cost_trainX_vector,dim = c(length(cost_trainX_vector),1,1) )



input_list_fwts <- list(
  lag_fwts_trainX_array,temp_trainX_array,dew_trainX_array,hum_trainX_array,wspd_trainX_array,vis_trainX_array,pres_trainX_array,mon_trainX_array,tue_trainX_array,wed_trainX_array,thu_trainX_array,fri_trainX_array,sat_trainX_array,sun_trainX_array,sint_trainX_array,cost_trainX_array
)

input_list_brts <- list(
  lag_brts_trainX_array,temp_trainX_array,dew_trainX_array,hum_trainX_array,wspd_trainX_array,vis_trainX_array,pres_trainX_array,mon_trainX_array,tue_trainX_array,wed_trainX_array,thu_trainX_array,fri_trainX_array,sat_trainX_array,sun_trainX_array,sint_trainX_array,cost_trainX_array
)

input_list_pats <- list(
  lag_pats_trainX_array,temp_trainX_array,dew_trainX_array,hum_trainX_array,wspd_trainX_array,vis_trainX_array,pres_trainX_array,mon_trainX_array,tue_trainX_array,wed_trainX_array,thu_trainX_array,fri_trainX_array,sat_trainX_array,sun_trainX_array,sint_trainX_array,cost_trainX_array
)

fwts_trainY_vector<- data$fwts[1:nrow(yesterday_df)]
fwts_trainY_array <- array(data = fwts_trainY_vector,dim = c(length(fwts_trainY_vector),1))

brts_trainY_vector<- data$brts[1:nrow(yesterday_df)]
brts_trainY_array <- array(data = brts_trainY_vector,dim = c(length(brts_trainY_vector),1))

pats_trainY_vector<- data$pats[1:nrow(yesterday_df)]
pats_trainY_array <- array(data = pats_trainY_vector,dim = c(length(pats_trainY_vector),1))


summary(fwts_lstm_model_lag288_mv)  
summary(brts_lstm_model_lag288_mv)  
summary(pats_lstm_model_lag288_mv)  

n_epochs_fwts<-40
n_epochs_brts<-20
n_epochs_pats<-20

batch_size <- 72

cat("starting training fwts model")

for(i in 1:n_epochs_fwts){
  fwts_lstm_model_lag288_mv %>% fit(input_list_fwts,fwts_trainY_array,batch_size = batch_size,epochs = 1,verbose=1,shuffle=FALSE)
  
  fwts_lstm_model_lag288_mv %>% reset_states()
  
}  

cat("starting training brts model")
for(i in 1:n_epochs_brts){
  brts_lstm_model_lag288_mv %>% fit(input_list_brts,brts_trainY_array,batch_size = batch_size,epochs = 1,verbose=1,shuffle=FALSE)
  
  brts_lstm_model_lag288_mv %>% reset_states()
  
}  

cat("starting training patsmodel")
for(i in 1:n_epochs_pats){
  pats_lstm_model_lag288_mv %>% fit(input_list_pats,pats_trainY_array,batch_size = batch_size,epochs = 1,verbose=1,shuffle=FALSE)
  
  pats_lstm_model_lag288_mv %>% reset_states()
  
}  

testX_sint_vector<-data$sint[289:576]
testX_sint_array <- array(data = testX_sint_vector,dim = c(length(testX_sint_vector),1,1))

testX_cost_vector<-data$cost[289:576]
testX_cost_array <- array(data = testX_cost_vector,dim = c(length(testX_cost_vector),1,1))

testX_temp_vector<-data$temp[289:576]
testX_temp_array <- array(data = testX_temp_vector,dim = c(length(testX_temp_vector),1,1))

testX_hum_vector<-data$hum[289:576]
testX_hum_array <- array(data = testX_hum_vector,dim = c(length(testX_hum_vector),1,1))

testX_dew_vector<-data$dew[289:576]
testX_dew_array <- array(data = testX_dew_vector,dim = c(length(testX_dew_vector),1,1))

testX_wspd_vector<-data$wspd[289:576]
testX_wspd_array <- array(data = testX_wspd_vector,dim = c(length(testX_wspd_vector),1,1))

testX_vis_vector<-data$vis[289:576]
testX_vis_array <- array(data = testX_vis_vector,dim = c(length(testX_vis_vector),1,1))

testX_pres_vector<-data$pres[289:576]
testX_pres_array <- array(data = testX_pres_vector,dim = c(length(testX_pres_vector),1,1))


testX_mon_vector<-data$mon[289:576]
testX_mon_array <- array(data = testX_mon_vector,dim = c(length(testX_mon_vector),1,1))

testX_tue_vector<-data$tue[289:576]
testX_tue_array <- array(data = testX_tue_vector,dim = c(length(testX_tue_vector),1,1))

testX_wed_vector<-data$wed[289:576]
testX_wed_array <- array(data = testX_wed_vector,dim = c(length(testX_wed_vector),1,1))

testX_thu_vector<-data$thu[289:576]
testX_thu_array <- array(data = testX_thu_vector,dim = c(length(testX_thu_vector),1,1))

testX_fri_vector<-data$fri[289:576]
testX_fri_array <- array(data = testX_fri_vector,dim = c(length(testX_fri_vector),1,1))

testX_sat_vector<-data$sat[289:576]
testX_sat_array <- array(data = testX_sat_vector,dim = c(length(testX_sat_vector),1,1))

testX_sun_vector<-data$sun[289:576]
testX_sun_array <- array(data = testX_sun_vector,dim = c(length(testX_sun_vector),1,1))

testX_brts_vector<-data$brts[1:288]
testX_brts_array <- array(data = testX_brts_vector,dim = c(length(testX_brts_vector),1,1))

testX_pats_vector<-data$pats[1:288]
testX_pats_array <- array(data = testX_pats_vector,dim = c(length(testX_pats_vector),1,1))

testX_fwts_vector<-data$fwts[1:288]
testX_fwts_array <- array(data = testX_fwts_vector,dim = c(length(testX_fwts_vector),1,1))

brts_testX_input_list <- list(
  testX_brts_array,testX_temp_array,testX_dew_array,testX_hum_array,testX_wspd_array,testX_vis_array,testX_pres_array,testX_mon_array,testX_tue_array,testX_wed_array,testX_thu_array,testX_fri_array,testX_sat_array,testX_sun_array,testX_sint_array,testX_cost_array
)
fwts_testX_input_list <- list(
  testX_fwts_array,testX_temp_array,testX_dew_array,testX_hum_array,testX_wspd_array,testX_vis_array,testX_pres_array,testX_mon_array,testX_tue_array,testX_wed_array,testX_thu_array,testX_fri_array,testX_sat_array,testX_sun_array,testX_sint_array,testX_cost_array
)
pats_testX_input_list <- list(
  testX_pats_array,testX_temp_array,testX_dew_array,testX_hum_array,testX_wspd_array,testX_vis_array,testX_pres_array,testX_mon_array,testX_tue_array,testX_wed_array,testX_thu_array,testX_fri_array,testX_sat_array,testX_sun_array,testX_sint_array,testX_cost_array
)

predictions_brts <-
  brts_lstm_model_lag288_mv %>% predict(brts_testX_input_list, batch_size = 72) %>% .[, 1]
pred_array_brts <-
  array(data = predictions_brts, dim = c(length(predictions_brts), 1, 1))

predictions_fwts <-
  fwts_lstm_model_lag288_mv %>% predict(fwts_testX_input_list, batch_size = 72) %>% .[, 1]
pred_array_fwts <-
  array(data = predictions_fwts, dim = c(length(predictions_fwts), 1, 1))



predictions_pats <-
  pats_lstm_model_lag288_mv %>% predict(pats_testX_input_list, batch_size = 72) %>% .[, 1]
pred_array_pats <-
  array(data = predictions_pats, dim = c(length(predictions_pats), 1, 1))

predictions_df <-
  data.frame(matrix(ncol = 4, nrow = 288))
columns = c("datetime",
            "brts_pred",
            "fwts_pred",
            "pats_pred")
colnames(predictions_df) <- columns

predictions_df$datetime <-
  seq(ymd_hm(paste(Sys.Date(), "00:00")), ymd_hm(paste(Sys.Date(), "23:55")), by =
        "5 min")
predictions_df$brts_pred <- predictions_brts
predictions_df$fwts_pred <- predictions_fwts
predictions_df$pats_pred <- predictions_pats


attribute_centre_brts <-
  as.numeric(
    read_lines(
      "C:/Users/developer/Documents/prediction/saved_attributes/attribute_centre_brts_stateful_lag288.txt"
    )
  )
attribute_scale_brts <-
  as.numeric(
    read_lines(
      "C:/Users/developer/Documents/prediction/saved_attributes/attribute_scale_brts_stateful_lag288.txt"
    )
  )

attribute_centre_fwts <-
  as.numeric(
    read_lines(
      "C:/Users/developer/Documents/prediction/saved_attributes/attribute_centre_fwts_stateful_lag288.txt"
    )
  )
attribute_scale_fwts <-
  as.numeric(
    read_lines(
      "C:/Users/developer/Documents/prediction/saved_attributes/attribute_scale_fwts_stateful_lag288.txt"
    )
  )

attribute_centre_pats <-
  as.numeric(
    read_lines(
      "C:/Users/developer/Documents/prediction/saved_attributes/attribute_centre_pats_stateful_lag288.txt"
    )
  )
attribute_scale_pats <-
  as.numeric(
    read_lines(
      "C:/Users/developer/Documents/prediction/saved_attributes/attribute_scale_pats_stateful_lag288.txt"
    )
  )

#Rescale Predictions using Saved Attributes for Each Feed
predictions_df$brts_pred <-
  (predictions_df$brts_pred * attribute_scale_brts + attribute_centre_brts) ^ 2

predictions_df$fwts_pred <-
  (predictions_df$fwts_pred * attribute_scale_fwts + attribute_centre_fwts) ^ 2

predictions_df$pats_pred <-
  (predictions_df$pats_pred * attribute_scale_pats + attribute_centre_pats) ^ 2


predictions_df$date<-as.Date(predictions_df$datetime)
predictions_df$time<-strftime(predictions_df$datetime,format="%H:%M:%S",tz="UTC")

##***************************Calculations*******************************#

#BRTS Predicted Daily Peak
ymax_brts_pred = max(predictions_df$brts_pred)
xcoord_brts_pred <-
  predictions_df$datetime[which.max(predictions_df$brts_pred)]


#FWTS Predicted Daily Peak
ymax_fwts_pred = max(predictions_df$fwts_pred)
xcoord_fwts_pred <-
  predictions_df$datetime[which.max(predictions_df$fwts_pred)]

#PATS Predicted Daily Peak
ymax_pats_pred = max(predictions_df$pats_pred)
xcoord_pats_pred <-
  predictions_df$datetime[which.max(predictions_df$pats_pred)]

#BRTS Predicted Daily Min
ymin_brts_pred = min(predictions_df$brts_pred)
xcoord_brts_min_pred <-
  predictions_df$datetime[which.min(predictions_df$brts_pred)]


#FWTS Predicted Daily Min
ymin_fwts_pred = min(predictions_df$fwts_pred)
xcoord_fwts_min_pred <-
  predictions_df$datetime[which.min(predictions_df$fwts_pred)]

# #PATS Predicted Daily Min
ymin_pats_pred = min(predictions_df$pats_pred)
xcoord_pats_min_pred <-
  predictions_df$datetime[which.min(predictions_df$pats_pred)]




pl <-
  plot_ly(
    mode = 'lines+markers'
  ) %>%
  add_trace(
    x =  ~ predictions_df$datetime,
    y =  ~ predictions_df$brts_pred,
    mode = 'lines',
    type = 'scatter',
    name = "BRTS P.C Prediction",
    line = list(color = "black")
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
    x =  ~ predictions_df$datetime,
    y =  ~ predictions_df$pats_pred,
    mode = 'lines',
    type = 'scatter',
    name = "PATS P.C Prediction",
    line = list(color = ("blue"))
  ) %>%
  add_trace(
    x =  ~ xcoord_brts_pred,
    y =  ~ ymax_brts_pred,
    mode = 'markers',
    type = 'scatter',
    name = paste("BRTS Predicted Daily Peak",xcoord_brts_pred),
    marker = list(color = ("red"),size=9,symbol="triangle-up")
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
    x =  ~ xcoord_pats_pred,
    y =  ~ ymax_pats_pred,
    mode = 'markers',
    type = 'scatter',
    name = paste("PATS Predicted Daily Peak",xcoord_pats_pred),
    marker = list(color = ("red"),size=9,symbol="triangle-up")
  ) %>%
  add_trace(
    x =  ~ xcoord_brts_min_pred,
    y =  ~ ymin_brts_pred,
    mode = 'markers',
    type = 'scatter',
    name = paste("BRTS Predicted Daily Min",xcoord_brts_min_pred),
    marker = list(color = ("orange"),size=9,symbol="triangle-down")
  ) %>%
  add_trace(
    x =  ~ xcoord_fwts_min_pred,
    y =  ~ ymin_fwts_pred,
    mode = 'markers',
    type = 'scatter',
    name = paste("FWTS Predicted Daily Min",xcoord_fwts_min_pred),
    marker = list(color = ("orange"),size=9,symbol="triangle-down")
  ) %>%
  add_trace(
    x =  ~ xcoord_pats_min_pred,
    y =  ~ ymin_pats_pred,
    mode = 'markers',
    type = 'scatter',
    name = paste("PATS Predicted Daily Min",xcoord_pats_min_pred),
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

graphfileName <-
  paste(
    format(
      Sys.time(),
      "C:/Users/developer/Documents/prediction/predictionGraphs/weatherModel_predictions/prediction_weatherModel_%Y-%m-%d_%H-%M"
    ),
    "html",
    sep = "."
  )
saveWidget(as_widget(pl), graphfileName, selfcontained = FALSE)





# #Save Newly Fit Models
save_model_hdf5(fwts_lstm_model_lag288_mv,"C:/Users/developer/Documents/prediction/saved_models/Stateful_Lag288_FWTS_MV16_MODEL",overwrite = TRUE,include_optimizer = TRUE)
save_model_hdf5(brts_lstm_model_lag288_mv,"C:/Users/developer/Documents/prediction/saved_models/Stateful_Lag288_BRTS_MV16_MODEL",overwrite = TRUE,include_optimizer = TRUE)
save_model_hdf5(pats_lstm_model_lag288_mv,"C:/Users/developer/Documents/prediction/saved_models/Stateful_Lag288_PATS_MV16_MODEL",overwrite = TRUE,include_optimizer = TRUE)



myConnection_pg <-
  odbcDriverConnect(
    "Driver={PostgreSQL ODBC Driver(UNICODE)};Server=localhost;Port=5432;Database=postgres;UID=postgres;PWD=PeakShave2018"
  )

odbcSetAutoCommit(myConnection_pg, autoCommit = TRUE)
#####################################################################################
modelName<-"sf_lstm_mv16"
feedId<-"fwts"
#Insert new Predictions into Prediction Table, Update previous predictions


for (k in 1:nrow(predictions_df)) {
  sqlInsert <-
    paste("INSERT INTO public.prediction (datetime,date,time,feed_id,power,model_id) VALUES (","'",predictions_df$datetime[k],"'",
          ",","'",predictions_df$date[k],"'",",","'",predictions_df$time[k],"'",",","'",feedId,"'",",",predictions_df$fwts_pred[k],",","'",modelName,"'",
          ") ON CONFLICT (datetime,model_id,feed_id) DO UPDATE SET power=",predictions_df$fwts_pred[k],";",sep = "")
  sprintf(sqlInsert)
  odbcQuery(myConnection_pg, sqlInsert)
  
}
odbcEndTran(myConnection_pg,commit = TRUE)


#####################################################################################
modelName<-"sf_lstm_mv16"
feedId<-"brts"
#Insert new Predictions into Prediction Table, Update previous predictions

for (i in 1:nrow(predictions_df)) {
  sqlInsert <-
    paste("INSERT INTO public.prediction (datetime,date,time,feed_id,power,model_id) VALUES (","'",predictions_df$datetime[i],"'",
          ",","'",predictions_df$date[i],"'",",","'",predictions_df$time[i],"'",",","'",feedId,"'",",",predictions_df$brts_pred[i],",","'",modelName,"'",
          ") ON CONFLICT (datetime,model_id,feed_id) DO UPDATE SET power=",predictions_df$brts_pred[i],";",sep = "")
  sprintf(sqlInsert)
  odbcQuery(myConnection_pg, sqlInsert)
  
}
odbcEndTran(myConnection_pg,commit = TRUE)

#####################################################################################
modelName<-"sf_lstm_mv16"
feedId<-"pats"
#Insert new Predictions into Prediction Table, Update previous predictions

for (i in 1:nrow(predictions_df)) {
  sqlInsert <-
    paste("INSERT INTO public.prediction (datetime,date,time,feed_id,power,model_id) VALUES (","'",predictions_df$datetime[i],"'",
          ",","'",predictions_df$date[i],"'",",","'",predictions_df$time[i],"'",",","'",feedId,"'",",",predictions_df$pats_pred[i],",","'",modelName,"'",
          ") ON CONFLICT (datetime,model_id,feed_id) DO UPDATE SET power=",predictions_df$pats_pred[i],";",sep = "")
  sprintf(sqlInsert)
  odbcQuery(myConnection_pg, sqlInsert)
  
}
odbcEndTran(myConnection_pg,commit = TRUE)


#####################################################################################

nowDatetime<-Sys.time()
feed_id_fwts<-"fwts"
feed_id_pats<-"pats"
feed_id_brts<-"brts"
nowDate<-Sys.Date()

fwts_peak_time<-strftime(xcoord_fwts_pred,format="%H:%M",tz="UTC")
fwts_peak_time_obj<-as.POSIXlt(xcoord_fwts_pred)
fwts_start_discharge_time<-fwts_peak_time_obj-1380
fwts_start_discharge_time<-strftime(fwts_start_discharge_time,format="%H:%M",tz="UTC")

pats_peak_time<-strftime(xcoord_pats_pred,format="%H:%M",tz="UTC")
pats_peak_time_obj<-as.POSIXlt(xcoord_pats_pred)
pats_start_discharge_time<-pats_peak_time_obj-1380
pats_start_discharge_time<-strftime(pats_start_discharge_time,format="%H:%M",tz="UTC")

brts_peak_time<-strftime(xcoord_brts_pred,format="%H:%M",tz="UTC")
brts_peak_time_obj<-as.POSIXlt(xcoord_brts_pred)
brts_start_discharge_time<-brts_peak_time_obj-1380
brts_start_discharge_time<-strftime(brts_start_discharge_time,format="%H:%M",tz="UTC")


#Insert Predicted FWTS Peak into BatteryEvent Table
myConnection_pg <-
  odbcDriverConnect(
    "Driver={PostgreSQL ODBC Driver(UNICODE)};Server=localhost;Port=5432;Database=postgres;UID=postgres;PWD=PeakShave2018"
  )

odbcSetAutoCommit(myConnection_pg, autoCommit = TRUE)

sqlInsert_fwts_batteryEvent <-
  paste("INSERT INTO public.battery_event (created_at,discharge_date,peak_time,start_discharge_time,feed_id,updated_at) VALUES (","'",nowDatetime,"'",",","'",nowDate,"'",",","'",fwts_peak_time,"'",",","'",fwts_start_discharge_time,"'",",","'",feed_id_fwts,"'",",","'",nowDatetime,"');",sep = "")
sprintf(sqlInsert_fwts_batteryEvent)

odbcQuery(myConnection_pg, sqlInsert_fwts_batteryEvent)

nowDatetime<-Sys.time()
feed_id_fwts<-"fwts"


#Insert Predicted PATS Peak into BatteryEvent Table
sqlInsert_pats_batteryEvent <-
  paste("INSERT INTO public.battery_event (created_at,discharge_date,peak_time,start_discharge_time,feed_id,updated_at) VALUES (","'",nowDatetime,"'",",","'",nowDate,"'",",","'",pats_peak_time,"'",",","'",pats_start_discharge_time,"'",",","'",feed_id_pats,"'",",","'",nowDatetime,"');",sep = "")
sprintf(sqlInsert_pats_batteryEvent)

odbcQuery(myConnection_pg, sqlInsert_pats_batteryEvent)


#Insert Predicted BRTS Peak into BatteryEvent Table
sqlInsert_brts_batteryEvent <-
  paste("INSERT INTO public.battery_event (created_at,discharge_date,peak_time,start_discharge_time,feed_id,updated_at) VALUES (","'",nowDatetime,"'",",","'",nowDate,"'",",","'",brts_peak_time,"'",",","'",brts_start_discharge_time,"'",",","'",feed_id_brts,"'",",","'",nowDatetime,"');",sep = "")
sprintf(sqlInsert_brts_batteryEvent)

odbcQuery(myConnection_pg, sqlInsert_brts_batteryEvent)

odbcCloseAll()

}
generatePredictions_mv()

