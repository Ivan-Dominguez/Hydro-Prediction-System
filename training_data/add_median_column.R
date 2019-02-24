#correct real peak column
results_fileT2days$real_peak <-results_fileT6days$real_peak
results_fileT3days$real_peak <-results_fileT6days$real_peak
results_fileT4days$real_peak <-results_fileT6days$real_peak
results_fileT5days$real_peak <-results_fileT6days$real_peak
results_fileT6days$real_peak <-results_fileT6days$real_peak
results_fileT7days$real_peak<-results_fileT6days$real_peak
results_fileT1year$real_peak <-results_fileT6days$real_peak


#median time
<-MEDIAN_pred <- strptime(prediction_date, "%Y-%m-%d")-mean(difftime(
  paste(prediction_date, "00:00:00", sep=" "),
  c(RFcoord_fwts_pred,DLcoord_pred,XGcoord_pred,CUcoord_pred),
  units = "secs"
))
