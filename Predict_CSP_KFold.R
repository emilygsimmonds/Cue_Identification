#### Function to predict from CSP parameters altered function for K-fold cross validation

get_preds_CSP_K <- function(actual_days, bio_data, climate_data, var="temp", 
                            tot_years, pred_years,
                            date_var = c("DOY95", "lay_mean")){
  # actual days will be entered as length = 2 vector of start and end
  # expand to full range
  actual_days <- as.numeric(actual_days)
  actual_days <- seq(actual_days[1], actual_days[2], 1)
  climate_window <- climate_extract(actual_days, bio_data, climate_data, var = var, 
                                    pred = T, tot_years = tot_years)
  # will find mean enviro value for window for all years
  marker <- which(tot_years == min(pred_years))
  marker1 <- which(tot_years == max(pred_years))
  old_climate <- climate_window[-(marker:marker1)]
  if(date_var == "DOY95"){bio_data <- format_CSP_bio(bio_data)}
  if(date_var == "lay_mean"){bio_data$CSP <- bio_data$lay_mean}
  lay_date <- bio_data$CSP
  model <- lm(lay_date ~ old_climate)
  # model just for years with data
  marker2 <- which(tot_years == pred_years)
  new_climate <- climate_window[marker2]
  predictions <- predict(model, newdata = data.frame(old_climate = new_climate), 
                         interval = "predict", type = "response", se.fit = T)
  return(as.matrix(predictions$fit))
}