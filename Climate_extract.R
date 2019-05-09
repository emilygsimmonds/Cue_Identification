#### Function to take mean of the temperature in the key window identified

climate_extract <- function(actual_days, bio_data, climate_data, var = "temp", 
                            pred = FALSE, tot_years = NULL){
  climate_var <- climate_data$temp
  # set up climate variable
  years <- unique(bio_data$Year)
  if(pred == TRUE){years <- tot_years}
  # want the mean temperature across window for each year
  # subset climate to correct days but goes across year boundary so can't use apply
  climate2 <- rep(NA, length(years))
  for(i in 1:length(years)){
    actual_days <- sort(actual_days)
    marker <- which(actual_days<0)
    ifelse(length(marker) == 0, 
           climate2[i] <- mean(climate_var[which(climate_data$yday==actual_days[1]
                                                 &climate_data$year==years[i]):which(climate_data$yday==actual_days[length(actual_days)]
                                                                                     &climate_data$year==years[i])]), 
           ifelse(length(marker) == length(actual_days), 
                  climate2[i] <- mean(climate_var[which(climate_data$yday==(actual_days[1]+366)
                                                        &climate_data$year==years[i]-1):which(climate_data$yday==(actual_days[length(actual_days)]+366)
                                                                                              &climate_data$year==years[i]-1)]),
                  climate2[i] <- mean(climate_var[which(climate_data$yday==(actual_days[1]+366)
                                                        &climate_data$year==years[i]-1):which(climate_data$yday==actual_days[length(actual_days)]&climate_data$year==years[i])])))
  }
  return(climate2)
}
