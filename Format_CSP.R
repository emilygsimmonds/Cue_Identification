#### Function to format data for CSP model

# re-formats bio data for CSP analysis
format_CSP_bio <- function(bio_data){
  # Convert doy95 column to a date
  bio_data$CSP <- rep(NA, length(bio_data[,1]))
  bio_data$doy95_date <- as.Date(bio_data$doy95, format="Y%-m%-d%", origin = paste(1990, 1, 1, sep="-"))
  # needs to be a loop because of leap years
  leap_years <- seq(1960,2015,4)
  for(i in 1:length(bio_data[,1])){
    marker <- which(leap_years == bio_data$Year[i]) # is this year a leap year?
    ifelse(length(marker)>0, bio_data$CSP[i] <- bio_data$doy95[i]+89, bio_data$CSP[i] <- bio_data$doy95[i]+90)
    bio_data$doy95_date[i] <- as.Date(bio_data$CSP[i], origin = paste(bio_data$Year[i], 1, 1, sep="-"))
  }
  return(bio_data) 
}

# re-formats the climate data
format_CSP_clim <- function(climate_data, var = "temp", refday){
  climate_var <- climate_data$temp
  # set up climate data
  years <- unique(climate_data$year)[-1]
  climate_var_mat <- matrix(nrow=length(years), ncol=365)
  # easiest as a loop due to leap years and going back from reference day
  for(j in 1:length(years)){
    # check if leap year if so use refday = refday+1 otherwise just refday
    ifelse(length(which(climate_data$year == years[j])) == 366,date <- which(climate_data$yday == (refday+1) & climate_data$year == years[j]),
           date <- which(climate_data$yday == (refday) & climate_data$year == years[j]))
    # take climate data from ref day to 365 days prior and save in reverse order
    climate_var_mat[j,] <- rev(climate_var[(date-364):date])
    # produces file with row = years and columns = dats prior to ref
  }
  return(climate_var_mat)
}