#### Functions to run the CSP model

# Function takes temperature, one day at a time beginning at reference day and going back in time
# then regress each day against the event and save result.

# Wrapper function to do this for each dataset and return result
model_CSP <- function(bio_data, climate2){
  model <- lm(bio_data$CSP ~ climate2)
  output <- c(est = coef(model)[2],
              r2 = summary(model)$r.squared)
  return(output)
}

# Function to run the CSP model
# input arguments = climate data (a single day of data for various years), 
# bio_data,
# refyears = total num years in study
run_CSP <- function(climate_data, bio_data, refyears, type = c("DOY95", "lay_mean")){
  if(type == "DOY95"){bio_data2 <- format_CSP_bio(bio_data)}
  if(type == "lay_mean"){bio_data$CSP <- bio_data$lay_mean
  bio_data2 <- bio_data}
  climate2 <- climate_CSP[which(refyears == min(unique(bio_data2$Year))):which(refyears == max(unique(bio_data2$Year))),]
  # want function to run model for a day and generate result
  output2 <- t(apply(climate2, 2, model_CSP, bio_data=bio_data2))
  return(as.data.frame(output2))
}
