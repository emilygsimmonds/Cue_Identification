#### Functions to run the CSP model for K-fold cross validation

# extracts years in a different way to Run_CSP

# input arguments = climate data (a single day of data for various years), bio_data,
# refyears = total num years in study

run_CSP_K <- function(climate_data, bio_data, refyears, type = c("DOY95", "lay_mean"), pred_years){
  if(type == "DOY95"){bio_data2 <- format_CSP_bio(bio_data)}
  if(type == "lay_mean"){ bio_data$CSP <- bio_data$lay_mean
  bio_data2 <- bio_data}
  climate2 <- climate_CSP[-(which(refyears == min(pred_years)):which(refyears == max(pred_years))),]
  # want function to run model for a day and generate result
  output2 <- t(apply(climate2, 2, model_CSP, bio_data=bio_data2))
  return(as.data.frame(output2))
}