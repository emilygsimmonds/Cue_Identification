#### Function to extract parameters from the CSP results

# run gam on the resulting data and extract parameters
get_params_CSP <- function(slope, day, r_s, bio_data, climate_data, var=c("temp", "rain"), 
                           type = c("plot", "params", "model"), 
                           date_var = c("DOY95", "lay_mean"), data_names){
  temporary <- data.frame(slope = unlist(slope), day = day, r_s = unlist(r_s))
  slope_gam <- gam(slope ~ s(day), data=temporary)
  rs_gam <- gam(r_s ~ s(day), data=temporary)
  
  # create predictions from those gams
  pred_S<-predict(slope_gam,se.fit=TRUE,type="response")
  pred_R<-predict(rs_gam,se.fit=TRUE,type="response")
  
  # To extract the key climate window of sensitivity from the GAMS
  # we used code from Thackeray et al. 2016, which can be found at:
  #https://github.com/NERC-CEH/Phenology_Climate/blob/master/Source_Code/Functions.R
  # Lines 419 to 428
  
  days <- find_concurrent_period(c(temp_window_l, temp_window_u), pred_S)
  
  # This produces a vector of the days that are included in the critical window
  # days are in fact days before 140 so window open = days[1]
  actual_days <- 140-days # dates in day of year
  # extract mean temperature for this window for all years
  climate_window <- climate_extract(actual_days, bio_data, climate_data, var = var)
  # now run a linear model of lay date as a response to cue
  if(date_var == "DOY95"){bio_data <- format_CSP_bio(bio_data)}
  if(date_var == "lay_mean"){bio_data$CSP <- bio_data$lay_mean}
  model <- lm(bio_data$CSP ~ climate_window)
  # store parameters in output
  output_CSP <- c(actual_days[length(actual_days)], actual_days[1],
                  summary(model)$coef[1], summary(model)$coef[2], summary(model)$coef[4],
                  summary(model)$adj.r.squared)
  names(output_CSP) <- c("WindowOpen", "WindowClose", "Int", "Est", "SE", "R2")
  # return parameters
  if(type == "params"){return(output_CSP)}
  # return lm and data
  if(type == "model"){return(list(model, climate_window, bio_data$CSP))}
  # create a plot of the CSP
  if(type == "plot"){
    plot(slope_gam, ylab = "Coefficient", xlab = "Day prior to 20th May", rug=F, las=1)
    rect(min(days), -10, max(days), 10, 
         col= rgb(0.5,0.5,0.5,1/4), border = NA)
    title(paste(data_names, " Coef", sep = ""), cex.main=2)
    plot(rs_gam, ylab = "Adj. R Squared", xlab = "Day prior to 20th May", rug=F, las=1)
    rect(min(days), -10, max(days), 10, 
         col= rgb(0.5,0.5,0.5,1/4), border = NA)
    title(paste(data_names, " R squared", sep = ""), cex.main=2)
  }
}