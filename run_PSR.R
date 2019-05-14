#### Function to run the PSR model for K-fold cross validation and any other data
#### Also predicts, extracts parameters, and the model object

library(tidyr) # need this to use spread() function

# type dictates what the output of the function should be
# tot_days = the number of days of temperature to consider
# pred_years = years on which to predict data if predicting
# window = window of most influential days, output from params but needed as input to plot
run_PSR <- function(climate_data, bio_data, tot_days, type = c("params", "plot", "pred"), 
                    pred_years = NULL, window = NULL, data_names){
  selectrows <- bio_data
  bio_data$Juliandays <- round(bio_data$lay_mean)
  # need climate data to be arranged with year as row
  # need to reduce climate dataframe to only year, yday and temp
  climate2 <- data.frame(year = climate_data$year, yday = climate_data$yday, temp = climate_data$temp)
  tempmat <- climate2 %>% spread(, key = yday, value = temp)
  tempmat <- tempmat[,-1]
  ny<-length(bio_data[,1])
  #number of years
  nt<-tot_days
  #nt is the number of days on which you want to consider temperature
  
  ## Formatting data
  # create an index matrix to reference to covariate matrix
  # row = years, entries = day prior to reference
  index.matrix=matrix(1:nt,ny,nt,byrow=TRUE)
  # the days on which you want to consider temperature 
  covariate.matrix=matrix(nrow=ny,ncol=nt)
  # populate this with the daily temperature data for each year
  # start at day prior and go along to 365 days prior and have column of phenology too
  # tempmat inc leap years - need to account for that
  # need to fill backwards so create reference sequence as matrix to inc leap years
  leap_years <- seq(1960,2015,4)
  ref <- matrix(NA, nrow=ny, ncol=365)
  for(i in 1:ny){
    starter <- 140
    ender <- c(tot_days, 141)
    if(any(leap_years == bio_data$Year[i])){starter <- 141
    ender <- c(365,142)} # if current year is leap year need to start one yday later and finish one yday earlier
    if(any(leap_years == bio_data$Year[i]-1)){ender <- c(366,142)}
    # if previous year is leap year need to start at 366 in prev year and finish one yday earlier
    ref[i,] <- c(seq(starter,1,-1), seq(ender[1],ender[2],-1))
  }
  Year <- seq(1960,2015,1)
  for(k in 1:(ny)){
    for(j in 1:nt){
      # need to account for leap years here too
      if(j<ref[k,1]+1){covariate.matrix[k,j] <- tempmat[which(Year == bio_data$Year[k]),ref[k,j]]}
      if(j>ref[k,1]){covariate.matrix[k,j] <- tempmat[which(Year == (bio_data$Year[k]-1)),ref[k,j]]}
    }
    
  }
  # now have data formatted, can run the model
  # k = 10 if length data > 20, 8 if less 
  ifelse(length(bio_data[,1]) < 21, K <- 8, K <- 10)
  if(length(bio_data[,1]) < 11){K <- 6}
  # m = cubic and first differences
  # k = knots
  Year <- bio_data$Year
  model<-gam(Juliandays~s(index.matrix,k=K,m=c(2,1),bs="ps",by=covariate.matrix), 
             data = bio_data, method="GCV.Cp")
  # want to extract either parameters or prediction
  # PLOT
  if(type == "plot"){
    plot(model, ylab="Partial coefficients", xlab="Days prior to 20th May",
         rug = F, las = 1)
    rect(min(window), -10, max(window), 10, 
         col= rgb(0.5,0.5,0.5,1/4), border = NA)
    title(paste(data_names, sep = ""), cex.main=2)
  }
  # EXTRACT PARAMETERS
  if(type == "params"){
    # find the most extreme coefficient values (>±1.96*se)
    plotted <- plot(model)
    coefs <- data.frame(fit = plotted[[1]]$fit)
    marker <- c(which(coefs$fit > round((1.96*sd(coefs$fit))+mean(coefs$fit),2)), 
                which(coefs$fit < round((1.96*sd(coefs$fit))-mean(coefs$fit)),2))
    window <- 141-plotted[[1]]$x[find_concurrent_period(marker, coefs)]
    output <- list(window, unlist(c(summary(model)[1], summary(model)[10], summary(model)[15])))
    return(output)
  }
  # EXTRACT THE MODEL
  if(type == "model"){
    plotted <- plot(model)
    coefs <- data.frame(fit = plotted[[1]]$fit)
    marker <- c(which(coefs$fit > round((1.96*sd(coefs$fit))+mean(coefs$fit),2)), 
                which(coefs$fit < round((1.96*sd(coefs$fit))-mean(coefs$fit)),2))
    window <- 141-plotted[[1]]$x[find_concurrent_period(marker, coefs)]
    temp_window <- climate_extract(round(window), bio_data, climate_data, var = "temp", 
                                   pred = T, tot_years = bio_data$Year)
    return(list(model, temp_window))}
  # CREATE PREDICTIONS
  if(type == "pred"){
    # predict new values
    plotted <- plot(model)
    coefs <- data.frame(fit = plotted[[1]]$fit)
    ny<-length(pred_years)
    index.matrix.new=matrix(1:nt,ny,nt,byrow=TRUE)
    covariate.matrix.new=matrix(nrow=ny,ncol=nt)
    ref <- matrix(NA, nrow=ny, ncol=365)
    for(i in 1:ny){
      starter <- 140
      ender <- c(tot_days, 141)
      if(any(leap_years == pred_years[i])){starter <- 141
      ender <- c(365,142)} # if current year is leap year need to start one yday later and finish one yday earlier
      if(any(leap_years == pred_years[i]-1)){ender <- c(366,142)}
      # if previous year is leap year need to start at 366 in prev year and finish one yday earlier
      ref[i,] <- c(seq(starter,1,-1), seq(ender[1],ender[2],-1))
    }
    Year <- seq(1960,2015,1)
    for(k in 1:(ny)){
      for(j in 1:nt){
        # need to account for leap years here too
        if(j<ref[k,1]+1){covariate.matrix.new[k,j] <- tempmat[which(Year == pred_years[k]),ref[k,j]]}
        if(j>ref[k,1]){covariate.matrix.new[k,j] <- tempmat[which(Year == (pred_years[k]-1)),ref[k,j]]}
      }
    }
    new_data <- list(index.matrix= index.matrix.new, covariate.matrix = covariate.matrix.new, Year = pred_years)
    output <- predict(model, newdata = new_data, type="response", se.fit=T, interval = "predict")
    return(output)
  }
}