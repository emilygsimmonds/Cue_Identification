#### Function to predict for uniform 1ºC increase in temperature

library(tidyr) # need this to use spread() function

# runs the PSR model twice, with a single unit difference in X to represent the slope change
PSR_effect_size <- function(climate_data, bio_data, tot_days, model, pred_years = NULL){
  selectrows <- bio_data
  bio_data$Juliandays <- round(bio_data$lay_mean)
  # need climate data to be arranged with year as row
  # need to reduce climate dataframe to only year, yday and temp
  climate2 <- data.frame(year = climate_data$year, yday = climate_data$yday, temp = climate_data$temp)
  tempmat <- climate2 %>% spread(, key = yday, value = temp)
  tempmat <- tempmat[,-1]
  ny<-length(pred_years)
  nt<-tot_days
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
  output <- predict(model, newdata = new_data, type="response", se.fit=T, interval = "confidence")
  return(output)
}