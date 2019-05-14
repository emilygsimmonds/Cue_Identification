#### Function to format climate data for GDD analysis

# need a column of Julian days
# specify whether for prediction or not
format_GDD_clim <- function(climate_data, years, pred = FALSE){
  # choose how to subset the climate data dependent on whether it is for prediction or not
  ifelse(pred == T, climate2 <- filter(climate_data, year > min(years)-1), 
         climate2 <- filter(climate_data, year < max(years)+1))
  # now create the matrix of temperature data
  # each column is a day, each row is a year
  tempmat <- matrix(nrow=length(years), ncol=365)
  for(i in 1:length(years)){
    temp_climate <- subset(climate2, climate2$year == years[i])
    tempmat[i,] <- temp_climate$temp[1:365]
  } # remove the extra day if leap year
  # take off 31st December so shouldn't affect model
  rownames(tempmat) <- years
  colnames(tempmat) <- seq(1, 365, 1)
  # return the new temperature matrix
  return(tempmat)
}
