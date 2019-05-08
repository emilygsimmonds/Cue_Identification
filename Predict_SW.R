#### Function to predict from sliding window analysis parameters 
# Can be used for both K-fold cross validation and general prediction

# This function runs a linear model for each set of data and parameters
# which were outputs from Params_SW.R

# It then predicts new lay dates based on the observed temperature in test years

# Lay_dates = lay date observed data
# window_climate = observed climate in the focal window (produced by Params_SW.R (data=T))
# climate_data = all climate data
# parameters = the window open/window closed identified in Run_SW.R
# years = years to be predicted
# refday = c(day, mon)

get_preds_SW <- function(Lay_dates, window_climate, climate_data, parameters, years, refday){
  # first subset climate data to only years for predicting 
  # and to the days of the window
  parameters <- as.numeric(parameters)
  climate3 <- rep(NA, length(years))
  for(i in 1:length(years)){
    marker <- which(climate_data$month == 5 & climate_data$day == 20 & climate_data$year == years[i])
    climate2 <- climate_data$temp[(marker-(140-parameters[1])):(marker-(140-parameters[2]))]
    if(parameters[3] == 1){climate3[i] <- mean(climate2)}
    if(parameters[3] == 2){climate3[i] <- min(climate2)}
    if(parameters[3] == 3){climate3[i] <- max(climate2)}
    if(parameters[3] == 4){climate3[i] <- summary(lm(climate2~seq(length(climate2),1,-1)))$coef[2]}
  }
  # run model, from which you can then predict
  model <- lm(Lay_dates ~ window_climate)
  # generate predictions with se.fit and prediction interval
  predictions <- predict(model, data.frame(window_climate = as.numeric(climate3)), 
                         interval = "predict", type = "response", se.fit = T)
  # will produce dataframe with 3 columns
  return(predictions$fit)
}
