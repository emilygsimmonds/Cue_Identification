#### Code to run all analyses

# all analyses run on the whole dataset AND
# for K-fold cross validation

#### Data, packages, and functions ####
# This section loads all of the packages, functions, and data needed

## Packages
library(climwin)
library(mgcv)
library(GenSA)
library(boot)

## Data - import two datasets (biological and climate)
# then split to ones needed

bio_data <- read.csv("bio_data.csv", header = T)
climate_data <- read.csv("climate_data.csv", header = T)

# make a vector of all lay dates for last 5 years of data
Prediction_lay_dates <- c(bio_data$lay_mean[51:55])

# subset to include first 50 years of data
data_all <- subset(bio_data, Year < 2011)

# K-fold cross validation data creation as a list
list_K_fold <- list(data_all) # 1st training dataset 1961-2010
list_K_pred <- list(2011:2015) # begin with most recent 5 years as test data

# loop through to add new training and test data until 11 created
for(i in seq(6,55,5)){marker <- seq(which(bio_data$Year == 2015-(i-1)), 
                                    which(bio_data$Year == 2015-(i+3)), length.out = 5)
list_K_fold[[((i+4)/5)]] <- bio_data[-marker,]
list_K_pred[[((i+4)/5)]] <- seq(2015-(i+3), 2015-(i-1), 1)}

# save a vector of names for the different k-fold subsets
data_names_k <- as.character(1:11)

#### Code to run SWA model ####

## Run the model and get results 
source('Run_SW.R')

# Run SWA for complete dataset
Results_SWA <- run_SW(data_all, absolute = T, climate = climate_data, refday = c(20,5))

# pulls out everything into a big list can then extract from that

## Get data and parameters for prediction
source('Params_SW.R')

# extract parameters for complete dataset
Parameters_SWA <- get_params_SW(Results_SWA, data_all$lay_mean, "complete", type = "Params")
# SAVE
write.csv(Parameters_SWA, "Parameters_SWA.csv", row.names=T)

# extract climate data for complete dataset 'best' window
Results_data_SWA <- get_params_SW(Results_SWA, data_all$lay_mean, "complete", type = "Data")
# SAVE
write.csv(Results_data_SWA, "Results_data_SWA.csv")

## Predict for complete dataset (predicts for 2011-2015)
source('Predict_SW.R')

# predict
Predictions_SWA <- get_preds_SW(window_climate = Results_data_SWA[,-1],
                          parameters = Parameters_SWA[1:3],
                          Lay_dates = Results_data_SWA$Lay_dates,
                          climate_data = climate_data,
                          years = seq(2011,2015,1), 
                          refday = 141)
# SAVE
save(Predictions_SWA, file = "Predictions_SWA.RData")

#### K-fold cross validation - SWA ####

## Run SWA for k-fold cross validation
source('Run_SW.R')

# run for the list of datasets
Results_SWA_K <- mapply(run_SW, list_K_fold, 
                        MoreArgs = list(absolute = T, climate = climate_data, 
                                        refday = c(20,5)))
# SAVE
save(Results_SWA_K, file="Results_SWA_K.RData")

## Get data and parameter estimates for prediction
source('Params_SW_KFold.R')

# run for the list of outputs from run_SW()
# pulls out everything into a big list can then extract from that

# data
Results_data_SWA_K <- get_params_SW_K(Results_SWA_K, bio_data$lay_mean, data_names_k, type = "Data")
# SAVE
write.csv(Results_data_SWA_K, "Results_data_SWA_K.csv", row.names=F)

# parameters
Parameters_SWA_K <- get_params_SW_K(Results_SWA_K, data_all$lay_mean, data_names_k, type = "Params")
# SAVE
write.csv(Parameters_SWA_K, "Parameters_SWA_K.csv", row.names=T)

## Predict for k-fold cross validation 

# extract the parameters into a list
list_params_SWA_K <- split(Parameters_SWA_K[,1:3], seq(nrow(Parameters_SWA_K)))
# parse into the prediction function (removing the first column of results data as these are lay dates)
# here we need temperature
Predictions_SWA_K <- mapply(get_preds_SW, window_climate = as.list(Results_data_SWA_K[,-1]),
                            parameters = list_params_SWA_K, years = list_K_pred,
                            MoreArgs = list(Lay_dates = Results_data_SWA_K$Lay_dates,
                                            climate_data = climate_data,
                                            refday = 141), SIMPLIFY = F)
# SAVE
save(Predictions_SWA_K, file = "Predictions_SWA_K.RData")



#### Code to run SWR model ####

## Run the model and get results 
source('Run_SW.R')

# Run SWA for complete dataset
Results_SWR <- run_SW(data_all, absolute = F, climate = climate_data)

# pulls out everything into a big list can then extract from that

## Get data and parameters for prediction
source('Params_SW.R')

# extract parameters for complete dataset
Parameters_SWR <- get_params_SW(Results_SWR, data_all$lay_mean, "complete", type = "Params")
# SAVE
write.csv(Parameters_SWR, "Parameters_SWR.csv", row.names=T)

# extract climate data for complete dataset 'best' window
Results_data_SWR <- get_params_SW(Results_SWR, data_all$lay_mean, "complete", type = "Data")
# SAVE
write.csv(Results_data_SWR, "Results_data_SWR.csv")

#### Code to run CSP model ####

## Run models and get results
source('Format_CSP.R')
source('Run_CSP.R')

# start by formatting the climate data
climate_CSP <- format_CSP_clim(climate_data, var = "temp", refday = 141)
row.names(climate_CSP) <- seq(1961,2015,1) # name rows as years of study

# run the model for complete dataset
Results_CSP <- run_CSP(bio_data = data_all, climate_data = as.data.frame(climate_CSP), 
                       refyears = seq(1961,2015,1),
                       type = "lay_mean")

# all odd = estimates, all even = r squared
# extract each
list_slope <- as.list(Results_CSP[1])
list_rs <- as.list(Results_CSP[2])

## Get data and parameters for prediction
source('Params_CSP.R')
source('Climate_extract.R')

# extract parameters for complete dataset
Parameters_CSP <- t(get_params_CSP(slope = list_slope, r_s = list_rs,
                           bio_data = data_all, climate_data = climate_data, 
                           var = "temp", type = "params", day = seq(1,365,1), 
                           date_var = "lay_mean"))
# SAVE
write.csv(Parameters_CSP, "Parameters_CSP.csv")

## Predict for complete dataset (predicts for 2011-2015)
source('Predict_CSP.R')

# predict
Predictions_CSP <- get_preds_CSP(actual_days = Parameters_CSP[1:2], bio_data = data_all, 
                          climate_data = climate_data, var = "temp", tot_years = seq(1961,2015,1),
                          pred_years = 2011:2015, date_var="lay_mean")
#SAVE
save(Predictions_CSP, file="Predictions_CSP.RData")

#### K-fold cross validation - CSP ####

## Run CSP for k-fold cross validation

# all Year columns in list are factor - need to correct this to numeric
for(i in 1:11){list_K_fold[[i]]$Year <- as.numeric(as.character(list_K_fold[[i]]$Year))}

source('Run_CSP_KFold.R')

# run for the list of datasets
Results_CSP_K <- mapply(run_CSP_K, bio_data = list_K_fold, pred_years = list_K_pred, 
                        MoreArgs = list(climate_data = as.data.frame(climate_CSP), 
                                        refyears = seq(1961,2015,1),
                                        type = "lay_mean"))
# SAVE
save(Results_CSP_K, file="Results_CSP_K.RData")

## Get data and parameter estimates for prediction
source('Params_CSP.R')

# all odd entries are slope estimates and even = r squared
list_slope_K <- as.list(Results_CSP_K[seq(1,22,2)])
list_rs_K <- as.list(Results_CSP_K[seq(2,22,2)])

# extract parameters
Parameters_CSP_K <- t(mapply(get_params_CSP, slope = list_slope_K, r_s = list_rs_K,
                             bio_data = list_K_fold, MoreArgs = list(climate_data = climate_data, 
                                                                     var = "temp", type = "params", 
                                                                     day = seq(1,365,1), 
                                                                     date_var = "lay_mean")))


## Predict for k-fold cross validation 
source('Predict_CSP_KFold.R')

list_actual_days_K <- split(Parameters_CSP_K[,1:2], seq(nrow(Parameters_CSP_K)))
Predictions_CSP_K <- mapply(get_preds_CSP_K, actual_days = list_actual_days_K, bio_data = list_K_fold, 
                            pred_years = list_K_pred,
                            MoreArgs = list(climate_data = climate_data, var = "temp", 
                                            tot_years = seq(1961,2015,1),
                                            date_var="lay_mean"), SIMPLIFY = F)
# SAVE
save(Predictions_CSP_K, file = "Predictions_CSP_K.RData")

#### Code to run PSR model ####

#### K-fold cross validation - PSR ####

#### Code to run GDD model ####

#### K-fold cross validation - GDD ####