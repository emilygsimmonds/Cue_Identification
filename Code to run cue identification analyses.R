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

bio_data <- read.csv("bio_data_example.csv", header = T)
climate_data <- read.csv("climate_data_example.csv", header = T)

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

#### Code to run GDD model ####

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


