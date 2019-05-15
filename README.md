---
title: "README"
output: html_document
---

## Code for manuscript: Cue Identification in Phenology: a case study of the predictive performance of current statistical tools. DOI: 10.5281/zenodo.2838825

This repository contains:

### Scripts relating to all methods:

[bio_data.csv](https://github.com/emilygsimmonds/Cue_Identification/blob/master/bio_data.csv) : biological data used in the analyses. Rows are years of study. Columns are; Year (year of study), lay_date (annual population mean lay date in YYYY-MM-DD format), lay_mean (annual population mean lay date as day of year), doy95 (day of the year by which 95% of the population have laid).

[climate_data.csv](https://github.com/emilygsimmonds/Cue_Identification/blob/master/climate_data.csv) : climate data used in the analyses. Originally sourced from https://www.metoffice.gov.uk/climate/uk/data/ukcp09/datasets. Rows are days. Columns are; date, year, yday (day of year), day (day of month), month, temp (daily mean temperature in degrees celsius). 

[Code to run cue identification analyses.R](https://github.com/emilygsimmonds/Cue_Identification/blob/master/Code%20to%20run%20cue%20identification%20analyses.R) : script to run all of the analyses for this paper. This script includes code to run both K-fold cross validation and analyses on a whole dataset. This script runs the SWA, SWR, CSP, PSR, and GDD models, extracts parameters, extracts climate window data, and predicts lay dates. Outputs from each step are saved out for future use. 

### Scripts to run sliding window methods:

[Run_SW.R](https://github.com/emilygsimmonds/Cue_Identification/blob/master/Run_SW.R) : wrapper function to run sliding window analyses through climwin package and extract appropriate results.

[Params_SW.R](https://github.com/emilygsimmonds/Cue_Identification/blob/master/Params_SW.R) : function to extract model parameters or data for the optimal climate window from the output of Run_SW.R.

[Params_SW_KFold.R](https://github.com/emilygsimmonds/Cue_Identification/blob/master/Params_SW_KFold.R) : the same as Params_SW.R but editted for K-fold cross validation. When extracting the data for the optimal climate window, test years for cross validation need to be removed. This function includes that functionality.

[Predict_SW.R](https://github.com/emilygsimmonds/Cue_Identification/blob/master/Predict_SW.R) : function to predict lay dates from the results of Params_SW.R or Params_SW_KFold.R. It can be used for both K-fold cross validation and prediction for complete datasets.

### Scripts to run climate sensitivity profile method:

[Format_CSP.R](https://github.com/emilygsimmonds/Cue_Identification/blob/master/Format_CSP.R) : functions to re-format climate data and biological data for use in the CSP analyses. Climate data requires row as year and columns as days. If using doy95, biological data requires a date format version of this column in addition to the day of year version.

[Run_CSP.R](https://github.com/emilygsimmonds/Cue_Identification/blob/master/Run_CSP.R) : functions to run the CSP model.

[Run_CSP_KFold.R](https://github.com/emilygsimmonds/Cue_Identification/blob/master/Run_CSP_KFold.R) : functions to run the CSP model for K-fold cross validation. Years are extracted in a different way to Run_CSP.R.

[Params_CSP.R](https://github.com/emilygsimmonds/Cue_Identification/blob/master/Params_CSP.R) : function to extract parameters (duration of optimal window, coefficients, and r-squared) from Run_CSP.R or Run_CSP_KFold.R outputs. **This code is currently incomplete**; two lines were taken from Thackeray et al. 2016, which can be found at: https://github.com/NERC-CEH/Phenology_Climate/blob/master/Source_Code/Functions.R. Lines 419 to 428 of their code needs to be added to this script where indicated.

[Find_concurrent_period.R](https://github.com/emilygsimmonds/Cue_Identification/blob/master/Find_concurrent_period.R) : script detailing where to find the code for this function. It is required to Params_CSP.R and Run_PSR.R.

[Climate_extract.R](https://github.com/emilygsimmonds/Cue_Identification/blob/master/Climate_extract.R) : function to extract mean of the daily mean temperatures in the key window identified. This function is also used for the PSR method.

[Predict_CSP.R](https://github.com/emilygsimmonds/Cue_Identification/blob/master/Predict_CSP.R) : function to predict lay dates from the results of the CSP analysis.

[Predict_CSP_KFold.R](https://github.com/emilygsimmonds/Cue_Identification/blob/master/Predict_CSP_KFold.R) : function to predict lay dates from the results of the CSP analysis for K-fold cross valdiation. The extraction of prediction years and model data differ from Predict_CSP.R.

### Scripts to run P-spline signal regression method:

[Run_PSR.R](https://github.com/emilygsimmonds/Cue_Identification/blob/master/run_PSR.R) : function to run the PSR model for both a whole dataset and K-fold cross validation. This function runs the model, extracts parameter values and climate window data. It can also be used to predict new lay dates.

[PSR_effect_size.R](https://github.com/emilygsimmonds/Cue_Identification/blob/master/PSR_effect_size.R) : function to estimate the slope of the relationship between temperature across the whole year and lay dates. Runs predictions from the PSR results for the actual data and +1 degree celsius across the whole year to estimate the effect of 1 degree increase in temperature.

### Scripts to run growing degree day method:

[Format_GDD.R](https://github.com/emilygsimmonds/Cue_Identification/blob/master/Format_GDD.R) : function to format climate data for GDD analysis. Columns must be Julian days and rows represent years, each entry within the matrix is a daily mean temperature.

[Run_GDD.R](https://github.com/emilygsimmonds/Cue_Identification/blob/master/Run_GDD.R) : function to run the GDD model. This function includes the equations needed to run the model and produces an output of the negative loglikelihood of the model.

[Uncertainty_GDD.R](https://github.com/emilygsimmonds/Cue_Identification/blob/master/Uncertainty_GDD.R) : functions to run a bootstrap analysis of the GDD model and to generate confidence intervals for the model. Bootstrap replications currently set to 2 for speed.

[Params_GDD.R](https://github.com/emilygsimmonds/Cue_Identification/blob/master/Params_GDD.R) : function to run the GDD model through an optimiser to get estimates of the parameters of this model. This function uses Run_GDD.R and Uncertainty_GDD.R.

[Predict_GDD.R](https://github.com/emilygsimmonds/Cue_Identification/blob/master/Predict_GDD.R) : function to predict lay dates from the GDD model using parameters estimated in Params_GDD.R.




