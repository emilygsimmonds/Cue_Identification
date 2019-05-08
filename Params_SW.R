#### Function to extract parameters from sliding window analysis  

# This function assigns the output from Run_SW 
# it can run for data or parameters

# Take the whole list of results from many data subsets
# and produces a single table of either parameters or data

# labels = labels of different data subsets used
get_params_SW <- function(Results, lay_dates, labels, type=c("Data", "Params")){
  # every odd item is a list of parameters
  # every even is data
  # this extracts both
  odd <- seq(1, length(Results), 2)
  even <- seq(2, length(Results), 2)
  
  # extract parameters only
  if(type == "Params"){
    Parameters <- Results[[odd[1]]]
    if(is.null(dim(Parameters))==FALSE){for(i in 2:length(odd)){Parameters <- as.data.frame(rbind(Parameters, Results[[odd[i]]]))}
    rownames(Parameters) <- labels}
    # need to convert to correct format
    Parameters <- matrix(Parameters, ncol=7)
    Parameters <- as.data.frame(Parameters) 
    colnames(Parameters) <- c("WindowOpen", "WindowClose", "Variable", "Int", "EST", "SE", "R2")
    Parameters$WindowOpen <- 140 - Parameters$WindowOpen # scaling to be year day 
    Parameters$WindowClose <- 140 - Parameters$WindowClose # scaling to be year day
    return(Parameters)
  }
  
  # extract data only
  if(type == "Data"){
    Data_results <- data.frame(matrix(ncol=length(labels)+1, nrow=length(lay_dates)))
    colnames(Data_results) <- c("Lay_dates", labels)
    # have reference lay dates as first column
    Data_results[,1] <- lay_dates
    # add new columns of climate variables for each data subset
    # append into a single dataframe
    for(i in 1:length(even)){
      temporary <- Results[[even[i]]]
      vectorX <- rep(NA, length(lay_dates))
      vectorX[which(lay_dates == temporary$yvar[1]):which(lay_dates == temporary$yvar[length(temporary$yvar)])] <- temporary$climate
      Data_results[,i+1] <- vectorX
    }
    return(Data_results)
  }
}