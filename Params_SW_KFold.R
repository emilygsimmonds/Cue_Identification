#### Function to extract parameters from sliding window analysis  
# Edited for K-fold cross valdiation

# Here need to remove predicted years from dataset as 
# training data is not continuous

# input otherwise the same as Params_SW.R

get_params_SW_K <- function(Results, lay_dates, labels, type=c("Data", "Params")){
  # every odd item is a list of parameters
  # every even is data
  # this extracts both
  odd <- seq(1, length(Results), 2)
  even <- seq(2, length(Results), 2)
  
  # extract parameters only
  if(type == "Params"){
    Parameters <- Results[[odd[1]]]
    for(i in 2: length(odd)){Parameters <- as.data.frame(rbind(Parameters, Results[[odd[i]]]))}
    rownames(Parameters) <- labels
    colnames(Parameters) <- c("WindowOpen", "WindowClose", "Variable", "Int", "EST", "SE", "R2")
    Parameters$WindowOpen <- 140 - Parameters$WindowOpen
    Parameters$WindowClose <- 140 - Parameters$WindowClose
    return(Parameters)
  }
  
  # extract data only
  if(type == "Data"){
    Data_results <- data.frame(matrix(ncol=length(labels)+1, nrow=length(lay_dates)))
    colnames(Data_results) <- c("Lay_dates", labels)
    Data_results[,1] <- lay_dates
    # have reference lay dates as first column
    # add new columns of climate variables but matching
    for(i in 1: length(even)){
      temporary <- Results[[even[i]]]
      vectorX <- rep(NA, length(lay_dates))
      ref <- lay_dates %in% temporary$yvar # remove the years to be predicted
      marker <- which(ref == TRUE)
      vectorX[marker] <- temporary$climate
      Data_results[,i+1] <- vectorX
    }
    return(Data_results)
  }
}
