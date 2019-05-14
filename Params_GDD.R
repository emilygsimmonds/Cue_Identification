#### Function to extra optimise parameters for GDD model

# Function to run the optimiser to find optimal values for GDD parameters
get_params_GDD <- function(data, init, lower, upper, tempmat){
  result <- GenSA(init,run_GDD,lower=c(50,1,1),upper=c(1000,10,200),data,
                  tempmat,control=list(temperature=10000,maxit=2000))
  return(result$par)
}