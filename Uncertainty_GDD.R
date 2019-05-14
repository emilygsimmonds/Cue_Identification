#### Functions to calculate uncertainty for GDD model

bootstrap_GDD <- function(data, indices, init, lower, upper, tempmat){
  new_data <- data[indices,]
  new_tempmat <- tempmat[indices,]
  return(get_params_GDD(init=init,lower=c(50,1,1),
                        upper=c(1000,10,200),data=new_data,tempmat=new_tempmat))
}

# iterations of bootstrap set to 2 for speed. Increase if running for actual analyses
CI_results_GDD <- function(data, indices, init, lower, upper, tempmat){
  temp_result <- boot(data=data[[i]], bootstrap_GDD, R=2, tempmat=tempmat[[i]], 
                      init=init, lower=c(50,1,1), upper=c(1000,10,200))
  ci1 <- boot.ci(temp_result, conf=0.95, type="perc", index=1)
  ci2 <- boot.ci(temp_result, conf=0.95, type="perc", index=2)
  ci3 <- boot.ci(temp_result, conf=0.95, type="perc", index=3)
  output <- c(temp_result[1]$t0, ci1[4]$perc[4:5], ci2[4]$perc[4:5], ci3[4]$perc[4:5])
}
