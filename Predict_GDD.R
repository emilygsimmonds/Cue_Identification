#### Function to predict from GDD model

get_preds_GDD <- function(pars, datafile, tempmat){
  # pars inc CI for each parameter
  cumDD<-pars[1] # initial value 1 - number of degrees we want to reach
  threshold<-pars[2] # threshold value above which degrees count
  start.val<-round(pars[3]) # Date on which we want to start accumulating degrees
  
  tempdatause<-tempmat[,(start.val):300] # subset out dataframe of temp to days we want
  correctedtemp<-tempdatause-threshold # subtract minimum temperature so it isn't counted
  correctedtemp2<-replace(correctedtemp,correctedtemp<0,0) # make all negatives 0
  
  cumulative.matrix<-t(apply(correctedtemp2,1,cumsum)) # sums up degrees for each year 
  
  newmatrix<-cumulative.matrix-cumDD # subtract the number of degrees we want to reach
  
  predicted<-apply(newmatrix>1,1,which.max)+start.val-1 # predicted is the first day on which target is exceeded
  
  # for lower CI
  cumDD<-pars[4] # initial value 1 - number of degrees we want to reach
  threshold<-pars[6] # threshold value above which degrees count
  start.val<-round(pars[8]) # Date on which we want to start accumulating degrees
  
  tempdatause<-tempmat[,(start.val):300] # subset out dataframe of temp to days we want
  correctedtemp<-tempdatause-threshold # subtract minimum temperature so it isn't counted
  correctedtemp2<-replace(correctedtemp,correctedtemp<0,0) # make all negatives 0
  
  cumulative.matrix<-t(apply(correctedtemp2,1,cumsum)) # sums up degrees for each year 
  
  newmatrix<-cumulative.matrix-cumDD # subtract the number of degrees we want to reach
  
  predicted_L<-apply(newmatrix>1,1,which.max)+start.val-1 # predicted is the first day on which target is exceeded
  
  # for upper CI
  cumDD<-pars[5] # initial value 1 - number of degrees we want to reach
  threshold<-pars[7] # threshold value above which degrees count
  start.val<-round(pars[9]) # Date on which we want to start accumulating degrees
  
  tempdatause<-tempmat[,(start.val):300] # subset out dataframe of temp to days we want
  correctedtemp<-tempdatause-threshold # subtract minimum temperature so it isn't counted
  correctedtemp2<-replace(correctedtemp,correctedtemp<0,0) # make all negatives 0
  
  cumulative.matrix<-t(apply(correctedtemp2,1,cumsum)) # sums up degrees for each year 
  
  newmatrix<-cumulative.matrix-cumDD # subtract the number of degrees we want to reach
  
  predicted_U<-apply(newmatrix>1,1,which.max)+start.val-1 # predicted is the first day on which target is exceeded
  
  output <- data.frame(fit = predicted,
                       CI_L = predicted_L,
                       CI_U = predicted_U)
  
  return(output)
}
