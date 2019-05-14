#### Function to run the GDD model

# Function to run growing degree day model to get parameters and to predict
run_GDD<-function(vals, datafile, tempmat){
  cumDD<-vals[1] # initial value 1 - number of degrees we want to reach
  threshold<-vals[2] # threshold value above which degrees count
  start.val<-round(vals[3]) # Date on which we want to start accumulating degrees
  
  tempdatause<-tempmat[,(start.val):300] # subset out dataframe of temp to days we want
  # stops at day 300 not 365 because event will always have occurred long before
  correctedtemp<-tempdatause-threshold # subtract minimum temperature so it isn't counted
  correctedtemp2<-replace(correctedtemp,correctedtemp<0,0) # make all negatives 0
  
  cumulative.matrix<-t(apply(correctedtemp2,1,cumsum)) # sums up degrees for each year 
  
  newmatrix<-cumulative.matrix-cumDD # subtract the number of degrees we want to reach
  
  predicted<-apply(newmatrix>1,1,which.max)+start.val-1 # predicted is the first day on which target is exceeded
  # plus the start date minus 1, so how far after start did criteria get reached?
  
  if(length(which(is.na(predicted)==TRUE))==0){ # now run an if statement with phenology as a function of offset of predicted
    model2<-lm(datafile$lay_mean~offset(predicted)-1)
    return(-logLik(model2))} # returns log likelihood of model
  
  if(length(which(is.na(predicted)==TRUE))>0){
    return(Inf)}
  
}