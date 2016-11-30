Fuzzy_K_Mode_M<-function(data,K,InitialCenters,a)
{
  library(parallel)
  cl.cores<-detectCores()
  cl<-makeCluster(cl.cores)
  res<-parLapply(cl=cl,Fuzzy_K_Mode(data,K,InitialCenters,a))
  stopCluster(cl)
}
