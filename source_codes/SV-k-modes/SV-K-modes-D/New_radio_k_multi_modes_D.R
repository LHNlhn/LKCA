New_ratio_k_multi_modes_D<-function(Data,K,InitialCenters)
{
  Data<-as.matrix(Data)
  nc<-ncol(Data)
  if(is.null(InitialCenters)){
    DDist<-K_initial_centers(Data,K)
    DD<-length(DDist)
    centers<-vector()
    ICenters<-vector()
    IniCenters<-vector()
    for(m in 1:DD)
    {
      ICenters<-Data[Data[,1]==DDist[m],2:nc]
      IniCenters<-cbind(m,ICenters)
      centers<-rbind(centers,IniCenters)
      }
  }else{
    for(n in 1:K)
    {
    ICenters<-Data[Data[,1]==InitialCenters[n],2:nc]
    IniCenters<-cbind(n,ICenters)
    centers<-rbind(centers,IniCenters)
    }
  }
  ni<-centers
  udata<-as.matrix(unique(Data[,1]))
  iter<-1
  iiter<-100
  library(rmr2)
  while(iter<=iiter)
  {
    nj<-values(from.dfs(New_ratio_k_multi_modes_mr(to.dfs(Data),ni,K)))
    
    if(all(nj==ni)) 
      break
    else{
      ni<-nj
      iter<-iter+1
  if(length(ni)==length(nj))
  {
    if(all(nj==ni))
      break
  }
  }
ni<-nj
iter<-iter+1
}
}




