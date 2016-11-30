Fuzzy_K_Mode_D<-function(data,K,InitialCenters,a)
{
if(is.null(InitialCenters)){
  DDist<-k_initial_center(data,K)
  centers<-data[DDist,]
}else{
  centers<-data[InitialCenters,]
}
row.names(centers)<-c(1:K)
ni<-centers
iter<-1
iiter<-100
library(rmr2)
while(iter<=iiter)
{
nj<-values(from.dfs(Fuzzy_K_Mode_mr(to.dfs(data),ni,K,a)))
  
if(all(nj==ni)) 
  break
else{
  ni<-nj
iter<-iter+1
}
}
}

