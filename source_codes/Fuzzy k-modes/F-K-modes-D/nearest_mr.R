nearest_mr<-function(data,K,ni,a)
{
  {
    n<-nrow(data)
    d<-ncol(data)
  }
  cid<-vector()
  oldcia<-matrix(1,nrow=n,ncol=K)
  nr<-matrix(0,nrow=n,ncol=K)
  FW_dist<-vector()
    for(i in 1:n)
    {
      Dist<-vector()
      j=Object_in_set(data[i,],ni)
      if(j!=0)
      {
        cid[i]<-j
      }else{
        for(j in 1:K)
        {
          dist=membershipdegree(data[i,],ni,j,a)
          Dist<-rbind(Dist,dist)
        }
      Fw_dist<-as.matrix(Dist)
      ind<-which(Fw_dist==max(Fw_dist[,1]),arr.ind = T)[1]
      cid[i]<-ind
      }
    }
      M<-cid
}



