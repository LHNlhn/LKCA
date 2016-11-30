Fuzzy_K_Mode<-function(data,K,InitialCenters,a)
{
  {
    n<-nrow(data)
    d<-ncol(data)
  }
  cid<-vector(length=n)
  oldcia<-matrix(1,nrow=n,ncol=K)
  nr<-matrix(0,nrow=n,ncol=K)
  if(is.null(InitialCenters)){
    DDist<-k_initial_center(data,K)
      centers<-data[DDist,]
  }else{
    centers<-data[InitialCenters,]
  }
  row.names(centers)<-c(1:K)
  ni<-centers
  nid<-nrow(ni)
  if(nid!=K)
    print(error)
  #Set up maximum number of iterations
  iter<-1
  iiter<-100
  while(iter<=iiter)
  {
    {
      ptm<-proc.time()
      FW_dist<-vector()
      cid<-vector()
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
    nr<-table(cid)
  nj<-ni
  data1<-vector()
  for(i in 1:K)
  {
    data1<-data[(cid==i),]
    if(length(data1)!=0)
    {
      nj[i,]<-Find_Mode(data1)
    }
  }
  if(all(nj==ni))  break
  }
  ni<-nj
  iter<-iter+1
  }
  cat("iter=",iter,"\n")
  cat("clustering result cid=","\n",cid,"\n")
  cat("time","\n")
  print(proc.time()-ptm)
}


