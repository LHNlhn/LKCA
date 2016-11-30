nearest_mr<-function(Data,K,ni)
{
  Data<-as.matrix(Data)
  nc<-ncol(Data)
  udata<-as.matrix(unique(Data[,1]))
  cid<-vector()
  objectvalue<-vector()
  ddist<-vector()
  for(i in 1:length(udata))
  {
    Dist<-vector()
    i_bolck_Newdata<-Data[Data[,1]==udata[i],]
    NN<-Object_in_set(i_bolck_Newdata,ni)
    if(NN!=0){
      cid[i]<-NN
    }else{
      for(j in 1:K)
      {
        dist=membershipdegree(i_bolck_Newdata,ni,j,a)
        Dist<-rbind(Dist,dist)
      }
      Fw_dist<-as.matrix(Dist)
      ind<-which(Fw_dist==max(Fw_dist[,1]),arr.ind = T)[1]
      cid[i]<-ind
    }
  }
  M<-cid
}



