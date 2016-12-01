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
      for(j in 1:K)
      {
        dist<-0
         i_bolck_Newdata<-Data[Data[,1]==udata[i],2]
         k_block<-ni[ni[,1]==j,2]
         Dist<-New_ratio_distance_between_objects(i_bolck_Newdata,k_block)
         dist<-Dist+dist
        ddist[j]<-dist
      }
    ind<-which(ddist==min(ddist),arr.ind=T)[1]
    cid[i]<-ind
    }
  M<-cid
}



