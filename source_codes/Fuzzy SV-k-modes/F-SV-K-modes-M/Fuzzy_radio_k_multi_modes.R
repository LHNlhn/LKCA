Fuzzy_ratio_k_multi_modes<-function(Data,K,InitialCenters,a)
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
  ni<-as.matrix(centers)
  udata<-as.matrix(unique(Data[,1]))
#????cidΪ??????????ʾ????
cid<-matrix(0,nrow=1,ncol=K)
nr<-matrix(1,nrow=1,ncol=K)
nj<-vector()
D_Dist<-vector()
iter<-1
objectvalue<-vector()
dist<-vector()
iiter<-1000
#??¼??ʼ?Ķ???modes
while(iter<=iiter)
{
  {
    ptm<-proc.time()
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
    nr<-table(cid)
    temp_new_clusters<-vector()
    modesset<-vector()
    new_modesset<-vector()
    NEW_modesset<-vector()
    for(p in 1:K)
    {
      np<-length(which(cid==p))
      t_temp<-udata[which(cid==p),]
      New_data<-vector()
      NNew_data<-vector()
      if(np==1){
        modesset<-matrix(c(Data[which(Data[,1]==t_temp),2:nc]))
      }else
      {
        for(j in 1:np)
        {
          NNew_data<-rbind(NNew_data,Data[Data[,1]==t_temp[j],])
        }
        for(q in 2:nc)
        {
          New_data<-cbind(NNew_data[,1],NNew_data[,q])
          MModesset<-My_Nean_New_ratio_computing_modes(New_data)
          modesset<-rbind(modesset,MModesset)
        }
      }
      modesset<-as.matrix(modesset)
      modesset<-matrix(c(t(modesset)))
      new_modesset<-cbind(p,modesset)
      NEW_modesset<-rbind(NEW_modesset,new_modesset)
    }
    nj<-as.matrix(NEW_modesset)
    if(length(ni)==length(nj))
    {
      if(all(nj==ni))
        break
    }
  }
  ni<-nj
  iter<-iter+1
  }
cat("time","\n")
print(proc.time()-ptm)
cat(nj)
}




