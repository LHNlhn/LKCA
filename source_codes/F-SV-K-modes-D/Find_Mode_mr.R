Find_Mode_mr<-function(Data)
{
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
  modesset<-matrix(c(t(modesset)),byrow = F)
  new_modesset<-cbind(p,modesset)
  NEW_modesset<-rbind(NEW_modesset,new_modesset)
}
nj<-as.matrix(NEW_modesset)




