Distance_of_Categorical_mr<-function(Data,Data2)
{
  nn<-nrow(Data)
  n<-nrow(Data2)
  d<-ncol(Data2)
  dataset<-vector()
  Distance<-vector()
  dist<-vector()
for(q in 1:nn)
{
  Data1<-Data[q,]
     for(i in 1:n)
       {
      Distance=0
        for(j in 1:d)
          {
          if (Data1[,j]!=Data2[i,j])
          Distance<-Distance+1
          }
            dataset[i]<-Distance
     }
  dist<-as.matrix(rbind(dist,dataset))
}
  Dist<-t(dist)
}

