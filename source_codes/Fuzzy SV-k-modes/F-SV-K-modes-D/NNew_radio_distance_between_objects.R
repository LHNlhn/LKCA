NNew_ratio_distance_between_objects<-function(Data1,Data2)
{
 NC<-ncol(Data1)
 DDistance<-0
 ddateset<-0
 for(i in 2:NC)
 {
   DData1<-Data1[,i]
     DData2<-Data2[,i]
  ml<-length(union(DData1, DData2))
  gl<-length(intersect(DData1, DData2))
  DDistance<-(1-gl/ml)
  ddateset<-DDistance+ddateset
 }
 M<-ddateset
}