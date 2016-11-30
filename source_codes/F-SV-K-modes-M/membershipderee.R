membershipdegree<-function(object,set,j,a)
{
  Udata<-as.matrix(unique(set[,1]))
 tempa=NNew_ratio_distance_between_objects(object,set[set[,1]==j,])
 tempc=0
 for(i in 1:length(Udata))
 {
   tempb=NNew_ratio_distance_between_objects(object,set[set[,1]==i,])
   tempc=tempc+(tempa/tempb)^(1/(a-1))
 }
 num=1/tempc
}
    

