Object_in_set<-function(object,set)
{
  Udata<-as.matrix(unique(set[,1]))
  Nc<-ncol(object)
  for(i in 1:length(Udata))
  {
    MM<-0
    for(q in 2:Nc)
  { 
    MM<-MM+New_ratio_distance_between_objects(object[,q],set[set[,1]==i,q])
    }
   if(MM==0)
    {num<-i
        break
    }else{
      num<-0
    }
  }
  j<-num
}


