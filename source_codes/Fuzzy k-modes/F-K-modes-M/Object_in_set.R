Object_in_set<-function(object,set)
{
  row=nrow(set)
  column=ncol(set)
  for(i in 1:row)
  
    if(all(set[i,]==object))
      {num<-i
        break
    }else{
      num<-0
    }
  j<-num
}


