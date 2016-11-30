Hard_K_Mode_mrr<-function(data,ni)
{
  kmodes.map=function(.,data)
  {
    nearest={
      D=Distance_of_Categorical_mr(ni,data)
      nearest=max.col(-D)
    }
      keval(nearest,data)
    }
  kmodes.reduce=function(.,data)
  {
    Find_Mode(data)
  }
  mapreduce(data,map=kmodes.map,reduce=kmodes.reduce)
  }


