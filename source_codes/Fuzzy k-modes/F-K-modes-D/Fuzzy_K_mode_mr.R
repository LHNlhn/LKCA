Fuzzy_K_Mode_mr<-function(data,K,ni,a)
{
      fkmodes.map=function(.,data)
      {
          nearest=nearesr_mr(data,K,ni,a)
          keyval(nearest,data)
      }
     fkmodes.reduce=function(.,data)
     {
       Find_Mode(data)
     }
mapreduce(data,map=fkmodes.map,reduce=fkmodes.reduce)
}


