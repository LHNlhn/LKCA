Fuzzy_ratio_k_multi_modes_mr<-function(Data,K,ni)
{
 FSV_k.map=function(.,Data)
 {
   nearest=nearest_mr(Data,ni,K)
   keyval(nearest,Data)
 }
 FSV_K.reduce=function(.,Data)
 {
   My_Nean_New_ratio_computing_modes(Data)
 }
 mapreduce(Data,map=FSV_k.map,reduce=FSV_K.reduce)
}




