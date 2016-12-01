New_ratio_k_multi_modes_mr<-function(Data,K,ni)
{
 SV_k.map=function(.,Data)
 {
   nearest=nearest_mr(Data,ni,K)
   keyval(nearest,Data)
 }
 SV_K.reduce=function(.,Data)
 {
   My_Nean_New_ratio_computing_modes(Data)
 }
 mapreduce(Data,map=SV_k.map,reduce=SV_K.reduce)
}




