membershipdegree<-function(object,set,j,a)
{
 row=nrow(set)
 column=ncol(set)
 sum1=New_Distance_of_Categorical(object,set[j,])
 sum2=0
 sum3=0
 for(i in 1:row)
 {
   sum2=New_Distance_of_Categorical(object,set[i,])
   sum3=sum3+(sum1/sum2)^(1/(a-1))
 }
 num=1/sum3
}
    

