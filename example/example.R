#load data
data<-read.csv("Zoo.csv",header=FALSE)
n<-nrow(data)
d<-ncol(data)
#set the number of clusters
K<-7
#set the initial cluster center
InitialCenters<-NULL
#load the single-threaded k-modes algorithm package 
library(KmodesS)
#running algorithm
Hard_K_Mode(data,K,InitialCenters)