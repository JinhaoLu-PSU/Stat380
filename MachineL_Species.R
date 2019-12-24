setwd("/Volumes/Transcend/PSU/STAT 380 DATA/Species")

library(data.table)
library(Rtsne)
library(ggplot2)
library(caret)
library(ggplot2)
library(ClusterR)

getOptK <- function(x){
  x <- abs(x)
  max_delta_k_pos <- which.max(x)
  max_delta_k <- max(na.omit(x))
  n2eval<-(length(x) - max_delta_k_pos) - 2
  for(i in max_delta_k_pos:(max_delta_k_pos+n2eval)){
    if(x[max_delta_k_pos + 1]/max_delta_k < 0.15 & x[max_delta_k_pos + 2]/max_delta_k < 0.15 & x[max_delta_k_pos + 3]/max_delta_k < 0.15){
    }else{
      max_delta_k_pos <- max_delta_k_pos + 1
    }
  }
  max_delta_k_pos
}


data<-fread("data.csv")
sub<-fread("example_sub.csv")

id<-data$id
data$id<-NULL

j_data<-data.frame(lapply(data, jitter,factor=0.01))
pca<-prcomp(j_data)


pca_dt<-data.table(unclass(pca)$x)



tsne<-Rtsne(pca_dt,pca = F)

tsne_dt<-data.table(tsne$Y)


k_bic<-Optimal_Clusters_GMM(tsne_dt[,.(V1,V2)],max_clusters = 10,criterion = "BIC")

delta_k<-c(NA,k_bic[-1] - k_bic[-length(k_bic)])


opt_k<-getOptK(delta_k)

gmm_data<-GMM(tsne_dt[,.(V1,V2)],opt_k)

l_clust<-gmm_data$Log_likelihood^10

l_clust<-data.table(l_clust)

net_lh<-apply(l_clust,1,FUN=function(x){sum(1/x)})

cluster_prob<-1/l_clust/net_lh

tsne_dt$Cluster_1_prob<-cluster_prob$V1

sub$species1 <- cluster_prob$V2
sub$species2 <- cluster_prob$V3
sub$species3 <- cluster_prob$V1

sub$species4<- 0
sub$species5<- 0
sub$species6<- 0
sub$species7<- 0
sub$species8<- 0
sub$species9<- 0
sub$species10<- 0

#write csv
write.csv(sub,"k3pca231.csv",row.names = FALSE)

