library(data.table)
library(Rtsne)
library(ggplot2)
library(ClusterR)

dataemb <- fread("./project/volume/data/raw/test_emb.csv")
datatrain<- fread("./project/volume/data/raw/training_data.csv")
datatrainemb <- fread("./project/volume/data/raw/training_emb.csv")

tsne1 <- Rtsne(datatrainemb, perplexity=10, check_duplicates=FALSE, verbose=TRUE)
tsne1_dt <- data.table(tsne1$Y)
gmm_data<-GMM(tsne1_dt[,.(V1,V2)],10)
l_clust<-gmm_data$Log_likelihood^10
l_clust<-data.table(l_clust)
net_lh<-apply(l_clust,1,FUN=function(x){sum(1/x)})
cluster_prob<-1/l_clust/net_lh
tsne1_dt$Cluster_1_prob<-cluster_prob$V1
ggplot(tsne1_dt,aes(x=V1,y=V2,col=Cluster_1_prob))+geom_point()
train_ans <- data.table("subredditcars"=cluster_prob$V2, "subredditscience"=cluster_prob$V10,
                        "subredditmagicTCG"=cluster_prob$V5, "subredditMachineLearning"=cluster_prob$V6,
                        "subredditReal_Estate"=cluster_prob$V5)


tsne <- Rtsne(dataemb, perplexity=10, check_duplicates = FALSE, verbose = TRUE)
tsne_dt <- data.table(tsne$Y)
ggplot(tsne_dt,aes(x=V1,y=V2))+geom_point()
gmm_data<-GMM(tsne_dt[,.(V1,V2)],10)
l_clust<-gmm_data$Log_likelihood^10

l_clust<-data.table(l_clust)

net_lh<-apply(l_clust,1,FUN=function(x){sum(1/x)})

cluster_prob<-1/l_clust/net_lh

tsne_dt$Cluster_1_prob<-cluster_prob$V1

ggplot(tsne_dt,aes(x=V1,y=V2,col=Cluster_1_prob))+geom_point()
