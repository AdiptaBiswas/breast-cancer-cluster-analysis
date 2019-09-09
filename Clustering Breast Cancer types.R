library(cluster)
library(factoextra)
library(clustertend)
library(ggplot2)
library(NbClust)
library(fpc)
setwd("C:\\Users\\User\\Documents\\Datasets\\Analysis\\Classifier")

# Data loading and prep.
data<-read.csv("Cancer Data.csv")
head(data)
depend<-data[,2]
data<- data[,3:31]
colnames(data)
dim(data)
str(data)
data<-scale(data)
data<-as.data.frame(data)
length(data)

# Testing and Training data
set.seed(666)
data_train<-sample(1:nrow(data),round(nrow(data)/10,0))
data_test<-data[-data_train,]
sum(data_train=!data_test)
data_test<-as.data.frame(data_test)
data_train<-as.data.frame(data_train)

# Checking whether the data is clusterable
set.seed(123)
hopkins(data,n=nrow(data)-1) # 0.1496466, Highly clusterable

# This plot shows that the data has randomness and we can't distinguish the clusters
ggplot(data,aes(x=fractal_dimension_mean,y=texture_worst))+geom_point()+geom_density2d()

# Checking the best possible clusters for k-means

fviz_nbclust(data,kmeans,method="silhouette") # k=2 

# Re-checking the best possible 'k'
NbClust(data,distance="euclidean",min.nc=2,max.nc=15,method="kmeans",index="silhouette")

# Doing kmeans on the data and visualizing
clust_data<-kmeans(data,2,nstart=15)
fviz_cluster(clust_data, data=data, pointsize = 2,geom="point",Xlab="Points", ylab=FALSE,palette="Dark2",ellipse=TRUE,ggtheme = theme(),main="Cluster Plot")



