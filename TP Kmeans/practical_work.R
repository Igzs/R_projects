library(dplyr)
#import csv file as a dataframe called ligue1
ligue1 <- read.csv("ligue1_17_18.csv", header=T,sep=";",row.names=1)
View(ligue1)
#print the first two lines of dataframe ligue1
head(ligue1,2)
#total number of features
ncol(ligue1)
#create smaller dataset based on the Points and yellow.cards features
pointCards <- ligue1 %>% select(Points, yellow.cards)
#set seed for reproductible results
set.seed(28112019)
#run kmeans algorithms with 2 clusters and 20 iterations
km <- kmeans(pointCards,centers=2, iter.max = 20)
#describe km
print(km)
#coordinates of the centers of the clusters
km$centers
#plot points according to the belongs they belong to
plot(pointCards, col = km$cluster,xlab="Points",ylab="Yellow Cards")
points(km$centers, pch = 4, col="blue")

#rerun kmeans with 3 and 4 clusters
km3 <- kmeans(pointCards,centers=3, iter.max = 20)
#plot new clusters
plot(pointCards, col = km3$cluster,xlab="Points",ylab="Yellow Cards")
points(km3$centers, pch = 4, col="blue")

#rerun kmeans with 3 and 4 clusters
km4 <- kmeans(pointCards,centers=4, iter.max = 20)
#plot new clusters
plot(pointCards, col = km4$cluster,xlab="Points",ylab="Yellow Cards")
points(km4$centers, pch = 4, col="blue")

# 10.
# Visualize the "within groups sum of squares" of the k-means clustering results (use the code in the link above).
mydata <- pointsCards
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

# 11.
# Modify the code of the previous question in order to visualize the 'between_SS / total_SS'. Interpret the results.
mydata <- pointsCards
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- (kmeans(mydata, centers=i)$betweenss / kmeans(mydata, centers=i)$totss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="between_SS / total_SS")
# The real improvement is between 1 and 2 clusters, so the best number of cluster is 2


#visualise the within groups sum of squares to determine the best number of clusters
wss <- (nrow(pointCards)-1)*sum(apply(pointCards,2,var))
for (i in 2:10) 
  wss[i] <- sum(km$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

#another way
pamk.best <- pamk(pointCards)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
plot(pam(pointCards, pamk.best$nc))



ligue1_scaled <- as.data.frame(scale(ligue1))

set.seed(28112019)
km.ligue1 <- kmeans(ligue1,centers=3, iter.max = 20)
km.ligue1.scaled <- kmeans(ligue1_scaled,centers=3, iter.max = 20)

X <-  as.data.frame(cbind(c(1, 1, 0, 5, 6, 4),c(4, 3, 4, 1, 2, 0)))

V1 <- runif(2,0,4)
V2 <- runif(2,0,4)
plot(X[,1],X[,2],xlab="X1",ylab="X2")
points(V1,V2,col="red")
clusters <- round(runif(dim(X)[1],1,2))


euclidianDist <- function(X,Y){
  sqrt((X[1]-X[2])**2+(Y[1]-Y[2])**2)
}

nb_clusters=2
D = matrix()
for (i in 1:dim(X)[1]){
  for(k in 1:2){
    print(c(i,k))
    D[i][k] <- euclidianDist(X[i,],V[k])
  }
}

