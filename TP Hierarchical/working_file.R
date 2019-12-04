iris_df <- tbl_df(iris)
iris_df
set.seed(2122019) 
sampleiris <- sample_n(iris_df,40)
#sampleiris <- iris_df[1:40,]
sampleiris_num <- sampleiris %>% select(-Species)
sampleiris_scaled <- scale(sampleiris_num)
D <- dist(sampleiris_scaled)


dendro.avg <- hclust(D, method = "average")

plot(dendro.avg)
plot(dendro.avg, hang=-1, label=sampleiris$Species)

rect.hclust(dendro.avg,k=3, border = c("red","blue","green"))

groups.avg <- cutree(dendro.avg,k = 3)

table(sampleiris$Species,groups.avg)


iris_num <- iris_df %>% select(-Species)

iris_scaled <- scale(iris_num)
D <- dist(iris_scaled)

dendro.iris.avg <- hclust(D, method="average")
dendro.iris.comp <- hclust(D, method="complete")
dendro.iris.sgl <- hclust(D, method="single")

plot(dendro.iris.avg, hang=-1, label=iris_df$Species)
rect.hclust(dendro.iris.avg,k=3, border = c("red","blue","green"))

plot(dendro.iris.comp, hang=-1, label=iris_df$Species)
rect.hclust(dendro.iris.comp,k=3, border = c("red","blue","green"))

plot(dendro.iris.sgl, hang=-1, label=iris_df$Species)
rect.hclust(dendro.iris.sgl,k=3, border = c("red","blue","green"))



groups.iris.avg <- cutree(dendro.iris.avg, k=3)
groups.iris.comp <- cutree(dendro.iris.comp, k=3)
groups.iris.sgl <-cutree(dendro.iris.sgl, k=3)

table(iris_df$Species,groups.iris.avg)
table(iris_df$Species,groups.iris.comp)
table(iris_df$Species,groups.iris.sgl)

