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