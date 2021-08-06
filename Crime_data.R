# H-Clustering

# Reading the dataset
input <- read.csv(file.choose())   
# Exploring the dataset
mydat <- input[,-1]
#Scaling the dataset
nor <- scale(mydat)
d <- dist(nor,method = "euclidean")
fit <- hclust(d,method = "complete")
plot(fit,hang=-1)
group <- cutree(fit,k=4)
rect <- rect.hclust(fit,k=4,border = "red")
H_Cluster <- as.matrix(group)
Final <- data.frame(input,H_Cluster)
Final <- arrange(Final,H_Cluster)
View(Final)

# K-Means Clustering
install.packages("kselection")
install.packages("factoextra")
library("plyr")
library("kselection")
library("factoextra")
d <- kmeans(nor,7)
str(d)
Cluster <- d$cluster
Fin <- data.frame(input,Cluster)
Fin <- arrange(Fin,Cluster)
View(Fin)
k <- kselection(nor)
fviz_nbclust(mydat,kmeans,method = "wss")+labs(subtitle = "Elbow Curve Method")
write.csv(Fin,file = "K_means_Crime_data.csv")
write.csv(Final,file = "Hclust_Crime_data.csv")
aggregate(mydat,by=list(d$cluster),FUN=mean)
