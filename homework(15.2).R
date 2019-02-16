#import utilities.df
Pharmaceuticals.df <- read.csv("Pharmaceuticals.csv")

# set row names in utilities.df to values stored in the Company name column
row.names(Pharmaceuticals.df) <- Pharmaceuticals.df[,1]

# remove the Company name column
Pharmaceuticals.df <- Pharmaceuticals.df[,3:11]
View(Pharmaceuticals.df)

# compute Euclidean distance using all variables
d.all <- dist(Pharmaceuticals.df, method = "euclidean")
print(d.all)

# normalize input variables
Pharmaceuticals.df.norm <- sapply(Pharmaceuticals.df, scale)

# set row names in utilities.df.norm to the company names column of utilities.df
row.names(Pharmaceuticals.df.norm) <- row.names(Pharmaceuticals.df) 
View(Pharmaceuticals.df.norm)

library(NbClust)
library(cluster)
library(factoextra)
fviz_nbclust(Pharmaceuticals.df.norm, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
#fviz_nbclust(Pharmaceuticals.df.norm, kmeans, method = "silhouette")+
#  labs(subtitle = "Silhouette method")

#fviz_nbclust(Pharmaceuticals.df.norm, kmeans,  method = "gap_stat", nboot = 500)+
#  labs(subtitle = "Gap statistic method")

wss <- (nrow(Pharmaceuticals.df.norm)-1)*sum(apply(Pharmaceuticals.df.norm,2,var))
for (i in 3:11) wss[i] <- sum(kmeans(Pharmaceuticals.df.norm,
                                     centers=i)$withinss)
plot(1:14, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


# run kmeans algorithm 
set.seed(2)
km <- kmeans(Pharmaceuticals.df.norm, 5)

# show cluster membership
km$cluster
cluster.df <- data.frame(km$cluster)
View(cluster.df)

# centroids
km$centers


merged_df <- merge(Pharmaceuticals.df.norm, cluster.df, by = "row.names")
head(merged_df)
aggregate(merged_df[,2:10], list(km$cluster), mean)
sapply(Filter(is.numeric, Cereals.omit.new.df), mean)

# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 8))

# label x-axes
axis(1, at = c(1:8), labels = names(Pharmaceuticals.df.norm))

# plot centroids
for (i in c(1:6))
  lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                       "black", "dark grey"))

# name clusters
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:6)))

#distance between centers
dist(km$centers)


