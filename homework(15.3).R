#import Cereals.df
Cereals.df <- read.csv("Cereals.csv")
View((Cereals.df))
Cereals.omit.df <- na.omit(Cereals.df)
View(Cereals.omit.df)

# set row names in to values stored in the Cereal names column
row.names(Cereals.omit.df) <- Cereals.omit.df[,1]

# remove the Cereals names column
Cereals.omit.new.df <- Cereals.omit.df[,4:16]
View(Cereals.omit.new.df)


# normalize input variables
Cereals.omit.new.df.norm <- sapply(Cereals.omit.new.df, scale)

# set row names in Cereals.omit.new.df.norm to the cereals names column of Cereals.omit.df
row.names(Cereals.omit.new.df.norm) <- row.names(Cereals.omit.df) 
View(Cereals.omit.new.df.norm)

# compute Euclidean distance using all variables
d.all <- dist(Cereals.omit.new.df.norm, method = "euclidean")
print(d.all)

library(NbClust)
library(cluster)
library(factoextra)
fviz_nbclust(Cereals.omit.new.df, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

# run kmeans algorithm 
set.seed(2)
km <- kmeans(Cereals.omit.new.df.norm, 6)

# show cluster membership
km$cluster
cluster.df <- data.frame(km$cluster)
View(cluster.df)
merged_df <- merge(Cereals.omit.new.df.norm, cluster.df, by = "row.names")
head(merged_df)
aggregate(merged_df[,2:14], list(merged_df$km.cluster), mean)
sapply(Filter(is.numeric, Cereals.omit.new.df), mean)
# centroids
km$centers

# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 13))

# label x-axes
axis(1, at = c(1:13), labels = names(Cereals.omit.new.df))

# plot centroids
for (i in c(1:6))
  lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                       "black", "red"))

# name clusters
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:6)))

#distance between centers
dist(km$centers)
