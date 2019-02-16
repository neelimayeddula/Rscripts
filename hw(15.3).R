#import Cereals.df
Cereals.df <- read.csv("Cereals.csv")
View((Cereals.df))
Cereals.omit.df <- na.omit(Cereals.df)
View(Cereals.omit.df)

# set row names in Cereals.df to values stored in the Cereals name column
row.names(Cereals.omit.df) <- Cereals.omit.df[,1]

# remove the Cereals names column
Cereals.omit.new.df <- Cereals.omit.df[,4:16]
View(Cereals.omit.new.df)

# normalize input variables
Cereals.omit.new.df.norm <- sapply(Cereals.omit.new.df, scale)

# set row names in Cereals.omit.new.df.norm to the company names column of Cereals.omit.df
row.names(Cereals.omit.new.df.norm) <- row.names(Cereals.omit.df) 
View(Cereals.omit.new.df.norm)

# compute Euclidean distance using all variables
d.all <- dist(Cereals.omit.new.df, method = "euclidean")
print(d.all)

library(cluster)
fviz_nbclust(Cereals.omit.new.df.norm, hcut, method = "silhouette",
             hc_method = "complete")

#In hclust() set argument method = to "ward.D", "single", "complete", "average", "median", or "centroid"
#Hierarchical clustering using "single" distance measure
hc1 <- hclust(d.all, method = "single")

#display dendogram of hc1
#hang: hang -1 adjusts cluster names so that names appear on x axis
#ann: a logical value indicating whether the default annotation (title and x and y axis labels) 
#should appear on the plot.
plot(hc1, hang = -1, ann = FALSE)

rect.hclust(hc1, k = 10, border = 2:4)

memb <- cutree(hc1, k = 10)
View(memb)

#Hierarchical clustering using "average" distance measure
hc2 <- hclust(d.all, method = "average")
plot(hc2, hang = -1, ann = FALSE)
rect.hclust(hc2, k = 6, border = "red")

#Hierarchical clustering using "centroid" distance measure
hc3 <- hclust(d.all, method = "centroid")
plot(hc3, hang = -1, ann = FALSE)
rect.hclust(hc3, k = 6, border = 2:4)

#assign observations to '10' clusters using splits depicted in map
memb <- cutree(hc1, k = 6)
memb
View(memb)

merged_df <- merge(Cereals.omit.new.df.norm, cluster.df, by = "row.names")
head(merged_df)
aggregate(merged_df[,2:14], list(merged_df$km.cluster), mean)
sapply(Filter(is.numeric, Cereals.omit.new.df.norm), mean)

#assign observations to '6' clusters using splits depicted in map
memb_average <- cutree(hc2, k = 10)
memb_average
View(memb_average)
# label clusters and add company name
row.names(Cereals.omit.new.df.norm) <- paste(memb_average, ": ", row.names(Cereals.omit.new.df), sep = "")
View(Cereals.omit.new.df.norm)
# plot heatmap 
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(Cereals.omit.new.df.norm), Colv = NA, hclustfun = hclust, 
        col=rev(paste("gray",1:99,sep="")))
