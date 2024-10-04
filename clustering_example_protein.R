# script for clustering 
library(gridExtra)
library(ggplot2)
library(fpc)
library('grDevices')

path <- './protein_data/'
protein_df <- read.table(paste0(path,'protein.txt'), sep='\t', header=TRUE)
str(protein_df)

vars.to.use <- colnames(protein_df)[-1] # all column names except for the 1st column
scaled_protein_df <- scale(protein_df[,vars.to.use]) # this is a 25-by-9 matrix where each

# column has 0 mean and unit standard deviation

# The scaled:center attribute (a vector of 9 elements) contains the mean values
# of all the columns.
attr(scaled_protein_df, "scaled:center")

# The scaled:scale attribute (a vector of 9 elements) contains the variances
# of all the columns.
attr(scaled_protein_df, "scaled:scale")

d <- dist(scaled_protein_df, method="euclidean")

# hierachical clustering
pfit <- hclust(d, method="ward.D2") # perform hierarchical clustering

# To examine `pfit`, type: summary(pfit) and pfit$height
plot(pfit, labels=protein_df$Country, main="Cluster Dendrogram for Protein Consumption")
rect.hclust(pfit, k=5) # k=5 means we want rectangles to be put around 5 clusters
xx <- c(3, 7.5, 13.5, 19.5, 23.5); yy <- -3.5; clusterID <- c(3,4,2,1,5)
text(xx, yy, clusterID, col="red")

# dist() returns an object of class 'dist', which includes the pairwise
# Euclidean distances of the 25 observations. In this case, we have
# 25-choose-2 pairs (i.e., 25*24/2 = 300). So 300 Euclidean distance values
# are stored in variable `d` for further operation.
d <- dist(scaled_protein_df, method="euclidean")

groups <- cutree(pfit, k=5)
groups

print_clusters <- function(protein_df, groups, cols_to_print) {
  Ngroups <- max(groups)
  for (i in 1:Ngroups) {
    print(paste("cluster", i))
    print(protein_df[groups == i, cols_to_print])
  }
}

cols_to_print <- c("Country","RedMeat","Fish","Fr.Veg")
print_clusters(protein_df, groups, cols_to_print)

# Calculate the principal components of scaled_protein_df
princ <- prcomp(scaled_protein_df) 
nComp <- 2 # focus on the first two principal components

# project scaled_protein_df onto the first 2 principal components to form a new
# 2-column data frame.
project2D <- as.data.frame(predict(princ, newdata=scaled_protein_df)[,1:nComp])

# combine with `groups` and protein_df$Country to form a 4-column data frame
hclust.project2D <- cbind(project2D, cluster=as.factor(groups), country=protein_df$Country)
head(hclust.project2D)

# finding convex hull
find_convex_hull <- function(proj2Dprotein_df, groups) {
  do.call(rbind,
          lapply(unique(groups),
                 FUN = function(c) {
                   f <- subset(proj2Dprotein_df, cluster==c);
                   f[chull(f),]
                 }
          )
  )
}
hclust.hull <- find_convex_hull(hclust.project2D, groups)

ggplot(hclust.project2D, aes(x=PC1, y=PC2)) +
  geom_point(aes(shape=cluster, color=cluster)) +
  geom_text(aes(label=country, color=cluster), hjust=0, vjust=1, size=3) +
  geom_polygon(data=hclust.hull, aes(group=cluster, fill=as.factor(cluster)),
               alpha=0.4, linetype=0) + theme(text=element_text(size=20))

kbest.p <- 5
cboot.hclust <- clusterboot(scaled_protein_df, clustermethod=hclustCBI,
                            method="ward.D2", k=kbest.p)

summary(cboot.hclust$result)
groups.cboot <- cboot.hclust$result$partition
print_clusters(protein_df, groups.cboot, "Country")

# cboot.hclust$bootbrd = number of times a cluster is desolved.
values <- 1 - cboot.hclust$bootbrd/100 # large values here => highly stable
## [1] 0.75 0.79 0.52 0.75 0.60
cat("So clusters", order(values)[5], "and", order(values)[4], "are highly stable")
## So clusters 1, 2 and 4 are highly stable


# Function to return the squared Euclidean distance of two given points x and y
sqr_euDist <- function(x, y) {
  sum((x - y)^2)
}

# Function to calculate WSS of a cluster, represented as a n-by-d matrix
# (where n and d are the numbers of rows and columns of the matrix)
# which contains only points of the cluster.
wss <- function(clustermat) {
  c0 <- colMeans(clustermat)
  sum(apply( clustermat, 1, FUN=function(row) {sqr_euDist(row, c0)} ))
}

# Function to calculate the total WSS. Argument `scaled_protein_df`: data frame
# with normalised numerical columns. Argument `labels`: vector containing
# the cluster ID (starting at 1) for each row of the data frame.
wss_total <- function(scaled_protein_df, labels) {
  wss.sum <- 0
  k <- length(unique(labels))
  for (i in 1:k)
    wss.sum <- wss.sum + wss(subset(scaled_protein_df, labels == i))
  wss.sum
}

# Function to calculate total sum of squared (TSS) distance of data
# points about the (global) mean. This is the same as WSS when the
# number of clusters (k) is 1.
tss <- function(scaled_protein_df) {
  wss(scaled_protein_df)
}
# Function to return the CH indices computed using hierarchical
# clustering (function `hclust`) or k-means clustering (`kmeans`)
# for a vector of k values ranging from 1 to kmax.
CH_index <- function(scaled_protein_df, kmax, method="kmeans") {
  if (!(method %in% c("kmeans", "hclust")))
    stop("method must be one of c('kmeans', 'hclust')")
  npts <- nrow(scaled_protein_df)
  wss.value <- numeric(kmax) # create a vector of numeric type
  # wss.value[1] stores the WSS value for k=1 (when all the
  # data points form 1 large cluster).
  wss.value[1] <- wss(scaled_protein_df)
  if (method == "kmeans") {
    # kmeans
    for (k in 2:kmax) {
      clustering <- kmeans(scaled_protein_df, k, nstart=10, iter.max=100)
      wss.value[k] <- clustering$tot.withinss
    }
  } else {
    # hclust
    d <- dist(scaled_protein_df, method="euclidean")
    pfit <- hclust(d, method="ward.D2")
    for (k in 2:kmax) {
      labels <- cutree(pfit, k=k)
      wss.value[k] <- wss_total(scaled_protein_df, labels)
    }
  }
  bss.value <- tss(scaled_protein_df) - wss.value # this is a vector
  B <- bss.value / (0:(kmax-1)) # also a vector
  W <- wss.value / (npts - 1:kmax) # also a vector
  data.frame(k = 1:kmax, CH_index = B/W, WSS = wss.value)
}

# calculate the CH criterion
crit.protein_df <- CH_index(scaled_protein_df, 10, method="hclust")

fig1 <- ggplot(crit.protein_df, aes(x=k, y=CH_index)) +
  geom_point() + geom_line(colour="red") +
  scale_x_continuous(breaks=1:10, labels=1:10) +
  labs(y="CH index") + theme(text=element_text(size=20))
fig2 <- ggplot(crit.protein_df, aes(x=k, y=WSS), color="blue") +
  geom_point() + geom_line(colour="blue") +
  scale_x_continuous(breaks=1:10, labels=1:10) +
  theme(text=element_text(size=20))

grid.arrange(fig1, fig2, nrow=1)

plot(pfit, labels=protein_df$Country, main="Cluster Dendrogram for Protein Consumption")
rect.hclust(pfit, k=2)

# ----------------- Kmeans clustering -------------------
kbest.p <- 5
# run kmeans with 5 clusters, 100 random starts, and 100
# maximum iterations per run.
kmClusters <- kmeans(scaled_protein_df, kbest.p, nstart=100, iter.max=100)

kmClusters$centers
kmClusters$size

cat("Total of cluster sizes =", sum(kmClusters$size))
## Total of cluster sizes = 25
cat("Total number of observations =", nrow(protein_df))
## Total number of observations = 25

groups <- kmClusters$cluster
print_clusters(protein_df, groups, "Country")


kmClustering.ch <- kmeansruns(scaled_protein_df, krange=1:10, criterion="ch")
kmClustering.ch$bestk
## [1] 2

kmClustering.asw <- kmeansruns(scaled_protein_df, krange=1:10, criterion="asw")
kmClustering.asw$bestk
## [1] 3

# Compare the CH values for kmeans() and hclust().
print("CH index from kmeans for k=1 to 10:")
## [1] "CH index from kmeans for k=1 to 10:"

print(kmClustering.ch$crit)
## [1] 0.000000 14.094814 11.417985 10.418801 10.011797 9.964967 9.861682 9.412089 9.166676 9.075569

print("CH index from hclust for k=1 to 10:")
## [1] "CH index from hclust for k=1 to 10:"

hclusting <- CH_index(scaled_protein_df, 10, method="hclust")
print(hclusting$CH_index)


kmCritframe <- data.frame(k=1:10, ch=kmClustering.ch$crit,
                          asw=kmClustering.asw$crit)
fig1 <- ggplot(kmCritframe, aes(x=k, y=ch)) +
  geom_point() + geom_line(colour="red") +
  scale_x_continuous(breaks=1:10, labels=1:10) +
  labs(y="CH index") + theme(text=element_text(size=20))
fig2 <- ggplot(kmCritframe, aes(x=k, y=asw)) +
  geom_point() + geom_line(colour="blue") +
  scale_x_continuous(breaks=1:10, labels=1:10) +
  labs(y="ASW") + theme(text=element_text(size=20))
grid.arrange(fig1, fig2, nrow=1)

fig <- c()
kvalues <- seq(2,5)
for (k in kvalues) {
  groups <- kmeans(scaled_protein_df, k, nstart=100, iter.max=100)$cluster
  kmclust.project2D <- cbind(project2D, cluster=as.factor(groups),
                             country=protein_df$Country)
  kmclust.hull <- find_convex_hull(kmclust.project2D, groups)
  assign(paste0("fig", k),
         ggplot(kmclust.project2D, aes(x=PC1, y=PC2)) +
           geom_point(aes(shape=cluster, color=cluster)) +
           geom_polygon(data=kmclust.hull, aes(group=cluster, fill=cluster),
                        alpha=0.4, linetype=0) +
           labs(title = sprintf("k = %d", k)) +
           theme(legend.position="none", text=element_text(size=20))
  )
}

grid.arrange(fig2, fig3, fig4, fig5, nrow=2)
