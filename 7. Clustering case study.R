# The SSE_Ratio is a measure of the total variance in your data set that is explained by the clustering. 
# k-means minimize the within group dispersion and maximize the between-group dispersion.
# By assigning the samples to k clusters rather than n (number of samples) clusters achieved 
# a reduction in sums of squares of SSE_Ratio.

# Choose the data on which clustering has to be done
# Always remove the target variable before passing the data for clustering
ClusterData=iris[, -5]
#ClusterData=mtcars
#ClusterData=Boston

# Create a data frame to store results
IterationData=data.frame(
numberOfClusters=numeric(0),
SSE_Ratio=numeric(0)
)

# Trying to find out best number of clusters
for (i in c(1:10)){
KmeansCluster=kmeans(ClusterData,i)
IterationData=rbind(IterationData,data.frame(
numberOfClusters=i,
SSE_Ratio=round(100*(KmeansCluster$betweenss/KmeansCluster$totss), 2)))
}

# Stopping R to abbreviate numbers to scientific notation
options(scipen=100000)
print('#### clustering iteration results ####')
IterationData

# By looking at the curve we can see 3 clusters are optimal 
# which are explaining 88.8% of the variance in data
plot(IterationData$numberOfClusters,IterationData$SSE_Ratio)
lines(IterationData$numberOfClusters,IterationData$SSE_Ratio)

########################################################################
# Creating final number of clusters based on above curve
OptimalClusters=kmeans(ClusterData,4)
OptimalClusters

########################################################################
# Assigning cluster id for each row
ClusterData$ClusterID=OptimalClusters$cluster
head(ClusterData)

########################################################################
# Visualizing the clusters using first and third columns
# Choose those two columns which has highest range
# pch means plotting character
# cex means magnification factor

plot(ClusterData[,1], ClusterData[,2], col=ClusterData$ClusterID, pch='*', cex=2, legend=T)
legend("topleft",legend=c('Cluster-1', 'Cluster-2','Cluster-3', 'Cluster-4'),col=unique(ClusterData$ClusterID),cex=1,pch='*')


