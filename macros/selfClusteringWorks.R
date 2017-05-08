### Projection EASs onto continent. What % self-clustering do we see?
require(antarcticR)

dataFrame <- csvToDF("../../EAScsvs/output_EAS50_seed2.csv")

## generate a Haversine Matrix from the lat-long dataFrame
havMat <- genHaversineMat(dataFrame)
head(havMat)

antarcticMap <- drawAntarctica()
#antarcticMap

## Perform clustering
blob<- clusterResult(havMat, 200000, 2, 70000)

dataFrame$clust <- blob$cluster

unclustered <- dataFrame[ which(dataFrame$clust == 0), ]

selfClusteringPercentage <- nrow(unclustered)/nrow(dataFrame) * 100
selfClusteringPercentage

mapWResults <- plotAntarctica(antarcticMap, dataFrame, clusterPlot=TRUE, pointSize = 5, shapes=TRUE)
mapWResults
