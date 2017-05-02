## Quick example to add a value (100) to a column in a data frame
require(antarcticR)

dataFrame1 <- csvToDF("../data/examplePoints.csv")
havMat1 <- genHaversineMat(dataFrame1)
antarcticMap <- drawAntarctica()
clustered1<- clusterResult(havMat1, 200000, 2, 70000)
dataFrame1$clust <- clustered1$cluster

dataFrame1$clust <- dataFrame1$clust + 100

dataFrame1
