### Quick example macro of how to run some antarcticR functions
require(antarcticR)

# produce a latitude-longitude dataframe froma .csv file: used to look at the actually long-lat positions
dataFrame1 <- csvToDF("../data/dividedEvents1.csv")
dataFrame2 <- csvToDF("../data/dividedEvents2.csv")

# generate a Haversine Matrix from the lat-long dataFrame
havMat1 <- genHaversineMat(dataFrame)
havMat2 <- genHaversineMat(dataFrame2)

antarcticMap <- drawAntarctica()

# Perform clustering
clustered<- clusterResult(havMat, 200000, 2, 70000)
dataFrame$clust <- clustered$cluster

clustered2<- clusterResult(havMat2, 200000, 2, 70000)
dataFrame2$clust2 <- clustered2$cluster2

mapWResults <- plotAntarctica(antarcticMap, dataFrame, cluster=TRUE)
mapWResults2 <- plotAntarctica(mapWResults, dataFrame2, cluster=TRUE)
