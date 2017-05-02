### Quick example macro of how to run some antarcticR functions
require(antarcticR)

## produce a latitude-longitude dataframe from a .csv file: used to look at the actually long-lat positions
dataFrame <- csvToDF("../data/examplePoints.csv")

distMat <- genCartesianMat(dataFrame)
head(distMat)

antarcticMap <- drawAntarctica()

## Perform clustering
blob<- clusterResult(distMat, 200000, 2, 70000)

dataFrame$clust <- blob$cluster

mapWResults <- plotAntarctica(antarcticMap, dataFrame, clusterPlot=TRUE, pointSize = 5, shapes=TRUE)
mapWResults
