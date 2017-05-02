### Quick example macro of how to run some antarcticR functions
require(antarcticR)

## produce a latitude-longitude dataframe from a .csv file: used to look at the actually long-lat positions
dataFrame <- csvToDF("../data/examplePoints.csv")

toRad <- pi / 180 

x <-  6371000 *  cos(dataFrame[,2] * toRad) * cos(dataFrame[,1] * toRad)
y <-  6371000 *  cos(dataFrame[,2] * toRad) * sin(dataFrame[,1] * toRad)
z <-  6371000 * sin(dataFrame[,2] * toRad)
eventNumber <- dataFrame[,3]

xyzDataFrame <- data.frame(x,y,z)

distMat <- dist(xyzDataFrame, method = "euclidean")
head(distMat)

antarcticMap <- drawAntarctica()

## Perform clustering
blob<- clusterResult(distMat, 200000, 2, 70000)

dataFrame$clust <- blob$cluster
#dataFrame$clust

mapWResults <- plotAntarctica(antarcticMap, dataFrame, clusterPlot=TRUE, pointSize = 5, shapes=TRUE)
mapWResults
