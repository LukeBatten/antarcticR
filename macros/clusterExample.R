### Quick example macro of how to run some antarcticR functions
require(antarcticR)

## produce a latitude-longitude dataframe froma .csv file: used to look at the actually long-lat positions
dataFrame <- csvToDF("../../areaSigmaList.csv")

## generate a Haversine Matrix from the lat-long dataFrame
havMat <- genHaversineMat(dataFrame)
#havMat

testingMat <- as.matrix(havMat)

antarcticMap <- drawAntarctica()
#antarcticMap

## Perform clustering
blob<- clusterResult(havMat, 200000, 2, 80000)
#blob$clusterp

dataFrame$clust <- blob$cluster

unclustered <- dataFrame[ which(dataFrame$clust == 0), ]
#unclustered

mapWResults <- plotAntarctica(antarcticMap, dataFrame, clusterPlot=TRUE, BEDMAP=TRUE, pointSize = 7, shapes=TRUE,BEDMAP_GRAD="thickness",reduceResolutionBy=1.5)
mapWResults
