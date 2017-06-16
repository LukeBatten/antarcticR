### Test macro for using the Xi steepness method
require(antarcticR)
require(dbscan)

## produce a latitude-longitude dataframe froma .csv file: used to look at the actually long-lat positions
##dataFrame <- csvToDF("../data/examplePointsSmall.csv")
dataFrame <- csvToDF("../../areaSigmaList.csv")

## generate a Haversine Matrix from the lat-long dataFrame
havMat <- genHaversineMat(dataFrame)
#havMat
#havMat

antarcticMap <- drawAntarctica()
#antarcticMap

## Perform clustering
res <- optics(havMat, 200000, 2)
res2 <- extractXi(res, xi=0.3)
plot(res2)

dataFrame$clust <- res2$cluster
                                        #dataFrame$clust

unclustered <- dataFrame[ which(dataFrame$clust == 0), ]
nrow(unclustered)


##mapWResults <- plotAntarctica(antarcticMap, dataFrame, clusterPlot=TRUE, pointSize = 5, shapes=TRUE)
mapWResults <- plotAntarctica(antarcticMap, dataFrame, clusterPlot=TRUE, pointSize = 5, shapes=TRUE)
#mapWResults
