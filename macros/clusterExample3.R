### Quick example macro of how to run some antarcticR functions
require(antarcticR)
require(ggplot2)

## produce a latitude-longitude dataframe froma .csv file: used to look at the actually long-lat positions
dataFrame <- csvToDF("../../areaSigmaList.csv")

## generate a Haversine Matrix from the lat-long dataFrame
havMat <- genHaversineMat(dataFrame)
#havMat

                                        #antarcticMap <- drawAntarctica()
bedMap <- drawBedmap()
#antarcticMap

## Perform clustering
blob<- clusterResult(havMat, 5000000, 2, 70000) ## meters
#blob$clusterp

dataFrame$clust <- blob$cluster
                                        #dataFrame$clust

unclustered <- dataFrame[ which(dataFrame$clust == 0), ]
nrow(unclustered)

clustered <- dataFrame[ which(dataFrame$clust != 0), ]
                                        #unclustered

unclustered <- longLatToSimpleBEDMAP(unclustered)
clustered <- longLatToSimpleBEDMAP(clustered)

G7 <- bedMap +
    geom_point(data = unclustered, aes(x = easting, y = northing), size=4, colour = "green") +
    geom_point(data = clustered, aes(x = easting, y = northing), size=4, colour = "red")

G7

#mapWResults <- plotAntarctica(bedMap, dataFrame, clusterPlot=TRUE, pointSize = 8, shapes=TRUE, BEDMAP=TRUE)
#mapWResults
