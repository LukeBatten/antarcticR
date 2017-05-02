### Quick example macro of plotting 2 sets of data and clustering them -individually-
### Individually as these two datasets are far enough apart that it won't affect clustering, but will make it run quicker!
### Explicitly goes through what clusters and what doesn't, then plots them with alphabetical markers, n sequnce

require(antarcticR)

# produce a latitude-longitude dataframe froma .csv file: used to look at the actually long-lat positions
dataFrame1 <- csvToDF("../data/dividedEvents1.csv")
dataFrame2 <- csvToDF("../data/dividedEvents2.csv")

# generate a Haversine Matrix from the lat-long dataFrame
havMat1 <- genHaversineMat(dataFrame1)
havMat2 <- genHaversineMat(dataFrame2)

antarcticMap <- drawAntarctica()

### Perform clustering
clustered1<- clusterResult(havMat1, 200000, 2, 70000)
dataFrame1$clust <- clustered1$cluster

clustered2<- clusterResult(havMat2, 200000, 2, 70000)
dataFrame2$clust <- clustered2$cluster

###

unclustered <- dataFrame1[ which(dataFrame1$clust == 0), ]
unclustered2 <- dataFrame2[ which(dataFrame2$clust == 0), ]

theseClustered <- dataFrame1[ which(dataFrame1$clust != 0), ]
uniqueClusters <- length(unique(theseClustered$clust))

theseClustered2 <- dataFrame2[ which(dataFrame2$clust != 0), ]
theseClustered2$clust <- theseClustered2$clust + uniqueClusters
#theseClustered2

print("Printing all candidate events:")
totalUnclustered <- rbind(unclustered,unclustered2)
totalUnclustered

totalClustered <- rbind(theseClustered, theseClustered2)

mapWResults <- plotAntarctica(antarcticMap, totalUnclustered, cluster=TRUE, shapes=TRUE, pointSize=5)

mapWResults2 <- plotAntarctica(mapWResults, totalClustered, cluster=TRUE, shapes=TRUE, pointSize=5)
mapWResults2
