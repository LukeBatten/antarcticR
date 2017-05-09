### Quick example plot macro for singlets, doublets and triplets
require(antarcticR)

# produce a latitude-longitude dataframe froma .csv file: used to look at the actually long-lat positions
dataFrame <- csvToDF("../../EAScsvs/output_EAS1000_seed9.csv")

havMat <- genHaversineMat(dataFrame)
head(havMat)

antarcticMap <- drawAntarctica()
#antarcticMap

## Perform clustering
blob<- clusterResult(havMat, 200000, 4, 40000)

dataFrame$clust <- blob$cluster
                                        #dataFrame$clust
unclustered <- dataFrame[ which(dataFrame$clust == 0), ]
clustered <- dataFrame[ which(dataFrame$clust != 0), ]

mapWResults <- antarcticMap +
    geom_point(data = unclustered, aes(x = long, y = lat), size=2) +
    geom_point(data = clustered, aes(x = long, y = lat, color=factor(clust)), size=2)

mapWResults
