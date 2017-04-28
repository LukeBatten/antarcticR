### Quick example macro of how to run some antarcticR functions
require(antarcticR)

# produce a latitude-longitude dataframe froma .csv file: used to look at the actually long-lat positions
dataFrame <- csvToDF("../data/examplePoints.csv")

# generate a Haversine Matrix from the lat-long dataFrame
havMat <- genHaversineMat(dataFrame)

antarcticMap <- drawAntarctica()
antarcticMap

# Perform clustering
blob<- clusterResult(havMat, 200000, 2, 70000)
dataFrame$clust <- blob$cluster
#dataFrame$clust

map <- plotAntarctica(antarcticMap, dataFrame, cluster=FALSE, size=2)
map

#mapWResults <- plotAntarctica(antarcticMap, dataFrame, cluster=FALSE)
#mapWResults
