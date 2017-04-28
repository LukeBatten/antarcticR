### Quick example macro of plotting 2 sets of data and clustering them -individually-
### Individually as these two datasets are far enough apart that it won't affect clustering, but will make it run quicker!
require(antarcticR)

# produce a latitude-longitude dataframe froma .csv file: used to look at the actually long-lat positions
dataFrame1 <- csvToDF("../data/dividedEvents1.csv")
dataFrame2 <- csvToDF("../data/dividedEvents2.csv")

# generate a Haversine Matrix from the lat-long dataFrame
havMat1 <- genHaversineMat(dataFrame1)
havMat2 <- genHaversineMat(dataFrame2)

antarcticMap <- drawAntarctica()

# Perform clustering
clustered1<- clusterResult(havMat1, 200000, 2, 70000)
dataFrame1$clust <- clustered1$cluster

clustered2<- clusterResult(havMat2, 200000, 2, 70000)
dataFrame2$clust <- clustered2$cluster

mapWResults <- plotAntarctica(antarcticMap, dataFrame1, cluster=TRUE)
mapWResults2 <- plotAntarctica(mapWResults, dataFrame2, cluster=TRUE)
mapWResults2 
