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
blob<- clusterResult(havMat, 200000, 2, 50000)
#blob$clusterp

dataFrame$clust <- blob$cluster
                                        #dataFrame$clust

unclustered <- dataFrame[ which(dataFrame$clust == 0), ]
nrow(unclustered)

havVec <- as.vector(havMat)

finalList <- data.frame(havVec)

if(0)
{
    ggplot(data=finalList, aes(finalList)) +
        geom_histogram(breaks=seq(min(finalList),max(finalList), by=10000), col="blue") +
                                        #scale_y_log10(breaks=c(1,10,100,1000,10000)) +
        labs(title="Haversine distance histogram for all events") +
        labs(x="Haversine distance (m)", y="Counts") +
        theme(axis.title.y = element_text(size=25, family="Trebuchet MS")) +
        theme(plot.title = element_text(size=30, family="Trebuchet MS", face="bold", hjust=0)) +
        theme(axis.text = element_text(size=35, family="Trebuchet MS"),axis.title=element_text(size=25)) +
        xlim(c( min(finalList) , max(finalList) ))
}

unclustered <- dataFrame[ which(dataFrame$clust == 0), ]
#unclustered

mapWResults <- plotAntarctica(antarcticMap, dataFrame, clusterPlot=TRUE, BEDMAP=TRUE, pointSize = 5, shapes=FALSE)
mapWResults
