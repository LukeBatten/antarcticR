### Quick example macro of how to run some antarcticR functions
require(antarcticR)

## produce a latitude-longitude dataframe froma .csv file: used to look at the actually long-lat positions
dataFrame <- csvToDF("../../areaSigmaList.csv")

## generate a Haversine Matrix from the lat-long dataFrame
havMat <- genHaversineMat(dataFrame)
#havMat

antarcticMap <- drawAntarctica()
#antarcticMap

## Perform clustering
blob<- clusterResult(havMat, 5000000, 2, 100000) ## meters
#blob$clusterp

dataFrame$clust <- blob$cluster
                                        #dataFrame$clust

unclustered <- dataFrame[ which(dataFrame$clust == 0), ]
nrow(unclustered)

havVec <- as.vector(havMat)

finalList <- data.frame(havVec)
length(havVec)
#min(finalList)
#max(finalList)

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

mapWResults <- plotAntarctica(antarcticMap, dataFrame, clusterPlot=TRUE, pointSize = 8, shapes=TRUE)
mapWResults

testingMat <- as.matrix(havMat)
#testingMat/1000
finalListing2 <- testingMat[1,]
finalListing22 <- finalListing2[finalListing2>0]
finalList2 <- data.frame(finalListing22)

if(0)
{
    ggplot(data=finalList2, aes(finalList2)) +
        geom_histogram(breaks=seq(min(finalList2),max(finalList2), by=10000), col="blue") +
                                        #scale_y_log10(breaks=c(1,10,100,1000,10000)) +
        labs(title="Haversine distance histogram for a McMurdo event") +
        labs(x="Haversine distance (m)", y="Counts") +
        theme(axis.title.y = element_text(size=25, family="Trebuchet MS")) +
        theme(plot.title = element_text(size=30, family="Trebuchet MS", face="bold", hjust=0)) +
        theme(axis.text = element_text(size=35, family="Trebuchet MS"),axis.title=element_text(size=25)) +
        xlim(c( 0 , max(finalList2) ))
}
