### Quick example macro of how to run some antarcticR functions
require(antarcticR)

## produce a latitude-longitude dataframe froma .csv file: used to look at the actually long-lat positions

finalMap = function(haversineMax)
{
    dataFrame <- csvToDF("../../areaSigmaList.csv")

    ## generate a Haversine Matrix from the lat-long dataFrame
    havMat <- genHaversineMat(dataFrame)
                                        #havMat
                                        #havMat

    antarcticMap <- drawAntarctica()
                                        #antarcticMap

    ## Perform clustering
    blob<- clusterResult(havMat, 1000000, 2, haversineMax)
                                        #blob$clusterp

    dataFrame$clust <- blob$cluster
                                        #dataFrame$clust

    unclustered <- dataFrame[ which(dataFrame$clust == 0), ]
    nrow(unclustered)

    mapWResults <- plotAntarctica(antarcticMap, dataFrame, clusterPlot=TRUE, pointSize = 5, shapes=TRUE)
    ##mapWResults
    write.png("map")

    return(mapWResults)
}

maxHavs <- seq(0,100000,10000)
clusterNumber <- sapply(maxHavs, attemptLoop)
clusterFrame <- data.frame(maxHavs, clusterNumber)


if(0)
{
    ggplot(clusterFrame, aes(x=maxHavs, y=clusterNumber)) +
        geom_line()+
                                        #geom_point(shape = 1, size = 5) +
        labs(title="Unclustered events vs Haversine distance") +
        labs(x="Haversine distance", y="Unclustered events") +
        theme(axis.title.y = element_text(size=25, family="Trebuchet MS")) +
        theme(plot.title = element_text(size=30, family="Trebuchet MS", face="bold", hjust=0)) +
        theme(axis.text = element_text(size=35, family="Trebuchet MS"),axis.title=element_text(size=25))
}
