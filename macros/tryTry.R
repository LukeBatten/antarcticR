require(antarcticR)
require(dbscan)
require(colorRamps)
require(ggplot2)
require(raster)

dataFrame <- csvToDF("../../areaSigmaList.csv")

## generate a Haversine Matrix from the lat-long dataFrame
havMat <- as.matrix(genHaversineMat(dataFrame))
diag(havMat) <- 99999
havMat <- apply(havMat,1,min)

dataFrame$minHav <- as.vector(havMat)

bedMap <- drawBedmap(reduceResolutionBy=10)

finalFrame <- longLatToSimpleBEDMAP(dataFrame)
finalFrame

G7 <- bedMap +
   geom_point(data = finalFrame, aes(x = easting, y = northing, size = minHav, colour = minHav)) +
    scale_fill_gradient(low="lightgrey",high="black") + ## This recolours the BEDMAP
    scale_colour_gradientn(colours=matlab.like2(100)) + ## This recolours the points themselves
   labs(size="Haversine distance", colour="Haversine distance")
G7

finalList <- data.frame(dataFrame$minHav)

max(finalList)

if(1)
{
    ggplot(data=finalList, aes(finalList)) +
        geom_histogram(breaks=seq(min(finalList),max(finalList+1), by=1000), col="blue") +
                                        ##scale_y_log10(breaks=c(10,100)) +
        labs(title="Haversine distance for each event") +
        labs(x="Haversine distance (m)", y="Counts") +
        theme(axis.title.y = element_text(size=25, family="Trebuchet MS")) +
        theme(plot.title = element_text(size=30, family="Trebuchet MS", face="bold", hjust=0)) +
        theme(axis.text = element_text(size=35, family="Trebuchet MS"),axis.title=element_text(size=25)) #+
        ###xlim(c( min(finalList) , max(finalList) ))
}
