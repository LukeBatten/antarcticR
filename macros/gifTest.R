require(antarcticR)

doplot = function(haversineMax)
{

    dataFrame <- csvToDF("../../areaSigmaList.csv")

    ## generate a Haversine Matrix from the lat-long dataFrame
    havMat <- genHaversineMat(dataFrame)

    antarcticMap <- drawAntarctica()

    ## Perform clustering
    blob<- clusterResult(havMat, 1000000, 2, haversineMax)
    dataFrame$clust <- blob$cluster

    unclustered <- dataFrame[ which(dataFrame$clust == 0), ]

    plot.new()
    
    mapWResults <- plotAntarctica(antarcticMap, dataFrame, clusterPlot=TRUE, pointSize = 5, shapes=TRUE)

    #### Annotate with x=lon, y=lat
    mapWResults +
        annotate("text", x=0, y=-64, label = sprintf("Reachability distance = %d m", haversineMax), size = 10) +
        annotate("text", x=0, y=-68, label = sprintf("Unclustered events = %d", nrow(unclustered)), size = 10) 

    ##return(mapWResults)
    
}

if(0)
{
    X11()
    jpeg("test.jpeg", width = 16, height = 16, units = 'in', res = 300)
    doplot(10000)
    dev.off()
}

reachList <- c(0,1000,10000,20000,30000,40000,50000,100000,200000,300000,400000)

for (j in reachList)
{
    X11()
    name <- paste("plot",sprintf("%09.f", j),".jpeg", sep="")
    jpeg(name, width = 16, height = 16, units = 'in', res = 100)
    print(doplot(j))
    dev.off()
}

print("PROCESSING: Producing GIF")
system("convert -delay 100 -loop 0 *.jpeg clusterAnimNEW.gif")

print("Done!")
