### Project EASs onto continent. What % self-clustering do we see?
require(antarcticR)
require(ggplot2)

epsilon <- 40000

selfCluster = function(df)
{
    havMat <- genHaversineMat(df)

    blob<- clusterResult(havMat, 200000, 2, epsilon)
    df$clust <- blob$cluster
    ##antarcticMap <- drawAntarctica()
    
    unclustered <- df[ which(df$clust == 0), ]
    nonSelfClusteringPercentage <- nrow(unclustered)/nrow(df) * 100
}

determineSelfClusteringPerc = function(EASnum)
{
    NSCP <- 0
    NSCPtot <- 0
    start <- 0
    end <- 19 # max different seed

    for(i in start:end) # read in data gen'd from different seeds
    {

        name1 <- "../../EAScsvs/output_EAS"
        name2 <- "_seed"
        fileName <- paste0(name1,EASnum,name2,i,".csv")

        assign(paste0("dataFrame", i), csvToDF(fileName))
        assign(paste0("NSCP",i), selfCluster(get(paste0("dataFrame", i))))

        NSCPtot <- (NSCPtot + get(paste0("NSCP",i)))
    }

    return(NSCPtot/(end+1))
}

easStartVals <- c(1,10,25,50,75,100,300,500,700,1000)
##easStartVals <- c(1,10,25,50,75,100,300)

makeFinalFrame= function()
{
    selfClusteringResult = c()

    for(i in easStartVals)
    {
        selfClusteringResult <- append(selfClusteringResult, determineSelfClusteringPerc(i))   
    }

    finalDf <- data.frame(easStartVals, selfClusteringResult,epsilon)
    return(finalDf)
}

epsilon <- 20000
finalDf <- makeFinalFrame()

epsilon <- 30000
finalDf2 <- makeFinalFrame()

epsilon <- 40000
finalDf3 <- makeFinalFrame()

epsilon <- 50000
finalDf4 <- makeFinalFrame()

### Add Linda's approximate results as a dataframe ###
blobX <- c(10,25,50,75,100,300,500,700,1000)
blobY <- c(99.2,97,93,90,87,75,61,50,41)
finalDfLin <- data.frame(blobX, blobY)

finalPlot <- ggplot() +
    geom_line(data=finalDf, aes(x=easStartVals, y=selfClusteringResult,group=epsilon,color=factor(epsilon),size=1.25)) +
    geom_line(data=finalDf2, aes(x=easStartVals, y=selfClusteringResult,group=epsilon,color=factor(epsilon),size=1.25)) +
    geom_line(data=finalDf3, aes(x=easStartVals, y=selfClusteringResult,group=epsilon,color=factor(epsilon),size=1.25))+
    geom_line(data=finalDf4, aes(x=easStartVals, y=selfClusteringResult,group=epsilon,color=factor(epsilon),size=1.25))+
    geom_line(data=finalDfLin, aes(x=blobX, y=blobY,size=1.25))+ 
        ggtitle(bquote(list( "Non self-clustering efficiency vs EASs injected"))) +
        labs(x="EASs injected", y="Non self-clustering %") +
        theme(axis.title.y = element_text(size=25, family="Trebuchet MS")) +
        theme(plot.title = element_text(size=30, family="Trebuchet MS", face="bold", hjust=0)) +
    theme(axis.text = element_text(size=35, family="Trebuchet MS"),axis.title=element_text(size=25)) +
    theme(legend.justification=c(1,0), legend.position=c(1,0.7), legend.title=element_text(size=30), legend.text=element_text(size=30), legend.key = element_rect(size = 3), legend.key.size = unit(2.5, 'lines')) +
    guides(size = FALSE)
    
finalPlot
