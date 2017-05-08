### Project EASs onto continent. What % self-clustering do we see?

require(antarcticR)
require(ggplot2)

epsilon <- 40000

selfCluster = function(df)
{
    havMat <- genHaversineMat(df)

    blob<- clusterResult(havMat, 200000, 2, epsilon)
    df$clust <- blob$cluster
                                        #antarcticMap <- drawAntarctica()
    
    unclustered <- df[ which(df$clust == 0), ]
    nonSelfClusteringPercentage <- nrow(unclustered)/nrow(df) * 100
    
}

determineSelfClusteringPerc = function(EASnum)
{
    NSCP <- 0
    NSCPtot <- 0
    start <- 0
    end <- 9 # max different seed

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

plotResults = function()
{
    ##easStartVals <- c(1,10,25,50,75,100,300,500,700,1000)
    easStartVals <- c(1,10,25,50,75,100, 300)
    selfClusteringResult = c()

    for(i in easStartVals)
    {
        selfClusteringResult <- append(selfClusteringResult, determineSelfClusteringPerc(i))   
    }

    df.selfC <- data.frame(easStartVals, selfClusteringResult,epsilon)

    ggplot(data=df.selfC, aes(x=easStartVals, y=selfClusteringResult,group=epsilon)) +
        geom_line(color="#007FFF", size=1.75) +
    geom_point(color="#100202", size=3.5) +
    ggtitle(bquote(list( "Non-self clustering efficiency" , epsilon==.(epsilon)))) +
        labs(x="EASs inserted", y="Non self-clustering %") +
        theme(axis.title.y = element_text(size=25, family="Trebuchet MS")) +
        theme(plot.title = element_text(size=30, family="Trebuchet MS", face="bold", hjust=0)) +
        theme(axis.text = element_text(size=35, family="Trebuchet MS"),axis.title=element_text(size=25))
}

epsilon <- 20000
mainPlot <- plotResults()
mainPlot

