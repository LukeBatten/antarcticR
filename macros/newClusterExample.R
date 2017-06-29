### Quick example macro of how to run some antarcticR functions
require(antarcticR)
library(ggplot2)

## produce a latitude-longitude dataframe froma .csv file: used to look at the actually long-lat positions
dataFrame <- csvToDF("../../areaSigmaList.csv")

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## generate a Haversine Matrix from the lat-long dataFrame
havMat <- genHaversineMat(dataFrame)
#havMat

testingMat <- as.matrix(havMat)

antarcticMap <- drawAntarctica()
#antarcticMap

## Perform clustering
blob<- clusterResult(havMat, 200000, 2, 80000)
#blob$clusterp

dataFrame$clust <- blob$cluster

unclustered <- dataFrame[ which(dataFrame$clust == 0), ]
#unclustered

mapWResults <- plotAntarctica(antarcticMap, dataFrame, clusterPlot=TRUE, BEDMAP=TRUE, pointSize = 7, shapes=TRUE,BEDMAP_GRAD="thickness",reduceResolutionBy=3)

mapWResults2 <- plotAntarctica(antarcticMap, dataFrame, clusterPlot=TRUE, BEDMAP=TRUE, pointSize = 7, shapes=TRUE,BEDMAP_GRAD="bed",reduceResolutionBy=3)

mapWResults4 <- plotAntarctica(antarcticMap, dataFrame, clusterPlot=TRUE, BEDMAP=TRUE, pointSize = 7, shapes=TRUE,BEDMAP_GRAD="surface",reduceResolutionBy=3)

mapWResults5 <- plotAntarctica(antarcticMap, dataFrame, clusterPlot=TRUE, BEDMAP=TRUE, pointSize = 7, shapes=TRUE,BEDMAP_GRAD="icemask",reduceResolutionBy=3)
multiplot(mapWResults,mapWResults2,mapWResults4,mapWResults5, cols=2)

##mapWResults
