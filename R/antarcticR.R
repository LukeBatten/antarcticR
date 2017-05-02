#' A function to generate a Haversine matrix from a dataframe
#' 
#' Generate a distance matrix of great-circle distances from a dataframe with longitude and latitude distances 
#' @param df Your data frame
#' @return A haversine distance matrix
#' @keywords matrix, Haversine, dataframe, distance
#' @export
#' @examples
#' 
#' points <- read.csv("dividedEvents1.csv",header=T, sep=",")
#' df.points <- as.matrix(points)
#' antFrame = data.frame(df.points)
#' print("Computing distance matrix...")
#' require(geosphere)
#' d  <- genHaversineMat(antFrame)
#'

genHaversineMat = function(df)
{
    require(geosphere)
    d <- function(i,z) #z[1:2] contain long, lat
    {          
        dist <- rep(0,nrow(z))
        dist[i:nrow(z)] <- distHaversine(z[i:nrow(z),1:2],z[i,1:2])
        return(dist)
    }
    dm <- do.call(cbind,lapply(1:nrow(df),d,df))
    return(as.dist(dm))
}

#' A function to generate a Cartesian matrix from a dataframe
#' 
#' Generate a distance matrix of x-y-z distances from a dataframe with longitude and latitude points
#' @param df Your data frame
#' @return A Cartesian distance matrix
#' @keywords matrix, Cartesian, dataframe, distance
#' @export
#' @examples
#' 
#' points <- read.csv("dividedEvents1.csv",header=T, sep=",")
#' df.points <- as.matrix(points)
#' antFrame = data.frame(df.points)
#' print("Computing distance matrix...")
#' d  <- genCartesianMatrix(antFrame)
#'

genCartesianMat = function(df)
{
    toRad <- pi / 180 
    
    x <-  6371000 *  cos(df[,2] * toRad) * cos(df[,1] * toRad)
    y <-  6371000 *  cos(df[,2] * toRad) * sin(df[,1] * toRad)
    z <-  6371000 * sin(df[,2] * toRad)
    
    xyzDataFrame <- data.frame(x,y,z)
    
    distMat <- dist(xyzDataFrame, method = "euclidean")
    
}

#' A function to generate a Haversine matrix from a csv file
#' 
#' Generate a distance matrix of great-circle distances from a csv file with longitude and latitude distances 
#' @param csvFile Your csv file
#' @return A haversine distance matrix
#' @keywords matrix, Haversine, csv, distance
#' @export
#' @examples
#' mat <- csvToHaversineMat("myData.csv")

csvToHaversineMat = function(csvFile)
{
    print("Reading the csv...")
    points <- read.csv(csvFile,header=T, sep=",")
    df.points <- as.matrix(points)
    antFrame = data.frame(df.points)
    print("Computing distance matrix...")
    d  <- genHaversineMat(antFrame)
}

#' Turn a longitude, latitude csv file into a dataframe
#' 
#' Generate a dataframe from a longitude-latitude csv file 
#' @param csvFile Your csv file
#' @return A dataframe
#' @keywords dataframe
#' @export
#' @examples
#' df <- csvToHaversineMat("myData.csv")

csvToDF = function(csvFile)
{
    points <- read.csv(csvFile,header=T, sep=",")
    df.points <- as.matrix(points)
    antFrame = data.frame(df.points)#long=df.points$long, lat=df.points$lat)
}

###
#' Set up the drawing of a map of Antarctica
#'
#' @return 
#' @keywords draw, Antarctica, plot
#' @export
#' @examples world3 <- drawAntarctica()
#' world3

drawAntarctica = function()
{
    print("Loading geo libraries")
    library(ggplot2)
    library(rgdal)
    print("Reading the map..")
    world <- map_data("world") 
    ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group)) +
        coord_fixed(1.3)
### Make it pretty
    world2 <- ggplot() + 
        geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "white", color = "blue") + 
        coord_fixed(1.3)
### Rotate world map to focus on Antarctica
    print("Transform world map to focus on Antarctica")
    world3 <- world2 + coord_map("ortho", orientation=c(-90, 0, 0)) 
}

###
#' Plot points on the antarctic map 
#'
#' @param antMap your map made from drawAntarctica
#' @param df Your data frame
#' @return 
#' @keywords draw, Antarctica, plotx
#' @export
#' @examples world4 <- plotAntarctica(map, dataFrame)
#' world4

plotAntarctica = function(antMap, df, clusterPlot=FALSE, pointSize=2, shapes=TRUE, newSetPlot=0)
{
    print("Plotting points")

#### STANDARD PLOT
    if(clusterPlot==FALSE)
    {
        world4 <- antMap +
            geom_point(data = df, aes(x = long, y = lat), size=pointSize)
    }

#### CLUSTER PLOT
    else
    {
        if(shapes==FALSE)
        {
            world4 <- antMap +
                geom_point(data = df, aes(x = long, y = lat, color=factor(clust)), size=pointSize)
        }

        else if(shapes==TRUE)
        {
            world4 <- antMap +
                geom_point(data = df, aes(x = long, y = lat, color=factor(clust), shape=factor(clust)), size=pointSize)+
                scale_shape_manual(values=seq(65+newSetPlot,122+newSetPlot))
                                        #scale_color_discrete("Cluster") ## comment this out as we are now using shapes too
        }

        else
        {
            print("This shape argument is not accepted!")
        }
    }
    
}

###
#' A function to use some clustering methods from the dbscan package 
#'
#' @param haversineMatrix
#' @return 
#' @keywords draw, Antarctica, plot
#' @export
#' @examples
#' 

clusterResult = function(haversineMatrix, eps=200000, minPts,eps_cl)
{
    library(dbscan)
    print("Employing the clustering algorithm...")

    res <- optics(haversineMatrix, eps, minPts)    
    res2 <- extractDBSCAN(res, eps_cl)

                                        #antFrame$clust <- res2$cluster
}
