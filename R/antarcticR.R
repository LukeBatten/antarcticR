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
    
    x <-  6378137 *  cos(df[,2] * toRad) * cos(df[,1] * toRad)
    y <-  6378137 *  cos(df[,2] * toRad) * sin(df[,1] * toRad)
    z <-  6378137 * sin(df[,2] * toRad)
    
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
#' Set up the drawing of a map of Antarctica BEDMAP2
#'
#' @return 
#' @keywords draw, Antarctica, plot
#' @export
#' @examples world3 <- drawAntarctica()
#' world3


drawBedmap = function(BEDMAP_GRAD = "thickness",reduceResolutionBy=5)
{

    library(raster)
### BEDMAP2 options
    if(BEDMAP_GRAD=="bed")
    {
        BMgradient=raster("/home/berg/Dropbox/LinuxSync/PhD/ANITA/2017Stuff/clusterDir/antarcticR/data/bedmap2_bin/bedmap2_bed.flt",xmn=-3333500, xmax=3333500, ymin=-3333500, ymax=3333500,crs=NA,template=NULL)
    }
    
    if(BEDMAP_GRAD=="cov")
    {
        BMgradient=raster("/home/berg/Dropbox/LinuxSync/PhD/ANITA/2017Stuff/clusterDir/antarcticR/data/bedmap2_bin/bedmap2_coverage.flt",xmn=-3333500, xmax=3333500, ymin=-3333500, ymax=3333500,crs=NA,template=NULL)
    }

    if(BEDMAP_GRAD=="thickness")
    {
        BMgradient=raster("/home/berg/Dropbox/LinuxSync/PhD/ANITA/2017Stuff/clusterDir/antarcticR/data/bedmap2_bin/bedmap2_thickness.flt",xmn=-3333500, xmax=3333500, ymin=-3333500, ymax=3333500,crs=NA,template=NULL)
        
    }
    if(BEDMAP_GRAD=="surface")
    {
        BMgradient=raster("../data/bedmap2_bin/bedmap2_surface.flt",xmn=-3333500, xmax=3333500, ymin=-3333500, ymax=3333500,crs=NA,template=NULL)
    }
    if(BEDMAP_GRAD=="icemask")
    {
        BMgradient=raster("../data/bedmap2_bin/bedmap2_icemask_grounded_and_shelves.flt",xmn=-3333500, xmax=3333500, ymin=-3333500, ymax=3333500,crs=NA,template=NULL)
    }
    if(BEDMAP_GRAD=="rockmask")
    {
        BMgradient=raster("../data/bedmap2_bin/bedmap2_rockmask.flt",xmn=-3333500, xmax=3333500, ymin=-3333500, ymax=3333500,crs=NA,template=NULL)
    }
###

    if(reduceResolutionBy > 1)
    {        
        BMgradient <- aggregate(BMgradient, fact=reduceResolutionBy, fun=max)
    }
    
    p <- rasterToPoints(BMgradient)
    bmdf <- data.frame(p)
    colnames(bmdf) <- c("x", "y", "varFill")
    bedMap <- ggplot(data=bmdf) + geom_tile(aes(x,y,fill=varFill)) +
        guides(fill=guide_legend(title=BEDMAP_GRAD))
    
}

###
#' Plot points on the antarctic map 
#'
#' @param antMap your map made from drawAntarctica
#' @param df Your lon/lat data frame
#' @return 
#' @keywords draw, Antarctica, plotx
#' @export
#' @examples world4 <- plotAntarctica(map, dataFrame)
#' world4

plotAntarctica = function(antMap, df, clusterPlot=FALSE, selfClusterPlot=FALSE, pointSize=2, shapes=TRUE, newSetPlot=0, BEDMAP=FALSE, BEDMAP_GRAD = "thickness",reduceResolutionBy=5)
{
    library(raster)
### BEDMAP2 options
    if(BEDMAP_GRAD=="bed")
    {
        BMgradient=raster("../data/bedmap2_bin/bedmap2_bed.flt",xmn=-3333500, xmax=3333500, ymin=-3333500, ymax=3333500,crs=NA,template=NULL)
    }
    
    if(BEDMAP_GRAD=="cov")
    {
        BMgradient=raster("../data/bedmap2_bin/bedmap2_coverage.flt",xmn=-3333500, xmax=3333500, ymin=-3333500, ymax=3333500,crs=NA,template=NULL)
    }

    if(BEDMAP_GRAD=="thickness")
    {
        BMgradient=raster("../data/bedmap2_bin/bedmap2_bin/bedmap2_thickness.flt",xmn=-3333500, xmax=3333500, ymin=-3333500, ymax=3333500,crs=NA,template=NULL)
        
    }
    if(BEDMAP_GRAD=="surface")
    {
        BMgradient=raster("../data/bedmap2_bin/bedmap2_surface.flt",xmn=-3333500, xmax=3333500, ymin=-3333500, ymax=3333500,crs=NA,template=NULL)
    }
    if(BEDMAP_GRAD=="icemask")
    {
        BMgradient=raster("../data/bedmap2_bin/bedmap2_icemask_grounded_and_shelves.flt",xmn=-3333500, xmax=3333500, ymin=-3333500, ymax=3333500,crs=NA,template=NULL)
    }
    if(BEDMAP_GRAD=="rockmask")
    {
        BMgradient=raster("../data/bedmap2_bin/bedmap2_rockmask.flt",xmn=-3333500, xmax=3333500, ymin=-3333500, ymax=3333500,crs=NA,template=NULL)
    }
###

    if(reduceResolutionBy > 1)
        {
            BMgradient <- aggregate(BMgradient, fact=reduceResolutionBy, fun=max)
        }
    
    if(BEDMAP==TRUE)
    {
        p <- rasterToPoints(BMgradient)
        bmdf <- data.frame(p)
        colnames(bmdf) <- c("x", "y", "varFill")
        finalFrame <- longLatToSimpleBEDMAP(df)
        bedMap <- ggplot(data=bmdf) + geom_tile(aes(x,y,fill=varFill)) +
            guides(fill=guide_legend(title=BEDMAP_GRAD))
    }
    else
    {
        antMap <- drawAntarctica()
    }
    
    
#### STANDARD PLOT
    if(clusterPlot==FALSE & selfClusterPlot==FALSE)
    {
        if(BEDMAP==FALSE)
        {
            world4 <- antMap +
                geom_point(data = df, aes(x = long, y = lat), size=pointSize)
        }
        else
        {            
             world4 <- bedMap +
                geom_point(data = finalFrame, aes(x = easting, y = northing), size=pointSize, color="green")
                
        }
        
    }

#### CLUSTER PLOT
    else if(clusterPlot == TRUE)
    {
        if(shapes==FALSE)
        {
            if(BEDMAP==FALSE)
            {
                world4 <- antMap +
                    geom_point(data = df, aes(x = long, y = lat, color=factor(clust)), size=pointSize)            
            }
            else
            {
                world4 <- bedMap +
                    geom_point(data = finalFrame, aes(x = easting, y = northing, color=factor(clust)), size=pointSize)
            }
            
        }

        else if(shapes==TRUE)
        {
            if(BEDMAP==FALSE)
            {
                world4 <- antMap +
                    geom_point(data = df, aes(x = long, y = lat, color=factor(clust), shape=factor(clust)), size=pointSize)+
                    scale_shape_manual(values=seq(65+newSetPlot,165+newSetPlot))
            }
            else
            {
                world4 <- bedMap +
                    geom_point(data = finalFrame, aes(x = easting, y = northing, color=factor(clust), shape=factor(clust)), size=pointSize) +
                    scale_shape_manual(values=seq(65+newSetPlot,165+newSetPlot))
            }
            
        }

        else
        {
            print("This shape argument is not accepted!")
        }
    }

    else if(selfClusterPlot == TRUE) ## shows those which clustered and those which did not only
    {

        if(BEDMAP==FALSE)
        {
            unclustered <- df[ which(df$clust == 0), ]
            clustered <- df[ which(df$clust != 0), ]
            
            mapWResults <- antarcticMap +
                geom_point(data = clustered, aes(x = long, y = lat), color="dodgerblue1", size=pointSize) +
                geom_point(data = unclustered, aes(x = long, y = lat), size=pointSize)
        }
    }

    else
    {
        print("Too many plot types selected!")
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

############
############
### BEDMAP 2#
############
############
## Requires the binary BEDMAP data set
## We use the .flt files
##
##
##

###
#' A function to convert from lon/lat to the BEDMAP grid
#'
#' @param longLatDataFrame
#' @return bedmapFrame
#' @keywords draw, Antarctica, plot, BEDMAP
#' @export
#' @examples
#' 
longLatToSimpleBEDMAP = function(longLatDataFrame)
{
    library(rgdal)
    library(raster)
    library(maptools)
    library(ggplot2)
    
    scaleFactor <- 0.97276901289
    ellipsoid_inv_f <- 298.257223563
    eccentricity <- sqrt( (1/ellipsoid_inv_f) * (2-(1/ellipsoid_inv_f)) )
    r_Earth <- 6378140
    c_0 <- (2*r_Earth / sqrt(1- eccentricity^2)) * ((1-eccentricity)/(1+eccentricity))^(eccentricity/2)
    
    long <- longLatDataFrame$long
    lat <- longLatDataFrame$lat

    long_rad <- long * pi/180.
    lat_rad <- -lat * pi/180.
    Rfactor <- scaleFactor*c_0*   ((1+eccentricity*sin(lat_rad))/(1-eccentricity*sin(lat_rad)))^(eccentricity/2) * tan(pi/4 - lat_rad/2)
    easting <- Rfactor*sin(long_rad)
    northing <- Rfactor*cos(long_rad)

    bedmapFrame <- data.frame(longLatDataFrame, easting, northing)
    
    return(bedmapFrame)
}


### Plot on raster
### Plotting on a raster is fast, but does not allow the user to EXPAND the canvas
### If the canvas is extended, the raster points are no longer fixed.
### A better, but slower approach is the gg-tile approach
#### This function has now been removed.
