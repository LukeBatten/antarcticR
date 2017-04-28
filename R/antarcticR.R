#' A function to generate a Haversine matrix from a dataframe
#' 
#' Generate a distance matrix of great-circle distances from a dataframe with longitude and latitude distances 
#' @param df Your data frame
#' @return A haversine distance matrix
#' @keywords matrix, Haversine, dataframe, distance
#' @export
#' @examples
#' 
#' cities <- read.csv("dividedEvents1.csv",header=T, sep=",")
#' df.cities <- as.matrix(cities)
#' antFrame = data.frame(df.cities)#long=df.cities$long, lat=df.cities$lat)
#' antFrame
#' print("Computing distance matrix...")
#' require(geosphere)
#' d  <- genHaversineMat(antFrame)   # distance matrix
#'
#' genHaversineMat()

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

###
#' Set up the drawing of a map of Antarctica
#'
#' @return 
#' @keywords draw, Antarctica, plot
#' @export
#' @examples drawAntarctica
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

