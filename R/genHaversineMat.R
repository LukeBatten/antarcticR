#' A function to generate a Haversine matrix from a dataframe
#' Generate a distance matrix of great-circle distances from a dataframe with longitude and latitude distances 
#'
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
