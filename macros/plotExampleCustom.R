### Quick example macro of how to run some antarcticR functions
require(antarcticR)
library(png)
library(ggplot2)
library(raster)
library(grid)

library("ggplot2")
library("ggimage")

### Map

BMgradient=raster("../data/bedmap2_bin/bedmap2_thickness.flt",xmn=-3333500, xmax=3333500, ymin=-3333500, ymax=3333500,crs=NA,template=NULL)
BMgradient <- aggregate(BMgradient, fact=10, fun=max)
p <- rasterToPoints(BMgradient)
bmdf <- data.frame(p)
colnames(bmdf) <- c("x", "y", "varFill")
bedMap <- ggplot(data=bmdf) + geom_tile(aes(x,y,fill=varFill))

### Geom points
long <- c(166.8, -112.0, -34.62, 76.3869)
lat <- c(-77.84, -79.4, -77.87, -69.471)
myLongLatFrame <- data.frame(long, lat)
finalFrame <- longLatToSimpleBEDMAP(myLongLatFrame)
finalFrame <- data.frame(finalFrame, image = "../img/transPlane.png",size=10, replace = TRUE)

long2 <- c(150.8, -100.0, -56.62, 100.3869)
lat2 <- c(-77.84, -79.4, -77.87, -69.471)
myLongLatFrame2 <- data.frame(long2, lat2)
finalFrame2 <- longLatToSimpleBEDMAP(myLongLatFrame2)
finalFrame2 <- data.frame(finalFrame2, image = "https://i1.wp.com/freepngimages.com/wp-content/uploads/2015/10/britishairwaysplane.png",size=10, replace = TRUE)

world4 <- bedMap +
    geom_image(data = finalFrame, aes(x = easting, y = northing, image=image))

world4 <- world4 + geom_image(data = finalFrame2, aes(x = easting, y = northing, image=image))

world4
