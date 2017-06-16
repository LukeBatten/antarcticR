library(rgdal)
library(raster)
library(maptools)
library(ggplot2)
library(antarcticR)

long <- c(166.8, -112.0, -34.62, 76.3869)
lat <- c(-77.84, -79.4, -77.87, -69.471)

myLongLatFrame <- data.frame(long, lat)

world4 <- plotAntarctica(antMap, myLongLatFrame, pointSize=5, shapes=FALSE, BEDMAP=TRUE,BEDMAP_GRAD="thickness")

world4
