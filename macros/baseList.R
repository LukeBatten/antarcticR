#### Just for fun
#### Antarctica base list visulisation
#### To do:
## Custom markers
## Names / info / country etc above site

## Current stage:
## Importing csv to DF

require(antarcticR)
library(png)
library(ggplot2)
library(raster)
library(grid)

library("ggplot2")
library("ggimage")

csvFile <- "~/Dropbox/LinuxSync/PhD/ANITA/baseListExtension/data/convertedFiles/baseListCSVs/base_list-A3-unrestricted.csv.0"

#### Function here to turn lats + cardinalities into 

points <- read.csv(csvFile, header=0, sep=",")
df.points <- as.matrix(points)
antFrame = data.frame(df.points)#long=df.points$long, lat=df.points$lat)

colnames(antFrame) = c("name", "latDeg", "latMin", "latCar", "longDeg", "longMin", "longCar", "alt", "altCert", "primaryOperator", "est", "facType", "seasonality")

latDeg <- as.numeric(as.character(antFrame$latDeg))
latMin <- as.numeric(as.character(antFrame$latMin))
lat <- -latDeg - latMin/60

longDeg <- as.numeric(as.character(antFrame$longDeg))
longMin <- as.numeric(as.character(antFrame$longMin))
longCar <- as.character(antFrame$longCar)

if(longCar == "E")
{
    long <- longDeg + (longMin)/60;
} else## longCar == "W"
{
    long <- -longDeg - (longMin)/60;
}

antFrame <- data.frame(antFrame, lat, long)
antFrame  <- longLatToSimpleBEDMAP(antFrame)

antFrame[14,]

world4 <- plotAntarctica(antMap, antFrame, pointSize=5, shapes=FALSE, BEDMAP=TRUE,BEDMAP_GRAD="thickness")
world4

#### Then convert to Easting/Northing.

