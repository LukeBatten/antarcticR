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
library(shiny)
library(plotly)

library("ggplot2")
library("ggimage")
library("dplyr")

csvFile <- "~/Dropbox/LinuxSync/PhD/ANITA/baseListExtension/data/convertedFiles/baseListCSVs/base_list-A3-unrestricted.csv.0"

#### Function here to turn lats + cardinalities into 

points <- read.csv(csvFile, header=0, sep=",")
df.points <- as.matrix(points)
antFrame <- data.frame(df.points)#long=df.points$long, lat=df.points$lat)

colnames(antFrame) = c("name", "latDeg", "latMin", "latCar", "longDeg", "longMin", "longCar", "alt", "altCert", "primaryOperator", "est", "facType", "seasonality")

antFrame$latDeg <- as.numeric(as.character(antFrame$latDeg))
antFrame$latMin <- as.numeric(as.character(antFrame$latMin))
antFrame$lat <- -antFrame$latDeg - antFrame$latMin/60

antFrame$longDeg <- as.numeric(as.character(antFrame$longDeg))
antFrame$longMin <- as.numeric(as.character(antFrame$longMin))
antFrame$longCar <- as.character(antFrame$longCar)

antFrame <- mutate( antFrame, long = ifelse(longCar == "E", longDeg + (longMin)/60, -longDeg - (longMin)/60) )
antFrame  <- longLatToSimpleBEDMAP(antFrame)

antFrame[1:20,]

world4 <- plotAntarctica(antMap, antFrame, pointSize=5, shapes=FALSE, BEDMAP=TRUE,BEDMAP_GRAD="thickness")
##world4

#### Then convert to Easting/Northing.

