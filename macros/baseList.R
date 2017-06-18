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

campsStations <- csvToDF("~/Dropbox/LinuxSync/PhD/ANITA/baseListExtension/data/convertedFiles/baseListCSVs/base_list-A3-unrestricted.csv.0")

#### Function here to turn lats + cardinalities into 

campsStations

#### Then convert to Easting/Northing.
