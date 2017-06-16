library(raster)
library(antarcticR)
library(ggplot2)

BMgradient=raster("../data/bedmap2_bin/bedmap2_thickness.flt",xmn=-3333500, xmax=3333500, ymin=-3333500, ymax=3333500,ncol=100,nrow=100)

BMgradient

BMgradient <- aggregate(BMgradient, fact=10, fun=max)

p <- rasterToPoints(BMgradient)
bmdf <- data.frame(p)
colnames(bmdf) <- c("x", "y", "varFill")
bedMap <- ggplot(data=bmdf) + geom_tile(aes(x,y,fill=varFill)) 

plot(bedMap)
