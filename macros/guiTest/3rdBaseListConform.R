csvFile <- "~/Dropbox/LinuxSync/PhD/ANITA/baseListExtension/data/convertedFiles/baseListCSVs/base_list-A3-unrestricted.csv.0"
points <- read.csv(csvFile, header=0, sep=",")
df.points <- as.matrix(points)
antFrame <- data.frame(df.points)### Site name	Latitude (degrees)	Latitude (minutes)	Latitude (cardinality)	Longitude (degrees)	Longitude (minutes)	Longitude (cardinality)	Altitude above sea level (m)	Altitude certainty (Y/N)	Primary operator	Established	Facility Type	Seasonality

colnames(antFrame) = c("name", "latDeg", "latMin", "latCar", "longDeg", "longMin", "longCar", "alt", "altCert", "primaryOperator", "est", "facType", "seasonality")

csvFile3 <- "~/Dropbox/LinuxSync/PhD/ANITA/baseListExtension/data/convertedFiles/baseListCSVs/base_list-A3-unrestricted.csv.3"
points3 <- read.csv(csvFile3, header=0, sep=",")
df.points3 <- as.matrix(points3)
antFrame3 <- data.frame(df.points3)## Site name,	Latitude (degrees)	Latitude (millidegrees)	Latitude (minutes)	Latitude (cardinality)	Longitude (degrees)	Longitude (millidegrees)	Longitude (minutes)	Longitude (cardinality)	Altitude above sea level (m)	Established	Current status

##antFrame3 <- data.frame(antFrame3[,1], antFrame3[,2], antFrame3[,4], antFrame3[,5],antFrame3[,6],antFrame3[,8],antFrame3[,9],antFrame3[,10],antFrame3[,11],antFrame3[,12]) ## pick the vars we want
##colnames(antFrame3) = c("name", "latDeg", "latMin", "latCar", "longDeg", "longMin", "longCar", "alt", "est","status")
##antFrame3
### This is how it should be, but it should conform to the normal dataframe layout

antFrame3a <- data.frame(antFrame3[,1], antFrame3[,2], antFrame3[,4], antFrame3[,5],antFrame3[,6],antFrame3[,8],antFrame3[,9],antFrame3[,10])

blob3 <- data.frame(matrix(nrow=nrow(antFrame3),ncol=2))
blob3b <- data.frame(matrix(nrow=nrow(antFrame3),ncol=2))

antFrame3 <- data.frame(antFrame3a,blob3,antFrame3[,10],blob3b)
colnames(antFrame3) = c("name", "latDeg", "latMin", "latCar", "longDeg", "longMin", "longCar", "alt", "altCert", "primaryOperator", "est", "facType", "seasonality")

antFrame3 <- transform(antFrame3, altCert = ifelse(is.na(altCert), as.character("Unknown"), altCert))  ##altCert
antFrame3 <- transform(antFrame3, primaryOperator = ifelse(is.na(primaryOperator), as.character("Unknown"), primaryOperator))  ##primOp
antFrame3 <- transform(antFrame3, facType = ifelse(is.na(facType), as.character("AWS"), as.character("AWS"))) ##facType
antFrame3 <- transform(antFrame3, seasonality = ifelse(is.na(seasonality), as.character("Unknown"), seasonality ))  ##seasonality

antFrame3
