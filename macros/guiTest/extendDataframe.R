csvFile2 <- "~/Dropbox/LinuxSync/PhD/ANITA/baseListExtension/data/convertedFiles/baseListCSVs/base_list-A3-unrestricted.csv.2"
points2 <- read.csv(csvFile2, header=0, sep=",")
df.points2 <- as.matrix(points2)
##antFrame2
##antFrame2

## INITIALISE EMPTY DATA FRAME
## empty
blob <- data.frame(matrix(nrow=10,ncol=10))
blob
