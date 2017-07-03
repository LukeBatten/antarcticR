csvFile <- "~/Dropbox/LinuxSync/PhD/ANITA/baseListExtension/data/convertedFiles/baseListCSVs/base_list-A3-unrestricted.csv.0"
points <- read.csv(csvFile, header=0, sep=",")
df.points <- as.matrix(points)
antFrame <- data.frame(df.points)

csvFile2 <- "~/Dropbox/LinuxSync/PhD/ANITA/baseListExtension/data/convertedFiles/baseListCSVs/base_list-A3-unrestricted.csv.2"
points2 <- read.csv(csvFile2, header=0, sep=",")
df.points2 <- as.matrix(points2)
antFrame2 <- data.frame(df.points2)

blob <- data.frame(matrix(nrow=nrow(antFrame2),ncol=ncol(antFrame) - ncol(antFrame2)))
blob <- transform(blob, X1 = ifelse(is.na(X1), "Unknown", X1))
blob <- transform(blob, X2 = ifelse(is.na(X2), "Unknown", X2))
blob <- transform(blob, X3 = ifelse(is.na(X3), "Unknown", X3))
blob <- transform(blob, X4 = ifelse(is.na(X4), "Unknown", X4))
blob <- transform(blob, X5 = ifelse(is.na(X5), "Unknown", X5))
## Find a better method to do the above
colnames(blob) = c("V9","V10","V11","V12","V13")

antFrame3 <- data.frame(antFrame2,blob) ## Additional fake columns

antFrame4 <- data.frame(rbind(antFrame, antFrame3))
antFrame4
