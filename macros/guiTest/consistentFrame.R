    csvFile <- "~/Dropbox/LinuxSync/PhD/ANITA/baseListExtension/data/convertedFiles/baseListCSVs/base_list-A3-unrestricted.csv.0"

    points <- read.csv(csvFile, header=0, sep=",")
    df.points <- as.matrix(points)

antFrame <- data.frame(df.points)#long=df.points$long, lat=df.points$lat)

print("antFrame1")

### Attempt to add more base types    
    csvFile1 <- "~/Dropbox/LinuxSync/PhD/ANITA/baseListExtension/data/convertedFiles/baseListCSVs/base_list-A3-unrestricted.csv.1"
    points1 <- read.csv(csvFile1, header=0, sep=",")
    df.points1 <- as.matrix(points1)
    antFrame1 <- data.frame(df.points1)


blob <- data.frame(rbind(antFrame, antFrame1))
blob
