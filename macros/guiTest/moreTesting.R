library(dplyr)
library(graphics)
library(ggplot2)
library(shiny)
library(antarcticR)
library(Cairo)
require(colorRamps)

ui <- fluidPage(

    headerPanel(HTML(paste("antarcticR",tags$sup("online")))),
    
    sidebarPanel(
        HTML("An online companion to the antarcticR package. Online visualiser for Antarctica."),
        a("Full repository here", href="https://github.com/LukeBatten/antarcticR", target="_blank"), ## Private current, so it won't load for other users
        width = 3
    ),   
    
    mainPanel(
        div(
            style = "position:relative",
            plotOutput("antarcticCanvas", 
                       hover = hoverOpts("plot_hover", delay = 10, delayType = "debounce"),
                       dblclick = "antarcticCanvas_dblclick",
                       brush = brushOpts(
                           id = "antarcticCanvas_brush",
                           resetOnNew = TRUE
                       )
                       ),
            uiOutput("hover_info")
        ),
        width = 7
    ),

    ##    column(1,
    ##     radioButtons("bList", label = h5("Base list"),
    ##            choices = list("ANITA-3 (2014-15)" = 1, "ANITA 4 (2017)" = 2), selected = 2)),

    column(1,
           radioButtons("colourGradient", label = h5("Colour gradient"),
                        choices = list("Original" = 1, "Ice" = 2, "Inverse ice" = 3,"Monochromatic" = 4), selected = 1)),
    
    column(2,
           radioButtons("bedmapChoice", label = h5("Map choice"),
                        choices = list("Ice thickness" = 1, "Bed" = 2,
                                       "Surface" = 3, "Icemask" = 4), selected = 1)),
    
    column(3, 
           sliderInput("sliderRes", label = h5("Resolution reduction"),
                       min = 2, max = 100, value = 20)
           )
    
) ## UI end

shinyServer <- function(input, output) {

    csvFile <- "~/Dropbox/LinuxSync/PhD/ANITA/baseListExtension/data/convertedFiles/baseListCSVs/base_list-A3-unrestricted.csv.0"
    points <- read.csv(csvFile, header=0, sep=",")
    df.points <- as.matrix(points)
    antFrame <- data.frame(df.points)
    
    ##Attempt to add more base types    
    csvFile1 <- "~/Dropbox/LinuxSync/PhD/ANITA/baseListExtension/data/convertedFiles/baseListCSVs/base_list-A3-unrestricted.csv.1"
    points1 <- read.csv(csvFile1, header=0, sep=",")
    df.points1 <- as.matrix(points1)
    antFrame1 <- data.frame(df.points1)

    csvFile2 <- "~/Dropbox/LinuxSync/PhD/ANITA/baseListExtension/data/convertedFiles/baseListCSVs/base_list-A3-unrestricted.csv.2"
    points2 <- read.csv(csvFile2, header=0, sep=",")
    df.points2 <- as.matrix(points2)
    antFrame2 <- data.frame(df.points2)
    blob <- data.frame(matrix(nrow=nrow(antFrame2),ncol=ncol(antFrame) - ncol(antFrame2)))
    blob <- transform(blob, X1 = ifelse(is.na(X1), as.character("Unknown"), X1))  ##altCert
    blob <- transform(blob, X2 = ifelse(is.na(X2), as.character("Unknown"), X2))  ##primOp
    blob <- transform(blob, X3 = ifelse(is.na(X3), as.character("Unknown"), X3))  ##est
    blob <- transform(blob, X4 = ifelse(is.na(X4), as.character("Fixed Wing"), as.character("Fixed Wing"))) ##facType
    blob <- transform(blob, X5 = ifelse(is.na(X5), as.character("Unknown"), X5))  ##seasonality
    ##Find a better method to do the above (??)
    colnames(blob) = c("V9","V10","V11","V12","V13")
    antFrame2 <- data.frame(antFrame2,blob)  ##Additional fake columns

    ## Inconsistent spreadsheet formatting with "millidegrees"
    ## Put csvFile3 here, come back to this
    csvFile3 <- "~/Dropbox/LinuxSync/PhD/ANITA/baseListExtension/data/convertedFiles/baseListCSVs/base_list-A3-unrestricted.csv.3"
    points3 <- read.csv(csvFile3, header=0, sep=",")
    df.points3 <- as.matrix(points3)
    antFrame3 <- data.frame(df.points3)## Site name,	Latitude (degrees)	Latitude (millidegrees)	Latitude (minutes)	Latitude (cardinality)	Longitude (degrees)	Longitude (millidegrees)	Longitude (minutes)	Longitude (cardinality)	Altitude above sea level (m)	Established	Current status

    antFrame3a <- data.frame(antFrame3[,1], antFrame3[,2], antFrame3[,4], antFrame3[,5],antFrame3[,6],antFrame3[,8],antFrame3[,9],antFrame3[,10])

    blob3 <- data.frame(matrix(nrow=nrow(antFrame3),ncol=2))
    blob3b <- data.frame(matrix(nrow=nrow(antFrame3),ncol=2))

    antFrame3 <- data.frame(antFrame3a,blob3,antFrame3[,10],blob3b)
    colnames(antFrame3) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13")

    antFrame3 <- transform(antFrame3, V9 = ifelse(is.na(V9), as.character("Unknown"), V9))  ##altCert
    antFrame3 <- transform(antFrame3, V10 = ifelse(is.na(V10), as.character("Unknown"), V10))  ##primOp
    antFrame3 <- transform(antFrame3, V12 = ifelse(is.na(V12), as.character("AWS"), as.character("AWS"))) ##facType
    antFrame3 <- transform(antFrame3, V13 = ifelse(is.na(V13), as.character("Unknown"), V13 ))  ##seasonality
    ##
    
    csvFile4 <- "~/Dropbox/LinuxSync/PhD/ANITA/baseListExtension/data/convertedFiles/baseListCSVs/base_list-A3-unrestricted.csv.4"
    points4 <- read.csv(csvFile4, header=0, sep=",")
    df.points4 <- as.matrix(points4)
    antFrame4 <- data.frame(df.points4)
    blob2 <- data.frame(matrix(nrow=nrow(antFrame4),ncol=ncol(antFrame) - ncol(antFrame4)))
    blob2 <- transform(blob2, X1 = ifelse(is.na(X1), as.character("Unknown"), X1))  ##altCert
    blob2 <- transform(blob2, X2 = ifelse(is.na(X2), as.character("Unknown"), X2))  ##primOp
    blob2 <- transform(blob2, X3 = ifelse(is.na(X3), as.character("Unknown"), X3))  ##est
    blob2 <- transform(blob2, X4 = ifelse(is.na(X4), as.character("BAS instruments"), as.character("BAS instruments"))) ##facType
    blob2 <- transform(blob2, X5 = ifelse(is.na(X5), as.character("Unknown"), X5))  ##seasonality
    ##Find a better method to do the above (??)
    colnames(blob2) = c("V9","V10","V11","V12","V13")
    antFrame4 <- data.frame(antFrame4,blob2)  ##Additional fake columns
    
    ## Combine into one for hovering function    
    antFrame <- data.frame(rbind(antFrame, antFrame1, antFrame2, antFrame3, antFrame4))

    colnames(antFrame) = c("name", "latDeg", "latMin", "latCar", "longDeg", "longMin", "longCar", "alt", "altCert", "primaryOperator", "est", "facType", "seasonality")

    antFrame$latDeg <- as.numeric(as.character(antFrame$latDeg))
    antFrame$latMin <- as.numeric(as.character(antFrame$latMin))
    antFrame$lat <- -antFrame$latDeg - antFrame$latMin/60

    antFrame$longDeg <- as.numeric(as.character(antFrame$longDeg))
    antFrame$longMin <- as.numeric(as.character(antFrame$longMin))
    antFrame$longCar <- as.character(antFrame$longCar)

    antFrame <- mutate( antFrame, long = ifelse(longCar == "E", longDeg + (longMin)/60, -longDeg - (longMin)/60) )
    antFrame  <- longLatToSimpleBEDMAP(antFrame)

    antFrame <- transform(antFrame, facType = ifelse(facType == "X", "Unknown", as.character(facType)))
    antFrame <- transform(antFrame, seasonality = ifelse(seasonality == "X", "Unknown" , as.character(seasonality)))
    antFrame <- transform(antFrame, est = ifelse(est == -999, "Unknown", as.character(est)))
    antFrame <- transform(antFrame, alt = ifelse(alt == -999, "Unknown", as.character(alt)))
    
#### ^ base selection
    
    output$antarcticCanvas <- renderPlot(        
    {

        if(input$bedmapChoice == 1)
        {
            BMgradient=raster("/home/berg/Dropbox/LinuxSync/PhD/ANITA/2017Stuff/clusterDir/antarcticR/data/bedmap2_bin/bedmap2_thickness.flt",xmn=-3333500, xmax=3333500, ymin=-3333500, ymax=3333500,crs=NA,template=NULL)
        }

        if(input$bedmapChoice == 2)
        {
            BMgradient=raster("/home/berg/Dropbox/LinuxSync/PhD/ANITA/2017Stuff/clusterDir/antarcticR/data/bedmap2_bin/bedmap2_bed.flt",xmn=-3333500, xmax=3333500, ymin=-3333500, ymax=3333500,crs=NA,template=NULL)
        }

        if(input$bedmapChoice == 3)
        {
            BMgradient=raster("/home/berg/Dropbox/LinuxSync/PhD/ANITA/2017Stuff/clusterDir/antarcticR/data/bedmap2_bin/bedmap2_surface.flt",xmn=-3333500, xmax=3333500, ymin=-3333500, ymax=3333500,crs=NA,template=NULL)
        }

        if(input$bedmapChoice == 4)
        {
            BMgradient=raster("/home/berg/Dropbox/LinuxSync/PhD/ANITA/2017Stuff/clusterDir/antarcticR/data/bedmap2_bin/bedmap2_icemask_grounded_and_shelves.flt",xmn=-3333500, xmax=3333500, ymin=-3333500, ymax=3333500,crs=NA,template=NULL)
        }
        
        resolutionFactor <- input$sliderRes
        
        BMgradient <- aggregate(BMgradient, fact=resolutionFactor, fun=max)
        
        p <- rasterToPoints(BMgradient)
        bmdf <- data.frame(p)
        colnames(bmdf) <- c("bbb", "ccc", "varFillBBB")

        if(input$colourGradient == 1){colorRemap <- scale_fill_gradient(low = "navyblue", high = "deepskyblue")}
        if(input$colourGradient == 2){colorRemap <- scale_fill_gradient(low = "dodgerblue1", high = "ghostwhite")}
        if(input$colourGradient == 3){colorRemap <- scale_fill_gradient(low = "snow", high = "dodgerblue1")}
        if(input$colourGradient == 4){colorRemap <- scale_fill_gradient(low = "white", high = "black")}

        ## raster ^
        
        ggplot()+
            geom_point(data = antFrame, aes(x = easting, y = northing)) +
            geom_tile(data=bmdf,aes(bbb,ccc,fill=varFillBBB)) +
            geom_point(data = antFrame, aes(x = easting, y = northing, color=facType, shape=facType), size=2, stroke=0.5) +
            scale_shape_manual(values=seq(1,100)) +
            colorRemap + ## Colour BEDMAP
            guides(fill=guide_legend(title="Gradient")) +
            coord_cartesian(xlim = ranges$easting, ylim = ranges$northing, expand = FALSE) ## Needed for zooming

    })

######

###### ZOOM in

    ranges <- reactiveValues(easting = NULL, northing = NULL)
    
    observeEvent(input$antarcticCanvas_dblclick, {
        brush <- input$antarcticCanvas_brush
        if (!is.null(brush)) {
            ranges$easting <- c(brush$xmin, brush$xmax)
            ranges$northing <- c(brush$ymin, brush$ymax)

        } else {
            ranges$easting <- NULL
            ranges$northing <- NULL
        }
    })
    
###### Hover

    output$hover_info <- renderUI({        
        hover <- input$plot_hover

        point <- nearPoints(antFrame, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
        
        if (nrow(point) == 0) return(NULL)
        
        left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
        top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
        
        left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
        top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
        
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", left_px + 2, "px; top:", top_px + 2, "px;")
        
        wellPanel(
            style = style,
            p(HTML(paste0("<b> Name: </b>", point$name, "<br/>",
                          "<b> Primary operator: </b>", point$primaryOperator, "<br/>",
                          "<b> Established: </b>", point$est, "<br/>",
                          "<b> Facility Type: </b>", point$facType, "<br/>",
                          "<b> Seasonality: </b>", point$seasonality, "<br/>",
                          "<b> Altitude: </b>", point$alt, "<br/>",
                          "<b> Longitude: </b>", point$long, "<br/>",
                          "<b> Latitude: </b>", point$lat, "<br/>"))))
        
    })
    
} ## server end

print("Processed code")

runApp(list(ui = ui, server = shinyServer))
