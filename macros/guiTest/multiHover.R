library("dplyr")
library("ggplot2")
library("shiny")
library("antarcticR")
library("Cairo")

ui <- fluidPage(
    
    ##headerPanel(HTML(paste("antarcticR",tags$sup("online")))),

    headerPanel(HTML(paste("antarcticR",tags$sup("offline")))), ### Set so flight attendants don't think I'm on the internet
    
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
    )
    
) ## UI end

shinyServer <- function(input, output) {

    #### Var selection 1
    
    csvFile <- "~/Dropbox/LinuxSync/PhD/ANITA/baseListExtension/data/convertedFiles/baseListCSVs/base_list-A3-unrestricted.csv.0"
    points <- read.csv(csvFile, header=0, sep=",")
    df.points <- as.matrix(points)
    antFrame <- data.frame(df.points)

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
    antFrame <- transform(antFrame, est = ifelse(est == -999, "Unknown", as.numeric(est)))
    antFrame <- transform(antFrame, alt = ifelse(alt == -999, "Unknown", as.numeric(alt)))

### Var selection 2

    csvFile <- "~/Dropbox/LinuxSync/PhD/ANITA/baseListExtension/data/convertedFiles/baseListCSVs/base_list-A3-unrestricted.csv.2"
    points2 <- read.csv(csvFile, header=0, sep=",")
    df.points2 <- as.matrix(points2)
    antFrame2 <- data.frame(df.points2)

    colnames(antFrame2) = c("name", "latDeg", "latMin", "latCar", "longDeg", "longMin", "longCar", "alt")
    antFrame2$latDeg <- as.numeric(as.character(antFrame2$latDeg))
    antFrame2$latMin <- as.numeric(as.character(antFrame2$latMin))
    antFrame2$lat <- -antFrame2$latDeg - antFrame2$latMin/60

    antFrame2$longDeg <- as.numeric(as.character(antFrame2$longDeg))
    antFrame2$longMin <- as.numeric(as.character(antFrame2$longMin))
    antFrame2$longCar <- as.character(antFrame2$longCar)

    antFrame2 <- mutate( antFrame2, long = ifelse(longCar == "E", longDeg + (longMin)/60, -longDeg - (longMin)/60) )
    antFrame2  <- longLatToSimpleBEDMAP(antFrame2)
    
#### ^ base selection
    
    output$antarcticCanvas <- renderPlot({

        BMgradient=raster("/home/berg/Dropbox/LinuxSync/PhD/ANITA/2017Stuff/clusterDir/antarcticR/data/bedmap2_bin/bedmap2_thickness.flt",xmn=-3333500, xmax=3333500, ymin=-3333500, ymax=3333500,crs=NA,template=NULL)
        
        resolutionFactor <- 20
        
        BMgradient <- aggregate(BMgradient, fact=resolutionFactor, fun=max)
        
        p <- rasterToPoints(BMgradient)
        bmdf <- data.frame(p)
        colnames(bmdf) <- c("bbb", "ccc", "varFillBBB")

        ## raster ^
        
        ggplot()+
            geom_point(data = antFrame, aes(x = easting, y = northing)) +
            geom_tile(data=bmdf,aes(bbb,ccc,fill=varFillBBB)) +
            geom_point(data = antFrame, aes(x = easting, y = northing, color=facType), size=2) +
            geom_point(data = antFrame2, aes(x = easting, y = northing), size=2) +
            guides(fill=guide_legend(title="Gradient"))

    })
    
###### Hover

    output$hover_info <- renderUI({        
        hover <- input$plot_hover
        hover2 <- input$plot_hover

        point <- nearPoints(antFrame, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
        point2 <- nearPoints(antFrame2, hover2, threshold = 5, maxpoints = 1, addDist = TRUE)
                
        left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
        top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
        left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
        top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

        left_pct2 <- (hover2$x - hover2$domain$left) / (hover2$domain$right - hover2$domain$left)
        top_pct2 <- (hover2$domain$top - hover2$y) / (hover2$domain$top - hover2$domain$bottom)
        left_px2 <- hover2$range$left + left_pct2 * (hover2$range$right - hover2$range$left)
        top_px2 <- hover2$range$top + top_pct2 * (hover2$range$bottom - hover2$range$top)
        
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

    wellPanel(
            style = style,
            p(HTML(paste0("<b> Name: </b>", point2$name, "<br/>",
                          "<b> Altitude: </b>", point2$alt, "<br/>",
                          "<b> Longitude: </b>", point2$long, "<br/>",
                          "<b> Latitude: </b>", point2$lat, "<br/>"))))

        ## Need some sort of conditional to decide which wellpanel to pop up
        
    })
    
    
} ## server end

print("Processed code")

runApp(list(ui = ui, server = shinyServer))
