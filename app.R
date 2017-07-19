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
library("shiny")

ui <- pageWithSidebar(
    headerPanel("antarcticR online"),
    
    sidebarPanel(
        HTML("Online visualiser for Antarctica."),
        width = 3
    ),
    
    mainPanel(
        
                                        # this is an extra div used ONLY to create positioned ancestor for tooltip
                                        # we don't change its position
        div(
            style = "position:relative",
            plotOutput("scatterplot", 
                       hover = hoverOpts("plot_hover", delay = 10, delayType = "debounce")),
            uiOutput("hover_info")
        ),
        width = 7
    )
)

server <- function(input, output) {

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

    antFrame
    
    ##worldBM <- plotAntarctica(antMap, antFrame, pointSize=5, shapes=FALSE, BEDMAP=TRUE,BEDMAP_GRAD="thickness") ## Doesnt work

    ## Replacement blah blah

    library(ggplot2)
    library(rgdal)
    print("Reading the map..")
    world <- map_data("world") 
            world2 <- ggplot() + 
            geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "white", color = "blue")
                                        # Rotate world map to focus on Antarctica
        print("Transform world map to focus on Antarctica")
        world3 <- world2 + coord_map("ortho", orientation=c(-90, 0, 0)) 
    testWorld <- world3 + geom_point(data = antFrame, aes(x = long, y = lat, col=lat), size=5)
    
    output$scatterplot <- renderPlot({
    ggplot(antFrame, aes(x=long, y = lat,color=lat)) +
    geom_point()
    })

    ##output$scatterplot <- renderPlot({
        ##testWorld
    ##})

    library(raster)
    
    BMgradient=raster("/home/berg/Dropbox/LinuxSync/PhD/ANITA/2017Stuff/clusterDir/antarcticR/data/bedmap2_bin/bedmap2_thickness.flt",xmn=-3333500, xmax=3333500, ymin=-3333500, ymax=3333500,crs=NA,template=NULL)

    BMgradient <- aggregate(BMgradient, fact=100, fun=max)

    p <- rasterToPoints(BMgradient)
    bmdf <- data.frame(p)
    colnames(bmdf) <- c("x", "y", "varFill")
    finalFrame <- longLatToSimpleBEDMAP(antFrame)
    bedMap <- ggplot(data=bmdf) + geom_tile(aes(x,y,fill=varFill)) +
        guides(fill=guide_legend(title="thickness"))

    worldBM <- bedMap +
        geom_point(data = finalFrame, aes(x = easting, y = northing), size=5, color="red")
    
    output$scatterplot <- renderPlot({
        worldBM
    })

    
### 
    ##    output$scatterplot <- renderPlot({
    ##       ggplot(antFrame, aes(x=long, y = lat,color=lat)) +
    ##          geom_point()
    ## })
    
    output$hover_info <- renderUI({        
        hover <- input$plot_hover
        
        point <- nearPoints(finalFrame, xvar=finalFrame$easting, yvar=finalFrame$northing, threshold=10000, maxpoints=1, hover, addDist = TRUE)
        
        if(0)
        {
            
        if (nrow(point) == 0) return(NULL)
        
        left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
        top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
        
                                        # calculate distance from left and bottom side of the picture in pixels
        left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
        top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
        
                                        # create style property fot tooltip
                                        # background color is set so tooltip is a bit transparent
                                        # z-index is set so we are sure are tooltip will be on top
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", left_px + 2, "px; top:", top_px + 2, "px;")
        
                                        # actual tooltip created as wellPanel
        wellPanel(
            style = style,
            p(HTML(paste0("<b> name: </b>", point$name, "<br/>",
                          "<b> Seasonality: </b>", point$seasonality, "<br/>",
                          "<b> long: </b>", point$easting, "<br/>",
                          "<b> lat: </b>", point$northing, "<br/>"))))
                }
    })
}

print("Processed code")

runApp(list(ui = ui, server = server))
