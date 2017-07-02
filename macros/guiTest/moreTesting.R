library("dplyr")
library("ggplot2")
library("shiny")
library("antarcticR")
library("Cairo")

########## TO DO##
## Radio buttons for type of bedmap 2 data
#################

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
   
    column(1, 
      checkboxGroupInput("checkGroup", 
        label = h5("Base list"), 
        choices = list("ANITA-3 (2014-2015)" = 1, 
           "ANITA-4 (2016)" = 2),
        selected = 2)),
   
   column(2,
      radioButtons("radio", label = h5("Map choice"),
        choices = list("Ice thickness" = 1, "Bed" = 2,
                       "Surface" = 3, "Icemask" = 4), selected = 2)),
   
   column(3, 
      sliderInput("sliderRes", label = h5("Resolution reduction"),
        min = 1, max = 100, value = 5)
      )
   
) ## UI end

shinyServer <- function(input, output) {

    csvFile <- "~/Dropbox/LinuxSync/PhD/ANITA/baseListExtension/data/convertedFiles/baseListCSVs/base_list-A3-unrestricted.csv.0"
    csvFileA4 <- "~/Dropbox/LinuxSync/PhD/ANITA/baseListExtension/data/convertedFiles/baseListCSVs/base_list-A4-unrestricted.csv.0"

#### Function here to turn lats + cardinalities into

    a <- 2
    
    if(a == 1)
    {
        
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
    }

    if(a == 2)
    {

        points <- read.csv(csvFileA4, header=0, sep=",")
        df.points <- as.matrix(points)
        antFrame <- data.frame(df.points)#long=df.points$long, lat=df.points$lat)

        colnames(antFrame) = c("name", "latDeg", "latMin", "latCar", "longDeg", "longMin", "longCar", "alt", "altCert", "primaryOperator", "est", "facType", "seasonality", "current status")

        antFrame$latDeg <- as.numeric(as.character(antFrame$latDeg))
        antFrame$latMin <- as.numeric(as.character(antFrame$latMin))
        antFrame$lat <- -antFrame$latDeg - antFrame$latMin/60

        antFrame$longDeg <- as.numeric(as.character(antFrame$longDeg))
        antFrame$longMin <- as.numeric(as.character(antFrame$longMin))
        antFrame$longCar <- as.character(antFrame$longCar)

        antFrame <- mutate( antFrame, long = ifelse(longCar == "E", longDeg + (longMin)/60, -longDeg - (longMin)/60) )
        antFrame  <- longLatToSimpleBEDMAP(antFrame)
        
    }

    ####
    
    output$antarcticCanvas <- renderPlot({

        BMgradient=raster("/home/berg/Dropbox/LinuxSync/PhD/ANITA/2017Stuff/clusterDir/antarcticR/data/bedmap2_bin/bedmap2_thickness.flt",xmn=-3333500, xmax=3333500, ymin=-3333500, ymax=3333500,crs=NA,template=NULL)
        
        resolutionFactor <- input$sliderRes
        
        BMgradient <- aggregate(BMgradient, fact=resolutionFactor, fun=max)
        
        p <- rasterToPoints(BMgradient)
        bmdf <- data.frame(p)
        colnames(bmdf) <- c("bbb", "ccc", "varFillBBB")
        
        ggplot()+
            geom_point(data = antFrame, aes(x = easting, y = northing)) +
            geom_tile(data=bmdf,aes(bbb,ccc,fill=varFillBBB)) +
            geom_point(data = antFrame, aes(x = easting, y = northing), size=2, color="red") +
            guides(fill=guide_legend(title="ice thickness")) +
            coord_cartesian(xlim = ranges$easting, ylim = ranges$northing, expand = FALSE) ## Needed for zooming

    })

    output$text1 <- renderText({ 
        paste("You have selected", input$sliderRes)
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

        if(1)
        {
            
            if (nrow(point) == 0) return(NULL)
            
            left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
            top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
            
                                        # calculate distance from left and bottom side of the picture in pixels
            left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
            top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
            
                                        # create style property for tooltip
                                        # background color is set so tooltip is a bit transparent
                                        # z-index is set so we are sure are tooltip will be on top
            style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                            "left:", left_px + 2, "px; top:", top_px + 2, "px;")
            
                                        # actual tooltip created as wellPanel
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
        }
    })

    
    
    
} ## server end

print("Processed code")

runApp(list(ui = ui, server = shinyServer))
