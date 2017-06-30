library("dplyr")
library("ggplot2")
library("shiny")
library("antarcticR")
library("Cairo")

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
      radioButtons("radio", label = h5("Map choice"),
        choices = list("Ice thickness" = 1, "Bed" = 2,
                       "Surface" = 3, "Icemask" = 4), selected = 1)),

   
   column(2, 
      sliderInput("sliderRes", label = h5("Resolution reduction"),
        min = 1, max = 100, value = 5)
      )
   
) ## UI end

shinyServer <- function(input, output) {


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

    BMgradient=raster("/home/berg/Dropbox/LinuxSync/PhD/ANITA/2017Stuff/clusterDir/antarcticR/data/bedmap2_bin/bedmap2_thickness.flt",xmn=-3333500, xmax=3333500, ymin=-3333500, ymax=3333500,crs=NA,template=NULL)
    
    ##resolutionFactor <- input$sliderRes ## The best is 1, but this is mega slow
    
    ##resolutionFactor <- input$sliderRes
    
    resolutionFactor <- 5
    
    BMgradient <- aggregate(BMgradient, fact=resolutionFactor, fun=max)
    
    p <- rasterToPoints(BMgradient)
    bmdf <- data.frame(p)
    colnames(bmdf) <- c("bbb", "ccc", "varFillBBB")
    
    output$antarcticCanvas <- renderPlot({
        ggplot()+
            geom_point(data = antFrame, aes(x = easting, y = northing), size=input$sliderRes, color="red") +
            geom_tile(data=bmdf,aes(bbb,ccc,fill=varFillBBB)) +
            geom_point(data = antFrame, aes(x = easting, y = northing), size=input$sliderRes, color="red") +
          guides(fill=guide_legend(title="ice thickness"))

    })

} ## server end

print("Processed code")

runApp(list(ui = ui, server = shinyServer))
