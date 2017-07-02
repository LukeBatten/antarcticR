library("dplyr")
library("ggplot2")
library("shiny")
library("raster")

ui <- fluidPage(
    
    mainPanel(
            plotOutput("canvasHere")
    ),
    
      sliderInput("sliderRes", label = h5("Resolution reduction"),
        min = 1, max = 100, value = 5)      
   
) ## UI end

shinyServer <- function(input, output) {
    
    BMgradient <- raster("/home/berg/Downloads/ETOPO2v2c_f4_LSB/ETOPO2v2c_f4_LSB.flt",crs=NA,template=NULL)
    
    ##resolutionFactor <- input$sliderRes
    resolutionFactor <- 5
    
    BMgradient <- aggregate(BMgradient, fact=resolutionFactor, fun=max)
    
    p <- rasterToPoints(BMgradient)
    bmdf <- data.frame(p)
    colnames(bmdf) <- c("bbb", "ccc", "varFillBBB")
    
    output$canvasHere <- renderPlot({
        ggplot()+
            geom_tile(data=bmdf,aes(bbb,ccc,fill=varFillBBB)) 

    })
}

print("Processed code")

runApp(list(ui = ui, server = shinyServer))
