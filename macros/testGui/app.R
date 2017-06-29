library(shiny)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage("BC Liquor Store", "prices")

server <- function(input, output, session) {
print(str(bcl))
}

shinyApp(ui = ui, server = server)
