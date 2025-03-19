rm(list=ls())
getwd()
setwd("C:/Users/lsethna_smm/Documents/GitHub/SaltyLakesShinyApp")

librarian::shelf(readxl,shiny,tidyverse)

waterchem <- read_excel("SaltyLakesWaterChem.xlsx",
                        col_types = c("text", "text", "date", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric"))
glimpse(waterchem)

variables <- colnames(waterchem)[4:13]

## create functions to generate plots ##

#time series plot
timeseries_plot <- function(data,variable) {
  
  p <- ggplot(data = data, aes(x = Date, y = .data[[variable]], color = Lake)) +
    geom_point(size = 3) +
    theme_classic()

  return(p)
}

ui <- fluidPage(
  titlePanel("Water chemistry data collected as part of the LCCMR - Salty Lakes project"),
  tabsetPanel(
    tabPanel("Site Locations",
             p("There are 12 study lakes located across the Twin Cities metro area."),
             imageOutput("map")
             ),
    tabPanel("Monitoring data exploration",
             p("Select the variable you want to plot over time:"),
             selectInput("variable",label="Variable",choices=variables),
             plotOutput("timeseriesplot")
             )
    ),
)


server <- function(input, output, session) {
  output$map <- renderImage({
    list(
      src = file.path("Picture1.png"),
      contentType = "png",
      width = 1000,
      height = 500
    )
  }, deleteFile = FALSE)
  
  #user_variable <- get(input$variable)
  #waterchem_plot <- waterchem %>% select(Lake,Depth,Date,user_variable)
  
  data <- reactive(waterchem)
  
  output$timeseriesplot <- renderPlot({
    timeseries_plot(data(),input$variable)
  })
}

shinyApp(ui, server)
