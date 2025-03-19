rm(list=ls())
getwd()
setwd("C:/Users/lsethna_smm/Documents/GitHub/SaltyLakesShinyApp")

librarian::shelf(readxl,shiny,tidyverse)

waterchem <- read_excel("SaltyLakesWaterChem.xlsx",
                        col_types = c("text", "text", "date", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric")) %>% janitor::clean_names()
glimpse(waterchem)

variables <- colnames(waterchem)[4:13]


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
  
  output$timeseriesplot <- renderPlot({
   ggplot(waterchem,aes(x=date,y=input$variable))+
      geom_point()
  })
}

shinyApp(ui, server)
