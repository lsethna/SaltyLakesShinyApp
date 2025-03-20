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

#convert to long format
waterchem_v2 <- tidyr::pivot_longer(waterchem,cols=c(4:13),names_to="variable")
glimpse(waterchem_v2)

variables <- unique(waterchem_v2$variable)
lakes <- unique(waterchem_v2$Lake)

ui <- fluidPage(
  titlePanel("Water chemistry data collected as part of the LCCMR - Salty Lakes project"),
  tabsetPanel(
    tabPanel("Site Locations",
             p("There are 12 study lakes located across the Twin Cities metro area."),
             imageOutput("map")
             ),
    tabPanel("Monitoring data exploration",
             #side bar layout allows you to add box on the side of the page, good for plotting
             sidebarLayout(
               sidebarPanel(
                 p("Select the variable you want to plot over time:"),
                 selectInput("time_variable",label="Variable",choices=variables),
                 p("Select the lakes you want to compare:"),
                 selectInput("time_lake",label="Lake",choices=lakes,multiple=T)
               ),
               #plot variable time series
               mainPanel(
                 plotOutput("timeseriesplot")
               )
             ), #close sidebar panel
             sidebarLayout(
               sidebarPanel(
                 p("Select the variable you want to compare between lake depths:"),
                 selectInput("depth_variable",label="Variable",choices=variables),
                 p("Select the lakes you want to compare:"),
                 selectInput("depth_lake",label="Lake",choices=lakes,multiple=T)
               ),
               mainPanel(
                 plotOutput("depth_boxplot")
               )
             ),
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
  
  time_data <- reactive({
    waterchem_v2 %>%
      dplyr::filter(variable %in% input$time_variable,
                    Lake %in% input$time_lake)
  })
  
  output$timeseriesplot <- renderPlot({
    ggplot(time_data(),aes(x=Date,y=value,color=Lake,shape=Depth))+
      geom_point(size=3)+
      theme_classic(base_size=14)
  })
}

shinyApp(ui, server)
