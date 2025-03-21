rm(list=ls())
# getwd()
# setwd("C:/Users/lsethna_smm/Documents/GitHub/SaltyLakesShinyApp")

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

waterchem_v3 <- waterchem %>% 
  rename(chloride_mg_L="Cl- (mg/L)")

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
                 p("Select the variable you want to compare between lakes and depths:"),
                 selectInput("depth_variable",label="Variable",choices=variables),
                 p("Select lakes to compare:"),
                 selectInput("depth_lake",label="Lake",choices=lakes,multiple=T)
               ),
               mainPanel(
                 plotOutput("lake_depth_boxplot")
               )
             ), #close sidebar panel
             ),
    tabPanel("Exploring the role of road salt on nutrients",
             sidebarLayout(
               sidebarPanel(
                 p("Select the variable you want to plot in relation to chloride concentration:"),
                 varSelectInput(inputId = "y_variable_chloride",
                                label= "Variable",
                                data= waterchem_v3 %>% 
                                  select(`Chl-a (ug/L)`, 
                                         `SRP (ug P/L)`, 
                                         `Total Phosphorus (ug P/L)`,
                                         `Total Nitrogen (mg N/L)`,
                                         `NH4 (mg N/L)`,
                                         `NO3 + NO2 (mg N/L)`,
                                         `DIC (mg C/L)`,
                                         `DOC (mg C/L)`,
                                         `DSi (mg SiO2/L)`)) 
                 # p("Select the lakes you want to compare:"),
                 # selectInput("time_lake",label="Lake",choices=lakes,multiple=T)
               ),
               #plot variable time series
               mainPanel(
                 plotOutput("chloride_plot")
               )
             )
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
  
  depth_data <- reactive({
    waterchem_v2 %>%
      dplyr::filter(variable %in% input$depth_variable,
                    Lake %in% input$depth_lake)
  })
  
  output$lake_depth_boxplot <- renderPlot({
    ggplot(depth_data(),aes(y=value,x=Lake,color=Depth))+
      geom_boxplot()+
      theme_classic(base_size=14)
  })
  
  output$chloride_plot <- renderPlot({
    ggplot(waterchem_v3, aes(x=chloride_mg_L, y=!!input$y_variable_chloride))+
      geom_point()+
      theme_classic(base_size=14)
  })
  
}

shinyApp(ui, server)


