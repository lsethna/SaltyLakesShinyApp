rm(list=ls())
# getwd()
# setwd("C:/Users/lsethna_smm/Documents/GitHub/SaltyLakesShinyApp")

librarian::shelf(readxl,shiny,tidyverse)

#read in cleaned dataset
waterchem <- read_csv("Salty_2023_2024_monitoring_data_clean.csv") %>% 
  select(!`...1`)
glimpse(waterchem)

#convert to long format
waterchem_v2 <- tidyr::pivot_longer(waterchem,cols=c(6:15),names_to="variable") %>% mutate(value=as.numeric(value))

glimpse(waterchem_v2)

variables <- unique(waterchem_v2$variable)
lakes <- unique(waterchem_v2$lake)
depths <- unique(waterchem_v2$Depth)

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
                 selectInput("lake_chloride",label="Lake",choices=lakes,multiple=T),
                 selectInput("variable_chloride",label="Variable",choices=variables[1:9]), #limits choices to everything but Cl
                 p("Select what depth you want to look at:"),
                 checkboxGroupInput(inputId = "depth_chloride",
                                    label= "Depth",
                                    choices=depths)
               ), #close sidebar panel
               mainPanel(
                 plotOutput("chloride_plot")
               )
             ), #close sidebar layout
             sidebarLayout(
               sidebarPanel(
                 p("Select the variable you want to plot in relation to chloride concentration:"),
                 varSelectInput(inputId = "y_variable_chloride2",
                                label= "Variable",
                                data= waterchem %>%
                                  select(`Chl-a (ug/L)`,
                                         `SRP (ug P/L)`,
                                         `Total Phosphorus (ug P/L)`,
                                         `Total Nitrogen (mg N/L)`,
                                         `NH4 (mg N/L)`,
                                         `NO3 + NO2 (mg N/L)`,
                                         `DIC (mg C/L)`,
                                         `DOC (mg C/L)`,
                                         `DSi (mg SiO2/L)`),
                                selected = "Chl-a (ug/L)"),
                 ), #close sidebar panel
               mainPanel(
                 plotOutput("chloride_plot2")
                 )
               ) #close sidebar layout
             
             
    ) #close tab panel
  ) #close all panels
) #close UI

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
                    lake %in% input$time_lake)
  })
  
  output$timeseriesplot <- renderPlot({
    ggplot(time_data(),aes(x=Date,y=value,color=lake,shape=Depth))+
      geom_point(size=3)+
      theme_classic(base_size=14)
  })
  
  depth_data <- reactive({
    waterchem_v2 %>%
      dplyr::filter(variable %in% input$depth_variable,
                    lake %in% input$depth_lake)
  })
  
  output$lake_depth_boxplot <- renderPlot({
    ggplot(depth_data(),aes(y=value,x=lake,color=Depth))+
      geom_boxplot()+
      theme_classic(base_size=14)
  })
  
  chloride_data <- reactive({
    waterchem_v2 %>%
      pivot_wider(names_from=variable,values_from=value) %>%
      pivot_longer(!c(lake,Date,Depth,`Cl- (mg/L)`),names_to="variable") %>%
      dplyr::filter(lake %in% input$lake_chloride,
                    variable %in% input$variable_chloride,
                    Depth %in% input$depth_chloride) 
    
  })
  
  output$chloride_plot <- renderPlot({
    ggplot(chloride_data(),aes(x=`Cl- (mg/L)`,y=value,color=lake,shape=Depth))+
      geom_point(size=3)+
      theme_classic(base_size=14)
  })
  
  output$chloride_plot2 <- renderPlot({
    ggplot(data= waterchem,
           aes(x=`Cl- (mg/L)`, y=!!input$y_variable_chloride2, color=Depth))+
      geom_point()+
      facet_wrap(~Risk_Level, nrow=2) +
      theme_classic(base_size=14)
  })
  
}

shinyApp(ui, server)
