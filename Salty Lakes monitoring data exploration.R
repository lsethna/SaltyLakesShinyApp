### -------------------------------------------------------------------------------------------------- ###
### -------------------------------------------  Housekeeping ---------------------------------------- ###
### -------------------------------------------------------------------------------------------------- ###

rm(list=ls())
# getwd()
# setwd("C:/Users/lsethna_smm/Documents/GitHub/SaltyLakesShinyApp")

librarian::shelf(readxl,zoo,shiny,tidyverse,tidyr)

#read in cleaned dataset
waterchem <- read_csv("Salty_2023_2024_monitoring_data_clean.csv") %>% 
  select(!`...1`)
glimpse(waterchem)

#plot data to check for outliers
waterchem %>% tidyr::pivot_longer(cols=c(6:15),names_to="variable") %>% mutate(value=as.numeric(value)) %>%
  ggplot(aes(x=Date,y=value))+
  geom_point()+
  facet_wrap(~variable,scales="free")
  
# remove outliers
# Consider these columns for outlier removal 
cols_to_consider <- colnames(waterchem)[6:15]
sd_limit <- 3 #will count anything with a sd>3 to be an outlier
#create function that filters outliers based on sd_limit
remove_outlier_values <- function(data_to_filter, cols = cols_to_consider, limit = sd_limit){
  # Copy the data to avoid modifying the original
  data_filtered <- data_to_filter
    # Loop through each specified column
  for (col in cols) {
    # Compute z-scores
    z_scores <- abs((data_to_filter[[col]] - mean(data_to_filter[[col]], na.rm = TRUE)) / 
                      sd(data_to_filter[[col]], na.rm = TRUE))
        # Replace values exceeding the limit with NA
    data_filtered[[col]][z_scores > limit] <- NA
  }
    return(data_filtered)
}
#run function to remove outliers from data columns
waterchem_v2 <- remove_outlier_values(waterchem)

#check outlier removal
waterchem_v2 %>% tidyr::pivot_longer(cols=c(6:15),names_to="variable") %>% mutate(value=as.numeric(value)) %>%
  ggplot(aes(x=Date,y=value))+
  geom_point()+
  facet_wrap(~variable,scales="free")

#convert to long format
waterchem_long <- tidyr::pivot_longer(waterchem_v2,cols=c(6:15),names_to="variable") %>% mutate(value=as.numeric(value))
glimpse(waterchem_long)

variables <- unique(waterchem_long$variable)
lakes <- unique(waterchem_long$lake)
depths <- unique(waterchem_long$Depth)

### -------------------------------------------------------------------------------------------------- ###
### ------------------------------------- Set up User Interface -------------------------------------- ###
### -------------------------------------------------------------------------------------------------- ###

ui <- fluidPage(
  titlePanel("Water chemistry data collected as part of the LCCMR - Salty Lakes project"),
  tabsetPanel(
    tabPanel("Site Locations",
             p("The Salty Lakes project monitored 12 lakes within the Twin Cities Metro area and 3 lakes outside the city of Alexandria.
                Each of the study lakes represented a gradient of salinity and impairment risk due to salt.
                The data shown as part of this project are from water quality monitoring efforts between June 2023 and January 2025 and help scientists understand how road salt impacts lake water quality and ecosystem function."),
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
             ), #close sidebar layout
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
             ) #close sidebar layout
    ),
    tabPanel("Exploring the role of road salt across lakes, regions, and risk levels",
             sidebarLayout(
               sidebarPanel(
                 p("Select the variable you want to plot in relation to chloride concentration:"),
                 selectInput("lake_chloride",label="Lake",choices=lakes,multiple=T),
                 selectInput("variable_chloride",label="Variable", choices=variables[1:9]), #limits choices to everything but Cl
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
                 p("Select the variable you want to compare between risk levels:"),
                 selectInput("y_variable_risk_level",label="Variable",choices=variables),
               ),
               mainPanel(
                 plotOutput("risk_level_boxplot")
               )
             ), #close sidebar layout
             sidebarLayout(
               sidebarPanel(
                 p("Select the variable you want to plot in relation to chloride concentration:"),
                 selectInput("y_variable_chloride2",label= "Variable",choices=variables[1:9]),
               ), #close sidebar panel
               mainPanel(
                 plotOutput("chloride_plot2")
               )
             ), #close sidebar layout
             sidebarLayout(
               sidebarPanel(
                 p("Select the variable you want to plot:"),
                 selectInput("region_variable",label="Variable",choices=variables),
               ),
               mainPanel(
                 plotOutput("region_boxplot")
               )
             ) #close sidebar layout
    ), #close tab panel
    tabPanel("Exploring the role of road salt by season",
             sidebarLayout(
               sidebarPanel(
                 p("Select what lake(s) to look at:"),
                 selectInput("chloride_lake",label="Lake",choices=lakes,multiple=T)
               ),
               mainPanel(
                 plotOutput("chloride_timeseries")
               )
             ), #close sidebar layout
             sidebarLayout(
               sidebarPanel(
                 p("Select the variable you want to plot over time:"),
                 selectInput("season_variable",label="Variable",choices=variables),
                 p("Select what lake(s) to look at:"),
                 selectInput("season_lake",label="Lake",choices=lakes,multiple=T)
               ),
               mainPanel(
                 plotOutput("season_timeseries")
               )
             ) #close sidebar layout
    )
  ) #close all panels
) #close UI

### -------------------------------------------------------------------------------------------------- ###
### ----------------------------------------- Set up Server ------------------------------------------ ###
### -------------------------------------------------------------------------------------------------- ###

server <- function(input, output, session) {
  output$map <- renderImage({
    list(
      src = file.path("MinnesotaMap_SaltyLakes_highlight.png"),
      contentType = "png",
      width = 808.5,
      height = 722.7
    )
  }, deleteFile = FALSE)
  
  time_data <- reactive({
    waterchem_long %>%
      dplyr::filter(variable %in% input$time_variable,
                    lake %in% input$time_lake)
  })
  
  output$timeseriesplot <- renderPlot({
    ggplot(time_data(),aes(x=Date,y=value,color=lake,shape=Depth))+
      geom_point(size=3)+
      theme_classic(base_size=14)
  })
  
  depth_data <- reactive({
    waterchem_long %>%
      dplyr::filter(variable %in% input$depth_variable,
                    lake %in% input$depth_lake)
  })
  
  output$lake_depth_boxplot <- renderPlot({
    ggplot(depth_data(),aes(y=value,x=lake,fill=Depth))+
      geom_boxplot()+
      scale_fill_manual(values=c("#c1e7ff","#004c6d"))+
      theme_classic(base_size=14)
  })
  
  chloride_data <- reactive({
    waterchem_long %>%
      pivot_wider(names_from=variable,values_from=value,values_fn=mean) %>%
      pivot_longer(!c(lake,Risk_Level,Region,Date,Depth,`Cl- (ug/L)`),names_to="variable") %>%
      dplyr::filter(lake %in% input$lake_chloride,
                    variable %in% input$variable_chloride,
                    Depth %in% input$depth_chloride) 
    
  })
  output$chloride_plot <- renderPlot({
    ggplot(chloride_data(),aes(x=`Cl- (ug/L)`,y=value,color=lake,shape=Depth))+
      geom_point(size=3)+
      theme_classic(base_size=14)
  })
  
  region_data <- reactive({
    waterchem_long %>%
      mutate(Region = fct_relevel(Region, 
                                  "North Metro", "East Metro", "South Metro", "West Metro", "Central MN"),
             Risk_Level = fct_relevel(Risk_Level, 
                                      "Impacted", "At Risk", "Least Impacted")) %>%
      dplyr::filter(variable %in% input$region_variable)
  })
  
  output$region_boxplot <- renderPlot({
    ggplot(region_data(),aes(y=value,x=lake))+
      geom_boxplot(aes(fill=Risk_Level,alpha=Depth),outliers=F)+
      scale_fill_manual(values=c("#bc5090","#ffa600","#003f5c"))+
      guides(alpha=guide_legend(override.aes=list(fill=hcl(c(15,195),100,0,alpha=c(0.2,0.7)),
                                                  colour=NA))) +
      scale_alpha_manual(values=c(0.2,1))+
      ylab(input$region_variable)+
      facet_wrap(~Region, scales="free", nrow=2) +
      theme_classic(base_size=14) +
      theme(axis.text.x=element_text(angle=40, hjust=1),
            axis.title.x=element_blank(),
            strip.background = element_blank())

  }) 
  
  risk_level_data <- reactive({
    waterchem_long %>%
      mutate(Risk_Level = fct_relevel(Risk_Level, 
                                "Impacted", "At Risk", "Least Impacted")) %>%
      dplyr::filter(variable %in% input$y_variable_risk_level)
  })
  output$risk_level_boxplot <- renderPlot({
    ggplot(risk_level_data(),aes(y=value,x=Risk_Level, color=Depth))+
      geom_boxplot()+
      theme_classic(base_size=14) +
      ylab("Value") + xlab("Risk Level")
  })
  
  risk_level_chloride_data <- reactive({
    waterchem_long %>%
      mutate(Risk_Level = fct_relevel(Risk_Level, 
                                      "Impacted", "At Risk", "Least Impacted")) %>%
      pivot_wider(names_from=variable,values_from=value,values_fn=mean) %>%
      pivot_longer(!c(lake,Risk_Level,Region,Date,Depth,`Cl- (ug/L)`),names_to="variable") %>%
      dplyr::filter(variable %in% input$y_variable_chloride2)
  })
  
  output$chloride_plot2 <- renderPlot({
    ggplot(risk_level_chloride_data(),aes(x=`Cl- (ug/L)`, y=value, color=Depth))+
      geom_point()+
      facet_wrap(~Risk_Level, ncol=3) +
      theme_classic(base_size=14)
  })
  
  chloride_timeseries_data <- reactive({
    waterchem_long %>%
      dplyr::filter(lake %in% input$chloride_lake)
  })
  output$chloride_timeseries <- renderPlot({
    ggplot(chloride_timeseries_data(),aes(x=Date,y=value))+
      geom_point(size=1)+
      geom_smooth()+
      theme_classic(base_size=14)
  })
  
  season_data <- reactive({
    waterchem_long %>%
      dplyr::filter(variable %in% input$season_variable,
                    lake %in% input$season_lake)
  })
  output$season_timeseries <- renderPlot({
    ggplot(season_data(),aes(x=Date,y=value))+
      geom_point(size=1)+
      geom_smooth()+
      theme_classic(base_size=14)
  })
  
  
}

### -------------------------------------------------------------------------------------------------- ###
### ------------------------------------------- Launch App ------------------------------------------- ###
### -------------------------------------------------------------------------------------------------- ###

shinyApp(ui, server)

