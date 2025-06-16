### -------------------------------------------------------------------------------------------------- ###
### -------------------------------------------  Housekeeping ---------------------------------------- ###
### -------------------------------------------------------------------------------------------------- ###

rm(list=ls())

library(zoo)
library(shiny)
library(bslib)
library(shinythemes)
library(tidyverse)
library(tidyr)
library(hydroTSM)

#read in cleaned dataset
waterchem <- read_csv("Salty_2023_2024_monitoring_data_clean_2June2025.csv") %>% 
  select(!`...1`)
glimpse(waterchem)

#convert to long format
waterchem_long <- tidyr::pivot_longer(waterchem,cols=c(6:15),names_to="variable") %>% mutate(value=as.numeric(value))
glimpse(waterchem_long)

variables <- unique(waterchem_long$variable)
lakes <- unique(waterchem_long$lake)
depths <- unique(waterchem_long$Depth)

### -------------------------------------------------------------------------------------------------- ###
### ------------------------------------- Set up User Interface -------------------------------------- ###
### -------------------------------------------------------------------------------------------------- ###

ui <- fluidPage( theme = bs_theme(bootswatch = "yeti"), #sets theme for entire app

  titlePanel("Water chemistry data collected as part of the LCCMR - Salty Lakes project"),

  tabsetPanel(
    
#####
    tabPanel("About",
             #side bar layout allows you to add box on the side of the page, good for plotting
             sidebarLayout(
               sidebarPanel(
               p("The Salty Lakes project monitored 12 lakes within the Twin Cities Metro area and 3 lakes outside the city of Alexandria.
                Each of the study lakes represented a gradient of salinity and impairment risk due to salt.
                The data shown as part of this project are from water quality monitoring efforts between June 2023 and January 2025 and help 
                 scientists understand how road salt impacts lake water quality and ecosystem function.")),
               mainPanel(imageOutput("map"))
  
             ), #close sidebar layout
             sidebarLayout(
               sidebarPanel(
                 p("We care about salt concentrations (here, as chloride) in lakes because it disrupts the way in which lakes process nutrients 
                 and can negatively impact water quality. This is because salt increases the density of water, creating a layer of dense, salty 
                 water at the bottom of lakes that does not mix with the rest of the lake. Contaminants, excess nutrients, and salt all remain 
                 in the bottom waters, called the hypolimnion.")
                 ), #close sidebar panel
               mainPanel(
                 imageOutput("diagram")
                 ) #close main panel
               
            ) #close sidebar
    ), # close tab
#####
    tabPanel("Monitoring data exploration",
             card( #cards will organize each of the sets of plots together on the page
               card_title("Visualize the data over time for each sample lake"),
               card_body(full_screen=T, #eliminates need to scroll
               p(class = "text-muted",
                 "The plot below will show a timeseries of a specific variable for selected lakes. 
                 Each point represents the measured value for the variable, and the shape of the point corresponds to the depth at which 
                 the sample was taken. The epilimnion (epi) of the lake is the top layer of the lake and samples were taken using a 2 m tube 
                 that integrates water within the top 2 m of the lake. The hypolimnion (hypo) of the lake is the bottom layer, samples were taken 
                 using a Van Dorn samples that collects water from 1 m off the lake bottom"),
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
             ) #close sidebar layout
             )), #close card_body and card
             card(
               card_title("A title for the plot here"),
               card_body(full_screen=T,
                         p(class="text_muted",
                           "What is this plot doing?"),
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
    )), #close card_body and card
), #close tab panel
##### 
    tabPanel("Exploring the role of road salt across lakes, regions, and risk levels",
             card(
               card_title("A title for the plot here"),
               card_body(full_screen=T,
                         p(class="text_muted",
                           "What is this plot doing?"),
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
               )), #close card
             card(
               card_title("A title for the plot here"),
               card_body(full_screen=T,
                         p(class="text_muted",
                           "What is this plot doing?"),
             sidebarLayout(
               sidebarPanel(
                 p("Select the variable you want to compare between risk levels:"),
                 selectInput("y_variable_risk_level",label="Variable",choices=variables),
               ),
               mainPanel(
                 plotOutput("risk_level_boxplot")
               )
             ), #close sidebar layout
               )), #close card
             card(
               card_title("A title for the plot here"),
               card_body(full_screen=T,
                         p(class="text_muted",
                           "What is this plot doing?"),
             sidebarLayout(
               sidebarPanel(
                 p("Select the variable you want to plot in relation to chloride concentration:"),
                 selectInput("y_variable_chloride2",label= "Variable",choices=variables[1:9]),
               ), #close sidebar panel
               mainPanel(
                 plotOutput("chloride_plot2")
               )
             ), #close sidebar layout
               )), #close card
             card(
               card_title("A title for the plot here"),
               card_body(full_screen=T,
                         p(class="text_muted",
                           "What is this plot doing?"),
             sidebarLayout(
               sidebarPanel(
                 p("Select the variable you want to plot:"),
                 selectInput("region_variable",label="Variable",choices=variables),
               ),
               mainPanel(
                 plotOutput("region_boxplot")
               )
             ) #close sidebar layout
               )), #close card
    ), #close tab panel
#####
    tabPanel("Exploring the role of road salt by season",
             card(
               card_title("A title for the plot here"),
               card_body(full_screen=T,
                         p(class="text_muted",
                           "What is this plot doing?"),
             sidebarLayout(
               sidebarPanel(
                 p("Select what lake to look at:"),
                 selectInput("chloride_lake",label="Lake",choices=lakes,multiple=F)
               ),
               mainPanel(
                 plotOutput("chloride_timeseries")
               )
             ), #close sidebar layout
             sidebarLayout(
               sidebarPanel(
                 p("Select the variable you want to plot over time:"),
                 selectInput("season_variable",label="Variable",choices=variables)
               ),
               mainPanel(
                 plotOutput("season_timeseries")
               )
             ) #close sidebar layout
    )), #close card
    ), #close tab
#####
    tabPanel("Download raw data",
             card(
               card_title("Download the .csv file with data shown on this website"),
               card_body(full_screen=T,
                         p(class="text_muted",
                           "Click the button below to download the data. The data have been processed through quality assurance 
                           and quality control protocols including duplicate checks and outlier removals 
                           (>3 sds from the mean for each variable)."),
             downloadButton("downloadData","Download")
             )) #close card
             ), #close tab
#####
    tabPanel("Credits",
             card(
               card_title("Funding"),
               card_body(full_screen=T,
                         p(class="text_muted",
                           "This project was funded by the Minnesota Environment and Natural Resources Trust Fund (2022-072 Subd. 04l)"),
             imageOutput("lccmr")
               )), #close card
             card(
               card_title(""),
               card_body(full_screen=T,
                         p(class="text_muted",
                          "The project was managed and implemented by the Science Museum of Minnesota's St. Croix Watershed Research Station"),
             imageOutput("scwrs"),
             p("Project Managers: Mark Edlund and Hailey Sauer"),
             p("Field and laboratory scientists: Zoe Plechaty, Ari Pouchek, Kelsey Boeff"),
             p("Website design and creation: Lienne Sethna and Jackalyn Wyrobek")
               )) #close card
             ) #close tab
#####
  ) #close tabset panels
) #close UI

### -------------------------------------------------------------------------------------------------- ###
### ----------------------------------------- Set up Server ------------------------------------------ ###
### -------------------------------------------------------------------------------------------------- ###

server <- function(input, output, session) {
  output$map <- renderImage({
    list(
      src = file.path("MinnesotaMap_SaltyLakes_highlight.png"),
      contentType = "png",
      width = 404,
      height = 361
    )
  }, deleteFile = FALSE)
  
  output$diagram <- renderImage({
    list(
      src = file.path("saline strat diagram.png"),
      contentType = "png",
      width = 349,
      height = 274
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
      theme_classic(base_size=14) +
      ylab("Value")
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
      theme_classic(base_size=14) +
      xlab("Lake") + ylab("Value")
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
      theme_classic(base_size=14) +
      ylab("Value")
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
      theme_classic(base_size=14)+
      theme(strip.background=element_blank()) +
      ylab("Value")
  })
  
  chloride_timeseries_data <- reactive({
    waterchem_long %>%
      mutate(season=hydroTSM::time2season(Date,out.fmt="seasons")) %>%
      filter(variable=="Cl- (ug/L)") %>%
      dplyr::filter(lake %in% input$chloride_lake)
  })
  output$chloride_timeseries <- renderPlot({
    ggplot(chloride_timeseries_data(),aes(x=Date,y=value))+
      geom_point(aes(color=season),size=3)+
      scale_color_manual(values=c("#b87e39","#3bc54f","#de4f53","#4b98de"))+
      geom_smooth(se=F,color="black")+
      scale_x_date(date_labels = "%b")+
      ylab("Cl- (ug/L)")+
      theme_classic(base_size=14)
  })
  
  season_data <- reactive({
    waterchem_long %>%
      mutate(season=hydroTSM::time2season(Date,out.fmt="seasons")) %>%
      dplyr::filter(variable %in% input$season_variable,
                    lake %in% input$chloride_lake)
  })
  output$season_timeseries <- renderPlot({
    ggplot(season_data(),aes(x=Date,y=value))+
      geom_point(aes(color=season),size=3)+
      scale_color_manual(values=c("#b87e39","#3bc54f","#de4f53","#4b98de"))+
      geom_smooth(se=F,color="black")+
      scale_x_date(date_labels = "%b")+
      ylab("Value")+
      theme_classic(base_size=14)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(waterchem, file)
    })
  
  output$lccmr <- renderImage({
    list(
      src = file.path("ENTRF Logo - color.png"),
      contentType = "png",
      width = 212,
      height = 154
    )
  }, deleteFile = FALSE)
  
  output$scwrs <- renderImage({
    list(
      src = file.path("SMM+SCWRS.png"),
      contentType = "png",
      width = 596,
      height = 126
    )
  }, deleteFile = FALSE)
  
  
}

### -------------------------------------------------------------------------------------------------- ###
### ------------------------------------------- Launch App ------------------------------------------- ###
### -------------------------------------------------------------------------------------------------- ###

shinyApp(ui, server)

