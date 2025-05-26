## -------------------------------------------------------- ##
# Tidying data example - Salty Lakes WQ data
## -------------------------------------------------------- ##
# Script Author: Lienne Sethna

# PURPOSE:
## Use tidyverse to clean up Salty Lakes WQ data and plot trends

#NOTES:
#functions not included in base R are written with their package (e.g. librarian::shelf = package::function)

## ----------------------------------- ##
# Housekeeping ----
## ----------------------------------- ##

# Load libraries needed for script
# install.packages("librarian") #run this line if you don't have "librarian" package installed
librarian::shelf(readxl,tidyverse,janitor,randomcoloR)

# Clear environment
rm(list = ls())

#check working directory
getwd()

#read in the data
#you'll need to change this based on where the data is stored on your computer
WQ_Data_23 <- readxl::read_excel("2023 Salty Lakes water chemistry results.xlsx")
WQ_Data_24 <- readxl::read_excel("2024 Salty Lakes water chemistry results.xlsx")

#look at structure of data
dplyr::glimpse(WQ_Data_23)
dplyr::glimpse(WQ_Data_24)

#merge together
WQ_Data <- WQ_Data_23 %>% 
  #keep only monitoring data
  select(`Sample ID`,Depth,`Collection Date`,`Chl-a (ug/L)`:`Cl- (ug/L)`) %>%
  #combine
  bind_rows(WQ_Data_24) %>%
  #remove NA columns
  select(`Sample ID`:`Cl- (ug/L)`) %>%
  #remove rows with NA Sample ID
  filter(!is.na(`Sample ID`))
  
glimpse(WQ_Data)

#all nutrient data should be numeric/double
WQ_Data <- WQ_Data %>% mutate_at(vars(`Chl-a (ug/L)`:`Cl- (ug/L)`), as.numeric)

#remove first row with no data
WQ_Data <- WQ_Data[2:382,]

## ----------------------------------- ##
# Data tidying ----
## ----------------------------------- ##

#get list of unique sites and dates in WQ_Data
unique(WQ_Data$`Sample ID`)
unique(WQ_Data$`Collection Date`)

#create new column indicating duplicate samples
#the dplyr %>% (pipe) feeds the dataframe into the following functions, this eliminates the need to explicitly call the df and its columns (no need to use WQ_Data$dup)
WQ_Data <- WQ_Data %>%
  #look for the pattern "DUP" in the Sample ID, if it is a dup sample, write "dup" in the dup column. If not, leave blank.
  dplyr::mutate(dup = ifelse(grepl("DUP",`Sample ID`,ignore.case=T), "dup","")) 

#create new column with standardized lake names
WQ_Data <- WQ_Data %>%
  dplyr::mutate(lake = case_when(`Sample ID`=="McCarons"|`Sample ID`=="McCarron"|`Sample ID`=="Dup McCarrons"~"McCarrons",
                                 `Sample ID`=="Lil Jo"|`Sample ID`=="Little Johana"~"Little Johanna",
                                 `Sample ID`=="DUP Parkers"|`Sample ID`=="Parkers DUP"~"Parkers",
                                 `Sample ID`=="DUP Smith"~"Smith",
                                 `Sample ID`=="DUP Snail"~"Snail",
                                 `Sample ID`=="DUP Medicine"~"Medicine",
                                 `Sample ID`=="Henry (dup)"~"Henry",
                                 `Sample ID`=="Minnetonka DUP"|`Sample ID`=="Minnetona"~"Minnetonka",
                                 `Sample ID`=="Dup Wabasso"~"Wabasso",
                                 T~`Sample ID`))
#check unique site names
unique(WQ_Data$lake)
#how many unique sites?
length(unique(WQ_Data$lake))

#check depths
unique(WQ_Data$Depth)
#change all caps "HYPO" to "Hypo" to match
#and dups...
WQ_Data = WQ_Data %>% mutate(Depth = case_when(Depth=="HYPO"|Depth=="hypo"~"Hypo",
                                                Depth=="DUP_Epi"~"Epi",
                                                T~Depth))

## ----------------------------------- ##
# Data QC - dup check ----
## ----------------------------------- ##

#duplicate measurements should be within 10%
#creates a new "dups" dataframe with only the duplicate samples
dups <- subset(WQ_Data,WQ_Data$dup=="dup") %>% 
  #filter that dataframe to only the columns lake, collection date, and depth
  dplyr::select(lake,`Collection Date`,Depth)

#filter data from WQ_Data that has a duplicate measurement
#semi_join filters rows from left df based on matches in right df
dup_check <- dplyr::semi_join(WQ_Data,dups)

#create table to calculate percent error
#create percent_error function
percent_error = function(x){(max(x)-min(x))/max(x)}
#calculate percent error
perc_err <- dup_check %>%
  #compare the duplicates taken from the same lake, date, and depth
  dplyr::group_by(lake,`Collection Date`,Depth) %>%
  #create new df that calculates %err in columns
  dplyr::summarize(
    chla_perc_err = percent_error(`Chl-a (ug/L)`),
    srp_perc_err = percent_error(`SRP (ug P/L)`),
    tp_perc_err = percent_error(`Total Phosphorus (ug P/L)`),
    tn_perc_err = percent_error(`Total Nitrogen (mg N/L)`),
    nh4_perc_err = percent_error(`NH3 (mg N/L)`),
    no3_perc_err = percent_error(`NO3 + NO2 (mg N/L)`),
    dic_perc_err = percent_error(`DIC (mg C/L)`),
    doc_perc_err = percent_error(`DOC (mg C/L)`),
    dsi_perc_err = percent_error(`DSi (mg SiO2/L)`),
    cl_perc_err = percent_error(`Cl- (ug/L)`),
  )

#some of these variables are well over 10% different - what to do?

## ----------------------------------- ##
# Add region and risk level column
## ----------------------------------- ##

WQ_Data <- WQ_Data %>% 
  mutate(Risk_Level = case_when(lake=="Tanners"|lake=="Parkers"|lake=="Brownie"|lake=="Little Johanna"|lake=="Henry"~"Impacted",
                                lake=="Medicine"|lake=="Bde Maka Ska"|lake=="Wabasso"|lake=="Uhlenkolts"|lake=="McCarrons"~"At Risk",
                                lake=="Minnetonka"|lake=="Cedar"|lake=="Phalen"|lake== "Snail"|lake=="Smith"~"Least Impacted"),
         Region = case_when(lake=="Little Johanna"|lake=="Wabasso"|lake =="Snail" ~ "North Metro", 
                            lake=="Brownie"|lake=="Bde Maka Ska"|lake=="Cedar" ~ "South Metro", 
                            lake=="Tanners"|lake=="McCarrons"|lake=="Phalen" ~ "East Metro", 
                            lake=="Parkers"|lake=="Medicine"|lake=="Minnetonka" ~ " West Metro",
                            lake=="Henry"|lake=="Uhlenkolts"|lake=="Smith" ~ "Central MN")
         )
unique(WQ_Data$Risk_Level)
unique(WQ_Data$Region)

## ----------------------------------- ##
# Export cleaned data
## ----------------------------------- ##

glimpse(WQ_Data)

#export without the dups
WQ_Data_clean <- WQ_Data %>% filter(dup=="") %>%
  select(lake,Risk_Level,Region,`Collection Date`,Depth,`Chl-a (ug/L)`:`Cl- (ug/L)`) %>%
  rename(Date=`Collection Date`)
glimpse(WQ_Data_clean)

write.csv(WQ_Data_clean,file="Salty_2023_2024_monitoring_data_clean.csv")

## ----------------------------------- ##
# Plotting
## ----------------------------------- ##

#create distinct color palette with randomcoloR
palette <- randomcoloR::distinctColorPalette(15) #15 distinct colors for 15 different sites

#plot all data, facet by depth and variable
WQ_Data %>% dplyr::filter(dup=="") %>% #filter out duplicate measures
  select(lake,Depth,`Collection Date`,`Sample Month`,c(7:16)) %>% #select only variables of interest
  tidyr::pivot_longer(cols=!c(1:4),names_to="variable",values_to="value") %>% #pivot data to long format
  ggplot2::ggplot(aes(x=`Sample Month`,y=value,color=lake))+ #set the "aesthetics" of the plot, what is on the x and y axes, and how to color the points
  geom_point()+ #specify how to visualize the data. could be points, lines, boxplots, etc.
  scale_color_manual(values=palette)+ #set colors to manual palette we created above
  facet_grid(rows=vars(Depth),cols=vars(variable),scales="free_y") #facet_grid does not seem to allow the y-axes to vary within the columns...

#compares epi and hypo samples for all sites
WQ_Data %>% dplyr::filter(dup=="") %>% #filter out duplicate measures
  select(lake,Depth,`Collection Date`,`Sample Month`,c(7:16)) %>% #select only variables of interest
  tidyr::pivot_longer(cols=!c(1:4),names_to="variable",values_to="value") %>% #pivot data to long format
  ggplot(aes(x=`Sample Month`,y=value,fill=Depth))+
  geom_boxplot()+
  facet_wrap(~variable,scales="free_y",ncol=5)

#plot one variable
WQ_Data %>%
  ggplot(aes(x=`Sample Month`,y=`SRP (ug P/L)`))+
  geom_point()+
  facet_wrap(~lake)
