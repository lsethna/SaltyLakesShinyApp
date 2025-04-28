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
librarian::shelf(readxl,tidyverse,randomcoloR)

# Clear environment
rm(list = ls())

#check working directory
getwd()

#read in the data
#you'll need to change this based on where the data is stored on your computer
WQ_Data <- readxl::read_excel("G:/Shared drives/SCWRS/Shared/ABC Shared Lab Folder/R Code for Lab/Tidying data/Lienne example/WQ_Data.xlsx")

#look at structure of data
dplyr::glimpse(WQ_Data)

#chl-a should be numeric/double
WQ_Data$`Chl-a (ug/L)` <- as.numeric(WQ_Data$`Chl-a (ug/L)`)
dplyr::glimpse(WQ_Data)

## ----------------------------------- ##
# Data tidying ----
## ----------------------------------- ##

#get list of unique sites and dates in WQ_Dataunique(WQ_Data$`Sample ID`)
unique(WQ_Data$`Collection Date`)
unique(WQ_Data$`Sample ID`)

#create new column indicating duplicate samples
#the dplyr %>% (pipe) feeds the dataframe into the following functions, this eliminates the need to explicitly call the df and its columns (no need to use WQ_Data$dup)
WQ_Data <- WQ_Data %>%
  #look for the pattern "DUP" in the Sample ID, if it is a dup sample, write "dup" in the dup column. If not, leave blank.
  dplyr::mutate(dup = ifelse(grepl("DUP",`Sample ID`), "dup","")) 

#create new column with standardized lake names
WQ_Data <- WQ_Data %>%
  dplyr::mutate(lake = case_when(`Sample ID`=="McCarons"|`Sample ID`=="McCarron"~"McCarrons",
                                 `Sample ID`=="Lil Jo"|`Sample ID`=="Little Johana"~"Little Johanna",
                                 `Sample ID`=="DUP Parkers"~"Parkers",
                                 `Sample ID`=="DUP Smith"~"Smith",
                                 `Sample ID`=="DUP Snail"~"Snail",
                                 `Sample ID`=="DUP Medicine"~"Medicine",
                                 T~`Sample ID`))
#check unique site names
unique(WQ_Data$lake)
#how many unique sites?
length(unique(WQ_Data$lake))

#check depths
unique(WQ_Data$Depth)
#change all caps "HYPO" to "Hypo" to match
WQ_Data$Depth = ifelse(WQ_Data$Depth=="HYPO","Hypo",WQ_Data$Depth)

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
    nh4_perc_err = percent_error(`NH4 (mg N/L)`),
    no3_perc_err = percent_error(`NO3 + NO2 (mg N/L)`),
    dic_perc_err = percent_error(`DIC (mg C/L)`),
    doc_perc_err = percent_error(`DOC (mg C/L)`),
    dsi_perc_err = percent_error(`DSi (mg SiO2/L)`),
    cl_perc_err = percent_error(`Cl- (mg/L)`),
  )

#some of these variables are well over 10% different - what to do?

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
