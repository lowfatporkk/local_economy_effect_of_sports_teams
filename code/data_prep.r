##Data Preparation for the Synthetic Control Model
#importing libraries
library(tidyverse)
library(dplyr)

#creating a function for data parsing
parsing_oecd_records <- function(foo,column) {
  
  #filtering UK data
  foo <- filter(foo,Country=='GBR')
  
  #pivot longer by year
  foo <- foo %>% 
    pivot_longer(c("X2001"
                   , "X2002"
                   , "X2003"
                   , "X2004"
                   , "X2005"
                   , "X2006"
                   , "X2007"
                   , "X2008"
                   , "X2009"
                   , "X2010"
                   , "X2011"
                   , "X2012"
                   , "X2013"
                   , "X2014"
                   , "X2015"
                   , "X2016"
                   , "X2017"
                   ,"X2018"), names_to = "year", values_to = column)
  
  #converting the data type of year to integer
  foo$year <- as.integer(substr(foo$year,2,5))
  
  #returning the parsed dataframe
  return(foo)
  
}

#GDP per Capita
gdp <- read.csv("gdp_per_capita.csv",skip = 1)
gdp <- parsing_oecd_records(gdp,"gdp_per_capita")

#Population Growth
pop <- read.csv("population_growth.csv",skip = 1)
pop <- parsing_oecd_records(pop,"population_growth")
pop <- pop[,-4]

#Employment Rate
emp <- read.csv("employment.csv",skip = 1)
emp <- parsing_oecd_records(emp,"employment_rate")

#joining the tables 
data <- gdp %>% left_join(pop) %>% left_join(emp) 

#Secondary education attainment
edu <- read.csv("secondary_attainment.csv")
head(edu)

#renaming Columns
edu <- edu[,c("Region","Year","Value")]
colnames(edu) <- c("Region","year","schooling")

#removing duplicates
edu <- edu %>% group_by(Region,year) %>% summarise(
  schooling = mean(schooling)
)

#assigning Region
data <- mutate(data,
       Region = case_when(  
         data$Name == "Aberdeen" ~ "Scotland"
       ,    data$Name == "Blackburn with Darwen" ~ "Lancashire"
       ,    data$Name == "Bournemouth" ~ "Dorset And Somerset"
       ,    data$Name == "Brighton and Hove" ~ "Surrey East And West Sussex"
       ,    data$Name == "Bristol" ~ "Gloucestershire Wiltshire and Bristol/Bath Area"
       ,    data$Name == "Cambridge" ~ "East of England"
       ,    data$Name == "Cardiff" ~ "Wales"
       ,    data$Name == "Cheshire West and Chester" ~ "Cheshire"
       ,    data$Name == "Colchester" ~ "Essex"
       ,    data$Name == "Coventry" ~ "West Midlands"
       ,    data$Name == "Derby" ~ "East Midlands"
       ,    data$Name == "Dundee City" ~ "Scotland"
       ,    data$Name == "Edinburgh" ~ "Scotland"
       ,    data$Name == "Exeter" ~ "Devon"
       ,    data$Name == "Glasgow" ~ "Scotland"
       ,    data$Name == "Kingston upon Hull" ~ "East Yorkshire And Northern Lincolnshire"
       ,    data$Name == "Leeds" ~ "West Yorkshire"
       ,    data$Name == "Leicester" ~ "East Midlands"
       ,    data$Name == "Liverpool" ~ "Merseyside"
       ,    data$Name == "London" ~ "Greater London"
       ,    data$Name == "Manchester" ~ "Greater Manchester"
       ,    data$Name == "Medway" ~ "Kent"
       ,    data$Name == "Middlesbrough" ~ "North Yorkshire"
       ,    data$Name == "Milton Keynes" ~ "Berkshire Buckinghamshire and Oxfordshire"
       ,    data$Name == "Newcastle upon Tyne" ~ "Northumberland and Tyne And Wear"
       ,    data$Name == "Northampton" ~ "East Midlands"
       ,    data$Name == "Norwich" ~ "East of England"
       ,    data$Name == "Nottingham" ~ "East Midlands"
       ,    data$Name == "Oxford" ~ "Berkshire Buckinghamshire and Oxfordshire"
       ,    data$Name == "Plymouth" ~ "Devon"
       ,    data$Name == "Portsmouth" ~ "Hampshire And Isle Of Wight"
       ,    data$Name == "Preston" ~ "Lancashire"
       ,    data$Name == "Sheffield" ~ "South Yorkshire"
       ,    data$Name == "Southampton" ~ "Hampshire And Isle Of Wight"
       ,    data$Name == "Stoke-on-Trent" ~ "Shropshire and Staffordshire"
       ,    data$Name == "Sunderland" ~ "Northumberland and Tyne And Wear"
       ,    data$Name == "Swansea" ~ "Wales"
       ,    data$Name == "West Midlands urban area" ~ "West Midlands"
       ,    data$Name == "Wirral" ~ "Merseyside"
       ))

#removing NA values 
data[data==""] <- NA
data <- na.omit(data)

#joining the tables
data_with_schooling <- data %>% left_join(edu)

#adding identifier for model
data$Code <- group_indices(data, Name)
## Warning: The `...` argument of `group_keys()` is deprecated as of dplyr 1.0.0.
## Please `group_by()` first
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_warnings()` to see where this warning was generated.
data <- data[,-2]

#excluding cities without schooling data
data_with_schooling <- filter(data_with_schooling,!Name %in% unique(data_with_schooling[is.na(data_with_schooling$schooling),]$Name))

#adding identifier for model
data_with_schooling$Code <- group_indices(data_with_schooling, Name)
data_with_schooling <- data_with_schooling[,-2]
