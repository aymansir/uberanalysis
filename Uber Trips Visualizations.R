# load required packages

suppressMessages(library(ggplot2))
suppressMessages(library(data.table))
suppressMessages(library(lubridate))
suppressMessages(library(scales))
suppressMessages(library(stringr))
suppressMessages(library(zoo))
suppressMessages(library(dplyr))

# load file and clean

setwd("/Users/aymansiraj/Google Drive/ix 2016 /RUber")
Uber <- read.csv("trip-history.csv")
Uber$trip_id <- NULL
Uber$date_time <- NULL
Uber$driver <- NULL 
Uber$payment_method <- NULL
Uber$start_address <- NULL
Uber$end_address <- NULL
Uber <- Uber %>% subset(price != "")

# format data types currently

Uber$date <- as.character(Uber$date)
Uber$datenew <- parse_date_time(Uber$date, c('mdy'))
Uber$Month <- months(Uber$datenew)
Uber$Year <- year(Uber$datenew)

# create new columns to plot graphs

Uber$MonthYear = as.factor(paste(Uber$Month, Uber$Year, sep="-"))
Uber$currency<- as.factor(gsub("[0-9.]","", Uber$price))
Uber$pricenew <- as.numeric(gsub("[^0-9.]","", Uber$price))

# calculate amount spent on each currency

costpercurrency <- Uber %>% 
  group_by(currency) %>% 
  summarise(TotalCost = sum(pricenew))

# ggplot

city <- qplot(city, data=Uber, geom="bar", fill= car_type)
city + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

cartype <- qplot(car_type, data = Uber, geom = "bar", fill = city)
cartype + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggplot(Uber, aes(MonthYear)) + geom_bar() + geom_text(stat='count',aes(label=..count..),vjust=-1) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

costplot <- ggplot(costpercurrency, aes(x=currency, y=TotalCost)) + geom_bar(stat="identity", fill="#FF9999", colour="black") 
costplot + geom_text(aes(label=TotalCost), vjust=1.6, color="white", size=3.5)