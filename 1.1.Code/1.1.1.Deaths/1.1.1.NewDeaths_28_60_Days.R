
#________________________________________________________ 1.1 DEATHS ________________________________________________________ 

#___________________________________ 1.11 NEW DEATHS WITHIN 28 DAYS & 60 DAYS OF +VE TEST____________________________________



#____________________________________________________________________________________________________________________________
# Packages Used:

# dplyr
install.packages("dplyr")
library(dplyr)

# ggplot2
install.packages("ggplot2")
library(ggplot2)

# data.table
install.packages("data.table")
library(data.table)

#scales (within ggplot2)
#to customise dates (x-axis) in ggplot
library(scales)

#plotly
install.packages("plotly")
library(plotly)

#____________________________________________________________________________________________________________________________
# 1) loading data from UK Government Website (https://coronavirus.data.gov.uk/details/download)

en_deathsData <- read.csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=cumDeaths28DaysByDeathDate&metric=cumDeaths60DaysByDeathDate&metric=newDeathsByDeathDate&metric=cumDeathsByDeathDate&format=csv")



#____________________________________________________________________________________________________________________________
# 2) Find new death values for deaths within 28 and 60 days of positive test + add it to en_deathsData

cumColumnNames <- c("cumDeaths28DaysByDeathDate", "cumDeaths60DaysByDeathDate")

for (column in 1:length(cumColumnNames)) {
  
  #column <- 1
  cumColumnName <- cumColumnNames[column]
  newColumnName <- gsub(pattern = "cum", replacement = "new", cumColumnName)
  
  en_deathsData[, ncol(en_deathsData)+1] <- NA
  colnames(en_deathsData)[ncol(en_deathsData)] <- newColumnName
  
  for (dateValue in 1:length(en_deathsData$date)) {
    
    #dateValue <- 2
    newCumDeathValue <- en_deathsData[dateValue, which(colnames(en_deathsData) == cumColumnName)]
    oldCumDeathValue <- en_deathsData[dateValue + 1, which(colnames(en_deathsData) == cumColumnName)]
    
    if (is.na(oldCumDeathValue) == FALSE) {
      
      deathPerDay <- newCumDeathValue - oldCumDeathValue
      
      en_deathsData[dateValue, which(colnames(en_deathsData) == newColumnName)] <- deathPerDay
      
    } else {
      
      en_deathsData[dateValue, which(colnames(en_deathsData) == newColumnName)] <- newCumDeathValue
      
    }
    
  }
  
}



#____________________________________________________________________________________________________________________________
# 2) Re-organise the columns and dates

# dplyr
en_deathsData <- en_deathsData %>% select(date, areaType, areaCode, areaName, newDeaths28DaysByDeathDate, newDeaths60DaysByDeathDate, newDeathsByDeathDate, cumDeaths28DaysByDeathDate, cumDeaths60DaysByDeathDate, cumDeathsByDeathDate)

en_deathsData <- en_deathsData[order(as.Date(en_deathsData$date, format = "%Y-%m-%d")),]



#____________________________________________________________________________________________________________________________
# 3) Group death values into day windows (within 28 days, 29 to 60 days, 60+ days of +ve test)

deathsWithin28days <- en_deathsData$newDeaths28DaysByDeathDate
deathsBetween29to60days <- en_deathsData$newDeaths60DaysByDeathDate - en_deathsData$newDeaths28DaysByDeathDate
deathsAfter60days <- en_deathsData$newDeathsByDeathDate - en_deathsData$newDeaths60DaysByDeathDate

# add to en_deathsData
en_deathsData <- cbind(en_deathsData, deathsWithin28days, deathsBetween29to60days, deathsAfter60days)



#____________________________________________________________________________________________________________________________
# 4) Plotting data

# PREPARING DATA FOR ggplot2
# data.table package: convert en_deathsData from 'wide' data.frame to 'long'
en_deathsData_long <- melt(setDT(en_deathsData), id.vars = c("date","areaType", "areaCode", "areaName"), variable.name = "deathMeasurementType")

# convert $date into "date" data type
en_deathsData_long$date <- as.Date(en_deathsData_long$date, format = "%Y-%m-%d")



#### FIGURE 1 ####
## Bar chart of cummulative deaths over time ##

# subset cummulative data
cumData <- subset(en_deathsData_long, deathMeasurementType == "cumDeathsByDeathDate")

# plot data with ggplot2 package
cumPlot <- ggplot(data = cumData, aes(x = date, y = value, fill = deathMeasurementType, text = paste("Date:", date, "\nCummulative number of deaths:", value))) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Cummulative Number of Deaths over Time") +
  labs(x=("Date of Deaths"), y=("Number of Deaths")) + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  theme(legend.position = "none")

# ggplotly: making graph interactive
cumPlot_Interactive <- ggplotly(cumPlot, tooltip = "text")

# print plot
print(cumPlot_Interactive)



#### FIGURE 2 ####
## Bar chart of daily number of deaths over time ##

# subset new daily data
newData <- subset(en_deathsData_long, deathMeasurementType == "newDeathsByDeathDate")

# plot data with ggplot2 package
newPlot <- ggplot(data = newData, aes(x = date, y = value, fill = deathMeasurementType, text = paste("Date:", date, "\nNumber of deaths:", value))) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Daily Number of Deaths over Time") +
  labs(x=("Date of Deaths"), y=("Number of Deaths")) + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  theme(legend.position = "none")

# ggplotly: making graph interactive
newPlot_Interactive <- ggplotly(newPlot, tooltip = "text")

# print plot
print(newPlot_Interactive)



#### FIGURE 3.1 ####
## Stacked barch chart (≤28, 29-60, ≥60 days), daily number ##
# "Windows" data - Number of daily deaths grouped by length of time from testing positive until death (≤28, 29-60, ≥60 days) #

# subset windows data
windowData <- subset(en_deathsData_long, deathMeasurementType == "deathsWithin28days" | deathMeasurementType == "deathsBetween29to60days" | deathMeasurementType == "deathsAfter60days")

# plot data with ggplot2 package
windowBarPlot<- ggplot(windowData, aes(x = date, y = value, fill = deathMeasurementType, group = deathMeasurementType, text = paste(deathMeasurementType, "\n Date:", date, "\n Value:", value))) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Number of daily deaths grouped by length of time from testing positive until death") +
  labs(x=("Date of Deaths"), y=("Number of Deaths")) + 
  scale_fill_discrete(name="\n Number of days \n after +ve Test",
                      breaks=c("deathsWithin28days", "deathsBetween29to60days", "deathsAfter60days"),
                      labels=c("≤28 Days", "29 to 60 Days", ">60 Days")) +
  scale_x_date(date_breaks = "months", date_labels = "%b-%y")

# ggplotly: making graph interactive
windowBarPlotInteractive <- ggplotly(windowBarPlot, tooltip = "text")

# changing legend names
windowBarPlotInteractive$x$data[[1]]$name <- "≤28 Days"
windowBarPlotInteractive$x$data[[2]]$name <- "29 to 60 Days"
windowBarPlotInteractive$x$data[[3]]$name <- ">60 Days"

# print stacked bar plot
print(windowBarPlotInteractive)



#### FIGURE 3.2 ####
## Stacked barch chart (≤28, 29-60, ≥60 days), PERCENTAGE ##
# "Windows" data - Number of daily deaths grouped by length of time from testing positive until death (≤28, 29-60, ≥60 days) #

# subset windows data
windowData <- subset(en_deathsData_long, deathMeasurementType == "deathsWithin28days" | deathMeasurementType == "deathsBetween29to60days" | deathMeasurementType == "deathsAfter60days")

# plot data with ggplot2 package
windowBarPlot<- ggplot(windowData, aes(x = date, y = value, fill = deathMeasurementType, group = deathMeasurementType, text = paste(deathMeasurementType, "\n Date:", date, "\n Value:", value))) + 
  geom_bar(position="fill", stat="identity") +
  ggtitle("Percentage of Daily deaths grouped by length of time from testing positive until death") +
  labs(x=("Date of Deaths"), y=("Number of Deaths")) + 
  scale_fill_discrete(name="\n Number of days \n after +ve Test",
                      breaks=c("deathsWithin28days", "deathsBetween29to60days", "deathsAfter60days"),
                      labels=c("≤28 Days", "29 to 60 Days", ">60 Days")) +
  scale_x_date(date_breaks = "months", date_labels = "%b-%y")


# ggplotly: making graph interactive
windowBarPlotInteractive <- ggplotly(windowBarPlot, tooltip = "text")

# changing legend names
windowBarPlotInteractive$x$data[[1]]$name <- "≤28 Days"
windowBarPlotInteractive$x$data[[2]]$name <- "29 to 60 Days"
windowBarPlotInteractive$x$data[[3]]$name <- ">60 Days"

# print stacked bar plot
print(windowBarPlotInteractive)

# TODO: 
# 1) add percentage values in the hover box (create a function?)
