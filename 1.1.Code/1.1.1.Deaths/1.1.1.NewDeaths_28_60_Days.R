
#________________________________________________________ 1.1 DEATHS ________________________________________________________ 

#___________________________________ 1.11 NEW DEATHS WITHIN 28 DAYS & 60 DAYS OF +VE TEST____________________________________



#____________________________________________________________________________________________________________________________
#### Packages Used: ####

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
#to make plots interactive
install.packages("plotly")
library(plotly)

#____________________________________________________________________________________________________________________________
#### 1) loading data from UK Government Website (https://coronavirus.data.gov.uk/details/download) ####

en_deathsData <- read.csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=cumDeaths28DaysByDeathDate&metric=cumDeaths60DaysByDeathDate&metric=newDeathsByDeathDate&metric=cumDeathsByDeathDate&format=csv")



#____________________________________________________________________________________________________________________________
#### 2) Find new death values for deaths within 28 and 60 days of positive test + add it to en_deathsData ####

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
#### 3) Re-organise the columns and dates ####

# dplyr
en_deathsData <- en_deathsData %>% select(date, areaType, areaCode, areaName, newDeaths28DaysByDeathDate, newDeaths60DaysByDeathDate, newDeathsByDeathDate, cumDeaths28DaysByDeathDate, cumDeaths60DaysByDeathDate, cumDeathsByDeathDate)

en_deathsData <- en_deathsData[order(as.Date(en_deathsData$date, format = "%Y-%m-%d")),]



#____________________________________________________________________________________________________________________________
#### 4) Group death values into day windows (within 28 days, 29 to 60 days, 60+ days of +ve test) ####

deathsWithin28days <- en_deathsData$newDeaths28DaysByDeathDate
deathsBetween29to60days <- en_deathsData$newDeaths60DaysByDeathDate - en_deathsData$newDeaths28DaysByDeathDate
deathsAfter60days <- en_deathsData$newDeathsByDeathDate - en_deathsData$newDeaths60DaysByDeathDate

# add to en_deathsData
en_deathsData <- cbind(en_deathsData, deathsWithin28days, deathsBetween29to60days, deathsAfter60days)



#____________________________________________________________________________________________________________________________
#### 5) Plotting data ####

# PREPARING DATA FOR ggplot2
# data.table package: convert en_deathsData from 'wide' data.frame to 'long'
en_deathsData_long <- melt(setDT(en_deathsData), id.vars = c("date","areaType", "areaCode", "areaName"), variable.name = "deathMeasurementType")

# convert $date into "date" data type
en_deathsData_long$date <- as.Date(en_deathsData_long$date, format = "%Y-%m-%d")



#### FIGURE 1a ####
## Bar chart of cummulative deaths over time ##

# subset cummulative data
cumData <- subset(en_deathsData_long, deathMeasurementType == "cumDeathsByDeathDate")

# plot data with ggplot2 package
cumPlot <- ggplot(data = cumData, aes(x = date, y = value, fill = deathMeasurementType, text = paste("Date:", date, "\nNumber of deaths:", value))) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Cummulative Number of Deaths over Time") +
  labs(x=("Date of Deaths"), y=("Number of Deaths")) + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  theme(legend.position = "none")

# ggplotly: making graph interactive
cumPlot_Interactive <- ggplotly(cumPlot, tooltip = "text")

# print plot
print(cumPlot_Interactive)



#### FIGURE 1b ####
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




#### FIGURE 2 ####
## Bar plot of daily deaths and line plot of total (cummulated) deaths by date

en_deathsData$date <- as.Date(en_deathsData$date, format = "%Y-%m-%d")

cum_newPlot <- ggplot(data = en_deathsData, aes(x = date)) +
  geom_bar(aes(y = newDeathsByDeathDate, fill = "#69b3a2"), stat = "identity", position = "dodge") +
  geom_line(aes(y = cumDeathsByDeathDate / 75, color = "#e52b50")) +
  scale_y_continuous(name = "Number of Daily Deaths", sec.axis = sec_axis(trans = ~.*75, name = "Number of Total Deaths")) +
  labs(x=("Death Date")) +
  ggtitle("Daily and Total Number of Deaths by Date") +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  scale_fill_manual(values = "#69b3a2", labels = "Daily Deaths") +
  scale_colour_manual(values = "#e52b50", labels = "Total Deaths") +
  theme(legend.title=element_blank(), plot.title = element_text(face = "bold"))

print(cum_newPlot)


#### FIGURE 3a ####
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



#### FIGURE 3b ####
## Stacked barch chart (≤28, 29-60, ≥60 days), PERCENTAGE ##
# "Windows" data - Number of daily deaths grouped by length of time from testing positive until death (≤28, 29-60, ≥60 days) #

# subset windows data
windowData <- subset(en_deathsData_long, deathMeasurementType == "deathsWithin28days" | deathMeasurementType == "deathsBetween29to60days" | deathMeasurementType == "deathsAfter60days")


# plot data with ggplot2 package
windowBarPlot<- ggplot(windowData, aes(x = date, y = value, fill = deathMeasurementType, group = deathMeasurementType, text = paste(deathMeasurementType, "\n Date:", date, "\n Value:", value))) + 
  geom_bar(position="fill", stat="identity") +
  ggtitle("Proportion of Daily Deaths Grouped by length of time from testing positive until death") +
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




#### FIGURE 4 ####
## Bar chart showing the percentage of deaths grouped by the number of days between positive test and death date ##

# subset windows data
windowData <- subset(en_deathsData_long, deathMeasurementType == "deathsWithin28days" | deathMeasurementType == "deathsBetween29to60days" | deathMeasurementType == "deathsAfter60days")

# creating list of death windows
deathWindows <- c("deathsWithin28days", "deathsBetween29to60days", "deathsAfter60days")

# creating data.frame with two columns (1: name of death windows, 2: percentage of deaths [empty])
percentage_deathWindowsDF <- data.frame(matrix(nrow = 3, ncol = 2))
percentage_deathWindowsDF[,1]<- deathWindows
colnames(percentage_deathWindowsDF) <- c("deathMeasurementType", "percentage")


# calculating percentage of deaths for each death window and putting results in data.frame
for (deathWindow in 1:length(deathWindows)) {
  
  #deathWindow <- 1
  deathWindowName <- deathWindows[deathWindow]
  
  # sum of specific window
  deathWindowValues <- subset(windowData, windowData$deathMeasurementType == deathWindowName, select = value)
  sum_deathWindowValues <- sum(deathWindowValues)
  
  # sum of all windows
  other_deathWindowValues <- subset(windowData, windowData$deathMeasurementType != deathWindowName, select = value)
  sum_other_deathWindowValues <- sum(other_deathWindowValues)
  
  sum_all_WindowValuesbyDate <- sum_deathWindowValues + sum_other_deathWindowValues
  
  # percentage of specific window
  percentage_deathWindowValues <- (sum_deathWindowValues / sum_all_WindowValuesbyDate) * 100
  percentage_deathWindowValues <- round(percentage_deathWindowValues, digits = 2)
  
  percentage_deathWindowsDF[deathWindow, 2] <- percentage_deathWindowValues
  
}

# plotting the results
percentage_totalDeathWindowsPlot <- ggplot(data = percentage_deathWindowsDF, aes(x = deathMeasurementType, y = percentage, fill = deathMeasurementType, text = paste("\n Percentage:", percentage))) +
  geom_bar( stat="identity") +
  scale_x_discrete(limits = c(deathWindows), breaks=c(deathWindows), labels=c("≤28", "29 to 60", "60+")) +
  labs(x = "Number of Days Between Positive Test and Death Date", y = "Percentage of Deaths") +
  ggtitle("Percentage of Deaths by Number of Days between Positive Test and Death Date") + 
  theme(legend.position = "none", plot.title = element_text(face = "bold"), axis.title = element_text(face = "bold") )

percentage_totalDeathWindowsPlot_Interactive <- ggplotly(percentage_totalDeathWindowsPlot, tooltip = "text")

print(percentage_totalDeathWindowsPlot_Interactive)



# TODO: 
# 1) match the colour schemes of all figures
# 2) think about organising all figures into one document?
