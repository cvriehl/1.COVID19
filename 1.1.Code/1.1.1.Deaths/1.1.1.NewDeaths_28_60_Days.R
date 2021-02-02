
#________________________________________________________ 1.1 DEATHS ________________________________________________________ 



#___________________________________ 1.11 NEW DEATHS WITHIN 28 DAYS & 60 DAYS OF +VE TEST____________________________________




#____________________________________________________________________________________________________________________________
#### SCRIPT EXPLANATION ####
# This script takes data provided by the UK government (section 1), and manipulates it (sections 2-5) in order to visualise COVID-19 Death data (section 6)

# A] How to run the script:
# Code MUST be run from sections 1 to 5 
# Code for figures 1a, 1b, 2, 3a, 3b, 4 (section 6) can then be run separately
# Figure 5 contains multiple graphs in one plot (figures 2, 3a, 3b, and 4), so the code of each of those figures need to be run first

# B] Figures Content Guide
# Figure 1a: Bar chart of cummulative deaths over time
# Figure 1b: Bar chart of daily number of deaths over time
# Figure 2: Bar plot of daily deaths and line plot of total (cummulated) deaths by date
# Figure 3a: Stacked barch chart of NUMBER of daily deaths grouped by length of time from testing positive until death (≤28, 29-60, >60 days)
# Figure 3b: Stacked barch chart of PROPORTION of daily deaths grouped by length of time from testing positive until death (≤28, 29-60, >60 days)
# Figure 4: Bar chart showing the percentage of deaths grouped by the number of days between positive test and death date (≤28, 29-60, >60 days)
# Figure 5: Multiple graphs in one plot (side-by-side) ; standard versions of figures 2, 3a, 3b, and 4

# C] Interactive and Standard Version of Figures 
# Figures 1a, 1b, 3a, 3b, and 4 can have code to make them interactive
    # interactive versions of each plot are defined by *_Interactive 
    # eg Standard version of Figure 1a: cumPlot ; Interactive version of Figure 1a: cumPlot_Interactive
# Figures 2 and 5 are not interactive 




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

# scales (within ggplot2)
#to customise dates (x-axis) in ggplot
library(scales)

# plotly
#to make plots interactive
install.packages("plotly")
library(plotly)

# gridExtra
#to assemble multiple plots on a page
install.packages("gridExtra")
library(gridExtra)

# grid
#to be able to change the main title of multiple plots after using gridExtra

install.packages("grid")
library(grid)

#____________________________________________________________________________________________________________________________
#### 1) loading data from UK Government Website (https://coronavirus.data.gov.uk/details/download) ####
## rows (number increases over time): dates of deaths
## columns (8): date, areaType, areaCode, areaName, cumDeaths28DaysByDeathDate, cumDeaths60DaysByDeathDate, newDeathsByDeathDate, cumDeathsByDeathDate
## cumDeaths28DaysByDeathDate: cummulative number of people who died within 28 days of testing postive 
## cumDeaths60DaysByDeathDate: cummulative number of people who died within 60 days of testing postive
## cumDeathsByDeathDate: cummulative number of people who died within unlimited number of days after testing postive (within 60+ days)

en_deathsData <- read.csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=cumDeaths28DaysByDeathDate&metric=cumDeaths60DaysByDeathDate&metric=newDeathsByDeathDate&metric=cumDeathsByDeathDate&format=csv")
ncol(en_deathsData)


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
#### 4) Group death values into day windows (deaths within 28 days, deaths between 29 and 60 days, deaths after 60 days of +ve test) ####

deathsWithin28days <- en_deathsData$newDeaths28DaysByDeathDate
deathsBetween29to60days <- en_deathsData$newDeaths60DaysByDeathDate - en_deathsData$newDeaths28DaysByDeathDate
deathsAfter60days <- en_deathsData$newDeathsByDeathDate - en_deathsData$newDeaths60DaysByDeathDate

# add to en_deathsData
en_deathsData <- cbind(en_deathsData, deathsWithin28days, deathsBetween29to60days, deathsAfter60days)



#____________________________________________________________________________________________________________________________
#### 5) PREPARING DATA FOR ggplot2 ####

# data.table package: convert en_deathsData from 'wide' data.frame to 'long'
en_deathsData_long <- melt(setDT(en_deathsData), id.vars = c("date","areaType", "areaCode", "areaName"), variable.name = "deathMeasurementType")

# convert $date into "date" data type
en_deathsData_long$date <- as.Date(en_deathsData_long$date, format = "%Y-%m-%d")


#____________________________________________________________________________________________________________________________
#### 6) Plotting data ####

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
  ggtitle(label = "Daily and Total Number of Deaths", subtitle = "by Death Date") +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  theme(legend.position = "right", legend.box = "vertical", legend.title=element_blank(), plot.title = element_text(face = "bold"), axis.title = element_text(face = "bold")) +
  scale_fill_manual(values = "#69b3a2", labels = "Daily Deaths") +
  scale_colour_manual(values = "#e52b50", labels = "Total Deaths")


print(cum_newPlot)


#### FIGURE 3a ####
## Stacked barch chart (≤28, 29-60, ≥60 days), daily number ##
# "Windows" data - Number of daily deaths grouped by length of time from testing positive until death (≤28, 29-60, ≥60 days) #

# subset windows data
windowData <- subset(en_deathsData_long, deathMeasurementType == "deathsWithin28days" | deathMeasurementType == "deathsBetween29to60days" | deathMeasurementType == "deathsAfter60days")

# plot data with ggplot2 package
windowBarPlot<- ggplot(windowData, aes(x = date, y = value, fill = deathMeasurementType, group = deathMeasurementType, text = paste(deathMeasurementType, "\n Date:", date, "\n Value:", value))) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle(label = "Number of daily deaths", subtitle = "by length of time from testing positive until death") +
  labs(x=("Date of Deaths"), y=("Number of Deaths")) + 
  scale_fill_discrete(name="\n Number of days \n after +ve Test",
                      breaks=c("deathsWithin28days", "deathsBetween29to60days", "deathsAfter60days"),
                      labels=c("≤28 Days", "29 to 60 Days", ">60 Days")) +
  scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
  theme(plot.title = element_text(face = "bold"), axis.title = element_text(face = "bold"))


# ggplotly: making graph interactive
windowBarPlotInteractive <- ggplotly(windowBarPlot, tooltip = "text")

# changing legend names
windowBarPlotInteractive$x$data[[1]]$name <- "≤28 Days"
windowBarPlotInteractive$x$data[[2]]$name <- "29 to 60 Days"
windowBarPlotInteractive$x$data[[3]]$name <- ">60 Days"

# print stacked bar plot
print(windowBarPlotInteractive)



#### FIGURE 3b ####
## Stacked barch chart (≤28, 29-60, ≥60 days), PROPORTION ##
# "Windows" data - Number of daily deaths grouped by length of time from testing positive until death (≤28, 29-60, ≥60 days) #

# subset windows data
windowData <- subset(en_deathsData_long, deathMeasurementType == "deathsWithin28days" | deathMeasurementType == "deathsBetween29to60days" | deathMeasurementType == "deathsAfter60days")


# plot data with ggplot2 package
proportion_windowBarPlot<- ggplot(windowData, aes(x = date, y = value, fill = deathMeasurementType, group = deathMeasurementType, text = paste(deathMeasurementType, "\n Date:", date, "\n Value:", value))) + 
  geom_bar(position="fill", stat="identity") +
  ggtitle(label = "Proportion of Daily Deaths", subtitle = "by length of time from testing positive until death") +
  labs(x=("Date of Deaths"), y=("Proportion of Deaths")) + 
  scale_fill_discrete(name="\n Number of days \n after +ve Test",
                      breaks=c("deathsWithin28days", "deathsBetween29to60days", "deathsAfter60days"),
                      labels=c("≤28 Days", "29 to 60 Days", ">60 Days")) +
  scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
  theme(plot.title = element_text(face = "bold"), axis.title = element_text(face = "bold"))


# ggplotly: making graph interactive
proportion_windowBarPlotInteractive <- ggplotly(windowBarPlot, tooltip = "text")

# changing legend names
proportion_windowBarPlotInteractive$x$data[[1]]$name <- "≤28 Days"
proportion_windowBarPlotInteractive$x$data[[2]]$name <- "29 to 60 Days"
proportion_windowBarPlotInteractive$x$data[[3]]$name <- ">60 Days"

# print stacked bar plot
print(proportion_windowBarPlotInteractive)




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
  geom_bar(stat = "identity") +
  #scale_x_discrete(limits = c(deathWindows), breaks=c(deathWindows), labels=c("≤28", "29 to 60", "60+")) +
  labs(x = "Number of Days Between Positive Test and Death Date", y = "Percentage of Deaths") +
  ggtitle(label = "Percentage of Total Deaths", subtitle = "by length of time from testing positive until death") + 
  theme(legend.position = "none", plot.title = element_text(face = "bold"), axis.title = element_text(face = "bold"))

percentage_totalDeathWindowsPlot_Interactive <- ggplotly(percentage_totalDeathWindowsPlot, tooltip = "text")

print(percentage_totalDeathWindowsPlot_Interactive)



#### FIGURE 5 ####
## Multiple graphs in one plot (side-by-side) ; figures 2, 3a, 3b, and 4 ##

combinedPlot <- grid.arrange(cum_newPlot, windowBarPlot, percentage_totalDeathWindowsPlot, proportion_windowBarPlot, top = textGrob("COVID-19 Deaths Graphs", ))

print(combinedPlot)



#### TODO: ####
# 1) write code to simplify running each figure 
    # --> using functions: one function to load the data, one to run the data manipulation, one to run the appropriate figure code
# 2) figure out why percentage_windowBarPlot's colours don't match other plots

