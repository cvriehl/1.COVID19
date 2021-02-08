#________________________________________________________ 1.1 DEATHS ________________________________________________________ 



#_________________________________________________ 1.1.1. DEATH DATA SCRIPT _________________________________________________




#____________________________________________________________________________________________________________________________
#### SCRIPT EXPLANATION ####


#____________________________________________________________________________________________________________________________
#### Packages Used: ####

# data.table
install.packages("data.table")
library(data.table)

# scales (within ggplot2)
#to customise dates (x-axis) in ggplot
library(scales)

# ggplot2
install.packages("ggplot2")
library(ggplot2)

#____________________________________________________________________________________________________________________________
                                              ##### SECTION A #####

  ### Code to generate Figure1: Bar plot of daily cases and line plot of total (cummulated) cases by specimen date ###

#____________________________________________________________________________________________________________________________
#### 1) loading data from UK Government Website (https://coronavirus.data.gov.uk/details/download) ####

#en_CasesData <- read.csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=femaleCases&metric=maleCases&format=csv", row.names = NULL, header = TRUE, stringsAsFactors = TRUE, quote = "", skipNul = TRUE, comment.char = "", sep = ",")


en_New_Cum_CasesData <- read.csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesBySpecimenDate&metric=cumCasesBySpecimenDate&format=csv")

str(en_CasesData)
tail(en_New_Cum_CasesData_long)
head(en_New_Cum_CasesData)


#____________________________________________________________________________________________________________________________
#### 2) Prepare data for ggplot2 ####

# Package: data.table
# change en_New_Cum_CasesData from 'wide' to 'long' format
#en_New_Cum_CasesData_long <- melt(setDT(en_New_Cum_CasesData), id.vars = c("date","areaType", "areaCode", "areaName"), variable.name = "casesMeasurementType")

# change date order to ascending
en_New_Cum_CasesData <- en_New_Cum_CasesData[order(as.Date(en_New_Cum_CasesData$date, format = "%Y-%m-%d")),]

# change data type of en_New_Cum_CasesData_long$date from 'factor' to 'Date'
en_New_Cum_CasesData$date <- as.Date(en_New_Cum_CasesData$date, format = "%Y-%m-%d")

data.class(en_New_Cum_CasesData$date)


#____________________________________________________________________________________________________________________________
#### 3) Plotting data with ggplot2 ####


#### FIGURE 1 ####
## Bar plot of daily cases and line plot of total (cummulated) cases by specimen date

new_cum_CasesPlot <- ggplot(data = en_New_Cum_CasesData, aes(x = date)) +
  theme_minimal() +
  geom_bar(aes(y = newCasesBySpecimenDate, fill = "#69b3a2"), stat = "identity", position = "dodge") +
  geom_line(aes(y =  cumCasesBySpecimenDate / 50, colour = "#e52b50"), size = 1) +
  geom_vline(xintercept = as.Date("2020-05-14"), linetype = "dashed") +
  scale_y_continuous(name = "Daily Number of Cases", sec.axis = sec_axis(trans = ~.*50, name = "Total Number of Cases")) +
  labs(x=("")) +
  ggtitle(label = "Daily and Total Number of COVID-19 Cases", subtitle = "by Specimen Date") +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  theme(legend.position = "right", legend.box = "vertical", legend.title=element_blank(), plot.title = element_text(face = "bold"), axis.title = element_text(face = "bold")) +
  scale_fill_manual(values = "#69b3a2", labels = "Daily Cases") +
  scale_colour_manual(values = "#e52b50", labels = "Total Cases") +
  annotate(geom = "text", x = as.Date("2020-03-15"), y = 65000, label = "Targeted testing only") +
  annotate(geom = "text", x = as.Date("2020-08-01"), y = 65000, label = "Wider testing available")
  
print(new_cum_CasesPlot) 



#____________________________________________________________________________________________________________________________
                                                ##### SECTION B #####

### Code to generate Figure2: Bar plot of daily cases and line plot of total (cummulated) cases by specimen date ###

#____________________________________________________________________________________________________________________________
#### 1) Load data ####
# female cases by age and gender 
en_Fem_Male_Age_CasesData <- read.csv("~/Desktop/OwnProject/1.COVID19/1.2.Data/1.2.2.Cases/CasesData.csv", row.names = NULL, header = TRUE, stringsAsFactors = TRUE)

#____________________________________________________________________________________________________________________________
#### 2) Manipulate Data ####

## a) Change date order to ascending
en_Fem_Male_Age_CasesData <- en_Fem_Male_Age_CasesData[order(as.Date(en_Fem_Male_Age_CasesData$date, format = "%d/%m/%Y")),]

## b) Change data type of en_New_Cum_CasesData_long$date from 'factor' to 'Date'
en_Fem_Male_Age_CasesData$date <- as.Date(en_Fem_Male_Age_CasesData$date, format = "%d/%m/%Y")


## c) Find total rate of female and total rate of male cases by date (adding age groups together)

# Find list of dates and list of metrics (femaleCases and maleCases)
datesList <- unique(en_Fem_Male_Age_CasesData$date)
sexList <- as.character(unique(en_Fem_Male_Age_CasesData$metric)) 


# Create Data frame to put the resulting values of the for loop in
# empty data.frame
casesRates_byDateSex_DF <- data.frame(matrix(ncol = 3, nrow = 2*(length(unique(en_Fem_Male_Age_CasesData$date)))))

# add column names
colnames(casesRates_byDateSex_DF) <- c("date", "metric", "rate")

# add dates to date column so that they appear twice (once for female rates and once for male rates)
casesRates_byDateSex_DF$date <- rep(unique(en_Fem_Male_Age_CasesData$date), 2)

# add metrics values (femaleCases and maleCases), so that there are two  for each date
casesRates_byDateSex_DF$metric <- c(rep(sexList[1], length(unique(en_Fem_Male_Age_CasesData$date))), rep(sexList[2], length(unique(en_Fem_Male_Age_CasesData$date))))


# for loop to calculate rate for each sex at each date, results go into casesRates_byDateSex_DF data frame
for (dateNumber in 1:length(datesList)) {
  
  #dateNumber <- 1
  dateValue <- datesList[dateNumber]
  
  
  for (sexNumber in 1:length(sexList)) {
    
    #sexNumber <- 2
    sex <- sexList[sexNumber]
    
    ratesList <- subset(en_Fem_Male_Age_CasesData, en_Fem_Male_Age_CasesData$date == dateValue & en_Fem_Male_Age_CasesData$metric == sex, select = rate)
    sumRates <- sum(ratesList[,1])
    
    casesRates_byDateSex_DF[which(casesRates_byDateSex_DF$date == dateValue & casesRates_byDateSex_DF$metric == sex), which(colnames(casesRates_byDateSex_DF) == "rate")] <- sumRates
    
    #newRow <- cbind(dateValue, sex, sumRates)
    #casesRates_byDateSex_DF <- rbind(casesRates_byDateSex_DF, newRow)
    
  }
  
}
 
print(casesRates_byDateSex_DF)


#____________________________________________________________________________________________________________________________
#### 3) Plot data

#### FIGURE 2 ####
## Line plot of the cummulative rate of COVID-19 cases in Women and Men

female_male_casesPlot <- ggplot(data = casesRates_byDateSex_DF, aes(x = date, y = rate, group = metric, colour = metric)) +
  geom_line() +
  theme_minimal() +
  labs(x=("")) +
  ggtitle(label = "Cummulative Rate of COVID-19 Cases in Women and Men", subtitle = "by Specimen Date") +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  scale_y_continuous(name = "Cummulative Rate of Cases") +
  theme(legend.position = "right", legend.box = "vertical", legend.title=element_blank(), plot.title = element_text(face = "bold")) +
  scale_colour_manual(values = c("#e52b50", "#69b3a2"), labels = c("Female Cases", "Male Cases"))
  


