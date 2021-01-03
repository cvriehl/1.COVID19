
#________________________________________________________ 1.1 DEATHS ________________________________________________________ 

#___________________________________ 1.11 NEW DEATHS WITHIN 28 DAYS & 60 DAYS OF +VE TEST____________________________________



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
    oldCumDeathValue <- en_deathsData[dateValue + 1, which(colnames(en_deathsData)== cumColumnName)]
    
    if (is.na(oldCumDeathValue) == FALSE) {
      
      deathPerDay <- newCumDeathValue - oldCumDeathValue
      
      en_deathsData[dateValue, which(colnames(en_deathsData) == newColumnName)] <- deathPerDay
      
    } else {
      
      en_deathsData[dateValue, which(colnames(en_deathsData) == newColumnName)] <- newCumDeathValue
      
    }
    
  }
  
}
