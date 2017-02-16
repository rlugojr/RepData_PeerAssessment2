library(tidyverse)
library(data.table)

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
csvStormData <- paste0(getwd(),"/repdata_data_StormData.csv")

stormData <- fread(file = csvStormData, sep = ",", header = T, na.strings = "NA", verbose = T, autostart = T, strip.white = T, data.table = T,
                   quote = '\"', stringsAsFactors = F, showProgress = T)

HumanCost <- stormData %>% group_by(EVTYPE) %>% summarise(totalFatalities = sum(FATALITIES), totalInjuries = sum(INJURIES), totalCasualties = sum(FATALITIES + INJURIES)) %>% filter(totalFatalities > 0) %>% arrange(desc(totalCasualties))
