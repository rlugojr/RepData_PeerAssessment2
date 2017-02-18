library(tidyverse)
library(reshape2)
library(data.table)
library(stringi)

file_url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
source_file <- paste0(getwd(),"/data/repdata_data_StormData.csv.bz2")
csvStormData <- paste0(getwd(),"/data/repdata_data_StormData.csv")

if (!file.exists(csvStormData)) {
    if (!file.exists(source_file)) {
        download.file(file_url, source_file)
    }
    unzip(source_file)
}

keepCols <- c("BGN_DATE","STATE","EVTYPE","FATALITIES","INJURIES","PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")

stormData <- fread(file = csvStormData, sep = ",", header = T, select = keepCols, na.strings = "NA", verbose = T, autostart = T,
                   strip.white = T, data.table = T, quote = '\"', stringsAsFactors = F, showProgress = T)

stormData$BGN_DATE <- as.Date(stormData$BGN_DATE,"%m/%d/%Y")
stormData$YEAR <- year(stormData$BGN_DATE)

stormData$EVTYPE <- stri_replace(stormData$EVTYPE, "THUNDERSTORM WINDS", regex = "(.)*?(A-Z)?T\\w+\\sWIN(D)?(S)?\\D?(.*)?")
stormData$EVTYPE <- stri_replace(stormData$EVTYPE, "FLASH FLOODS", regex = "(.*)?FLOOD(.*)?")
stormData$EVTYPE <- stri_replace(stormData$EVTYPE, "EXTREME HEAT", regex = "(.*)HEAT(.*)")
stormData$EVTYPE <- stri_replace(stormData$EVTYPE, "TORNADO", regex = "(.*)TORNADO(.*)")

population_health_1993 <- stormData %>%
    filter(YEAR >= 1993 & (FATALITIES != 0 & INJURIES != 0)) %>%
    select(EVTYPE, FATALITIES, INJURIES) %>%
    group_by(EVTYPE) %>%
    summarize(FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES), CASUALTIES = sum(FATALITIES + INJURIES)) %>%
    arrange(desc(CASUALTIES))

top_5_events <- melt(population_health_1993[1:5,],id.vars = "EVTYPE")
levels(top_5_events$EVTYPE) <- population_health_1993[1:5,]$EVTYPE

bar_population_health_Top_5 <- ggplot(top_5_events, aes(x = reorder(EVTYPE, value), y = value, fill = variable, label = value)) +
    geom_bar(stat = "identity") +
    geom_text(size = 3, color = "white", position = position_stack(vjust = 0.5)) +
    scale_y_sqrt() +
    facet_grid(.~variable) +
    scale_fill_manual(values = c("firebrick","steelblue","black"), labels = c("Fatalities", "Injuries", "Casualties")) +
    theme(axis.text.x = element_text(angle = 45, size = 6, hjust = 1)) +
    labs(main = "Top 5 Events with Highest Casualties 1993 - 2011", x = "Event Type", y = "Total Casualties (Log Sqrt)")

bar_population_health_Top_5




sessionInfo()
