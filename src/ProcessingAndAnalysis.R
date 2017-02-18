library(tidyverse)
library(reshape2)
library(data.table)

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
csvStormData <- paste0(getwd(),"../../repdata_data_StormData.csv")

keepCols <- c("BGN_DATE","STATE","EVTYPE","FATALITIES","INJURIES","PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")

stormData <- fread(file = csvStormData, sep = ",", header = T, select = keepCols, na.strings = "NA", verbose = T, autostart = T,
                   strip.white = T, data.table = T, quote = '\"', stringsAsFactors = F, showProgress = T)

stormData$BGN_DATE <- as.Date(stormData$BGN_DATE,"%m/%d/%Y")
stormData$YEAR <- year(stormData$BGN_DATE)

stormData$EVTYPE <- stri_replace(stormData$EVTYPE, "THUNDERSTORM WINDS", regex = "(.)*?(A-Z)?T\\w+\\sWIN(D)?(S)?\\D?(.*)?")
stormData$EVTYPE <- stri_replace(stormData$EVTYPE, "FLASH FLOODS", regex = "(.*)?FLOOD(.*)?")
stormData$EVTYPE <- stri_replace(stormData$EVTYPE, "EXTREME HEAT", regex = "(.*)HEAT(.*)")
stormData$EVTYPE <- stri_replace(stormData$EVTYPE, "TORNADO", regex = "(.*)TORNADO(.*)")


Events_with_Casualties_by_Year <- stormData %>%
    select(YEAR, EVTYPE, FATALITIES, INJURIES) %>%
    filter(FATALITIES > 0 | INJURIES > 0) %>%
    group_by(YEAR, EVTYPE) %>%
    summarise(Events = n(),totalFatalities = sum(FATALITIES), totalInjuries = sum(INJURIES), totalCasualties = sum(FATALITIES + INJURIES))


Events_with_Casualty_Total <- Events_with_Casualties_by_Year %>% group_by(EVTYPE) %>% summarise(Events = n(), totalCasualties = sum(totalCasualties)) %>% arrange(desc(totalCasualties))

Events_with_Casualty_Total #Note Thunderstorm winds is in the top 20 3 times, textual variation, so the EVTYPE would require cleanup for a more precise report

top_10_EVTYPE <- data.frame( EVTYPE = Events_with_Casualty_Total[1:10,]$EVTYPE, stringsAsFactors = F) #added two more because of the 2 extra variations of "THUNDERSTORM WINDS"

top_10_EVTYPE

top_10_EVTYPE_OVER_TIME <- inner_join(Events_with_Casualties_by_Year, top_10_EVTYPE, by = "EVTYPE")

ggplot(top_10_EVTYPE_OVER_TIME, aes(x = YEAR, y = totalCasualties, fill = EVTYPE)) +
    geom_line(aes(color = EVTYPE))

ggplot(filter(top_10_EVTYPE_OVER_TIME, YEAR >= 1993), aes(x = YEAR, y = totalCasualties, fill = EVTYPE)) +
    geom_line(aes(color = EVTYPE))

Events_with_Casualty_Total_1993 <- Events_with_Casualties_by_Year %>% filter(YEAR >= 1993) %>% group_by(EVTYPE) %>% summarise(Events = n(), totalCasualties = sum(totalCasualties)) %>% arrange(desc(totalCasualties))

top_5_EVTYPE_1993 <- data.frame( EVTYPE = Events_with_Casualty_Total_1993[1:5,]$EVTYPE, stringsAsFactors = F)

top_5_EVTYPE_1993_OVER_TIME <- inner_join(filter(Events_with_Casualties_by_Year, YEAR >= 1993), top_5_EVTYPE_1993, by = "EVTYPE")


ggplot(top_5_EVTYPE_1993_OVER_TIME, aes(x = YEAR, y = totalCasualties, fill = EVTYPE)) +
    geom_line(aes(color = EVTYPE))

health_top_5_over_time <- top_5_EVTYPE_1993_OVER_TIME %>%
    select(EVTYPE, totalFatalities, totalInjuries)  %>%
    group_by(EVTYPE) %>%
    summarize(totalFatalities = sum(totalFatalities), totalInjuries = sum(totalInjuries), totalCasualties = sum(totalFatalities + totalInjuries)) %>%
    melt(id.vars = "EVTYPE")

bar_population_health_Top_5 <- ggplot(health_top_5_over_time, aes(x = reorder(EVTYPE, value), y = value, fill = variable, label = value)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(size = 3, color = "white", position = position_stack(vjust = 0.5)) +
    #scale_y_log10() +
    coord_flip() +
    facet_grid(variable~.) +
    scale_fill_manual(values = c("firebrick","steelblue","black"), name = "Outcome", labels = c("Fatalities", "Injuries", "Casualties")) +
    labs(Title = "Top 5 Events with Highest Casualties",x = "Event Type", y = "Casualties (log 10)")

bar_population_health_Top_5
