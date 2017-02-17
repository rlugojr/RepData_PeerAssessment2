library(tidyverse)
library(reshape2)
library(data.table)

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
csvStormData <- paste0(getwd(),"../../repdata_data_StormData.csv")

stormData <- fread(file = csvStormData, sep = ",", header = T, na.strings = "NA", verbose = T, autostart = T, strip.white = T, data.table = T,
                   quote = '\"', stringsAsFactors = F, showProgress = T)

HumanCost <- stormData %>% group_by(EVTYPE) %>% summarise(totalFatalities = sum(FATALITIES), totalInjuries = sum(INJURIES), totalCasualties = sum(FATALITIES + INJURIES)) %>% filter(totalFatalities > 0) %>% arrange(desc(totalCasualties))

top5 <- HumanCost[1:5,]$EVTYPE

HumanCost_Top_5_Long <- HumanCost %>% melt(id.vars = "EVTYPE") %>% filter(EVTYPE %in% top5 & variable != "totalCasualties")

HumanCost_Top_5_Long$EVTYPE <- factor(HumanCost_Top_5_Long$EVTYPE)
levels(HumanCost_Top_5_Long[1:5,])

#show stacked bar plot fatalities and injuries
bar_population_health_Top_5 <- ggplot(HumanCost_Top_5_Long, aes(x = EVTYPE, y = value, fill = variable, label = value)) +
    geom_bar(stat="identity", position = "stack") +
    geom_text(size = 3, color = "white", position = position_stack(vjust = 0.5)) +
    scale_y_log10() +
    coord_flip() +
    scale_fill_manual(values = c("firebrick","steelblue"), name = "Outcome", labels = c("Fatalities", "Injuries")) +
    labs(Title = " ",x = "Event Type", y = "Casualties (log 10)")

bar_population_health_Top_5


