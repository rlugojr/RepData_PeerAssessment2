#environment prep and report

library(tidyverse)
library(reshape2)
library(data.table)
library(stringi)
library(gridExtra)

options(scipen = 999)

sessionInfo()

#source data file download
data_dir <- paste0(getwd(),"/data")
file_url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
source_file <- paste(data_dir,"repdata_data_StormData.csv.bz2", sep = "/")
csvStormData <- paste(data_dir,"repdata_data_StormData.csv", sep = "/")

if (!file.exists(csvStormData)) {
    if (!file.exists(source_file)) {
        if (!dir.exists(data_dir)) {
            dir.create(data_dir)
        }
        download.file(file_url, source_file)
    }
    unzip(source_file, "repdata_data_StormData.csv", list = F, junkpaths = F,exdir = "./data", unzip = "internal")
}

# data File processing
keepCols <- c("BGN_DATE","EVTYPE","FATALITIES","INJURIES","PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")

stormData <- fread(file = csvStormData, sep = ",", header = T, select = keepCols, na.strings = "NA", verbose = T, autostart = T,
                   strip.white = T, data.table = T, quote = '\"', stringsAsFactors = F, showProgress = T)

#char to date field conversion
stormData$BGN_DATE <- as.Date(stormData$BGN_DATE,"%m/%d/%Y")
#create YEAR field from BGN_DATE
stormData$YEAR <- year(stormData$BGN_DATE)

#cleanup pertinent EVTYPE variants using regexes before analysis for more accuracy
stormData$EVTYPE <- stri_replace(stormData$EVTYPE, "THUNDERSTORM", regex = "(.)*?(A-Z)?T\\w+\\sWIN(D)?(S)?\\D?(.*)?")
stormData$EVTYPE <- stri_replace(stormData$EVTYPE, "THUNDERSTORM", regex = "(.*)THUNDERSTORM(.*)")
stormData$EVTYPE <- stri_replace(stormData$EVTYPE, "FLASH FLOODS", regex = "(.*)?FLOOD(.*)?")
stormData$EVTYPE <- stri_replace(stormData$EVTYPE, "EXTREME HEAT", regex = "(.*)HEAT(.*)")
stormData$EVTYPE <- stri_replace(stormData$EVTYPE, "TORNADO", regex = "(.*)TORNADO(.*)")
stormData$EVTYPE <- stri_replace(stormData$EVTYPE, "HAIL", regex = "(.*)HAIL(.*)")
stormData$EVTYPE <- stri_replace(stormData$EVTYPE, "HURRICANE", regex = "(.*)HURRICANE(.*)")
stormData$EVTYPE <- stri_replace(stormData$EVTYPE, "TROPICAL STORM", regex = "(.*)TROPICAL STORM(.*)")

#The majority of data reporting for all EVTYPES starts around 1993.
#Using that as a selection filter on YEAR to keep tornados from becoming an outlier.
#limiting records to ones that have FATALITIES or INJURIES
population_health_1993 <- stormData %>%
    filter(YEAR >= 1993 & (FATALITIES != 0 | INJURIES != 0)) %>%
    select(EVTYPE, FATALITIES, INJURIES) %>%
    group_by(EVTYPE) %>%
    summarize(FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES), CASUALTIES = sum(FATALITIES + INJURIES)) %>%
    arrange(desc(CASUALTIES))

#
top_5_pop_events <- melt(population_health_1993[1:5,],id.vars = "EVTYPE")
levels(top_5_pop_events$EVTYPE) <- population_health_1993[1:5,]$EVTYPE

bar_population_health_Top_5 <- ggplot(top_5_pop_events, aes(x = reorder(EVTYPE, value), y = value, fill = variable, label = format(round(value/1000,1), big.mark = ",", scientific = F))) +
    geom_bar(stat = "identity") +
    geom_text(size = 3, color = "white", position = position_stack(vjust = 0.5)) +
    scale_y_sqrt() +
    facet_grid(.~variable) +
    scale_fill_manual(values = c("firebrick","steelblue","black"), labels = c("Fatalities", "Injuries", "Casualties")) +
    theme(axis.text.x = element_text(angle = 45, size = 6, hjust = 1)) +
    labs(main = "Top 5 Events with Highest Casualties 1993 - 2011", x = "Event", y = "Total Casualties in Thousands (Log sqrt)") +
    ggsave("figures/top_5_pop_events.png")

bar_population_health_Top_5


#Question 2

#create tables to hold damage code and corresponding magnitude for PROP and CROP.
propDamageCalc <- data.frame(
    PROPDMGEXP = c("K", "M", "B"),
    PROPDMGMULT = c(1000,1000000,1000000000),
    stringsAsFactors = F
)

cropDamageCalc <- data.frame(
    CROPDMGEXP = c("K", "M", "B"),
    CROPDMGMULT = c(1000,1000000,1000000000),
    stringsAsFactors = F
)

damageData <- stormData %>%
    filter(YEAR >= 1993 & (PROPDMG != 0 | CROPDMG != 0)) %>%
    select(EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)

damageData <- inner_join(damageData, propDamageCalc, by = "PROPDMGEXP")
damageData <- inner_join(damageData, cropDamageCalc, by = "CROPDMGEXP")

damageData <- mutate(damageData, PROPDMGCOST = PROPDMG * PROPDMGMULT, CROPDMGCOST = CROPDMG * CROPDMGMULT)

Event_Crop_Prop_Cost <- damageData %>%
    select(EVTYPE, PROPDMGCOST, CROPDMGCOST) %>%
    group_by(EVTYPE) %>%
    summarize(PROPERTY = sum(PROPDMGCOST), CROP = sum(CROPDMGCOST), TOTAL = sum(PROPDMGCOST + CROPDMGCOST)) %>%
    arrange(desc(TOTAL))

top_5_cost_events <- melt(Event_Crop_Prop_Cost[1:5,],id.vars = "EVTYPE")
levels(top_5_cost_events$EVTYPE) <- Event_Crop_Prop_Cost[1:5,]$EVTYPE

bar_Cost_Top_5 <- ggplot(top_5_cost_events, aes(x = reorder(EVTYPE, value), y = value, fill = variable, label = format(round(value/1000000000,1), big.mark = ",", scientific = F))) +
    geom_bar(stat = "identity") +
    geom_text(size = 3, color = "white", position = position_stack(vjust = 0.5)) +
    scale_y_sqrt() +
    facet_grid(.~variable) +
    scale_fill_manual(values = c("darkgreen","brown","black"), labels = c("Property", "Crop", "Total")) +
    theme(axis.text.x = element_text(angle = 45, size = 6, hjust = 1)) +
    labs(title = "Top 5 Events with Highest Costs 1993 - 2011", x = "Event", y = "Cost in Billions (Log sqrt)") +
    ggsave("figures/top_5_cost_events.png")

bar_Cost_Top_5

#forecast
#find any records that are the top cause of casaulty and damage
top_events <- unique(top_5_pop_events[1:5,]$EVTYPE, top_5_cost_events[1:5,]$EVTYPE)
topEventsCountData <- stormData %>%
    filter(YEAR >= 1993 & (EVTYPE %in% top_events)) %>%
    select(YEAR, EVTYPE) %>%
    group_by(YEAR,EVTYPE) %>%
    summarize(event_count = mean(n()))

topEventsCountData$EVTYPE <- factor(topEventsCountData$EVTYPE)

#plot number of each event per year
plot_num_events_per_year <- ggplot(topEventsCountData, aes(x = YEAR, y = event_count, fill = EVTYPE, color = EVTYPE)) +
    geom_line(lwd = 1) +
    geom_smooth(method = "lm", se = F, linetype = 2)


topEventsMeanEffectsData <- stormData %>%
    filter(YEAR >= 1993 & (EVTYPE %in% top_events)) %>%
    select(YEAR, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)

topEventsMeanEffectsData <- inner_join(topEventsMeanEffectsData, propDamageCalc, by = "PROPDMGEXP")
topEventsMeanEffectsData <- inner_join(topEventsMeanEffectsData, cropDamageCalc, by = "CROPDMGEXP")
    mutate(topEventsMeanEffectsData, PROPDMGCOST = PROPDMG * PROPDMGMULT, CROPDMGCOST = CROPDMG * CROPDMGMULT)

topEventsMeanEffectsData <- topEventsMeanEffectsData %>%
    group_by(YEAR,EVTYPE) %>%
    summarize(FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES), CASUALTIES = sum(FATALITIES + INJURIES), PROPDMG = sum(PROPDMG), CROPDMG = sum(CROPDMG), COST = sum(PROPDMG + CROPDMG)) %>%
    arrange(YEAR, EVTYPE)

plot_effects_scatter <- ggplot(topEventsMeanEffectsData, aes(x = CASUALTIES, y = COST, fill = EVTYPE)) +
    geom_point(aes(color = EVTYPE, shape = EVTYPE)) +
    scale_x_log10()

topEventsReport <- topEventsMeanEffectsData %>%
    group_by(EVTYPE) %>%
    summarize(FATALITIES = mean(FATALITIES), INJURIES = mean(INJURIES), CASUALTIES = mean(FATALITIES + INJURIES), PROPDMG = mean(PROPDMG), CROPDMG = mean(CROPDMG), COST = sum(PROPDMG + CROPDMG)) %>%
    select(EVTYPE, CASUALTIES,COST) %>%
    arrange(desc(CASUALTIES),desc(COST))

grid.arrange(plot_num_events_per_year,plot_effects_scatter, nrow  = 2)

topEventsReport[1,]

topEventsReport[2,]


options(scipen = 0)
