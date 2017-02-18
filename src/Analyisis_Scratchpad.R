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

HumanCost <- stormData %>% group_by(EVTYPE) %>% summarise(totalFatalities = sum(FATALITIES), totalInjuries = sum(INJURIES), totalCasualties = sum(FATALITIES + INJURIES)) %>% filter(totalFatalities > 0) %>% arrange(desc(totalCasualties))


top5 <- HumanCost[1:5,]$EVTYPE

HumanCost_Top_5_Long <- HumanCost %>% melt(id.vars = "EVTYPE") %>% filter(EVTYPE %in% top5 & variable != "totalCasualties")

HumanCost_Top_5_Long$EVTYPE <- factor(HumanCost_Top_5_Long$EVTYPE)
levels(HumanCost_Top_5_Long[1:5,])

#show stacked bar plot fatalities and injuries
bar_population_health_Top_5 <- ggplot(HumanCost_Top_5_Long, aes(x = reorder(EVTYPE, value), y = value, fill = variable, label = value)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(size = 3, color = "white", position = position_stack(vjust = 0.5)) +
    scale_y_log10() +
    coord_flip() +
    scale_fill_manual(values = c("firebrick","steelblue"), name = "Outcome", labels = c("Fatalities", "Injuries")) +
    labs(Title = "Top 5 Events with Highest Casualties",x = "Event Type", y = "Casualties (log 10)")

bar_population_health_Top_5

EventsPerYearMonthDayState <- stormData %>% filter(EVTYPE %in% top5) %>% group_by(BGN_DATE, STATE, EVTYPE) %>% summarise(Events = n(),totalFatalities = sum(FATALITIES), totalInjuries = sum(INJURIES))

EventsPerYearMonthDayState$Year <- year(EventsPerYearMonthDayState$BGN_DATE)
EventsPerYearMonthDayState$Month <- month(EventsPerYearMonthDayState$BGN_DATE)

EventsPerYearMonthState <- EventsPerYearMonthDayState %>% group_by(Year, Month, STATE, EVTYPE) %>% summarise(Events = n(),totalFatalities = sum(totalFatalities), totalInjuries = sum(totalInjuries))

EventsPerYearState <- EventsPerYearMonthState %>% group_by(Year, STATE, EVTYPE) %>% summarise(Events = n(),totalFatalities = sum(totalFatalities), totalInjuries = sum(totalInjuries))

EventsPerYear <- EventsPerYearState %>% group_by(Year, EVTYPE) %>% summarise(Events = n(),totalFatalities = sum(totalFatalities), totalInjuries = sum(totalInjuries))

EventsPerState <- EventsPerYearState %>% group_by(STATE, EVTYPE) %>% summarise(Events = n(),totalFatalities = sum(totalFatalities), totalInjuries = sum(totalInjuries))

ggplot(EventsPerYear, aes(x = totalFatalities + totalInjuries, y = Events, fill = EVTYPE)) +
    geom_point(aes(color = EVTYPE))

ggplot(EventsPerYear, aes(x = Year, y = totalFatalities + totalInjuries, fill = EVTYPE)) +
    geom_line(aes(color = EVTYPE))

ggplot(filter(EventsPerYear,Year > 1993), aes(x = Year, y = totalFatalities + totalInjuries, fill = EVTYPE)) +
    geom_line(aes(color = EVTYPE))


Revised_HumanCost <- stormData %>% filter(year(BGN_DATE) > 1993 & FATALITIES > 0 & INJURIES > 0) %>% select(BGN_DATE, STATE, EVTYPE, FATALITIES, INJURIES) %>% mutate(YEAR = year(BGN_DATE)) %>% select(YEAR, STATE, EVTYPE, FATALITIES, INJURIES) %>% group_by(YEAR, STATE, EVTYPE) %>% summarize(Events = n(), totalFatalities = sum(FATALITIES), totalInjuries = sum(INJURIES))

Revised_HumanCost_Year <-Revised_HumanCost %>% group_by(YEAR, EVTYPE) %>% summarize(Events = sum(Events), totalFatalities = sum(totalFatalities), totalInjuries = sum(totalInjuries), totalCasualties = sum(totalFatalities + totalInjuries)) %>% arrange(desc(totalCasualties))


Revised_HumanCost_Event <-Revised_HumanCost %>% select(EVTYPE,Events, totalFatalities, totalInjuries, -YEAR, -STATE) %>% group_by(EVTYPE) %>% summarize(Events = sum(Events),totalFatalities = sum(totalFatalities), totalInjuries = sum(totalInjuries), totalCasualties = sum(totalFatalities + totalInjuries)) %>% arrange(desc(totalCasualties))


ggplot(filter(, year(BGN_DATE) > 1993), aes(x = Year, y = totalFatalities + totalInjuries, fill = EVTYPE)) +
    geom_line(aes(color = EVTYPE))


events_raw <- data.frame(tags = unique(stormData[year(stormData$BGN_DATE) == 1993 & FATALITIES >0 & INJURIES >0]$EVTYPE), stringsAsFactors = F)
mutate(events_raw, linenumber = row_number())
events_raw %>% unnest_tokens(word, tags, to_lower = F)

events_unnested <- events_raw %>% unnest_tokens(word, tags, to_lower = F)
events_unnested %>% count(word, sort = T)


events_categories <- data.frame( categories = c("TORNADO", "HEAT","WIND", "FLOOD","ICE", "SNOW", "FIRE"), stringsAsFactors = F)
mutate(events_categories, linenumber = row_number())

fuzzy_matches <- events_unnested %>% stringdist_inner_join(events_categories, by=c(word = "categories"), max_dist = 0, distance_col = "distance")
