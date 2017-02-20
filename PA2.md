# Reproducible Research Peer Assessment 2
Ray Lugo, Jr.  
February 17, 2017  



###Synopsis
The NOAA Storm database contains observations of weather events across the United States.  The subset used in this report include observations, at the individual State level, from 1950 - 2011.  The purpose of this analysis is to determine the weather events which cause the highest number of casualties (injury and death) to the human population as well as the most damage caused to property and crops in terms of estimated cost to repair or replace.  During analysis, we have determined that the data required normalization, as there was substantial variance in the categorization of the Events themselves which would lead to error.
As a result of the analysis, we have learned that the highest human toll comes from Tornados and Flash Floods and the most property damage comes from Flash Floods and Thunderstorms. On average, Tornados cause 685.47 casualties, Thunderstorms cause \$68,992.79K in damage and Flash Floods cause 412.84 casualties and $84,480K in property and crop damage. 

###Environment Prep and Session Report


```r
library(tidyverse)
```

```
## Loading tidyverse: ggplot2
## Loading tidyverse: tibble
## Loading tidyverse: tidyr
## Loading tidyverse: readr
## Loading tidyverse: purrr
## Loading tidyverse: dplyr
```

```
## Conflicts with tidy packages ----------------------------------------------
```

```
## filter(): dplyr, stats
## lag():    dplyr, stats
```

```r
library(reshape2)
```

```
## 
## Attaching package: 'reshape2'
```

```
## The following object is masked from 'package:tidyr':
## 
##     smiths
```

```r
library(data.table)
```

```
## -------------------------------------------------------------------------
```

```
## data.table + dplyr code now lives in dtplyr.
## Please library(dtplyr)!
```

```
## -------------------------------------------------------------------------
```

```
## 
## Attaching package: 'data.table'
```

```
## The following objects are masked from 'package:reshape2':
## 
##     dcast, melt
```

```
## The following objects are masked from 'package:dplyr':
## 
##     between, first, last
```

```
## The following object is masked from 'package:purrr':
## 
##     transpose
```

```r
library(stringi)
library(gridExtra)
```

```
## 
## Attaching package: 'gridExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```r
options(scipen = 999)

sessionInfo()
```

```
## R version 3.3.2 (2016-10-31)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 14393)
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] gridExtra_2.2.1   stringi_1.1.2     data.table_1.10.4
##  [4] reshape2_1.4.2    dplyr_0.5.0       purrr_0.2.2      
##  [7] readr_1.0.0       tidyr_0.6.1       tibble_1.2       
## [10] ggplot2_2.2.1     tidyverse_1.1.1  
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.9      plyr_1.8.4       forcats_0.2.0    tools_3.3.2     
##  [5] digest_0.6.12    jsonlite_1.2     lubridate_1.6.0  evaluate_0.10   
##  [9] nlme_3.1-131     gtable_0.2.0     lattice_0.20-34  psych_1.6.12    
## [13] DBI_0.5-1        yaml_2.1.14      parallel_3.3.2   haven_1.0.0     
## [17] xml2_1.1.1       stringr_1.1.0    httr_1.2.1       knitr_1.15.9    
## [21] hms_0.3          rprojroot_1.2    grid_3.3.2       R6_2.2.0        
## [25] readxl_0.1.1     foreign_0.8-67   rmarkdown_1.3    modelr_0.1.0    
## [29] magrittr_1.5     backports_1.0.5  scales_0.4.1     htmltools_0.3.5 
## [33] rvest_0.3.2      assertthat_0.1   mnormt_1.5-5     colorspace_1.3-2
## [37] lazyeval_0.2.0   munsell_0.4.3    broom_0.4.1
```

### data file download

```r
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
```

```
## Input contains no \n. Taking this to be a filename to open
## File opened, filesize is 0.523066 GB.
## Memory mapping ... ok
## Detected eol as \r\n (CRLF) in that order, the Windows standard.
## Positioned on line 1 after skip or autostart
## This line is the autostart and not blank so searching up for the last non-blank ... line 1
## Using supplied sep ',' ... found ok
## Detected 37 columns. Longest stretch was from line 1 to line 30
## Starting data input on line 1 (either column names or first row of data). First 10 characters: "STATE__",
## 'header' changed by user from 'auto' to TRUE
## Count of eol: 1307675 (including 1 at the end)
## Count of sep: 34819802
## nrow = MIN( nsep [34819802] / (ncol [37] -1), neol [1307675] - endblanks [1] ) = 967216
## Type codes (point  0): 3444344430000303003343333430000333303
## Type codes (point  1): 3444344434444303443343333434444333343
## Type codes (point  2): 3444344434444303443343333434444333343
## Type codes (point  3): 3444344434444303443343333434444333343
## Type codes (point  4): 3444344434444303443343333434444333343
## Type codes (point  5): 3444344434444303443343333434444333343
## Type codes (point  6): 3444344434444303443343333434444333343
## Type codes (point  7): 3444344434444303443343333434444333343
## Type codes (point  8): 3444344434444303443343333434444333343
## Type codes (point  9): 3444344434444303443343333434444333343
## Type codes (point 10): 3444344434444303443343333434444333343
## Type codes: 3444344434444303443343333434444333343 (after applying colClasses and integer64)
## Type codes: 5455555455555555555555333434555555555 (after applying drop or select (if supplied)
## Allocating 8 column slots (37 - 29 dropped)
## 
Read 84.8% of 967216 rows
Read 902297 rows and 8 (of 37) columns from 0.523 GB file in 00:00:03
## Read fewer rows (902297) than were allocated (967216).
##    0.000s (  0%) Memory map (rerun may be quicker)
##    0.001s (  0%) sep and header detection
##    0.617s ( 29%) Count rows (wc -l)
##    0.004s (  0%) Column type detection (100 rows at 10 points)
##    0.258s ( 12%) Allocation of 902297x37 result (xMB) in RAM
##    1.268s ( 59%) Reading data
##    0.000s (  0%) Allocation for type bumps (if any), including gc time if triggered
##    0.000s (  0%) Coercing data already read in type bumps (if any)
##    0.003s (  0%) Changing na.strings to NA
##    2.151s        Total
```

```r
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
```



```r
#The majority of data reporting for all EVTYPES starts around 1993.
#Using that as a selection filter on YEAR to keep tornados from becoming an outlier.
#limiting records to ones that have FATALITIES or INJURIES
population_health_1993 <- stormData %>%
    filter(YEAR >= 1993 & (FATALITIES != 0 | INJURIES != 0)) %>%
    select(EVTYPE, FATALITIES, INJURIES) %>%
    group_by(EVTYPE) %>%
    summarize(FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES), CASUALTIES = sum(FATALITIES + INJURIES)) %>%
    arrange(desc(CASUALTIES))
```
##Results in which your results are presented.

###Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?


```r
top_5_pop_events <- melt(population_health_1993[1:5,],id.vars = "EVTYPE")
levels(top_5_pop_events$EVTYPE) <- population_health_1993[1:5,]$EVTYPE

bar_population_health_Top_5 <- ggplot(top_5_pop_events, aes(x = reorder(EVTYPE, value), y = value, fill = variable, label = format(round(value/1000,1), big.mark = ",", scientific = F))) +
    geom_bar(stat = "identity") +
    geom_text(size = 3, color = "white", position = position_stack(vjust = 0.5)) +
    scale_y_sqrt() +
    facet_grid(.~variable) +
    scale_fill_manual(values = c("firebrick","steelblue","black"), labels = c("Fatalities", "Injuries", "Casualties")) +
    theme(axis.text.x = element_text(angle = 45, size = 6, hjust = 1)) +
    labs(main = "Top 5 Events with Highest Casualties 1993 - 2011", x = "Event", y = "Total Casualties in Thousands (Log sqrt)")

bar_population_health_Top_5
```

![](PA2_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

###Across the United States, which types of events have the greatest economic consequences?


```r
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
    labs(title = "Top 5 Events with Highest Costs 1993 - 2011", x = "Event", y = "Cost in Billions (Log sqrt)")

bar_Cost_Top_5
```

![](PA2_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


##Future Events Trend and Estimated Damage



```r
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
topEventsMeanEffectsData <- mutate(topEventsMeanEffectsData, PROPDMGCOST = PROPDMG * PROPDMGMULT, CROPDMGCOST = CROPDMG * CROPDMGMULT)

topEventsMeanEffectsData <- topEventsMeanEffectsData %>%
    group_by(YEAR,EVTYPE) %>%
    summarize(FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES), CASUALTIES = sum(FATALITIES + INJURIES), PROPDMG = sum(PROPDMG), CROPDMG = sum(CROPDMG), COST = sum(PROPDMG + CROPDMG)) %>%
    arrange(YEAR, EVTYPE)


plot_effects_scatter <- ggplot(topEventsMeanEffectsData, aes(x = CASUALTIES, y = COST, fill = EVTYPE)) +
    geom_point(aes(color = EVTYPE, shape = EVTYPE)) +
    scale_x_log10()

grid.arrange(plot_num_events_per_year,plot_effects_scatter, nrow  = 2)
```

```
## Warning: Transformation introduced infinite values in continuous x-axis
```

![](PA2_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
topEventsReport <- topEventsMeanEffectsData %>%
    group_by(EVTYPE) %>%
    summarize(FATALITIES = mean(FATALITIES), INJURIES = mean(INJURIES), CASUALTIES = mean(FATALITIES + INJURIES), PROPDMG = mean(PROPDMG), CROPDMG = mean(CROPDMG), COST = sum(PROPDMG + CROPDMG)) %>%
    select(EVTYPE, CASUALTIES,COST) %>%
    arrange(desc(CASUALTIES),desc(COST))


topEventsReport[1,]
```

```
## # A tibble: 1 × 3
##    EVTYPE CASUALTIES     COST
##     <chr>      <dbl>    <dbl>
## 1 TORNADO   685.4737 38023.12
```

```r
topEventsReport[2,]
```

```
## # A tibble: 1 × 3
##         EVTYPE CASUALTIES     COST
##          <chr>      <dbl>    <dbl>
## 1 FLASH FLOODS   412.8421 84480.48
```




```r
options(scipen = 0)
```
