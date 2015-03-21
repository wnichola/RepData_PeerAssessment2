# Weather Impact on Population Health and the Economy
Nicholas Wee  
Wednesday, March 18, 2015  


# Synopsis
This reports looks at Weather Events that have significant impact on Population Health and the Economy.  From the period of 1950 to 2011, summary analysis on the data from US National Weather Services tells us that Tonardo alone, has resulted in over **83,058** injuries and fatalities, and over **US$40,972,120,087** of losses in Property and Crop Damages.  
  
This reports provides a summary analysis that attempts to answer the following two questions:  
1.  Which types of events, across the United States, are most harmful with respect to population health (in terms of injuries and fatalities), and  
2.  Which types of events, across the United States, have the greatest economic consequences (in terms of Property and Crop Damages)  
  
This reports provides the results of the analysis to answer the above two questions, and the details in processing the data, and resulting data to support the answers. 
  
# Data Processing
This section outlines how the data is processed for its use subsequently for analysis.
  
## Dataset Source
The data for this assignment come in the form of a comma-separated-value file compressed via the bzip2 algorithm to reduce its size. You can download the file from the course web site:  
  
https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2.  
  
The file is downloaded via the script below and stored into a subfolder "data".  Before the data is loaded into working datasets, the file is uncompressed using the bzip2 uncompression function found in the R library "R.utils".  

Then the contents of the file is read into the data frame "storm_data", using the headers provided in the file, and reading all strings as raw, rather than as factors.  

The data source processing logic can be found below here:  


```r
## Define the input source file
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

## Create data folder and target file if it doesn't exists
if (!file.exists("./data")) dir.create("./data")

targetURL <- "./data/repdata_data_StormData.csv.bz2"

## Download the file from the URL provided in the project brief
if (!file.exists(gsub("[.]bz2$", "", targetURL))) {
    setInternet2(TRUE)
    download.file(fileURL, destfile = targetURL)
    bunzip2(targetURL)
}
targetURL <- gsub("[.]bz2$", "", targetURL)

suppressWarnings({
storm_data <- read.csv(targetURL, header = TRUE, stringsAsFactor = FALSE, 
                       na.strings = "?")
})
```

## Data Cleaning Performed (or Not Peformed)
From preliminary analysis of the EVTYPE (Event Type) from the dataset, there is one NA value found in this field.  As the value involved in this observation is small, there would not be any significant impact in removing this observation from further analysis.   
  
It is also observed in the preliminary analysis of the EVTYPE, it is found that there are leading and trailing spaces in the values of the field. As such, there is a need to remove these leading and trail spaces to avoid miscalculating (e.g. summation) any data that would be based on the EVTYPE.  This field, EVTYPE, is converted into a factor to support easier segmentation if required.     
  
Other observations also include that the EVTYPE values include classification that are not really weather related, e.g. SUMMARY..., misspelling/different spelling of classifications, etc.  In addition, though the National Weather Service provides for 48 Events classification, there is no guidance provided on how the current 900 over classifications are to be matched against these 48 Event classifications.  Some are impossible to match, and attempts to do so may result in misclassification.  For example, which of the 48 Event should the following samples to match to:  
1.  Apache County (is this a place or a weather condition?)  
2.  Black Ice (is this a weather condition or road condition?)  
3.  Blizzard/Heavy Snow (should this be classifed as Blizzard or Heavy Snow)  
4.  Cold and Wet (is this Cold/Wind Chill?)  
5.  Cool and Wet (what is the difference between cool and cold?)  
6.  Dam Break  (is this a crisis or a weather condition?)  
7.  Dam Failure (is this a crisis or a weather condition?)  
8.  Driest Month (in comparison to??)  
9.  Dry (and all its variations)  
10.  and so forth...  
  
As such, it may not be prudent to exercise data cleanning blindly, and the resulting reclassification (and force fitting) does not guarantee that the analysis would be significantly more accurate.  A compariative approach have been taken to compare if there are any significant differences between a simple data cleaning process and a more detailed cleaning process - so as to understand if there are significant differences made due to cleaning, and if so to relook at the cleaning process/data to ensure that there is no undue errors made.  
  
Values for Property and Crop damages are not its actual dollar value but reduced based on their multiplier in the EXP field.  These values needs to be converted back to its dollar value.

The following are the data cleaning performed for this report:    


```r
## Remove the NA EVTYPE from storm_data
storm_data <- storm_data[!is.na(storm_data$EVTYPE), ]

## Remove records that are not relevant to our questions 
storm_data <- 
    filter(storm_data, FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0.0 | CROPDMG > 0.0)

## Adjust the Multiplier for Property and Crop damages
### Property Adjustments
storm_data$prop_multi[storm_data$PROPDMGEXP ==""] <- 1;
storm_data$prop_multi[storm_data$PROPDMGEXP =="-"] <- 1;
storm_data$prop_multi[storm_data$PROPDMGEXP =="?"] <- 1;
storm_data$prop_multi[storm_data$PROPDMGEXP =="+"] <- 1;
storm_data$prop_multi[storm_data$PROPDMGEXP =="0"] <- 1;
storm_data$prop_multi[storm_data$PROPDMGEXP =="1"] <- 1;
storm_data$prop_multi[storm_data$PROPDMGEXP =="2"] <- 100;
storm_data$prop_multi[storm_data$PROPDMGEXP =="3"] <- 1000;
storm_data$prop_multi[storm_data$PROPDMGEXP =="4"] <- 10000;
storm_data$prop_multi[storm_data$PROPDMGEXP =="5"] <- 100000;
storm_data$prop_multi[storm_data$PROPDMGEXP =="6"] <- 1000000;
storm_data$prop_multi[storm_data$PROPDMGEXP =="7"] <- 10000000;
storm_data$prop_multi[storm_data$PROPDMGEXP =="8"] <- 100000000;

storm_data$prop_multi[storm_data$PROPDMGEXP =="h"] <- 100;
storm_data$prop_multi[storm_data$PROPDMGEXP =="H"] <- 100;
storm_data$prop_multi[storm_data$PROPDMGEXP =="k"] <- 1000;
storm_data$prop_multi[storm_data$PROPDMGEXP =="K"] <- 1000;
storm_data$prop_multi[storm_data$PROPDMGEXP =="m"] <- 1000000;
storm_data$prop_multi[storm_data$PROPDMGEXP =="M"] <- 1000000;
storm_data$prop_multi[storm_data$PROPDMGEXP =="b"] <- 1000000000;
storm_data$prop_multi[storm_data$PROPDMGEXP =="B"] <- 1000000000;

### Crop Adjustments
storm_data$crop_multi[storm_data$CROPDMGEXP ==""] <- 1;
storm_data$crop_multi[storm_data$CROPDMGEXP =="-"] <- 1;
storm_data$crop_multi[storm_data$CROPDMGEXP =="?"] <- 1;
storm_data$crop_multi[storm_data$CROPDMGEXP =="+"] <- 1;
storm_data$crop_multi[storm_data$CROPDMGEXP =="0"] <- 1;
storm_data$crop_multi[storm_data$CROPDMGEXP =="1"] <- 1;
storm_data$crop_multi[storm_data$CROPDMGEXP =="2"] <- 100;
storm_data$crop_multi[storm_data$CROPDMGEXP =="3"] <- 1000;
storm_data$crop_multi[storm_data$CROPDMGEXP =="4"] <- 10000;
storm_data$crop_multi[storm_data$CROPDMGEXP =="5"] <- 100000;
storm_data$crop_multi[storm_data$CROPDMGEXP =="6"] <- 1000000;
storm_data$crop_multi[storm_data$CROPDMGEXP =="7"] <- 10000000;
storm_data$crop_multi[storm_data$CROPDMGEXP =="8"] <- 100000000;

storm_data$crop_multi[storm_data$CROPDMGEXP =="h"] <- 100;
storm_data$crop_multi[storm_data$CROPDMGEXP =="H"] <- 100;
storm_data$crop_multi[storm_data$CROPDMGEXP =="k"] <- 1000;
storm_data$crop_multi[storm_data$CROPDMGEXP =="K"] <- 1000;
storm_data$crop_multi[storm_data$CROPDMGEXP =="m"] <- 1000000;
storm_data$crop_multi[storm_data$CROPDMGEXP =="M"] <- 1000000;
storm_data$crop_multi[storm_data$CROPDMGEXP =="b"] <- 1000000000;
storm_data$crop_multi[storm_data$CROPDMGEXP =="B"] <- 1000000000;

storm_data$PROPDMG <- storm_data$PROPDMG * storm_data$prop_multi
storm_data$CROPDMG <- storm_data$CROPDMG * storm_data$crop_multi

## Clean the Even types.  Visual exploration had shown that it has issues such as
## leading and trailing spaces, same event but using different "codes", etc.
# Change EVTYPE to upper case and triming of leading and trailing spaces
storm_data$EVTYPE <- tolower(trim(storm_data$EVTYPE))

## Create a data.table that will be used for detailed cleaning later
storm_evtypes  <- data.table(evtype = storm_data$EVTYPE,    # The curent EVTYPE for search later  
                             result = storm_data$EVTYPE)    # As the value to replace EVTYPE with

## Make copy of storm data into a "clean" data table
storm_data_clean <- storm_data

## Codify the 48 NWS's Events
codebook <- c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood",
              "Cold/Wind Chill","Debris Flow", "Dense Fog","Dense Smoke",
              "Drought","Dust Devil","Dust Storm","Excessive Heat",
              "Extreme Cold/Wind Chill", "Flash Flood", "Flood", "Frost/Freeze",
              "Funnel Cloud", "Freezing Fog", "Hail", "Heat", "Heavy Rain", 
              "Heavy Snow", "High Surf", "High Wind", "Hurricane (Typhoon)",
              "Ice Storm", "Lake-Effect Snow", "Lakeshore Flood", "Lightning",
              "Marine Hail","Marine High Wind", "Marine Strong Wind",
              "Marine Thunderstorm Wind", "Rip Current", "Seiche", "Sleet",
              "Storm Surge/Tide", "Strong Wind", "Thunderstorm Wind",
              "Tornado",  "Tropical Depression", "Tropical Storm", "Tsunami",
              "Volcanic Ash", "Waterspout", "Wildfire",  "Winter Storm", 
              "Winter Weather")

# Define the regular expression to search the original EVTYPES so as to define the actual code 
# value to be used
reg_expr = c("^astro.*de$",
             "^avalanch?e",
             "^blizz.*?d|ground blizzard", 
             "coastal *?flood.*|cstl flood|coastal *?erosion|beach erosion|tidal flooding",
             "^(cold).*|(^extreme ) wind ?chill|^extended cold|^wind|cool and wet|low temperature|unseasonable?y? cold",
             "^debri?s.*|landslides?|landslump|landspout|[(mud)|(rock)] ?slides?",
             "(dense)? ?fog$",
             "^den?s?e? smo?k?e",
             "^droug?h?t",
             "^dust devi?l?",
             "^dust ?stor?m?|blowing dust",
             "^excessive ?heat|extreme ?heat|record heat|record.excessive heat",
             "^extreme ?cold|^extreme wind ?chill|hypo?e?r?thermia|record cold",
             "^flash ?[flood]?.*|flash.?flood|dam break|flood.flash",
             "^floods?$|^flooding|^breakup flooding|flood \\& heavy rain|flood.rain.winds|major flood|^flood.*flood$|^minor*|rapidly rising water|^river.*flood.*|rural flood|small stream flood|urban.*", 
             "frost|freez[e|ing]|black ice|glaze.*|^ice$|^ice and.*|^ice flo.*|^ice jam.*|^ice?y?.*road.*|^ice.strong.*",
             "funnel ?cloud", "(freezing)? ?fog.*?(cold).*?",
             "^hail|falling snow.ice|small hail", 
             "^heat|unseasonably warm.*|warm weather", 
             "^he?a?vy rain|excessive rainfall|excessive wetness|heavy mix|heavy precipitation|heavy shower|mixed precip(itation)?|^rain.*|record rainfall|unseasonal rain|torrential.*",
             "heavy snow|excessive snow|record snow|snow.*",
             "^high surf|hazardous surf",
             "^high wind$|^high .?winds$|^high wind.?.heav|^high winds.$|^high winds.[(cold)|(snow)]|^high winds?..[g8al]|^high$",
             "hurricane|typhoon",
             "ice storm",
             "lake snow|lake effect",
             "lake flood",
             "lightn?ing|ligntning",
             "marine hail",
             "^high wind and seas|^high winds?.seas|heavy surf.*",
             "marine strong wind",
             "^marine thunderstorm wind|coastal ?storm|heavy seas|marine accident|marine mishap|rogue wave|rough seas|rough surf",
             "^rip.*",
             "seiche", 
             "^sleet|snow.sleet$",
             "^storm surge|tide|swells|high waves|high water|high seas|coastal surge",
             "^strong wind|downburst|microburst|dry mircoburst( winds)?|gradient wind|gustnado|gusty wind|non-severe wind damage|severe turbulence",
             "^thun?d?ee?re?s?tr?om?rm ? ?(wind)?|thunderstrom wind|thundersnow|^severe thunder.*|tstm|tunder.*|storm force winds",
             "tornado|torndao|whirlwind", 
             "tropical depression", 
             "tropical storm.*",
             "tsunami",
             "volcanic",
             "waterspout", 
             "wildfire|forest fire|wild fires|brush fire|grass fires",
             "blowing snow",
             "winter weather|^winter|snow.winter|wintry mix|late season snow|light snow.*")

ref_evtype <- data.table(codebook, reg_expr)

storm_evtypes <- arrange(storm_evtypes, evtype)

evtype <- storm_evtypes$evtype 

for (i in 1:nrow(ref_evtype)) {
    if(sum(grepl(ref_evtype[i, ]$reg_expr, storm_evtypes$evtype)) > 0) {
        storm_evtypes$result <- 
            replace(storm_evtypes$result, 
                    grepl(ref_evtype[i, ]$reg_expr, storm_evtypes$evtype),
                    ref_evtype[i, ]$codebook)
    }
}

for (i in 1:nrow(storm_evtypes)) {
    storm_data_clean$EVTYPE <- 
        replace(storm_data_clean$EVTYPE, 
                !is.na(storm_data_clean$EVTYPE) &
                    storm_data_clean$EVTYPE == storm_evtypes[i, ]$evtype,
                storm_evtypes[i, ]$result) 
}
```


## Data Aggregation and Summarisation
The following has been performed to convert the measured data into analytic data to support our analysis:  
1.  The data is grouped by event type and aggregated by the total injuries, total fatalities, and the aggregate of the sum of both of these to determine the major contributing events that impacts Population Health.  The top five (5) were identified.  The reason why only the top 5 were select is because after these, the numbers involved is comparatively low, especially if we were to average the cases over 1950 to 2011. This is done with both sets of data.   
2.  The data is grouped by event type and aggregated by the total Property Damages, total Crop Damages, and the aggregate of the sum of both of these to determine the major contributing events that greatest economic consequences. The top five (5) were identified. The reason why only the top 5 were select is because after these, the dollar value involved is comparatively low, especially if we were to average the cases over 1950 to 2011.  
3.  The Begin Date of the Event is used to generate a Year field: BGN_DATE_YR, so that significant events or event and their impact can be tracked across multiple years to determine if there are any particular trend.  
4.  The data is further grouped by event type and year (created in step 1) and aggregated by total injuries, total fatalities, and by the sum of both of these fields for Population Health Impact, and by total Property Damages, total Crop Damages, and the sum of these two fields for the economic consequences.  
  

```r
## Measure for "harmful to population health" is the sum of total fatalities and total injuries
## Measure for "greatest economic consequences" is the sum of total property and total crop damages

## Two sets of summaries are taken for initial analysis: 
#  The first sums the measures grouped by events but regardless of years
storm_summary0 <- storm_data %>% 
    group_by(EVTYPE) %>% 
    summarise(Total_Fatalities = sum(FATALITIES), 
              Total_Injuries = sum(INJURIES),
              Total_Hlth_Impact = sum(FATALITIES + INJURIES),
              Total_PropDmg = sum(PROPDMG), 
              Total_CropDmg = sum(CROPDMG),
              Total_Econ_Impact = sum(PROPDMG + CROPDMG))

#  This sums the measures grouped by events but regardless of years for cleaned data
storm_summary00 <- storm_data_clean %>% 
    group_by(EVTYPE) %>% 
    summarise(Total_Fatalities = sum(FATALITIES), 
              Total_Injuries = sum(INJURIES),
              Total_Hlth_Impact = sum(FATALITIES + INJURIES),
              Total_PropDmg = sum(PROPDMG), 
              Total_CropDmg = sum(CROPDMG),
              Total_Econ_Impact = sum(PROPDMG + CROPDMG))

## Create a year to allow analysis over the years for the same event to 
## understand the trend of the event in terms of impact
storm_data$BGN_DATE_YR <- as.integer(year(as.Date(storm_data$BGN_DATE, 
                                                  format="%m/%d/%Y %H:%M:%S")))

storm_data_clean$BGN_DATE_YR <- as.integer(year(as.Date(storm_data_clean$BGN_DATE, 
                                                        format="%m/%d/%Y %H:%M:%S")))

#  The second sums the measures group by events and years
storm_summary1 <- storm_data %>% 
    group_by(EVTYPE, BGN_DATE_YR) %>% 
    summarise(Total_Fatalities = sum(FATALITIES), 
              Total_Injuries = sum(INJURIES),
              Total_Hlth_Impact = sum(FATALITIES + INJURIES),
              Total_PropDmg = sum(PROPDMG), 
              Total_CropDmg = sum(CROPDMG),
              Total_Econ_Impact = sum(PROPDMG + CROPDMG))

# Doing the same for cleaned data
storm_summary11 <- storm_data_clean %>% 
    group_by(EVTYPE, BGN_DATE_YR) %>% 
    summarise(Total_Fatalities = sum(FATALITIES), 
              Total_Injuries = sum(INJURIES),
              Total_Hlth_Impact = sum(FATALITIES + INJURIES),
              Total_PropDmg = sum(PROPDMG), 
              Total_CropDmg = sum(CROPDMG),
              Total_Econ_Impact = sum(PROPDMG + CROPDMG))

# Analysing top Events in terms of Total_Hlth_Impact
storm_sum_health <- arrange(storm_summary0, desc(Total_Hlth_Impact))
storm_sum_health <- storm_sum_health[1:5, ] # Take top 5 

storm_sum_health1 <- arrange(storm_summary00, desc(Total_Hlth_Impact))
storm_sum_health1 <- storm_sum_health1[1:5, ] # Take top 5 

# Analysing top Events in terms of Total_Econ_Impact
storm_sum_econ <- arrange(storm_summary0, desc(Total_Econ_Impact))
storm_sum_econ <- storm_sum_econ[1:5, ] # Take top 5

storm_sum_econ1 <- arrange(storm_summary00, desc(Total_Econ_Impact))
storm_sum_econ1 <- storm_sum_econ1[1:5, ] # Take top 5
```
# Results

The top five (5) most impactful events to Population Health are:  


```r
print(select(storm_sum_health, EVTYPE, Total_Injuries, Total_Fatalities, Total_Hlth_Impact))
```

```
## Source: local data frame [5 x 4]
## 
##           EVTYPE Total_Injuries Total_Fatalities Total_Hlth_Impact
## 1        tornado          78459             4555             83014
## 2          flood           6491              231              6722
## 3      tstm wind           5960              435              6395
## 4 excessive heat           4106             1337              5443
## 5      lightning           3166              484              3650
```
  
The top five (5) most impactful events to Population Health (based on cleaned data) are:  


```r
print(select(storm_sum_health1, EVTYPE, Total_Injuries, Total_Fatalities, Total_Hlth_Impact))
```

```
## Source: local data frame [5 x 4]
## 
##              EVTYPE Total_Injuries Total_Fatalities Total_Hlth_Impact
## 1           Tornado          78477             4581             83058
## 2 Thunderstorm Wind           7111              513              7624
## 3             Flood           6583              289              6872
## 4    Excessive Heat           4311             1452              5763
## 5         Lightning           3168              485              3653
```
  
The top five (5) events that have significant consequences to the Economy are:  


```r
print(select(storm_sum_econ, EVTYPE, Total_PropDmg, Total_CropDmg, Total_Econ_Impact))
```

```
## Source: local data frame [5 x 4]
## 
##        EVTYPE Total_PropDmg Total_CropDmg Total_Econ_Impact
## 1     tornado   39169852717     194975370       39364828087
## 2       flood   11583793277    2107721050       13691514327
## 3   hurricane    9350719010    2461400000       11812119010
## 4 river flood    5118945500    5029459000       10148404500
## 5     drought     200148000    9281945000        9482093000
```

The top five (5) events that have significant consequences to the Economy (based on cleaned data) are:  


```r
print(select(storm_sum_econ1, EVTYPE, Total_PropDmg, Total_CropDmg, Total_Econ_Impact))
```

```
## Source: local data frame [5 x 4]
## 
##                EVTYPE Total_PropDmg Total_CropDmg Total_Econ_Impact
## 1             Tornado   40774636467     197483620       40972120087
## 2 Hurricane (Typhoon)   13532895010    2627735000       16160630010
## 3             Drought     200148000    9281950780        9482098780
## 4                Hail    7040116513    1785428473        8825544986
## 5           Ice Storm    2580185060    5013448500        7593633560
```

The following figure provides a good overview of the Events and their impact and consequences.  




A perspective and comparison between the top five (5) events for both Population Health and Economic (based on cleaned data) consequences can be seen in the chart below:  

![](PA2_files/figure-html/unnamed-chunk-11-1.png) 
  
From the plots above, it can be seen that Tornados have the greatest impact on both Population Health and the Economy from 1950 to 2011, across the United States (US), measured in terms of total number of injuries and fatalities, and the total US dollar loss due to Property and Crop damages.  
  
The impact to Population Health is a total of **83,058 injuries and fatalities** from 1950 to 2011, or _78,477_ injuries and _4,581_ fatalities.  This is quite significant in comparison with the next highest impactful event to Population Health, Thunderstorm Wind, that results in a total of _7,624_ injuries of fatalities.  This is slightly more than ten (10) times the number of injuries and fatalities.
    
Tornados have an equally significant impact to the US economy, in terms of total dollar value loss to Property and Crops, with the total loss of approximately **US$40,972,120,087** from 1950 to 2011, comprising of approximately _US$40,774,636,467_ (rounded) in total Property damages and _US$197,483,620_ (rounded).  In comparison with the next highest impactful event to the US economy, that is Thunderstorm Wind, US$1,428,500 (rounded). 
  
## Further Analysis on Results
For further analysis of the most significant event, Tornado, a subset from the storm summary data by years is extracted for the last fifteen (15) years.  This will be used to plot both the Impact of Tornado on Population Health and Economy across these last  fifteen (15) years.  

![](PA2_files/figure-html/unnamed-chunk-12-1.png) 
  
Summary total Population Health and Econmy Impact for the 15 years period due to TOrnado: 
  

```r
print(select(sum_ttl_tornado, Hlth_Impact_15, Econ_Impact_15))
```

```
## Source: local data frame [1 x 2]
## 
##   Hlth_Impact_15 Econ_Impact_15
## 1          17622    17785578567
```
![](PA2_files/figure-html/unnamed-chunk-14-1.png) 
  
Summary total Population Health and Econmy Impact for the 15 years period due to Thunderstorm Wind: 
  

```r
print(select(sum_ttl_tstm, Hlth_Impact_15, Econ_Impact_15))
```

```
## Source: local data frame [1 x 2]
## 
##   Hlth_Impact_15 Econ_Impact_15
## 1           6681     4414324036
```
As it can be seen from the plots above, the impact of Tornado events on Population Health and the Economy have been increasing from 1987 to 2002 (available data).  What could be the cause of this given that there should be better preparation and better use of technology to help detect and to limit the loss of lives and property?

What is more interesting is that though Tornado is the most impactful to both Population Health and Economy based to pure total aggregate from since data was collected, but if we factor in the time period and compare the data within the same time period, we can see that Thunderstorm Wind would have significant impact to the Economy, where the lowest reported data in the last fifteen (15) years is greater than the highest reported data from Tornado.

## Summary Conclusion
Based on the diagrams above for the last 15 years, Tornado continues to have significant impact on Population Health, but Thunderstorm Wind (preliminary) would have the the most significant impact on the Economy.  And looking at the trend curve, Thunderstorm Wind event's impact is growing in significance.

# Further Research and Activities
## New Areas for Research
Further research can be conducted to determine the factors that contributes to this increasing cost due to Property Damages.  For example, could the scale of such an event be larger than previous years, and thus the extend of Property Damages are greater, or could it be that Property built in recent years are not designed to deal with Tornado events, or are Tornados hitting areas that are more densely built-up and thus causing far more extensive damages, or is it not one but multiple contributing factors.
  
## Needed Supporting Activities
To support new areas of research into Impact of Weather Events on Population or Economy, it is critical that the data file, in particular in the data classification values to be cleaned up in order for more meaning data analysis to be conducted, and in a more conclusive manner.  Currently, for this report, there were substantial "best effort" attempt in reclassifying the data into its proper categories - based on looking at the data itself.  However, what's needed is to correlated the classification with actual newspaper reports (or other historical documents) so that a more accurate picture/conclusion can be drawn.
  
###
