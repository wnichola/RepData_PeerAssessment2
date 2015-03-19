# Reproducible Research: Peer Assessment 2
Nicholas Wee  
Wednesday, March 18, 2015  


```r
## Libraries
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(data.table)
```

```
## 
## Attaching package: 'data.table'
## 
## The following objects are masked from 'package:dplyr':
## 
##     between, last
```

```r
library(R.utils)
```

```
## Warning: package 'R.utils' was built under R version 3.1.3
```

```
## Loading required package: R.oo
```

```
## Warning: package 'R.oo' was built under R version 3.1.3
```

```
## Loading required package: R.methodsS3
```

```
## Warning: package 'R.methodsS3' was built under R version 3.1.3
```

```
## R.methodsS3 v1.7.0 (2015-02-19) successfully loaded. See ?R.methodsS3 for help.
## R.oo v1.19.0 (2015-02-27) successfully loaded. See ?R.oo for help.
## 
## Attaching package: 'R.oo'
## 
## The following objects are masked from 'package:methods':
## 
##     getClasses, getMethods
## 
## The following objects are masked from 'package:base':
## 
##     attach, detach, gc, load, save
## 
## R.utils v2.0.0 (2015-02-28) successfully loaded. See ?R.utils for help.
## 
## Attaching package: 'R.utils'
## 
## The following object is masked from 'package:utils':
## 
##     timestamp
## 
## The following objects are masked from 'package:base':
## 
##     cat, commandArgs, getOption, inherits, isOpen, parse, warnings
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
## 
## The following objects are masked from 'package:data.table':
## 
##     hour, mday, month, quarter, wday, week, yday, year
```

```r
## Create data folder if it doesn't exists
if (!file.exists("./data")) dir.create("./data")

## Define the input source and file
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

targetURL <- "./data/repdata_data_StormData.csv.bz2"

## Download the file from the URL provided in the project brief
if (!file.exists(gsub("[.]bz2$", "", targetURL))) {
    setInternet2(TRUE)
    download.file(fileURL, destfile = targetURL)
    bunzip2(targetURL)
}
targetURL <- gsub("[.]bz2$", "", targetURL)
storm_data <- read.csv(targetURL, header = TRUE, stringsAsFactor = FALSE, 
                       na.strings = "?")
```

```
## Warning in scan(file, what, nmax, sep, dec, quote, skip, nlines,
## na.strings, : EOF within quoted string
```


```r
## Remove the NA EVTYPE from storm_data
storm_data <- storm_data[!is.na(storm_data$EVTYPE), ]

## Create a year to allow analysis over the years for the same event to 
## understand the trend of the event in terms of impact
storm_data$BGN_DATE <- as.Date(storm_data$BGN_DATE, format="%m/%d/%Y %H:%M:%S")
storm_data$BGN_DATE_YR <- factor(year(storm_data$BGN_DATE))

## Clean the Even types.  Visual exploration had shown that it has issues such as
## leading and trailing spaces, same event but using different "codes", etc.
# Change EVTYPE to upper case and triming of leading and trailing spaces
storm_data$EVTYPE <- as.factor(toupper(trim(storm_data$EVTYPE)))

## Measure for "harmful to population health" is the sum of total fatalities and total injuries
## Measure for "greatest economic consequences" is the sum of total property and total crop damages
```


```r
## Two summaries are taken for initial analysis: 
#  The first sums the measures grouped by events but regardless of years
storm_summary0 <- storm_data %>% 
    group_by(EVTYPE) %>% 
    summarise(Total_Fatalities = sum(FATALITIES), 
              Total_Injuries = sum(INJURIES),
              Total_Hlth_Impact = sum(FATALITIES + INJURIES),
              Total_PropDmg = sum(PROPDMG), 
              Total_CropDmg = sum(CROPDMG),
              Total_Econ_Impact = sum(PROPDMG + CROPDMG))

#  The second sums the measures group by events and years
storm_summary1 <- storm_data %>% 
    group_by(EVTYPE, BGN_DATE_YR) %>% 
    summarise(Total_Fatalities = sum(FATALITIES), 
              Total_Injuries = sum(INJURIES),
              Total_Hlth_Impact = sum(FATALITIES + INJURIES),
              Total_PropDmg = sum(PROPDMG), 
              Total_CropDmg = sum(CROPDMG),
              Total_Econ_Impact = sum(PROPDMG + CROPDMG))
```


```r
# Analysing top Events in terms of Total_Hlth_Impact
storm_summary0_0 <- arrange(storm_summary0, desc(Total_Hlth_Impact))

# Analysing top Events in terms of Total_Econ_Impact
storm_summary0_1 <- arrange(storm_summary0, desc(Total_Econ_Impact))

# Analysing top Events in terms of Total_Hlth_Impact across years
storm_summary1_0 <- arrange(storm_summary1, desc(Total_Hlth_Impact), desc(BGN_DATE_YR))

# Analysing top Events in terms of Total_Econ_Impact across years
storm_summary1_1 <- arrange(storm_summary1, desc(Total_Econ_Impact), desc(BGN_DATE_YR))
```


```r
print(storm_summary0_0)
```

```
## Source: local data frame [872 x 7]
## 
##            EVTYPE Total_Fatalities Total_Injuries Total_Hlth_Impact
## 1         TORNADO             4555          78459             83014
## 2           FLOOD              231           6491              6722
## 3       TSTM WIND              435           5960              6395
## 4  EXCESSIVE HEAT             1337           4106              5443
## 5       LIGHTNING              484           3166              3650
## 6       ICE STORM               68           1877              1945
## 7     FLASH FLOOD              451           1275              1726
## 8            HEAT              708            878              1586
## 9    WINTER STORM              144           1088              1232
## 10           HAIL               12           1005              1017
## ..            ...              ...            ...               ...
## Variables not shown: Total_PropDmg (dbl), Total_CropDmg (dbl),
##   Total_Econ_Impact (dbl)
```

```r
select(storm_summary0_1, EVTYPE, Total_PropDmg, Total_CropDmg, Total_Econ_Impact)
```

```
## Source: local data frame [872 x 4]
## 
##                EVTYPE Total_PropDmg Total_CropDmg Total_Econ_Impact
## 1             TORNADO    2452535.36      32787.72        2485323.08
## 2           TSTM WIND     843568.35      84587.15         928155.50
## 3         FLASH FLOOD     573774.45      62027.49         635801.94
## 4                HAIL     321840.73     299814.47         621655.20
## 5  THUNDERSTORM WINDS     446293.18      18684.93         464978.11
## 6               FLOOD     300065.30      57313.51         357378.81
## 7           LIGHTNING     292291.75       2323.81         294615.56
## 8           HIGH WIND     130605.38       7343.35         137948.73
## 9          HEAVY SNOW      77528.56       1835.72          79364.28
## 10         HIGH WINDS      55625.00       1759.60          57384.60
## ..                ...           ...           ...               ...
```

```r
print(storm_summary1_0)
```

```
## Source: local data frame [1,827 x 8]
## Groups: EVTYPE
## 
##                    EVTYPE BGN_DATE_YR Total_Fatalities Total_Injuries
## 1         ABNORMAL WARMTH        1998                0              0
## 2          ABNORMALLY DRY        2001                0              0
## 3          ABNORMALLY WET        2002                0              0
## 4    ACCUMULATED SNOWFALL        2001                0              0
## 5     AGRICULTURAL FREEZE        1997                0              0
## 6     AGRICULTURAL FREEZE        1995                0              0
## 7           APACHE COUNTY        1994                0              0
## 8  ASTRONOMICAL HIGH TIDE        2002                0              0
## 9                AVALANCE        1993                1              0
## 10              AVALANCHE        1999               26             10
## ..                    ...         ...              ...            ...
## Variables not shown: Total_Hlth_Impact (dbl), Total_PropDmg (dbl),
##   Total_CropDmg (dbl), Total_Econ_Impact (dbl)
```

```r
select(storm_summary1_1, EVTYPE, BGN_DATE_YR, Total_PropDmg, Total_CropDmg, Total_Econ_Impact)
```

```
## Source: local data frame [1,827 x 5]
## Groups: EVTYPE
## 
##                    EVTYPE BGN_DATE_YR Total_PropDmg Total_CropDmg
## 1         ABNORMAL WARMTH        1998             0          0.00
## 2          ABNORMALLY DRY        2001             0          0.00
## 3          ABNORMALLY WET        2002             0          0.00
## 4    ACCUMULATED SNOWFALL        2001             0          0.00
## 5     AGRICULTURAL FREEZE        1997             0         28.82
## 6     AGRICULTURAL FREEZE        1995             0          0.00
## 7           APACHE COUNTY        1994             5          0.00
## 8  ASTRONOMICAL HIGH TIDE        2002             0          0.00
## 9                AVALANCE        1993             0          0.00
## 10              AVALANCHE        2000           755          0.00
## ..                    ...         ...           ...           ...
## Variables not shown: Total_Econ_Impact (dbl)
```
