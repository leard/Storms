# Ttile

## Synopsis

Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?  

Across the United States, which types of events have the greatest economic consequences?  

## Load and Processing the Data

Load the libraries for this analysis.

```r
library(ggplot2)
library(chron)
library(stringr)
```

Verify if dataset exists.

```r
if(!file.exists("./storm.csv.bz2")){
    val<-download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                  "storm.csv.bz2")
    print(val)
    if(val != 0){
        stop("Failure on download of the file")
    }
}
```

Download Documentation.

```r
if(!file.exists("./stormDoc.pdf")){
    download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf",
                  "stormDoc.pdf")
}
if(!file.exists("./faqDoc.pdf")){
    download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf", "faqDoc.pdf")
}
```

Unpack and Read the dataset.

```r
if(file.exists("./storm.csv.bz2")){
    stormData<-read.csv(bzfile("./storm.csv.bz2"), na.strings=c("", "?"))    
}else{
    stop("The dataset not exists")
}
```

After read, we check the first few rows (there are 902,297 rows) in this dataset.

```r
dim(stormData)
```

```
## [1] 902297     37
```

```r
head(stormData)
```

```
##   STATE__           BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE
## 1       1  4/18/1950 0:00:00     0130       CST     97     MOBILE    AL
## 2       1  4/18/1950 0:00:00     0145       CST      3    BALDWIN    AL
## 3       1  2/20/1951 0:00:00     1600       CST     57    FAYETTE    AL
## 4       1   6/8/1951 0:00:00     0900       CST     89    MADISON    AL
## 5       1 11/15/1951 0:00:00     1500       CST     43    CULLMAN    AL
## 6       1 11/15/1951 0:00:00     2000       CST     77 LAUDERDALE    AL
##    EVTYPE BGN_RANGE BGN_AZI BGN_LOCATI END_DATE END_TIME COUNTY_END
## 1 TORNADO         0    <NA>       <NA>     <NA>     <NA>          0
## 2 TORNADO         0    <NA>       <NA>     <NA>     <NA>          0
## 3 TORNADO         0    <NA>       <NA>     <NA>     <NA>          0
## 4 TORNADO         0    <NA>       <NA>     <NA>     <NA>          0
## 5 TORNADO         0    <NA>       <NA>     <NA>     <NA>          0
## 6 TORNADO         0    <NA>       <NA>     <NA>     <NA>          0
##   COUNTYENDN END_RANGE END_AZI END_LOCATI LENGTH WIDTH F MAG FATALITIES
## 1         NA         0    <NA>       <NA>   14.0   100 3   0          0
## 2         NA         0    <NA>       <NA>    2.0   150 2   0          0
## 3         NA         0    <NA>       <NA>    0.1   123 2   0          0
## 4         NA         0    <NA>       <NA>    0.0   100 2   0          0
## 5         NA         0    <NA>       <NA>    0.0   150 2   0          0
## 6         NA         0    <NA>       <NA>    1.5   177 2   0          0
##   INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP  WFO STATEOFFIC ZONENAMES
## 1       15    25.0          K       0       <NA> <NA>       <NA>      <NA>
## 2        0     2.5          K       0       <NA> <NA>       <NA>      <NA>
## 3        2    25.0          K       0       <NA> <NA>       <NA>      <NA>
## 4        2     2.5          K       0       <NA> <NA>       <NA>      <NA>
## 5        2     2.5          K       0       <NA> <NA>       <NA>      <NA>
## 6        6     2.5          K       0       <NA> <NA>       <NA>      <NA>
##   LATITUDE LONGITUDE LATITUDE_E LONGITUDE_ REMARKS REFNUM
## 1     3040      8812       3051       8806    <NA>      1
## 2     3042      8755          0          0    <NA>      2
## 3     3340      8742          0          0    <NA>      3
## 4     3458      8626          0          0    <NA>      4
## 5     3412      8642          0          0    <NA>      5
## 6     3450      8748          0          0    <NA>      6
```

Formatting names and trim the rows values in the dataset.

```r
#Set the names for the R standard
names(stormData)<- make.names(names(stormData), allow_=F)

#removing space values
for(i in 1:dim(stormData)[2]){
    stormData[,i]<-str_trim(stormData[,i])
}
```

The column we are interested in is the EVTYPE. Here we print the quantity of unique values and NA's.

```r
print(length(unique(stormData$EVTYPE)))
```

```
## [1] 977
```

```r
print(sort(unique(stormData$EVTYPE)))
```

```
##   [1] "ABNORMAL WARMTH"                "ABNORMALLY DRY"                
##   [3] "ABNORMALLY WET"                 "ACCUMULATED SNOWFALL"          
##   [5] "AGRICULTURAL FREEZE"            "APACHE COUNTY"                 
##   [7] "ASTRONOMICAL HIGH TIDE"         "ASTRONOMICAL LOW TIDE"         
##   [9] "AVALANCE"                       "AVALANCHE"                     
##  [11] "BEACH EROSIN"                   "Beach Erosion"                 
##  [13] "BEACH EROSION"                  "BEACH EROSION/COASTAL FLOOD"   
##  [15] "BEACH FLOOD"                    "BELOW NORMAL PRECIPITATION"    
##  [17] "BITTER WIND CHILL"              "BITTER WIND CHILL TEMPERATURES"
##  [19] "Black Ice"                      "BLACK ICE"                     
##  [21] "BLIZZARD"                       "BLIZZARD AND EXTREME WIND CHIL"
##  [23] "BLIZZARD AND HEAVY SNOW"        "Blizzard Summary"              
##  [25] "BLIZZARD WEATHER"               "BLIZZARD/FREEZING RAIN"        
##  [27] "BLIZZARD/HEAVY SNOW"            "BLIZZARD/HIGH WIND"            
##  [29] "BLIZZARD/WINTER STORM"          "BLOW-OUT TIDE"                 
##  [31] "BLOW-OUT TIDES"                 "BLOWING DUST"                  
##  [33] "blowing snow"                   "Blowing Snow"                  
##  [35] "BLOWING SNOW"                   "BLOWING SNOW- EXTREME WIND CHI"
##  [37] "BLOWING SNOW & EXTREME WIND CH" "BLOWING SNOW/EXTREME WIND CHIL"
##  [39] "BREAKUP FLOODING"               "BRUSH FIRE"                    
##  [41] "BRUSH FIRES"                    "COASTAL  FLOODING/EROSION"     
##  [43] "COASTAL EROSION"                "Coastal Flood"                 
##  [45] "COASTAL FLOOD"                  "coastal flooding"              
##  [47] "Coastal Flooding"               "COASTAL FLOODING"              
##  [49] "COASTAL FLOODING/EROSION"       "Coastal Storm"                 
##  [51] "COASTAL STORM"                  "COASTAL SURGE"                 
##  [53] "COASTAL/TIDAL FLOOD"            "COASTALFLOOD"                  
##  [55] "COASTALSTORM"                   "Cold"                          
##  [57] "COLD"                           "COLD AIR FUNNEL"               
##  [59] "COLD AIR FUNNELS"               "COLD AIR TORNADO"              
##  [61] "Cold and Frost"                 "COLD AND FROST"                
##  [63] "COLD AND SNOW"                  "COLD AND WET CONDITIONS"       
##  [65] "Cold Temperature"               "COLD TEMPERATURES"             
##  [67] "COLD WAVE"                      "COLD WEATHER"                  
##  [69] "COLD WIND CHILL TEMPERATURES"   "COLD/WIND CHILL"               
##  [71] "COLD/WINDS"                     "COOL AND WET"                  
##  [73] "COOL SPELL"                     "CSTL FLOODING/EROSION"         
##  [75] "DAM BREAK"                      "DAM FAILURE"                   
##  [77] "Damaging Freeze"                "DAMAGING FREEZE"               
##  [79] "DEEP HAIL"                      "DENSE FOG"                     
##  [81] "DENSE SMOKE"                    "DOWNBURST"                     
##  [83] "DOWNBURST WINDS"                "DRIEST MONTH"                  
##  [85] "Drifting Snow"                  "DROUGHT"                       
##  [87] "DROUGHT/EXCESSIVE HEAT"         "DROWNING"                      
##  [89] "DRY"                            "DRY CONDITIONS"                
##  [91] "DRY HOT WEATHER"                "DRY MICROBURST"                
##  [93] "DRY MICROBURST 50"              "DRY MICROBURST 53"             
##  [95] "DRY MICROBURST 58"              "DRY MICROBURST 61"             
##  [97] "DRY MICROBURST 84"              "DRY MICROBURST WINDS"          
##  [99] "DRY MIRCOBURST WINDS"           "DRY PATTERN"                   
## [101] "DRY SPELL"                      "DRY WEATHER"                   
## [103] "DRYNESS"                        "DUST DEVEL"                    
## [105] "Dust Devil"                     "DUST DEVIL"                    
## [107] "DUST DEVIL WATERSPOUT"          "DUST STORM"                    
## [109] "DUST STORM/HIGH WINDS"          "DUSTSTORM"                     
## [111] "EARLY FREEZE"                   "Early Frost"                   
## [113] "EARLY FROST"                    "EARLY RAIN"                    
## [115] "EARLY SNOW"                     "Early snowfall"                
## [117] "EARLY SNOWFALL"                 "Erosion/Cstl Flood"            
## [119] "EXCESSIVE"                      "Excessive Cold"                
## [121] "EXCESSIVE HEAT"                 "EXCESSIVE HEAT/DROUGHT"        
## [123] "EXCESSIVE PRECIPITATION"        "EXCESSIVE RAIN"                
## [125] "EXCESSIVE RAINFALL"             "EXCESSIVE SNOW"                
## [127] "EXCESSIVE WETNESS"              "EXCESSIVELY DRY"               
## [129] "Extended Cold"                  "Extreme Cold"                  
## [131] "EXTREME COLD"                   "EXTREME COLD/WIND CHILL"       
## [133] "EXTREME HEAT"                   "EXTREME WIND CHILL"            
## [135] "EXTREME WIND CHILL/BLOWING SNO" "EXTREME WIND CHILLS"           
## [137] "EXTREME WINDCHILL"              "EXTREME WINDCHILL TEMPERATURES"
## [139] "EXTREME/RECORD COLD"            "EXTREMELY WET"                 
## [141] "FALLING SNOW/ICE"               "FIRST FROST"                   
## [143] "FIRST SNOW"                     "FLASH FLOOD"                   
## [145] "FLASH FLOOD - HEAVY RAIN"       "FLASH FLOOD FROM ICE JAMS"     
## [147] "FLASH FLOOD LANDSLIDES"         "FLASH FLOOD WINDS"             
## [149] "FLASH FLOOD/"                   "FLASH FLOOD/ FLOOD"            
## [151] "FLASH FLOOD/ STREET"            "FLASH FLOOD/FLOOD"             
## [153] "FLASH FLOOD/HEAVY RAIN"         "FLASH FLOOD/LANDSLIDE"         
## [155] "FLASH FLOODING"                 "FLASH FLOODING/FLOOD"          
## [157] "FLASH FLOODING/THUNDERSTORM WI" "FLASH FLOODS"                  
## [159] "FLASH FLOOODING"                "Flood"                         
## [161] "FLOOD"                          "FLOOD & HEAVY RAIN"            
## [163] "FLOOD FLASH"                    "FLOOD FLOOD/FLASH"             
## [165] "FLOOD WATCH/"                   "FLOOD/FLASH"                   
## [167] "Flood/Flash Flood"              "FLOOD/FLASH FLOOD"             
## [169] "FLOOD/FLASH FLOODING"           "FLOOD/FLASH/FLOOD"             
## [171] "FLOOD/FLASHFLOOD"               "FLOOD/RAIN/WIND"               
## [173] "FLOOD/RAIN/WINDS"               "FLOOD/RIVER FLOOD"             
## [175] "Flood/Strong Wind"              "FLOODING"                      
## [177] "FLOODING/HEAVY RAIN"            "FLOODS"                        
## [179] "FOG"                            "FOG AND COLD TEMPERATURES"     
## [181] "FOREST FIRES"                   "Freeze"                        
## [183] "FREEZE"                         "Freezing drizzle"              
## [185] "Freezing Drizzle"               "FREEZING DRIZZLE"              
## [187] "FREEZING DRIZZLE AND FREEZING"  "Freezing Fog"                  
## [189] "FREEZING FOG"                   "Freezing rain"                 
## [191] "Freezing Rain"                  "FREEZING RAIN"                 
## [193] "FREEZING RAIN AND SLEET"        "FREEZING RAIN AND SNOW"        
## [195] "FREEZING RAIN SLEET AND"        "FREEZING RAIN SLEET AND LIGHT" 
## [197] "FREEZING RAIN/SLEET"            "FREEZING RAIN/SNOW"            
## [199] "Freezing Spray"                 "Frost"                         
## [201] "FROST"                          "Frost/Freeze"                  
## [203] "FROST/FREEZE"                   "FROST\\FREEZE"                 
## [205] "FUNNEL"                         "Funnel Cloud"                  
## [207] "FUNNEL CLOUD"                   "FUNNEL CLOUD."                 
## [209] "FUNNEL CLOUD/HAIL"              "FUNNEL CLOUDS"                 
## [211] "FUNNELS"                        "Glaze"                         
## [213] "GLAZE"                          "GLAZE ICE"                     
## [215] "GLAZE/ICE STORM"                "gradient wind"                 
## [217] "Gradient wind"                  "GRADIENT WIND"                 
## [219] "GRADIENT WINDS"                 "GRASS FIRES"                   
## [221] "GROUND BLIZZARD"                "GUSTNADO"                      
## [223] "GUSTNADO AND"                   "GUSTY LAKE WIND"               
## [225] "GUSTY THUNDERSTORM WIND"        "GUSTY THUNDERSTORM WINDS"      
## [227] "Gusty Wind"                     "GUSTY WIND"                    
## [229] "GUSTY WIND/HAIL"                "GUSTY WIND/HVY RAIN"           
## [231] "Gusty wind/rain"                "Gusty winds"                   
## [233] "Gusty Winds"                    "GUSTY WINDS"                   
## [235] "HAIL"                           "HAIL 0.75"                     
## [237] "HAIL 0.88"                      "HAIL 075"                      
## [239] "HAIL 088"                       "HAIL 1.00"                     
## [241] "HAIL 1.75"                      "HAIL 1.75)"                    
## [243] "HAIL 100"                       "HAIL 125"                      
## [245] "HAIL 150"                       "HAIL 175"                      
## [247] "HAIL 200"                       "HAIL 225"                      
## [249] "HAIL 275"                       "HAIL 450"                      
## [251] "HAIL 75"                        "HAIL 80"                       
## [253] "HAIL 88"                        "HAIL ALOFT"                    
## [255] "HAIL DAMAGE"                    "HAIL FLOODING"                 
## [257] "HAIL STORM"                     "Hail(0.75)"                    
## [259] "HAIL/ICY ROADS"                 "HAIL/WIND"                     
## [261] "HAIL/WINDS"                     "HAILSTORM"                     
## [263] "HAILSTORMS"                     "HARD FREEZE"                   
## [265] "HAZARDOUS SURF"                 "HEAT"                          
## [267] "HEAT DROUGHT"                   "Heat Wave"                     
## [269] "HEAT WAVE"                      "HEAT WAVE DROUGHT"             
## [271] "HEAT WAVES"                     "HEAT/DROUGHT"                  
## [273] "Heatburst"                      "HEAVY LAKE SNOW"               
## [275] "HEAVY MIX"                      "HEAVY PRECIPATATION"           
## [277] "Heavy Precipitation"            "HEAVY PRECIPITATION"           
## [279] "Heavy rain"                     "Heavy Rain"                    
## [281] "HEAVY RAIN"                     "HEAVY RAIN AND FLOOD"          
## [283] "Heavy Rain and Wind"            "HEAVY RAIN EFFECTS"            
## [285] "HEAVY RAIN/FLOODING"            "Heavy Rain/High Surf"          
## [287] "HEAVY RAIN/LIGHTNING"           "HEAVY RAIN/MUDSLIDES/FLOOD"    
## [289] "HEAVY RAIN/SEVERE WEATHER"      "HEAVY RAIN/SMALL STREAM URBAN" 
## [291] "HEAVY RAIN/SNOW"                "HEAVY RAIN/URBAN FLOOD"        
## [293] "HEAVY RAIN/WIND"                "HEAVY RAIN; URBAN FLOOD WINDS;"
## [295] "HEAVY RAINFALL"                 "HEAVY RAINS"                   
## [297] "HEAVY RAINS/FLOODING"           "HEAVY SEAS"                    
## [299] "HEAVY SHOWER"                   "HEAVY SHOWERS"                 
## [301] "HEAVY SNOW"                     "HEAVY SNOW-SQUALLS"            
## [303] "HEAVY SNOW   FREEZING RAIN"     "HEAVY SNOW & ICE"              
## [305] "HEAVY SNOW AND"                 "HEAVY SNOW AND HIGH WINDS"     
## [307] "HEAVY SNOW AND ICE"             "HEAVY SNOW AND ICE STORM"      
## [309] "HEAVY SNOW AND STRONG WINDS"    "HEAVY SNOW ANDBLOWING SNOW"    
## [311] "Heavy snow shower"              "HEAVY SNOW SQUALLS"            
## [313] "HEAVY SNOW/BLIZZARD"            "HEAVY SNOW/BLIZZARD/AVALANCHE" 
## [315] "HEAVY SNOW/BLOWING SNOW"        "HEAVY SNOW/FREEZING RAIN"      
## [317] "HEAVY SNOW/HIGH"                "HEAVY SNOW/HIGH WIND"          
## [319] "HEAVY SNOW/HIGH WINDS"          "HEAVY SNOW/HIGH WINDS & FLOOD" 
## [321] "HEAVY SNOW/HIGH WINDS/FREEZING" "HEAVY SNOW/ICE"                
## [323] "HEAVY SNOW/ICE STORM"           "HEAVY SNOW/SLEET"              
## [325] "HEAVY SNOW/SQUALLS"             "HEAVY SNOW/WIND"               
## [327] "HEAVY SNOW/WINTER STORM"        "HEAVY SNOWPACK"                
## [329] "Heavy Surf"                     "HEAVY SURF"                    
## [331] "Heavy surf and wind"            "HEAVY SURF COASTAL FLOODING"   
## [333] "HEAVY SURF/HIGH SURF"           "HEAVY SWELLS"                  
## [335] "HEAVY WET SNOW"                 "HIGH"                          
## [337] "HIGH  SWELLS"                   "HIGH  WINDS"                   
## [339] "HIGH SEAS"                      "High Surf"                     
## [341] "HIGH SURF"                      "HIGH SURF ADVISORIES"          
## [343] "HIGH SURF ADVISORY"             "HIGH SWELLS"                   
## [345] "HIGH TEMPERATURE RECORD"        "HIGH TIDES"                    
## [347] "HIGH WATER"                     "HIGH WAVES"                    
## [349] "High Wind"                      "HIGH WIND"                     
## [351] "HIGH WIND (G40)"                "HIGH WIND 48"                  
## [353] "HIGH WIND 63"                   "HIGH WIND 70"                  
## [355] "HIGH WIND AND HEAVY SNOW"       "HIGH WIND AND HIGH TIDES"      
## [357] "HIGH WIND AND SEAS"             "HIGH WIND DAMAGE"              
## [359] "HIGH WIND/ BLIZZARD"            "HIGH WIND/BLIZZARD"            
## [361] "HIGH WIND/BLIZZARD/FREEZING RA" "HIGH WIND/HEAVY SNOW"          
## [363] "HIGH WIND/LOW WIND CHILL"       "HIGH WIND/SEAS"                
## [365] "HIGH WIND/WIND CHILL"           "HIGH WIND/WIND CHILL/BLIZZARD" 
## [367] "HIGH WINDS"                     "HIGH WINDS 55"                 
## [369] "HIGH WINDS 57"                  "HIGH WINDS 58"                 
## [371] "HIGH WINDS 63"                  "HIGH WINDS 66"                 
## [373] "HIGH WINDS 67"                  "HIGH WINDS 73"                 
## [375] "HIGH WINDS 76"                  "HIGH WINDS 80"                 
## [377] "HIGH WINDS 82"                  "HIGH WINDS AND WIND CHILL"     
## [379] "HIGH WINDS DUST STORM"          "HIGH WINDS HEAVY RAINS"        
## [381] "HIGH WINDS/"                    "HIGH WINDS/COASTAL FLOOD"      
## [383] "HIGH WINDS/COLD"                "HIGH WINDS/FLOODING"           
## [385] "HIGH WINDS/HEAVY RAIN"          "HIGH WINDS/SNOW"               
## [387] "HIGHWAY FLOODING"               "Hot and Dry"                   
## [389] "HOT PATTERN"                    "HOT SPELL"                     
## [391] "HOT WEATHER"                    "HOT/DRY PATTERN"               
## [393] "HURRICANE"                      "HURRICANE-GENERATED SWELLS"    
## [395] "Hurricane Edouard"              "HURRICANE EMILY"               
## [397] "HURRICANE ERIN"                 "HURRICANE FELIX"               
## [399] "HURRICANE GORDON"               "HURRICANE OPAL"                
## [401] "HURRICANE OPAL/HIGH WINDS"      "HURRICANE/TYPHOON"             
## [403] "HVY RAIN"                       "HYPERTHERMIA/EXPOSURE"         
## [405] "HYPOTHERMIA"                    "Hypothermia/Exposure"          
## [407] "HYPOTHERMIA/EXPOSURE"           "ICE"                           
## [409] "ICE AND SNOW"                   "ICE FLOES"                     
## [411] "Ice Fog"                        "ICE JAM"                       
## [413] "Ice jam flood (minor"           "ICE JAM FLOODING"              
## [415] "ICE ON ROAD"                    "ICE PELLETS"                   
## [417] "ICE ROADS"                      "ICE STORM"                     
## [419] "ICE STORM AND SNOW"             "ICE STORM/FLASH FLOOD"         
## [421] "Ice/Snow"                       "ICE/SNOW"                      
## [423] "ICE/STRONG WINDS"               "Icestorm/Blizzard"             
## [425] "Icy Roads"                      "ICY ROADS"                     
## [427] "LACK OF SNOW"                   "LAKE-EFFECT SNOW"              
## [429] "Lake Effect Snow"               "LAKE EFFECT SNOW"              
## [431] "LAKE FLOOD"                     "LAKESHORE FLOOD"               
## [433] "LANDSLIDE"                      "LANDSLIDE/URBAN FLOOD"         
## [435] "LANDSLIDES"                     "Landslump"                     
## [437] "LANDSLUMP"                      "LANDSPOUT"                     
## [439] "LARGE WALL CLOUD"               "Late-season Snowfall"          
## [441] "LATE FREEZE"                    "LATE SEASON HAIL"              
## [443] "LATE SEASON SNOW"               "Late Season Snowfall"          
## [445] "LATE SNOW"                      "LIGHT FREEZING RAIN"           
## [447] "Light snow"                     "Light Snow"                    
## [449] "LIGHT SNOW"                     "LIGHT SNOW AND SLEET"          
## [451] "Light Snow/Flurries"            "LIGHT SNOW/FREEZING PRECIP"    
## [453] "Light Snowfall"                 "LIGHTING"                      
## [455] "LIGHTNING"                      "LIGHTNING  WAUSEON"            
## [457] "LIGHTNING AND HEAVY RAIN"       "LIGHTNING AND THUNDERSTORM WIN"
## [459] "LIGHTNING AND WINDS"            "LIGHTNING DAMAGE"              
## [461] "LIGHTNING FIRE"                 "LIGHTNING INJURY"              
## [463] "LIGHTNING THUNDERSTORM WINDS"   "LIGHTNING THUNDERSTORM WINDSS" 
## [465] "LIGHTNING."                     "LIGHTNING/HEAVY RAIN"          
## [467] "LIGNTNING"                      "LOCAL FLASH FLOOD"             
## [469] "LOCAL FLOOD"                    "LOCALLY HEAVY RAIN"            
## [471] "LOW TEMPERATURE"                "LOW TEMPERATURE RECORD"        
## [473] "LOW WIND CHILL"                 "MAJOR FLOOD"                   
## [475] "Marine Accident"                "MARINE HAIL"                   
## [477] "MARINE HIGH WIND"               "MARINE MISHAP"                 
## [479] "MARINE STRONG WIND"             "MARINE THUNDERSTORM WIND"      
## [481] "MARINE TSTM WIND"               "Metro Storm, May 26"           
## [483] "Microburst"                     "MICROBURST"                    
## [485] "MICROBURST WINDS"               "Mild and Dry Pattern"          
## [487] "MILD PATTERN"                   "MILD/DRY PATTERN"              
## [489] "MINOR FLOOD"                    "Minor Flooding"                
## [491] "MINOR FLOODING"                 "MIXED PRECIP"                  
## [493] "Mixed Precipitation"            "MIXED PRECIPITATION"           
## [495] "MODERATE SNOW"                  "MODERATE SNOWFALL"             
## [497] "MONTHLY PRECIPITATION"          "Monthly Rainfall"              
## [499] "MONTHLY RAINFALL"               "Monthly Snowfall"              
## [501] "MONTHLY SNOWFALL"               "MONTHLY TEMPERATURE"           
## [503] "Mountain Snows"                 "MUD SLIDE"                     
## [505] "MUD SLIDES"                     "MUD SLIDES URBAN FLOODING"     
## [507] "MUD/ROCK SLIDE"                 "Mudslide"                      
## [509] "MUDSLIDE"                       "MUDSLIDE/LANDSLIDE"            
## [511] "Mudslides"                      "MUDSLIDES"                     
## [513] "NEAR RECORD SNOW"               "No Severe Weather"             
## [515] "NON-SEVERE WIND DAMAGE"         "NON-TSTM WIND"                 
## [517] "NON SEVERE HAIL"                "NON TSTM WIND"                 
## [519] "NONE"                           "NORMAL PRECIPITATION"          
## [521] "NORTHERN LIGHTS"                "Other"                         
## [523] "OTHER"                          "PATCHY DENSE FOG"              
## [525] "PATCHY ICE"                     "Prolong Cold"                  
## [527] "PROLONG COLD"                   "PROLONG COLD/SNOW"             
## [529] "PROLONG WARMTH"                 "PROLONGED RAIN"                
## [531] "RAIN"                           "RAIN (HEAVY)"                  
## [533] "RAIN AND WIND"                  "Rain Damage"                   
## [535] "RAIN/SNOW"                      "RAIN/WIND"                     
## [537] "RAINSTORM"                      "RAPIDLY RISING WATER"          
## [539] "RECORD  COLD"                   "Record Cold"                   
## [541] "RECORD COLD"                    "RECORD COLD AND HIGH WIND"     
## [543] "RECORD COLD/FROST"              "RECORD COOL"                   
## [545] "Record dry month"               "RECORD DRYNESS"                
## [547] "Record Heat"                    "RECORD HEAT"                   
## [549] "RECORD HEAT WAVE"               "Record High"                   
## [551] "RECORD HIGH"                    "RECORD HIGH TEMPERATURE"       
## [553] "RECORD HIGH TEMPERATURES"       "RECORD LOW"                    
## [555] "RECORD LOW RAINFALL"            "Record May Snow"               
## [557] "RECORD PRECIPITATION"           "RECORD RAINFALL"               
## [559] "RECORD SNOW"                    "RECORD SNOW/COLD"              
## [561] "RECORD SNOWFALL"                "Record temperature"            
## [563] "RECORD TEMPERATURE"             "Record Temperatures"           
## [565] "RECORD TEMPERATURES"            "RECORD WARM"                   
## [567] "RECORD WARM TEMPS."             "Record Warmth"                 
## [569] "RECORD WARMTH"                  "Record Winter Snow"            
## [571] "RECORD/EXCESSIVE HEAT"          "RECORD/EXCESSIVE RAINFALL"     
## [573] "RED FLAG CRITERIA"              "RED FLAG FIRE WX"              
## [575] "REMNANTS OF FLOYD"              "RIP CURRENT"                   
## [577] "RIP CURRENTS"                   "RIP CURRENTS HEAVY SURF"       
## [579] "RIP CURRENTS/HEAVY SURF"        "RIVER AND STREAM FLOOD"        
## [581] "RIVER FLOOD"                    "River Flooding"                
## [583] "RIVER FLOODING"                 "ROCK SLIDE"                    
## [585] "ROGUE WAVE"                     "ROTATING WALL CLOUD"           
## [587] "ROUGH SEAS"                     "ROUGH SURF"                    
## [589] "RURAL FLOOD"                    "Saharan Dust"                  
## [591] "SAHARAN DUST"                   "Seasonal Snowfall"             
## [593] "SEICHE"                         "SEVERE COLD"                   
## [595] "SEVERE THUNDERSTORM"            "SEVERE THUNDERSTORM WINDS"     
## [597] "SEVERE THUNDERSTORMS"           "SEVERE TURBULENCE"             
## [599] "SLEET"                          "SLEET & FREEZING RAIN"         
## [601] "SLEET STORM"                    "SLEET/FREEZING RAIN"           
## [603] "SLEET/ICE STORM"                "SLEET/RAIN/SNOW"               
## [605] "SLEET/SNOW"                     "small hail"                    
## [607] "Small Hail"                     "SMALL HAIL"                    
## [609] "SMALL STREAM"                   "SMALL STREAM AND"              
## [611] "SMALL STREAM AND URBAN FLOOD"   "SMALL STREAM AND URBAN FLOODIN"
## [613] "SMALL STREAM FLOOD"             "SMALL STREAM FLOODING"         
## [615] "SMALL STREAM URBAN FLOOD"       "SMALL STREAM/URBAN FLOOD"      
## [617] "Sml Stream Fld"                 "SMOKE"                         
## [619] "Snow"                           "SNOW"                          
## [621] "SNOW- HIGH WIND- WIND CHILL"    "Snow Accumulation"             
## [623] "SNOW ACCUMULATION"              "SNOW ADVISORY"                 
## [625] "SNOW AND COLD"                  "SNOW AND HEAVY SNOW"           
## [627] "Snow and Ice"                   "SNOW AND ICE"                  
## [629] "SNOW AND ICE STORM"             "Snow and sleet"                
## [631] "SNOW AND SLEET"                 "SNOW AND WIND"                 
## [633] "SNOW DROUGHT"                   "SNOW FREEZING RAIN"            
## [635] "SNOW SHOWERS"                   "SNOW SLEET"                    
## [637] "SNOW SQUALL"                    "Snow squalls"                  
## [639] "Snow Squalls"                   "SNOW SQUALLS"                  
## [641] "SNOW/ BITTER COLD"              "SNOW/ ICE"                     
## [643] "SNOW/BLOWING SNOW"              "SNOW/COLD"                     
## [645] "SNOW/FREEZING RAIN"             "SNOW/HEAVY SNOW"               
## [647] "SNOW/HIGH WINDS"                "SNOW/ICE"                      
## [649] "SNOW/ICE STORM"                 "SNOW/RAIN"                     
## [651] "SNOW/RAIN/SLEET"                "SNOW/SLEET"                    
## [653] "SNOW/SLEET/FREEZING RAIN"       "SNOW/SLEET/RAIN"               
## [655] "SNOW\\COLD"                     "SNOWFALL RECORD"               
## [657] "SNOWMELT FLOODING"              "SNOWSTORM"                     
## [659] "SOUTHEAST"                      "STORM FORCE WINDS"             
## [661] "STORM SURGE"                    "STORM SURGE/TIDE"              
## [663] "STREAM FLOODING"                "STREET FLOOD"                  
## [665] "STREET FLOODING"                "Strong Wind"                   
## [667] "STRONG WIND"                    "STRONG WIND GUST"              
## [669] "Strong winds"                   "Strong Winds"                  
## [671] "STRONG WINDS"                   "Summary August 10"             
## [673] "Summary August 11"              "Summary August 17"             
## [675] "Summary August 2-3"             "Summary August 21"             
## [677] "Summary August 28"              "Summary August 4"              
## [679] "Summary August 7"               "Summary August 9"              
## [681] "Summary Jan 17"                 "Summary July 23-24"            
## [683] "Summary June 18-19"             "Summary June 5-6"              
## [685] "Summary June 6"                 "Summary of April 12"           
## [687] "Summary of April 13"            "Summary of April 21"           
## [689] "Summary of April 27"            "Summary of April 3rd"          
## [691] "Summary of August 1"            "Summary of July 11"            
## [693] "Summary of July 2"              "Summary of July 22"            
## [695] "Summary of July 26"             "Summary of July 29"            
## [697] "Summary of July 3"              "Summary of June 10"            
## [699] "Summary of June 11"             "Summary of June 12"            
## [701] "Summary of June 13"             "Summary of June 15"            
## [703] "Summary of June 16"             "Summary of June 18"            
## [705] "Summary of June 23"             "Summary of June 24"            
## [707] "Summary of June 3"              "Summary of June 30"            
## [709] "Summary of June 4"              "Summary of June 6"             
## [711] "Summary of March 14"            "Summary of March 23"           
## [713] "Summary of March 24"            "SUMMARY OF MARCH 24-25"        
## [715] "SUMMARY OF MARCH 27"            "SUMMARY OF MARCH 29"           
## [717] "Summary of May 10"              "Summary of May 13"             
## [719] "Summary of May 14"              "Summary of May 22"             
## [721] "Summary of May 22 am"           "Summary of May 22 pm"          
## [723] "Summary of May 26 am"           "Summary of May 26 pm"          
## [725] "Summary of May 31 am"           "Summary of May 31 pm"          
## [727] "Summary of May 9-10"            "Summary Sept. 25-26"           
## [729] "Summary September 20"           "Summary September 23"          
## [731] "Summary September 3"            "Summary September 4"           
## [733] "Summary: Nov. 16"               "Summary: Nov. 6-7"             
## [735] "Summary: Oct. 20-21"            "Summary: October 31"           
## [737] "Summary: Sept. 18"              "Temperature record"            
## [739] "THUDERSTORM WINDS"              "THUNDEERSTORM WINDS"           
## [741] "THUNDERESTORM WINDS"            "THUNDERSNOW"                   
## [743] "Thundersnow shower"             "THUNDERSTORM"                  
## [745] "THUNDERSTORM  WINDS"            "THUNDERSTORM DAMAGE"           
## [747] "THUNDERSTORM DAMAGE TO"         "THUNDERSTORM HAIL"             
## [749] "THUNDERSTORM W INDS"            "Thunderstorm Wind"             
## [751] "THUNDERSTORM WIND"              "THUNDERSTORM WIND (G40)"       
## [753] "THUNDERSTORM WIND 50"           "THUNDERSTORM WIND 52"          
## [755] "THUNDERSTORM WIND 56"           "THUNDERSTORM WIND 59"          
## [757] "THUNDERSTORM WIND 59 MPH"       "THUNDERSTORM WIND 59 MPH."     
## [759] "THUNDERSTORM WIND 60 MPH"       "THUNDERSTORM WIND 65 MPH"      
## [761] "THUNDERSTORM WIND 65MPH"        "THUNDERSTORM WIND 69"          
## [763] "THUNDERSTORM WIND 98 MPH"       "THUNDERSTORM WIND G50"         
## [765] "THUNDERSTORM WIND G51"          "THUNDERSTORM WIND G52"         
## [767] "THUNDERSTORM WIND G55"          "THUNDERSTORM WIND G60"         
## [769] "THUNDERSTORM WIND G61"          "THUNDERSTORM WIND TREES"       
## [771] "THUNDERSTORM WIND."             "THUNDERSTORM WIND/ TREE"       
## [773] "THUNDERSTORM WIND/ TREES"       "THUNDERSTORM WIND/AWNING"      
## [775] "THUNDERSTORM WIND/HAIL"         "THUNDERSTORM WIND/LIGHTNING"   
## [777] "THUNDERSTORM WINDS"             "THUNDERSTORM WINDS      LE CEN"
## [779] "THUNDERSTORM WINDS 13"          "THUNDERSTORM WINDS 2"          
## [781] "THUNDERSTORM WINDS 50"          "THUNDERSTORM WINDS 52"         
## [783] "THUNDERSTORM WINDS 53"          "THUNDERSTORM WINDS 60"         
## [785] "THUNDERSTORM WINDS 61"          "THUNDERSTORM WINDS 62"         
## [787] "THUNDERSTORM WINDS 63 MPH"      "THUNDERSTORM WINDS AND"        
## [789] "THUNDERSTORM WINDS FUNNEL CLOU" "THUNDERSTORM WINDS G"          
## [791] "THUNDERSTORM WINDS G60"         "THUNDERSTORM WINDS HAIL"       
## [793] "THUNDERSTORM WINDS HEAVY RAIN"  "THUNDERSTORM WINDS LIGHTNING"  
## [795] "THUNDERSTORM WINDS SMALL STREA" "THUNDERSTORM WINDS URBAN FLOOD"
## [797] "THUNDERSTORM WINDS."            "THUNDERSTORM WINDS/ FLOOD"     
## [799] "THUNDERSTORM WINDS/ HAIL"       "THUNDERSTORM WINDS/FLASH FLOOD"
## [801] "THUNDERSTORM WINDS/FLOODING"    "THUNDERSTORM WINDS/FUNNEL CLOU"
## [803] "THUNDERSTORM WINDS/HAIL"        "THUNDERSTORM WINDS/HEAVY RAIN" 
## [805] "THUNDERSTORM WINDS53"           "THUNDERSTORM WINDSHAIL"        
## [807] "THUNDERSTORM WINDSS"            "THUNDERSTORM WINS"             
## [809] "THUNDERSTORMS"                  "THUNDERSTORMS WIND"            
## [811] "THUNDERSTORMS WINDS"            "THUNDERSTORMW"                 
## [813] "THUNDERSTORMW 50"               "THUNDERSTORMW WINDS"           
## [815] "THUNDERSTORMWINDS"              "THUNDERSTROM WIND"             
## [817] "THUNDERSTROM WINDS"             "THUNDERTORM WINDS"             
## [819] "THUNDERTSORM WIND"              "THUNDESTORM WINDS"             
## [821] "THUNERSTORM WINDS"              "TIDAL FLOOD"                   
## [823] "Tidal Flooding"                 "TIDAL FLOODING"                
## [825] "TORNADO"                        "TORNADO DEBRIS"                
## [827] "TORNADO F0"                     "TORNADO F1"                    
## [829] "TORNADO F2"                     "TORNADO F3"                    
## [831] "TORNADO/WATERSPOUT"             "TORNADOES"                     
## [833] "TORNADOES, TSTM WIND, HAIL"     "TORNADOS"                      
## [835] "TORNDAO"                        "TORRENTIAL RAIN"               
## [837] "Torrential Rainfall"            "TROPICAL DEPRESSION"           
## [839] "TROPICAL STORM"                 "TROPICAL STORM ALBERTO"        
## [841] "TROPICAL STORM DEAN"            "TROPICAL STORM GORDON"         
## [843] "TROPICAL STORM JERRY"           "TSTM"                          
## [845] "TSTM HEAVY RAIN"                "Tstm Wind"                     
## [847] "TSTM WIND"                      "TSTM WIND  (G45)"              
## [849] "TSTM WIND (41)"                 "TSTM WIND (G35)"               
## [851] "TSTM WIND (G40)"                "TSTM WIND (G45)"               
## [853] "TSTM WIND 40"                   "TSTM WIND 45"                  
## [855] "TSTM WIND 50"                   "TSTM WIND 51"                  
## [857] "TSTM WIND 52"                   "TSTM WIND 55"                  
## [859] "TSTM WIND 65)"                  "TSTM WIND AND LIGHTNING"       
## [861] "TSTM WIND DAMAGE"               "TSTM WIND G45"                 
## [863] "TSTM WIND G58"                  "TSTM WIND/HAIL"                
## [865] "TSTM WINDS"                     "TSTM WND"                      
## [867] "TSTMW"                          "TSUNAMI"                       
## [869] "TUNDERSTORM WIND"               "TYPHOON"                       
## [871] "Unseasonable Cold"              "UNSEASONABLY COLD"             
## [873] "UNSEASONABLY COOL"              "UNSEASONABLY COOL & WET"       
## [875] "UNSEASONABLY DRY"               "UNSEASONABLY HOT"              
## [877] "UNSEASONABLY WARM"              "UNSEASONABLY WARM & WET"       
## [879] "UNSEASONABLY WARM AND DRY"      "UNSEASONABLY WARM YEAR"        
## [881] "UNSEASONABLY WARM/WET"          "UNSEASONABLY WET"              
## [883] "UNSEASONAL LOW TEMP"            "UNSEASONAL RAIN"               
## [885] "UNUSUAL WARMTH"                 "UNUSUAL/RECORD WARMTH"         
## [887] "UNUSUALLY COLD"                 "UNUSUALLY LATE SNOW"           
## [889] "UNUSUALLY WARM"                 "URBAN AND SMALL"               
## [891] "URBAN AND SMALL STREAM"         "URBAN AND SMALL STREAM FLOOD"  
## [893] "URBAN AND SMALL STREAM FLOODIN" "Urban flood"                   
## [895] "Urban Flood"                    "URBAN FLOOD"                   
## [897] "URBAN FLOOD LANDSLIDE"          "Urban Flooding"                
## [899] "URBAN FLOODING"                 "URBAN FLOODS"                  
## [901] "URBAN SMALL"                    "URBAN SMALL STREAM FLOOD"      
## [903] "URBAN/SMALL"                    "URBAN/SMALL FLOODING"          
## [905] "URBAN/SMALL STREAM"             "URBAN/SMALL STREAM  FLOOD"     
## [907] "URBAN/SMALL STREAM FLOOD"       "URBAN/SMALL STREAM FLOODING"   
## [909] "URBAN/SMALL STRM FLDG"          "URBAN/SML STREAM FLD"          
## [911] "URBAN/SML STREAM FLDG"          "URBAN/STREET FLOODING"         
## [913] "VERY DRY"                       "VERY WARM"                     
## [915] "VOG"                            "Volcanic Ash"                  
## [917] "VOLCANIC ASH"                   "Volcanic Ash Plume"            
## [919] "VOLCANIC ASHFALL"               "VOLCANIC ERUPTION"             
## [921] "WAKE LOW WIND"                  "WALL CLOUD"                    
## [923] "WALL CLOUD/FUNNEL CLOUD"        "WARM DRY CONDITIONS"           
## [925] "WARM WEATHER"                   "WATER SPOUT"                   
## [927] "WATERSPOUT"                     "WATERSPOUT-"                   
## [929] "WATERSPOUT-TORNADO"             "WATERSPOUT FUNNEL CLOUD"       
## [931] "WATERSPOUT TORNADO"             "WATERSPOUT/"                   
## [933] "WATERSPOUT/ TORNADO"            "WATERSPOUT/TORNADO"            
## [935] "WATERSPOUTS"                    "WAYTERSPOUT"                   
## [937] "wet micoburst"                  "WET MICROBURST"                
## [939] "Wet Month"                      "WET SNOW"                      
## [941] "WET WEATHER"                    "Wet Year"                      
## [943] "Whirlwind"                      "WHIRLWIND"                     
## [945] "WILD FIRES"                     "WILD/FOREST FIRE"              
## [947] "WILD/FOREST FIRES"              "WILDFIRE"                      
## [949] "WILDFIRES"                      "Wind"                          
## [951] "WIND"                           "WIND ADVISORY"                 
## [953] "WIND AND WAVE"                  "WIND CHILL"                    
## [955] "WIND CHILL/HIGH WIND"           "Wind Damage"                   
## [957] "WIND DAMAGE"                    "WIND GUSTS"                    
## [959] "WIND STORM"                     "WIND/HAIL"                     
## [961] "WINDS"                          "WINTER MIX"                    
## [963] "WINTER STORM"                   "WINTER STORM HIGH WINDS"       
## [965] "WINTER STORM/HIGH WIND"         "WINTER STORM/HIGH WINDS"       
## [967] "WINTER STORMS"                  "Winter Weather"                
## [969] "WINTER WEATHER"                 "WINTER WEATHER MIX"            
## [971] "WINTER WEATHER/MIX"             "WINTERY MIX"                   
## [973] "Wintry mix"                     "Wintry Mix"                    
## [975] "WINTRY MIX"                     "WND"
```

```r
print(length(which(is.na(stormData$EVTYPE))))
```

```
## [1] 1
```

Set the values from EVTYPE column to upper-case.  Here we print the quantity of unique values

```r
stormData$EVTYPE<-toupper(stormData$EVTYPE)

print(length(unique(stormData$EVTYPE)))
```

```
## [1] 890
```

```r
print(sort(unique(stormData$EVTYPE)))
```

```
##   [1] "ABNORMAL WARMTH"                "ABNORMALLY DRY"                
##   [3] "ABNORMALLY WET"                 "ACCUMULATED SNOWFALL"          
##   [5] "AGRICULTURAL FREEZE"            "APACHE COUNTY"                 
##   [7] "ASTRONOMICAL HIGH TIDE"         "ASTRONOMICAL LOW TIDE"         
##   [9] "AVALANCE"                       "AVALANCHE"                     
##  [11] "BEACH EROSIN"                   "BEACH EROSION"                 
##  [13] "BEACH EROSION/COASTAL FLOOD"    "BEACH FLOOD"                   
##  [15] "BELOW NORMAL PRECIPITATION"     "BITTER WIND CHILL"             
##  [17] "BITTER WIND CHILL TEMPERATURES" "BLACK ICE"                     
##  [19] "BLIZZARD"                       "BLIZZARD AND EXTREME WIND CHIL"
##  [21] "BLIZZARD AND HEAVY SNOW"        "BLIZZARD SUMMARY"              
##  [23] "BLIZZARD WEATHER"               "BLIZZARD/FREEZING RAIN"        
##  [25] "BLIZZARD/HEAVY SNOW"            "BLIZZARD/HIGH WIND"            
##  [27] "BLIZZARD/WINTER STORM"          "BLOW-OUT TIDE"                 
##  [29] "BLOW-OUT TIDES"                 "BLOWING DUST"                  
##  [31] "BLOWING SNOW"                   "BLOWING SNOW- EXTREME WIND CHI"
##  [33] "BLOWING SNOW & EXTREME WIND CH" "BLOWING SNOW/EXTREME WIND CHIL"
##  [35] "BREAKUP FLOODING"               "BRUSH FIRE"                    
##  [37] "BRUSH FIRES"                    "COASTAL  FLOODING/EROSION"     
##  [39] "COASTAL EROSION"                "COASTAL FLOOD"                 
##  [41] "COASTAL FLOODING"               "COASTAL FLOODING/EROSION"      
##  [43] "COASTAL STORM"                  "COASTAL SURGE"                 
##  [45] "COASTAL/TIDAL FLOOD"            "COASTALFLOOD"                  
##  [47] "COASTALSTORM"                   "COLD"                          
##  [49] "COLD AIR FUNNEL"                "COLD AIR FUNNELS"              
##  [51] "COLD AIR TORNADO"               "COLD AND FROST"                
##  [53] "COLD AND SNOW"                  "COLD AND WET CONDITIONS"       
##  [55] "COLD TEMPERATURE"               "COLD TEMPERATURES"             
##  [57] "COLD WAVE"                      "COLD WEATHER"                  
##  [59] "COLD WIND CHILL TEMPERATURES"   "COLD/WIND CHILL"               
##  [61] "COLD/WINDS"                     "COOL AND WET"                  
##  [63] "COOL SPELL"                     "CSTL FLOODING/EROSION"         
##  [65] "DAM BREAK"                      "DAM FAILURE"                   
##  [67] "DAMAGING FREEZE"                "DEEP HAIL"                     
##  [69] "DENSE FOG"                      "DENSE SMOKE"                   
##  [71] "DOWNBURST"                      "DOWNBURST WINDS"               
##  [73] "DRIEST MONTH"                   "DRIFTING SNOW"                 
##  [75] "DROUGHT"                        "DROUGHT/EXCESSIVE HEAT"        
##  [77] "DROWNING"                       "DRY"                           
##  [79] "DRY CONDITIONS"                 "DRY HOT WEATHER"               
##  [81] "DRY MICROBURST"                 "DRY MICROBURST 50"             
##  [83] "DRY MICROBURST 53"              "DRY MICROBURST 58"             
##  [85] "DRY MICROBURST 61"              "DRY MICROBURST 84"             
##  [87] "DRY MICROBURST WINDS"           "DRY MIRCOBURST WINDS"          
##  [89] "DRY PATTERN"                    "DRY SPELL"                     
##  [91] "DRY WEATHER"                    "DRYNESS"                       
##  [93] "DUST DEVEL"                     "DUST DEVIL"                    
##  [95] "DUST DEVIL WATERSPOUT"          "DUST STORM"                    
##  [97] "DUST STORM/HIGH WINDS"          "DUSTSTORM"                     
##  [99] "EARLY FREEZE"                   "EARLY FROST"                   
## [101] "EARLY RAIN"                     "EARLY SNOW"                    
## [103] "EARLY SNOWFALL"                 "EROSION/CSTL FLOOD"            
## [105] "EXCESSIVE"                      "EXCESSIVE COLD"                
## [107] "EXCESSIVE HEAT"                 "EXCESSIVE HEAT/DROUGHT"        
## [109] "EXCESSIVE PRECIPITATION"        "EXCESSIVE RAIN"                
## [111] "EXCESSIVE RAINFALL"             "EXCESSIVE SNOW"                
## [113] "EXCESSIVE WETNESS"              "EXCESSIVELY DRY"               
## [115] "EXTENDED COLD"                  "EXTREME COLD"                  
## [117] "EXTREME COLD/WIND CHILL"        "EXTREME HEAT"                  
## [119] "EXTREME WIND CHILL"             "EXTREME WIND CHILL/BLOWING SNO"
## [121] "EXTREME WIND CHILLS"            "EXTREME WINDCHILL"             
## [123] "EXTREME WINDCHILL TEMPERATURES" "EXTREME/RECORD COLD"           
## [125] "EXTREMELY WET"                  "FALLING SNOW/ICE"              
## [127] "FIRST FROST"                    "FIRST SNOW"                    
## [129] "FLASH FLOOD"                    "FLASH FLOOD - HEAVY RAIN"      
## [131] "FLASH FLOOD FROM ICE JAMS"      "FLASH FLOOD LANDSLIDES"        
## [133] "FLASH FLOOD WINDS"              "FLASH FLOOD/"                  
## [135] "FLASH FLOOD/ FLOOD"             "FLASH FLOOD/ STREET"           
## [137] "FLASH FLOOD/FLOOD"              "FLASH FLOOD/HEAVY RAIN"        
## [139] "FLASH FLOOD/LANDSLIDE"          "FLASH FLOODING"                
## [141] "FLASH FLOODING/FLOOD"           "FLASH FLOODING/THUNDERSTORM WI"
## [143] "FLASH FLOODS"                   "FLASH FLOOODING"               
## [145] "FLOOD"                          "FLOOD & HEAVY RAIN"            
## [147] "FLOOD FLASH"                    "FLOOD FLOOD/FLASH"             
## [149] "FLOOD WATCH/"                   "FLOOD/FLASH"                   
## [151] "FLOOD/FLASH FLOOD"              "FLOOD/FLASH FLOODING"          
## [153] "FLOOD/FLASH/FLOOD"              "FLOOD/FLASHFLOOD"              
## [155] "FLOOD/RAIN/WIND"                "FLOOD/RAIN/WINDS"              
## [157] "FLOOD/RIVER FLOOD"              "FLOOD/STRONG WIND"             
## [159] "FLOODING"                       "FLOODING/HEAVY RAIN"           
## [161] "FLOODS"                         "FOG"                           
## [163] "FOG AND COLD TEMPERATURES"      "FOREST FIRES"                  
## [165] "FREEZE"                         "FREEZING DRIZZLE"              
## [167] "FREEZING DRIZZLE AND FREEZING"  "FREEZING FOG"                  
## [169] "FREEZING RAIN"                  "FREEZING RAIN AND SLEET"       
## [171] "FREEZING RAIN AND SNOW"         "FREEZING RAIN SLEET AND"       
## [173] "FREEZING RAIN SLEET AND LIGHT"  "FREEZING RAIN/SLEET"           
## [175] "FREEZING RAIN/SNOW"             "FREEZING SPRAY"                
## [177] "FROST"                          "FROST/FREEZE"                  
## [179] "FROST\\FREEZE"                  "FUNNEL"                        
## [181] "FUNNEL CLOUD"                   "FUNNEL CLOUD."                 
## [183] "FUNNEL CLOUD/HAIL"              "FUNNEL CLOUDS"                 
## [185] "FUNNELS"                        "GLAZE"                         
## [187] "GLAZE ICE"                      "GLAZE/ICE STORM"               
## [189] "GRADIENT WIND"                  "GRADIENT WINDS"                
## [191] "GRASS FIRES"                    "GROUND BLIZZARD"               
## [193] "GUSTNADO"                       "GUSTNADO AND"                  
## [195] "GUSTY LAKE WIND"                "GUSTY THUNDERSTORM WIND"       
## [197] "GUSTY THUNDERSTORM WINDS"       "GUSTY WIND"                    
## [199] "GUSTY WIND/HAIL"                "GUSTY WIND/HVY RAIN"           
## [201] "GUSTY WIND/RAIN"                "GUSTY WINDS"                   
## [203] "HAIL"                           "HAIL 0.75"                     
## [205] "HAIL 0.88"                      "HAIL 075"                      
## [207] "HAIL 088"                       "HAIL 1.00"                     
## [209] "HAIL 1.75"                      "HAIL 1.75)"                    
## [211] "HAIL 100"                       "HAIL 125"                      
## [213] "HAIL 150"                       "HAIL 175"                      
## [215] "HAIL 200"                       "HAIL 225"                      
## [217] "HAIL 275"                       "HAIL 450"                      
## [219] "HAIL 75"                        "HAIL 80"                       
## [221] "HAIL 88"                        "HAIL ALOFT"                    
## [223] "HAIL DAMAGE"                    "HAIL FLOODING"                 
## [225] "HAIL STORM"                     "HAIL(0.75)"                    
## [227] "HAIL/ICY ROADS"                 "HAIL/WIND"                     
## [229] "HAIL/WINDS"                     "HAILSTORM"                     
## [231] "HAILSTORMS"                     "HARD FREEZE"                   
## [233] "HAZARDOUS SURF"                 "HEAT"                          
## [235] "HEAT DROUGHT"                   "HEAT WAVE"                     
## [237] "HEAT WAVE DROUGHT"              "HEAT WAVES"                    
## [239] "HEAT/DROUGHT"                   "HEATBURST"                     
## [241] "HEAVY LAKE SNOW"                "HEAVY MIX"                     
## [243] "HEAVY PRECIPATATION"            "HEAVY PRECIPITATION"           
## [245] "HEAVY RAIN"                     "HEAVY RAIN AND FLOOD"          
## [247] "HEAVY RAIN AND WIND"            "HEAVY RAIN EFFECTS"            
## [249] "HEAVY RAIN/FLOODING"            "HEAVY RAIN/HIGH SURF"          
## [251] "HEAVY RAIN/LIGHTNING"           "HEAVY RAIN/MUDSLIDES/FLOOD"    
## [253] "HEAVY RAIN/SEVERE WEATHER"      "HEAVY RAIN/SMALL STREAM URBAN" 
## [255] "HEAVY RAIN/SNOW"                "HEAVY RAIN/URBAN FLOOD"        
## [257] "HEAVY RAIN/WIND"                "HEAVY RAIN; URBAN FLOOD WINDS;"
## [259] "HEAVY RAINFALL"                 "HEAVY RAINS"                   
## [261] "HEAVY RAINS/FLOODING"           "HEAVY SEAS"                    
## [263] "HEAVY SHOWER"                   "HEAVY SHOWERS"                 
## [265] "HEAVY SNOW"                     "HEAVY SNOW-SQUALLS"            
## [267] "HEAVY SNOW   FREEZING RAIN"     "HEAVY SNOW & ICE"              
## [269] "HEAVY SNOW AND"                 "HEAVY SNOW AND HIGH WINDS"     
## [271] "HEAVY SNOW AND ICE"             "HEAVY SNOW AND ICE STORM"      
## [273] "HEAVY SNOW AND STRONG WINDS"    "HEAVY SNOW ANDBLOWING SNOW"    
## [275] "HEAVY SNOW SHOWER"              "HEAVY SNOW SQUALLS"            
## [277] "HEAVY SNOW/BLIZZARD"            "HEAVY SNOW/BLIZZARD/AVALANCHE" 
## [279] "HEAVY SNOW/BLOWING SNOW"        "HEAVY SNOW/FREEZING RAIN"      
## [281] "HEAVY SNOW/HIGH"                "HEAVY SNOW/HIGH WIND"          
## [283] "HEAVY SNOW/HIGH WINDS"          "HEAVY SNOW/HIGH WINDS & FLOOD" 
## [285] "HEAVY SNOW/HIGH WINDS/FREEZING" "HEAVY SNOW/ICE"                
## [287] "HEAVY SNOW/ICE STORM"           "HEAVY SNOW/SLEET"              
## [289] "HEAVY SNOW/SQUALLS"             "HEAVY SNOW/WIND"               
## [291] "HEAVY SNOW/WINTER STORM"        "HEAVY SNOWPACK"                
## [293] "HEAVY SURF"                     "HEAVY SURF AND WIND"           
## [295] "HEAVY SURF COASTAL FLOODING"    "HEAVY SURF/HIGH SURF"          
## [297] "HEAVY SWELLS"                   "HEAVY WET SNOW"                
## [299] "HIGH"                           "HIGH  SWELLS"                  
## [301] "HIGH  WINDS"                    "HIGH SEAS"                     
## [303] "HIGH SURF"                      "HIGH SURF ADVISORIES"          
## [305] "HIGH SURF ADVISORY"             "HIGH SWELLS"                   
## [307] "HIGH TEMPERATURE RECORD"        "HIGH TIDES"                    
## [309] "HIGH WATER"                     "HIGH WAVES"                    
## [311] "HIGH WIND"                      "HIGH WIND (G40)"               
## [313] "HIGH WIND 48"                   "HIGH WIND 63"                  
## [315] "HIGH WIND 70"                   "HIGH WIND AND HEAVY SNOW"      
## [317] "HIGH WIND AND HIGH TIDES"       "HIGH WIND AND SEAS"            
## [319] "HIGH WIND DAMAGE"               "HIGH WIND/ BLIZZARD"           
## [321] "HIGH WIND/BLIZZARD"             "HIGH WIND/BLIZZARD/FREEZING RA"
## [323] "HIGH WIND/HEAVY SNOW"           "HIGH WIND/LOW WIND CHILL"      
## [325] "HIGH WIND/SEAS"                 "HIGH WIND/WIND CHILL"          
## [327] "HIGH WIND/WIND CHILL/BLIZZARD"  "HIGH WINDS"                    
## [329] "HIGH WINDS 55"                  "HIGH WINDS 57"                 
## [331] "HIGH WINDS 58"                  "HIGH WINDS 63"                 
## [333] "HIGH WINDS 66"                  "HIGH WINDS 67"                 
## [335] "HIGH WINDS 73"                  "HIGH WINDS 76"                 
## [337] "HIGH WINDS 80"                  "HIGH WINDS 82"                 
## [339] "HIGH WINDS AND WIND CHILL"      "HIGH WINDS DUST STORM"         
## [341] "HIGH WINDS HEAVY RAINS"         "HIGH WINDS/"                   
## [343] "HIGH WINDS/COASTAL FLOOD"       "HIGH WINDS/COLD"               
## [345] "HIGH WINDS/FLOODING"            "HIGH WINDS/HEAVY RAIN"         
## [347] "HIGH WINDS/SNOW"                "HIGHWAY FLOODING"              
## [349] "HOT AND DRY"                    "HOT PATTERN"                   
## [351] "HOT SPELL"                      "HOT WEATHER"                   
## [353] "HOT/DRY PATTERN"                "HURRICANE"                     
## [355] "HURRICANE-GENERATED SWELLS"     "HURRICANE EDOUARD"             
## [357] "HURRICANE EMILY"                "HURRICANE ERIN"                
## [359] "HURRICANE FELIX"                "HURRICANE GORDON"              
## [361] "HURRICANE OPAL"                 "HURRICANE OPAL/HIGH WINDS"     
## [363] "HURRICANE/TYPHOON"              "HVY RAIN"                      
## [365] "HYPERTHERMIA/EXPOSURE"          "HYPOTHERMIA"                   
## [367] "HYPOTHERMIA/EXPOSURE"           "ICE"                           
## [369] "ICE AND SNOW"                   "ICE FLOES"                     
## [371] "ICE FOG"                        "ICE JAM"                       
## [373] "ICE JAM FLOOD (MINOR"           "ICE JAM FLOODING"              
## [375] "ICE ON ROAD"                    "ICE PELLETS"                   
## [377] "ICE ROADS"                      "ICE STORM"                     
## [379] "ICE STORM AND SNOW"             "ICE STORM/FLASH FLOOD"         
## [381] "ICE/SNOW"                       "ICE/STRONG WINDS"              
## [383] "ICESTORM/BLIZZARD"              "ICY ROADS"                     
## [385] "LACK OF SNOW"                   "LAKE-EFFECT SNOW"              
## [387] "LAKE EFFECT SNOW"               "LAKE FLOOD"                    
## [389] "LAKESHORE FLOOD"                "LANDSLIDE"                     
## [391] "LANDSLIDE/URBAN FLOOD"          "LANDSLIDES"                    
## [393] "LANDSLUMP"                      "LANDSPOUT"                     
## [395] "LARGE WALL CLOUD"               "LATE-SEASON SNOWFALL"          
## [397] "LATE FREEZE"                    "LATE SEASON HAIL"              
## [399] "LATE SEASON SNOW"               "LATE SEASON SNOWFALL"          
## [401] "LATE SNOW"                      "LIGHT FREEZING RAIN"           
## [403] "LIGHT SNOW"                     "LIGHT SNOW AND SLEET"          
## [405] "LIGHT SNOW/FLURRIES"            "LIGHT SNOW/FREEZING PRECIP"    
## [407] "LIGHT SNOWFALL"                 "LIGHTING"                      
## [409] "LIGHTNING"                      "LIGHTNING  WAUSEON"            
## [411] "LIGHTNING AND HEAVY RAIN"       "LIGHTNING AND THUNDERSTORM WIN"
## [413] "LIGHTNING AND WINDS"            "LIGHTNING DAMAGE"              
## [415] "LIGHTNING FIRE"                 "LIGHTNING INJURY"              
## [417] "LIGHTNING THUNDERSTORM WINDS"   "LIGHTNING THUNDERSTORM WINDSS" 
## [419] "LIGHTNING."                     "LIGHTNING/HEAVY RAIN"          
## [421] "LIGNTNING"                      "LOCAL FLASH FLOOD"             
## [423] "LOCAL FLOOD"                    "LOCALLY HEAVY RAIN"            
## [425] "LOW TEMPERATURE"                "LOW TEMPERATURE RECORD"        
## [427] "LOW WIND CHILL"                 "MAJOR FLOOD"                   
## [429] "MARINE ACCIDENT"                "MARINE HAIL"                   
## [431] "MARINE HIGH WIND"               "MARINE MISHAP"                 
## [433] "MARINE STRONG WIND"             "MARINE THUNDERSTORM WIND"      
## [435] "MARINE TSTM WIND"               "METRO STORM, MAY 26"           
## [437] "MICROBURST"                     "MICROBURST WINDS"              
## [439] "MILD AND DRY PATTERN"           "MILD PATTERN"                  
## [441] "MILD/DRY PATTERN"               "MINOR FLOOD"                   
## [443] "MINOR FLOODING"                 "MIXED PRECIP"                  
## [445] "MIXED PRECIPITATION"            "MODERATE SNOW"                 
## [447] "MODERATE SNOWFALL"              "MONTHLY PRECIPITATION"         
## [449] "MONTHLY RAINFALL"               "MONTHLY SNOWFALL"              
## [451] "MONTHLY TEMPERATURE"            "MOUNTAIN SNOWS"                
## [453] "MUD SLIDE"                      "MUD SLIDES"                    
## [455] "MUD SLIDES URBAN FLOODING"      "MUD/ROCK SLIDE"                
## [457] "MUDSLIDE"                       "MUDSLIDE/LANDSLIDE"            
## [459] "MUDSLIDES"                      "NEAR RECORD SNOW"              
## [461] "NO SEVERE WEATHER"              "NON-SEVERE WIND DAMAGE"        
## [463] "NON-TSTM WIND"                  "NON SEVERE HAIL"               
## [465] "NON TSTM WIND"                  "NONE"                          
## [467] "NORMAL PRECIPITATION"           "NORTHERN LIGHTS"               
## [469] "OTHER"                          "PATCHY DENSE FOG"              
## [471] "PATCHY ICE"                     "PROLONG COLD"                  
## [473] "PROLONG COLD/SNOW"              "PROLONG WARMTH"                
## [475] "PROLONGED RAIN"                 "RAIN"                          
## [477] "RAIN (HEAVY)"                   "RAIN AND WIND"                 
## [479] "RAIN DAMAGE"                    "RAIN/SNOW"                     
## [481] "RAIN/WIND"                      "RAINSTORM"                     
## [483] "RAPIDLY RISING WATER"           "RECORD  COLD"                  
## [485] "RECORD COLD"                    "RECORD COLD AND HIGH WIND"     
## [487] "RECORD COLD/FROST"              "RECORD COOL"                   
## [489] "RECORD DRY MONTH"               "RECORD DRYNESS"                
## [491] "RECORD HEAT"                    "RECORD HEAT WAVE"              
## [493] "RECORD HIGH"                    "RECORD HIGH TEMPERATURE"       
## [495] "RECORD HIGH TEMPERATURES"       "RECORD LOW"                    
## [497] "RECORD LOW RAINFALL"            "RECORD MAY SNOW"               
## [499] "RECORD PRECIPITATION"           "RECORD RAINFALL"               
## [501] "RECORD SNOW"                    "RECORD SNOW/COLD"              
## [503] "RECORD SNOWFALL"                "RECORD TEMPERATURE"            
## [505] "RECORD TEMPERATURES"            "RECORD WARM"                   
## [507] "RECORD WARM TEMPS."             "RECORD WARMTH"                 
## [509] "RECORD WINTER SNOW"             "RECORD/EXCESSIVE HEAT"         
## [511] "RECORD/EXCESSIVE RAINFALL"      "RED FLAG CRITERIA"             
## [513] "RED FLAG FIRE WX"               "REMNANTS OF FLOYD"             
## [515] "RIP CURRENT"                    "RIP CURRENTS"                  
## [517] "RIP CURRENTS HEAVY SURF"        "RIP CURRENTS/HEAVY SURF"       
## [519] "RIVER AND STREAM FLOOD"         "RIVER FLOOD"                   
## [521] "RIVER FLOODING"                 "ROCK SLIDE"                    
## [523] "ROGUE WAVE"                     "ROTATING WALL CLOUD"           
## [525] "ROUGH SEAS"                     "ROUGH SURF"                    
## [527] "RURAL FLOOD"                    "SAHARAN DUST"                  
## [529] "SEASONAL SNOWFALL"              "SEICHE"                        
## [531] "SEVERE COLD"                    "SEVERE THUNDERSTORM"           
## [533] "SEVERE THUNDERSTORM WINDS"      "SEVERE THUNDERSTORMS"          
## [535] "SEVERE TURBULENCE"              "SLEET"                         
## [537] "SLEET & FREEZING RAIN"          "SLEET STORM"                   
## [539] "SLEET/FREEZING RAIN"            "SLEET/ICE STORM"               
## [541] "SLEET/RAIN/SNOW"                "SLEET/SNOW"                    
## [543] "SMALL HAIL"                     "SMALL STREAM"                  
## [545] "SMALL STREAM AND"               "SMALL STREAM AND URBAN FLOOD"  
## [547] "SMALL STREAM AND URBAN FLOODIN" "SMALL STREAM FLOOD"            
## [549] "SMALL STREAM FLOODING"          "SMALL STREAM URBAN FLOOD"      
## [551] "SMALL STREAM/URBAN FLOOD"       "SML STREAM FLD"                
## [553] "SMOKE"                          "SNOW"                          
## [555] "SNOW- HIGH WIND- WIND CHILL"    "SNOW ACCUMULATION"             
## [557] "SNOW ADVISORY"                  "SNOW AND COLD"                 
## [559] "SNOW AND HEAVY SNOW"            "SNOW AND ICE"                  
## [561] "SNOW AND ICE STORM"             "SNOW AND SLEET"                
## [563] "SNOW AND WIND"                  "SNOW DROUGHT"                  
## [565] "SNOW FREEZING RAIN"             "SNOW SHOWERS"                  
## [567] "SNOW SLEET"                     "SNOW SQUALL"                   
## [569] "SNOW SQUALLS"                   "SNOW/ BITTER COLD"             
## [571] "SNOW/ ICE"                      "SNOW/BLOWING SNOW"             
## [573] "SNOW/COLD"                      "SNOW/FREEZING RAIN"            
## [575] "SNOW/HEAVY SNOW"                "SNOW/HIGH WINDS"               
## [577] "SNOW/ICE"                       "SNOW/ICE STORM"                
## [579] "SNOW/RAIN"                      "SNOW/RAIN/SLEET"               
## [581] "SNOW/SLEET"                     "SNOW/SLEET/FREEZING RAIN"      
## [583] "SNOW/SLEET/RAIN"                "SNOW\\COLD"                    
## [585] "SNOWFALL RECORD"                "SNOWMELT FLOODING"             
## [587] "SNOWSTORM"                      "SOUTHEAST"                     
## [589] "STORM FORCE WINDS"              "STORM SURGE"                   
## [591] "STORM SURGE/TIDE"               "STREAM FLOODING"               
## [593] "STREET FLOOD"                   "STREET FLOODING"               
## [595] "STRONG WIND"                    "STRONG WIND GUST"              
## [597] "STRONG WINDS"                   "SUMMARY AUGUST 10"             
## [599] "SUMMARY AUGUST 11"              "SUMMARY AUGUST 17"             
## [601] "SUMMARY AUGUST 2-3"             "SUMMARY AUGUST 21"             
## [603] "SUMMARY AUGUST 28"              "SUMMARY AUGUST 4"              
## [605] "SUMMARY AUGUST 7"               "SUMMARY AUGUST 9"              
## [607] "SUMMARY JAN 17"                 "SUMMARY JULY 23-24"            
## [609] "SUMMARY JUNE 18-19"             "SUMMARY JUNE 5-6"              
## [611] "SUMMARY JUNE 6"                 "SUMMARY OF APRIL 12"           
## [613] "SUMMARY OF APRIL 13"            "SUMMARY OF APRIL 21"           
## [615] "SUMMARY OF APRIL 27"            "SUMMARY OF APRIL 3RD"          
## [617] "SUMMARY OF AUGUST 1"            "SUMMARY OF JULY 11"            
## [619] "SUMMARY OF JULY 2"              "SUMMARY OF JULY 22"            
## [621] "SUMMARY OF JULY 26"             "SUMMARY OF JULY 29"            
## [623] "SUMMARY OF JULY 3"              "SUMMARY OF JUNE 10"            
## [625] "SUMMARY OF JUNE 11"             "SUMMARY OF JUNE 12"            
## [627] "SUMMARY OF JUNE 13"             "SUMMARY OF JUNE 15"            
## [629] "SUMMARY OF JUNE 16"             "SUMMARY OF JUNE 18"            
## [631] "SUMMARY OF JUNE 23"             "SUMMARY OF JUNE 24"            
## [633] "SUMMARY OF JUNE 3"              "SUMMARY OF JUNE 30"            
## [635] "SUMMARY OF JUNE 4"              "SUMMARY OF JUNE 6"             
## [637] "SUMMARY OF MARCH 14"            "SUMMARY OF MARCH 23"           
## [639] "SUMMARY OF MARCH 24"            "SUMMARY OF MARCH 24-25"        
## [641] "SUMMARY OF MARCH 27"            "SUMMARY OF MARCH 29"           
## [643] "SUMMARY OF MAY 10"              "SUMMARY OF MAY 13"             
## [645] "SUMMARY OF MAY 14"              "SUMMARY OF MAY 22"             
## [647] "SUMMARY OF MAY 22 AM"           "SUMMARY OF MAY 22 PM"          
## [649] "SUMMARY OF MAY 26 AM"           "SUMMARY OF MAY 26 PM"          
## [651] "SUMMARY OF MAY 31 AM"           "SUMMARY OF MAY 31 PM"          
## [653] "SUMMARY OF MAY 9-10"            "SUMMARY SEPT. 25-26"           
## [655] "SUMMARY SEPTEMBER 20"           "SUMMARY SEPTEMBER 23"          
## [657] "SUMMARY SEPTEMBER 3"            "SUMMARY SEPTEMBER 4"           
## [659] "SUMMARY: NOV. 16"               "SUMMARY: NOV. 6-7"             
## [661] "SUMMARY: OCT. 20-21"            "SUMMARY: OCTOBER 31"           
## [663] "SUMMARY: SEPT. 18"              "TEMPERATURE RECORD"            
## [665] "THUDERSTORM WINDS"              "THUNDEERSTORM WINDS"           
## [667] "THUNDERESTORM WINDS"            "THUNDERSNOW"                   
## [669] "THUNDERSNOW SHOWER"             "THUNDERSTORM"                  
## [671] "THUNDERSTORM  WINDS"            "THUNDERSTORM DAMAGE"           
## [673] "THUNDERSTORM DAMAGE TO"         "THUNDERSTORM HAIL"             
## [675] "THUNDERSTORM W INDS"            "THUNDERSTORM WIND"             
## [677] "THUNDERSTORM WIND (G40)"        "THUNDERSTORM WIND 50"          
## [679] "THUNDERSTORM WIND 52"           "THUNDERSTORM WIND 56"          
## [681] "THUNDERSTORM WIND 59"           "THUNDERSTORM WIND 59 MPH"      
## [683] "THUNDERSTORM WIND 59 MPH."      "THUNDERSTORM WIND 60 MPH"      
## [685] "THUNDERSTORM WIND 65 MPH"       "THUNDERSTORM WIND 65MPH"       
## [687] "THUNDERSTORM WIND 69"           "THUNDERSTORM WIND 98 MPH"      
## [689] "THUNDERSTORM WIND G50"          "THUNDERSTORM WIND G51"         
## [691] "THUNDERSTORM WIND G52"          "THUNDERSTORM WIND G55"         
## [693] "THUNDERSTORM WIND G60"          "THUNDERSTORM WIND G61"         
## [695] "THUNDERSTORM WIND TREES"        "THUNDERSTORM WIND."            
## [697] "THUNDERSTORM WIND/ TREE"        "THUNDERSTORM WIND/ TREES"      
## [699] "THUNDERSTORM WIND/AWNING"       "THUNDERSTORM WIND/HAIL"        
## [701] "THUNDERSTORM WIND/LIGHTNING"    "THUNDERSTORM WINDS"            
## [703] "THUNDERSTORM WINDS      LE CEN" "THUNDERSTORM WINDS 13"         
## [705] "THUNDERSTORM WINDS 2"           "THUNDERSTORM WINDS 50"         
## [707] "THUNDERSTORM WINDS 52"          "THUNDERSTORM WINDS 53"         
## [709] "THUNDERSTORM WINDS 60"          "THUNDERSTORM WINDS 61"         
## [711] "THUNDERSTORM WINDS 62"          "THUNDERSTORM WINDS 63 MPH"     
## [713] "THUNDERSTORM WINDS AND"         "THUNDERSTORM WINDS FUNNEL CLOU"
## [715] "THUNDERSTORM WINDS G"           "THUNDERSTORM WINDS G60"        
## [717] "THUNDERSTORM WINDS HAIL"        "THUNDERSTORM WINDS HEAVY RAIN" 
## [719] "THUNDERSTORM WINDS LIGHTNING"   "THUNDERSTORM WINDS SMALL STREA"
## [721] "THUNDERSTORM WINDS URBAN FLOOD" "THUNDERSTORM WINDS."           
## [723] "THUNDERSTORM WINDS/ FLOOD"      "THUNDERSTORM WINDS/ HAIL"      
## [725] "THUNDERSTORM WINDS/FLASH FLOOD" "THUNDERSTORM WINDS/FLOODING"   
## [727] "THUNDERSTORM WINDS/FUNNEL CLOU" "THUNDERSTORM WINDS/HAIL"       
## [729] "THUNDERSTORM WINDS/HEAVY RAIN"  "THUNDERSTORM WINDS53"          
## [731] "THUNDERSTORM WINDSHAIL"         "THUNDERSTORM WINDSS"           
## [733] "THUNDERSTORM WINS"              "THUNDERSTORMS"                 
## [735] "THUNDERSTORMS WIND"             "THUNDERSTORMS WINDS"           
## [737] "THUNDERSTORMW"                  "THUNDERSTORMW 50"              
## [739] "THUNDERSTORMW WINDS"            "THUNDERSTORMWINDS"             
## [741] "THUNDERSTROM WIND"              "THUNDERSTROM WINDS"            
## [743] "THUNDERTORM WINDS"              "THUNDERTSORM WIND"             
## [745] "THUNDESTORM WINDS"              "THUNERSTORM WINDS"             
## [747] "TIDAL FLOOD"                    "TIDAL FLOODING"                
## [749] "TORNADO"                        "TORNADO DEBRIS"                
## [751] "TORNADO F0"                     "TORNADO F1"                    
## [753] "TORNADO F2"                     "TORNADO F3"                    
## [755] "TORNADO/WATERSPOUT"             "TORNADOES"                     
## [757] "TORNADOES, TSTM WIND, HAIL"     "TORNADOS"                      
## [759] "TORNDAO"                        "TORRENTIAL RAIN"               
## [761] "TORRENTIAL RAINFALL"            "TROPICAL DEPRESSION"           
## [763] "TROPICAL STORM"                 "TROPICAL STORM ALBERTO"        
## [765] "TROPICAL STORM DEAN"            "TROPICAL STORM GORDON"         
## [767] "TROPICAL STORM JERRY"           "TSTM"                          
## [769] "TSTM HEAVY RAIN"                "TSTM WIND"                     
## [771] "TSTM WIND  (G45)"               "TSTM WIND (41)"                
## [773] "TSTM WIND (G35)"                "TSTM WIND (G40)"               
## [775] "TSTM WIND (G45)"                "TSTM WIND 40"                  
## [777] "TSTM WIND 45"                   "TSTM WIND 50"                  
## [779] "TSTM WIND 51"                   "TSTM WIND 52"                  
## [781] "TSTM WIND 55"                   "TSTM WIND 65)"                 
## [783] "TSTM WIND AND LIGHTNING"        "TSTM WIND DAMAGE"              
## [785] "TSTM WIND G45"                  "TSTM WIND G58"                 
## [787] "TSTM WIND/HAIL"                 "TSTM WINDS"                    
## [789] "TSTM WND"                       "TSTMW"                         
## [791] "TSUNAMI"                        "TUNDERSTORM WIND"              
## [793] "TYPHOON"                        "UNSEASONABLE COLD"             
## [795] "UNSEASONABLY COLD"              "UNSEASONABLY COOL"             
## [797] "UNSEASONABLY COOL & WET"        "UNSEASONABLY DRY"              
## [799] "UNSEASONABLY HOT"               "UNSEASONABLY WARM"             
## [801] "UNSEASONABLY WARM & WET"        "UNSEASONABLY WARM AND DRY"     
## [803] "UNSEASONABLY WARM YEAR"         "UNSEASONABLY WARM/WET"         
## [805] "UNSEASONABLY WET"               "UNSEASONAL LOW TEMP"           
## [807] "UNSEASONAL RAIN"                "UNUSUAL WARMTH"                
## [809] "UNUSUAL/RECORD WARMTH"          "UNUSUALLY COLD"                
## [811] "UNUSUALLY LATE SNOW"            "UNUSUALLY WARM"                
## [813] "URBAN AND SMALL"                "URBAN AND SMALL STREAM"        
## [815] "URBAN AND SMALL STREAM FLOOD"   "URBAN AND SMALL STREAM FLOODIN"
## [817] "URBAN FLOOD"                    "URBAN FLOOD LANDSLIDE"         
## [819] "URBAN FLOODING"                 "URBAN FLOODS"                  
## [821] "URBAN SMALL"                    "URBAN SMALL STREAM FLOOD"      
## [823] "URBAN/SMALL"                    "URBAN/SMALL FLOODING"          
## [825] "URBAN/SMALL STREAM"             "URBAN/SMALL STREAM  FLOOD"     
## [827] "URBAN/SMALL STREAM FLOOD"       "URBAN/SMALL STREAM FLOODING"   
## [829] "URBAN/SMALL STRM FLDG"          "URBAN/SML STREAM FLD"          
## [831] "URBAN/SML STREAM FLDG"          "URBAN/STREET FLOODING"         
## [833] "VERY DRY"                       "VERY WARM"                     
## [835] "VOG"                            "VOLCANIC ASH"                  
## [837] "VOLCANIC ASH PLUME"             "VOLCANIC ASHFALL"              
## [839] "VOLCANIC ERUPTION"              "WAKE LOW WIND"                 
## [841] "WALL CLOUD"                     "WALL CLOUD/FUNNEL CLOUD"       
## [843] "WARM DRY CONDITIONS"            "WARM WEATHER"                  
## [845] "WATER SPOUT"                    "WATERSPOUT"                    
## [847] "WATERSPOUT-"                    "WATERSPOUT-TORNADO"            
## [849] "WATERSPOUT FUNNEL CLOUD"        "WATERSPOUT TORNADO"            
## [851] "WATERSPOUT/"                    "WATERSPOUT/ TORNADO"           
## [853] "WATERSPOUT/TORNADO"             "WATERSPOUTS"                   
## [855] "WAYTERSPOUT"                    "WET MICOBURST"                 
## [857] "WET MICROBURST"                 "WET MONTH"                     
## [859] "WET SNOW"                       "WET WEATHER"                   
## [861] "WET YEAR"                       "WHIRLWIND"                     
## [863] "WILD FIRES"                     "WILD/FOREST FIRE"              
## [865] "WILD/FOREST FIRES"              "WILDFIRE"                      
## [867] "WILDFIRES"                      "WIND"                          
## [869] "WIND ADVISORY"                  "WIND AND WAVE"                 
## [871] "WIND CHILL"                     "WIND CHILL/HIGH WIND"          
## [873] "WIND DAMAGE"                    "WIND GUSTS"                    
## [875] "WIND STORM"                     "WIND/HAIL"                     
## [877] "WINDS"                          "WINTER MIX"                    
## [879] "WINTER STORM"                   "WINTER STORM HIGH WINDS"       
## [881] "WINTER STORM/HIGH WIND"         "WINTER STORM/HIGH WINDS"       
## [883] "WINTER STORMS"                  "WINTER WEATHER"                
## [885] "WINTER WEATHER MIX"             "WINTER WEATHER/MIX"            
## [887] "WINTERY MIX"                    "WINTRY MIX"                    
## [889] "WND"
```

Filtering the accepted values. Here we print the quantity, unique and excluded values

```r
allowEvents<-c("Astronomical Low Tide", "Avalanche", "Blizzard", "Coastal Flood", "Cold/Wind Chill",
               "Debris Flow", "Dense Fog", "Dense Smoke", "Drought", "Dust Devil", "Dust Storm",
               "Excessive Heat", "Extreme Cold/Wind Chill", "Flash Flood", "Flood", "Frost/Freeze",
               "Funnel Cloud", "Freezing Fog", "Hail", "Heat", "Heavy Rain", "Heavy Snow", "High Surf",
               "High Wind", "Hurricane (Typhoon)", "Ice Storm", "Lake-Effect Snow", "Lakeshore Flood",
               "Lightning", "Marine Hail", "Marine High Wind", "Marine Strong Wind", "Marine Thunderstorm Wind",
               "Rip Current", "Seiche", "Sleet", "Storm Surge/Tide", "Strong Wind", "Thunderstorm Wind",
               "Tornado", "Tropical Depression", "Tropical Storm", "Tsunami", "Volcanic Ash", "Waterspout",
               "Wildfire", "Winter Storm", "Winter Weather")

allowEvents<-toupper(allowEvents)
print(length(unique(allowEvents)))
```

```
## [1] 48
```

```r
allowValues<-stormData$EVTYPE[stormData$EVTYPE %in% allowEvents]

print(length(unique(allowValues)))
```

```
## [1] 46
```

```r
print(unique(allowValues))
```

```
##  [1] "TORNADO"                  "HAIL"                    
##  [3] "WINTER STORM"             "HEAVY RAIN"              
##  [5] "LIGHTNING"                "THUNDERSTORM WIND"       
##  [7] "DENSE FOG"                "RIP CURRENT"             
##  [9] "FLASH FLOOD"              "FUNNEL CLOUD"            
## [11] "HEAT"                     "FLOOD"                   
## [13] "WATERSPOUT"               "HIGH WIND"               
## [15] "BLIZZARD"                 "HEAVY SNOW"              
## [17] "COASTAL FLOOD"            "ICE STORM"               
## [19] "AVALANCHE"                "DUST STORM"              
## [21] "SLEET"                    "DUST DEVIL"              
## [23] "EXCESSIVE HEAT"           "HIGH SURF"               
## [25] "STRONG WIND"              "WINTER WEATHER"          
## [27] "DROUGHT"                  "TROPICAL STORM"          
## [29] "WILDFIRE"                 "LAKE-EFFECT SNOW"        
## [31] "FREEZING FOG"             "VOLCANIC ASH"            
## [33] "FROST/FREEZE"             "SEICHE"                  
## [35] "TROPICAL DEPRESSION"      "EXTREME COLD/WIND CHILL" 
## [37] "MARINE HAIL"              "STORM SURGE/TIDE"        
## [39] "COLD/WIND CHILL"          "MARINE HIGH WIND"        
## [41] "TSUNAMI"                  "DENSE SMOKE"             
## [43] "LAKESHORE FLOOD"          "MARINE THUNDERSTORM WIND"
## [45] "MARINE STRONG WIND"       "ASTRONOMICAL LOW TIDE"
```

```r
print(allowEvents[ ! (allowEvents %in% allowValues) ])
```

```
## [1] "DEBRIS FLOW"         "HURRICANE (TYPHOON)"
```

As can be seen, some of the values are not acceptable (view the instruction service document from NOOA, pg 17, cap 2.1). So, here the acceptable values will be set, and the unacceptable values will be coerced to NA value.


```r
stormData$EVTYPE2<-factor(stormData$EVTYPE, labels=allowEvents, levels=allowEvents)

print(length(which(is.na(stormData$EVTYPE2))))
```

```
## [1] 266942
```

```r
table(stormData$EVTYPE)
```

```
## 
##                ABNORMAL WARMTH                 ABNORMALLY DRY 
##                              4                              2 
##                 ABNORMALLY WET           ACCUMULATED SNOWFALL 
##                              1                              4 
##            AGRICULTURAL FREEZE                  APACHE COUNTY 
##                              6                              1 
##         ASTRONOMICAL HIGH TIDE          ASTRONOMICAL LOW TIDE 
##                            103                            174 
##                       AVALANCE                      AVALANCHE 
##                              1                            386 
##                   BEACH EROSIN                  BEACH EROSION 
##                              1                              4 
##    BEACH EROSION/COASTAL FLOOD                    BEACH FLOOD 
##                              1                              2 
##     BELOW NORMAL PRECIPITATION              BITTER WIND CHILL 
##                              2                              1 
## BITTER WIND CHILL TEMPERATURES                      BLACK ICE 
##                              3                             17 
##                       BLIZZARD BLIZZARD AND EXTREME WIND CHIL 
##                           2719                              2 
##        BLIZZARD AND HEAVY SNOW               BLIZZARD SUMMARY 
##                              1                              1 
##               BLIZZARD WEATHER         BLIZZARD/FREEZING RAIN 
##                              1                              1 
##            BLIZZARD/HEAVY SNOW             BLIZZARD/HIGH WIND 
##                              2                              1 
##          BLIZZARD/WINTER STORM                  BLOW-OUT TIDE 
##                              1                              1 
##                 BLOW-OUT TIDES                   BLOWING DUST 
##                              1                              4 
##                   BLOWING SNOW BLOWING SNOW- EXTREME WIND CHI 
##                             17                              1 
## BLOWING SNOW & EXTREME WIND CH BLOWING SNOW/EXTREME WIND CHIL 
##                              2                              1 
##               BREAKUP FLOODING                     BRUSH FIRE 
##                              1                              3 
##                    BRUSH FIRES      COASTAL  FLOODING/EROSION 
##                              1                              1 
##                COASTAL EROSION                  COASTAL FLOOD 
##                              1                            657 
##               COASTAL FLOODING       COASTAL FLOODING/EROSION 
##                            183                              5 
##                  COASTAL STORM                  COASTAL SURGE 
##                             10                              2 
##            COASTAL/TIDAL FLOOD                   COASTALFLOOD 
##                              2                              1 
##                   COASTALSTORM                           COLD 
##                              1                             82 
##                COLD AIR FUNNEL               COLD AIR FUNNELS 
##                              4                              2 
##               COLD AIR TORNADO                 COLD AND FROST 
##                              1                              7 
##                  COLD AND SNOW        COLD AND WET CONDITIONS 
##                              1                              1 
##               COLD TEMPERATURE              COLD TEMPERATURES 
##                              2                              4 
##                      COLD WAVE                   COLD WEATHER 
##                              3                              4 
##   COLD WIND CHILL TEMPERATURES                COLD/WIND CHILL 
##                              6                            539 
##                     COLD/WINDS                   COOL AND WET 
##                              1                              1 
##                     COOL SPELL          CSTL FLOODING/EROSION 
##                              1                              2 
##                      DAM BREAK                    DAM FAILURE 
##                              4                              1 
##                DAMAGING FREEZE                      DEEP HAIL 
##                              8                              1 
##                      DENSE FOG                    DENSE SMOKE 
##                           1293                             10 
##                      DOWNBURST                DOWNBURST WINDS 
##                              2                              2 
##                   DRIEST MONTH                  DRIFTING SNOW 
##                              1                              1 
##                        DROUGHT         DROUGHT/EXCESSIVE HEAT 
##                           2488                             13 
##                       DROWNING                            DRY 
##                              1                              9 
##                 DRY CONDITIONS                DRY HOT WEATHER 
##                              6                              1 
##                 DRY MICROBURST              DRY MICROBURST 50 
##                            186                              1 
##              DRY MICROBURST 53              DRY MICROBURST 58 
##                              1                              2 
##              DRY MICROBURST 61              DRY MICROBURST 84 
##                              1                              1 
##           DRY MICROBURST WINDS           DRY MIRCOBURST WINDS 
##                              5                              1 
##                    DRY PATTERN                      DRY SPELL 
##                              1                              4 
##                    DRY WEATHER                        DRYNESS 
##                              4                              1 
##                     DUST DEVEL                     DUST DEVIL 
##                              1                            149 
##          DUST DEVIL WATERSPOUT                     DUST STORM 
##                              1                            427 
##          DUST STORM/HIGH WINDS                      DUSTSTORM 
##                              1                              1 
##                   EARLY FREEZE                    EARLY FROST 
##                              1                              2 
##                     EARLY RAIN                     EARLY SNOW 
##                              1                              3 
##                 EARLY SNOWFALL             EROSION/CSTL FLOOD 
##                              7                              2 
##                      EXCESSIVE                 EXCESSIVE COLD 
##                              1                              2 
##                 EXCESSIVE HEAT         EXCESSIVE HEAT/DROUGHT 
##                           1678                              1 
##        EXCESSIVE PRECIPITATION                 EXCESSIVE RAIN 
##                              1                              5 
##             EXCESSIVE RAINFALL                 EXCESSIVE SNOW 
##                              4                             25 
##              EXCESSIVE WETNESS                EXCESSIVELY DRY 
##                              1                              1 
##                  EXTENDED COLD                   EXTREME COLD 
##                              1                            657 
##        EXTREME COLD/WIND CHILL                   EXTREME HEAT 
##                           1002                             22 
##             EXTREME WIND CHILL EXTREME WIND CHILL/BLOWING SNO 
##                              6                              1 
##            EXTREME WIND CHILLS              EXTREME WINDCHILL 
##                              1                            204 
## EXTREME WINDCHILL TEMPERATURES            EXTREME/RECORD COLD 
##                             19                              4 
##                  EXTREMELY WET               FALLING SNOW/ICE 
##                              1                              2 
##                    FIRST FROST                     FIRST SNOW 
##                              1                              9 
##                    FLASH FLOOD       FLASH FLOOD - HEAVY RAIN 
##                          54278                              2 
##      FLASH FLOOD FROM ICE JAMS         FLASH FLOOD LANDSLIDES 
##                              5                              1 
##              FLASH FLOOD WINDS                   FLASH FLOOD/ 
##                              1                              1 
##             FLASH FLOOD/ FLOOD            FLASH FLOOD/ STREET 
##                              2                              1 
##              FLASH FLOOD/FLOOD         FLASH FLOOD/HEAVY RAIN 
##                             22                              1 
##          FLASH FLOOD/LANDSLIDE                 FLASH FLOODING 
##                              1                            682 
##           FLASH FLOODING/FLOOD FLASH FLOODING/THUNDERSTORM WI 
##                              8                              1 
##                   FLASH FLOODS                FLASH FLOOODING 
##                             32                              1 
##                          FLOOD             FLOOD & HEAVY RAIN 
##                          25327                              2 
##                    FLOOD FLASH              FLOOD FLOOD/FLASH 
##                              3                              1 
##                   FLOOD WATCH/                    FLOOD/FLASH 
##                              1                              2 
##              FLOOD/FLASH FLOOD           FLOOD/FLASH FLOODING 
##                            625                              2 
##              FLOOD/FLASH/FLOOD               FLOOD/FLASHFLOOD 
##                              1                              1 
##                FLOOD/RAIN/WIND               FLOOD/RAIN/WINDS 
##                              1                              6 
##              FLOOD/RIVER FLOOD              FLOOD/STRONG WIND 
##                              1                              1 
##                       FLOODING            FLOODING/HEAVY RAIN 
##                            120                              1 
##                         FLOODS                            FOG 
##                              3                            538 
##      FOG AND COLD TEMPERATURES                   FOREST FIRES 
##                              1                              1 
##                         FREEZE               FREEZING DRIZZLE 
##                             76                             24 
##  FREEZING DRIZZLE AND FREEZING                   FREEZING FOG 
##                              1                             46 
##                  FREEZING RAIN        FREEZING RAIN AND SLEET 
##                            260                              6 
##         FREEZING RAIN AND SNOW        FREEZING RAIN SLEET AND 
##                              1                              1 
##  FREEZING RAIN SLEET AND LIGHT            FREEZING RAIN/SLEET 
##                              1                              9 
##             FREEZING RAIN/SNOW                 FREEZING SPRAY 
##                              4                              1 
##                          FROST                   FROST/FREEZE 
##                             57                           1343 
##                  FROST\\FREEZE                         FUNNEL 
##                              1                             46 
##                   FUNNEL CLOUD                  FUNNEL CLOUD. 
##                           6844                              1 
##              FUNNEL CLOUD/HAIL                  FUNNEL CLOUDS 
##                              1                             87 
##                        FUNNELS                          GLAZE 
##                              1                             43 
##                      GLAZE ICE                GLAZE/ICE STORM 
##                              2                              1 
##                  GRADIENT WIND                 GRADIENT WINDS 
##                              9                              8 
##                    GRASS FIRES                GROUND BLIZZARD 
##                              1                              2 
##                       GUSTNADO                   GUSTNADO AND 
##                              6                              1 
##                GUSTY LAKE WIND        GUSTY THUNDERSTORM WIND 
##                              1                              3 
##       GUSTY THUNDERSTORM WINDS                     GUSTY WIND 
##                              5                             24 
##                GUSTY WIND/HAIL            GUSTY WIND/HVY RAIN 
##                              1                              1 
##                GUSTY WIND/RAIN                    GUSTY WINDS 
##                              1                             65 
##                           HAIL                      HAIL 0.75 
##                         288661                             18 
##                      HAIL 0.88                       HAIL 075 
##                              1                              1 
##                       HAIL 088                      HAIL 1.00 
##                              1                              6 
##                      HAIL 1.75                     HAIL 1.75) 
##                              4                              1 
##                       HAIL 100                       HAIL 125 
##                             13                              1 
##                       HAIL 150                       HAIL 175 
##                              2                             13 
##                       HAIL 200                       HAIL 225 
##                              1                              1 
##                       HAIL 275                       HAIL 450 
##                              3                              1 
##                        HAIL 75                        HAIL 80 
##                             29                              2 
##                        HAIL 88                     HAIL ALOFT 
##                              1                              1 
##                    HAIL DAMAGE                  HAIL FLOODING 
##                              2                              1 
##                     HAIL STORM                     HAIL(0.75) 
##                              1                              1 
##                 HAIL/ICY ROADS                      HAIL/WIND 
##                              1                              3 
##                     HAIL/WINDS                      HAILSTORM 
##                              2                              3 
##                     HAILSTORMS                    HARD FREEZE 
##                              1                              7 
##                 HAZARDOUS SURF                           HEAT 
##                              1                            767 
##                   HEAT DROUGHT                      HEAT WAVE 
##                              1                             75 
##              HEAT WAVE DROUGHT                     HEAT WAVES 
##                              1                              2 
##                   HEAT/DROUGHT                      HEATBURST 
##                              1                              1 
##                HEAVY LAKE SNOW                      HEAVY MIX 
##                             25                              8 
##            HEAVY PRECIPATATION            HEAVY PRECIPITATION 
##                              1                              3 
##                     HEAVY RAIN           HEAVY RAIN AND FLOOD 
##                          11742                              1 
##            HEAVY RAIN AND WIND             HEAVY RAIN EFFECTS 
##                              4                              1 
##            HEAVY RAIN/FLOODING           HEAVY RAIN/HIGH SURF 
##                              2                              1 
##           HEAVY RAIN/LIGHTNING     HEAVY RAIN/MUDSLIDES/FLOOD 
##                              1                              1 
##      HEAVY RAIN/SEVERE WEATHER  HEAVY RAIN/SMALL STREAM URBAN 
##                              2                              1 
##                HEAVY RAIN/SNOW         HEAVY RAIN/URBAN FLOOD 
##                              1                              1 
##                HEAVY RAIN/WIND HEAVY RAIN; URBAN FLOOD WINDS; 
##                              4                              1 
##                 HEAVY RAINFALL                    HEAVY RAINS 
##                              3                             26 
##           HEAVY RAINS/FLOODING                     HEAVY SEAS 
##                              9                              2 
##                   HEAVY SHOWER                  HEAVY SHOWERS 
##                              2                              1 
##                     HEAVY SNOW             HEAVY SNOW-SQUALLS 
##                          15708                             15 
##     HEAVY SNOW   FREEZING RAIN               HEAVY SNOW & ICE 
##                              1                              1 
##                 HEAVY SNOW AND      HEAVY SNOW AND HIGH WINDS 
##                              1                              2 
##             HEAVY SNOW AND ICE       HEAVY SNOW AND ICE STORM 
##                              2                              2 
##    HEAVY SNOW AND STRONG WINDS     HEAVY SNOW ANDBLOWING SNOW 
##                              1                              1 
##              HEAVY SNOW SHOWER             HEAVY SNOW SQUALLS 
##                              1                             32 
##            HEAVY SNOW/BLIZZARD  HEAVY SNOW/BLIZZARD/AVALANCHE 
##                              3                              1 
##        HEAVY SNOW/BLOWING SNOW       HEAVY SNOW/FREEZING RAIN 
##                              1                              2 
##                HEAVY SNOW/HIGH           HEAVY SNOW/HIGH WIND 
##                              1                              1 
##          HEAVY SNOW/HIGH WINDS  HEAVY SNOW/HIGH WINDS & FLOOD 
##                              1                              1 
## HEAVY SNOW/HIGH WINDS/FREEZING                 HEAVY SNOW/ICE 
##                              1                              5 
##           HEAVY SNOW/ICE STORM               HEAVY SNOW/SLEET 
##                              2                              1 
##             HEAVY SNOW/SQUALLS                HEAVY SNOW/WIND 
##                              2                              1 
##        HEAVY SNOW/WINTER STORM                 HEAVY SNOWPACK 
##                              1                              1 
##                     HEAVY SURF            HEAVY SURF AND WIND 
##                             87                              1 
##    HEAVY SURF COASTAL FLOODING           HEAVY SURF/HIGH SURF 
##                              1                            228 
##                   HEAVY SWELLS                 HEAVY WET SNOW 
##                              1                              1 
##                           HIGH                   HIGH  SWELLS 
##                              1                              1 
##                    HIGH  WINDS                      HIGH SEAS 
##                              1                              8 
##                      HIGH SURF           HIGH SURF ADVISORIES 
##                            734                              1 
##             HIGH SURF ADVISORY                    HIGH SWELLS 
##                              5                              5 
##        HIGH TEMPERATURE RECORD                     HIGH TIDES 
##                              3                              2 
##                     HIGH WATER                     HIGH WAVES 
##                              6                              3 
##                      HIGH WIND                HIGH WIND (G40) 
##                          20214                              2 
##                   HIGH WIND 48                   HIGH WIND 63 
##                              1                              1 
##                   HIGH WIND 70       HIGH WIND AND HEAVY SNOW 
##                              1                              1 
##       HIGH WIND AND HIGH TIDES             HIGH WIND AND SEAS 
##                              2                              1 
##               HIGH WIND DAMAGE            HIGH WIND/ BLIZZARD 
##                              2                              1 
##             HIGH WIND/BLIZZARD HIGH WIND/BLIZZARD/FREEZING RA 
##                              6                              1 
##           HIGH WIND/HEAVY SNOW       HIGH WIND/LOW WIND CHILL 
##                              3                              1 
##                 HIGH WIND/SEAS           HIGH WIND/WIND CHILL 
##                              1                              1 
##  HIGH WIND/WIND CHILL/BLIZZARD                     HIGH WINDS 
##                              1                           1533 
##                  HIGH WINDS 55                  HIGH WINDS 57 
##                              1                              1 
##                  HIGH WINDS 58                  HIGH WINDS 63 
##                              1                              2 
##                  HIGH WINDS 66                  HIGH WINDS 67 
##                              2                              1 
##                  HIGH WINDS 73                  HIGH WINDS 76 
##                              1                              1 
##                  HIGH WINDS 80                  HIGH WINDS 82 
##                              2                              1 
##      HIGH WINDS AND WIND CHILL          HIGH WINDS DUST STORM 
##                              1                              1 
##         HIGH WINDS HEAVY RAINS                    HIGH WINDS/ 
##                              1                              1 
##       HIGH WINDS/COASTAL FLOOD                HIGH WINDS/COLD 
##                              1                              5 
##            HIGH WINDS/FLOODING          HIGH WINDS/HEAVY RAIN 
##                              1                              1 
##                HIGH WINDS/SNOW               HIGHWAY FLOODING 
##                              3                              1 
##                    HOT AND DRY                    HOT PATTERN 
##                              2                              1 
##                      HOT SPELL                    HOT WEATHER 
##                              2                              1 
##                HOT/DRY PATTERN                      HURRICANE 
##                              1                            174 
##     HURRICANE-GENERATED SWELLS              HURRICANE EDOUARD 
##                              3                              2 
##                HURRICANE EMILY                 HURRICANE ERIN 
##                              1                              7 
##                HURRICANE FELIX               HURRICANE GORDON 
##                              2                              1 
##                 HURRICANE OPAL      HURRICANE OPAL/HIGH WINDS 
##                              9                              1 
##              HURRICANE/TYPHOON                       HVY RAIN 
##                             88                              2 
##          HYPERTHERMIA/EXPOSURE                    HYPOTHERMIA 
##                              1                              1 
##           HYPOTHERMIA/EXPOSURE                            ICE 
##                              6                             61 
##                   ICE AND SNOW                      ICE FLOES 
##                              1                              2 
##                        ICE FOG                        ICE JAM 
##                              2                              4 
##           ICE JAM FLOOD (MINOR               ICE JAM FLOODING 
##                              1                              5 
##                    ICE ON ROAD                    ICE PELLETS 
##                              1                              1 
##                      ICE ROADS                      ICE STORM 
##                              1                           2006 
##             ICE STORM AND SNOW          ICE STORM/FLASH FLOOD 
##                              1                              1 
##                       ICE/SNOW               ICE/STRONG WINDS 
##                              5                              1 
##              ICESTORM/BLIZZARD                      ICY ROADS 
##                              1                             32 
##                   LACK OF SNOW               LAKE-EFFECT SNOW 
##                              1                            636 
##               LAKE EFFECT SNOW                     LAKE FLOOD 
##                             23                              1 
##                LAKESHORE FLOOD                      LANDSLIDE 
##                             23                            600 
##          LANDSLIDE/URBAN FLOOD                     LANDSLIDES 
##                              1                              8 
##                      LANDSLUMP                      LANDSPOUT 
##                              2                              2 
##               LARGE WALL CLOUD           LATE-SEASON SNOWFALL 
##                              1                              1 
##                    LATE FREEZE               LATE SEASON HAIL 
##                              1                              1 
##               LATE SEASON SNOW           LATE SEASON SNOWFALL 
##                              1                              2 
##                      LATE SNOW            LIGHT FREEZING RAIN 
##                              2                             23 
##                     LIGHT SNOW           LIGHT SNOW AND SLEET 
##                            176                              2 
##            LIGHT SNOW/FLURRIES     LIGHT SNOW/FREEZING PRECIP 
##                              3                              1 
##                 LIGHT SNOWFALL                       LIGHTING 
##                              1                              3 
##                      LIGHTNING             LIGHTNING  WAUSEON 
##                          15755                              1 
##       LIGHTNING AND HEAVY RAIN LIGHTNING AND THUNDERSTORM WIN 
##                              1                              1 
##            LIGHTNING AND WINDS               LIGHTNING DAMAGE 
##                              1                              1 
##                 LIGHTNING FIRE               LIGHTNING INJURY 
##                              1                              1 
##   LIGHTNING THUNDERSTORM WINDS  LIGHTNING THUNDERSTORM WINDSS 
##                              1                              1 
##                     LIGHTNING.           LIGHTNING/HEAVY RAIN 
##                              1                              1 
##                      LIGNTNING              LOCAL FLASH FLOOD 
##                              1                              1 
##                    LOCAL FLOOD             LOCALLY HEAVY RAIN 
##                              1                              1 
##                LOW TEMPERATURE         LOW TEMPERATURE RECORD 
##                              7                              1 
##                 LOW WIND CHILL                    MAJOR FLOOD 
##                              1                              3 
##                MARINE ACCIDENT                    MARINE HAIL 
##                              1                            442 
##               MARINE HIGH WIND                  MARINE MISHAP 
##                            135                              2 
##             MARINE STRONG WIND       MARINE THUNDERSTORM WIND 
##                             48                           5812 
##               MARINE TSTM WIND            METRO STORM, MAY 26 
##                           6175                              1 
##                     MICROBURST               MICROBURST WINDS 
##                              9                              5 
##           MILD AND DRY PATTERN                   MILD PATTERN 
##                              1                              1 
##               MILD/DRY PATTERN                    MINOR FLOOD 
##                              1                              1 
##                 MINOR FLOODING                   MIXED PRECIP 
##                              4                             10 
##            MIXED PRECIPITATION                  MODERATE SNOW 
##                             37                              1 
##              MODERATE SNOWFALL          MONTHLY PRECIPITATION 
##                            101                             36 
##               MONTHLY RAINFALL               MONTHLY SNOWFALL 
##                             13                              2 
##            MONTHLY TEMPERATURE                 MOUNTAIN SNOWS 
##                              4                              1 
##                      MUD SLIDE                     MUD SLIDES 
##                              7                              1 
##      MUD SLIDES URBAN FLOODING                 MUD/ROCK SLIDE 
##                              1                              1 
##                       MUDSLIDE             MUDSLIDE/LANDSLIDE 
##                             17                              1 
##                      MUDSLIDES               NEAR RECORD SNOW 
##                              9                              1 
##              NO SEVERE WEATHER         NON-SEVERE WIND DAMAGE 
##                              1                              1 
##                  NON-TSTM WIND                NON SEVERE HAIL 
##                              1                              7 
##                  NON TSTM WIND                           NONE 
##                              2                              2 
##           NORMAL PRECIPITATION                NORTHERN LIGHTS 
##                              3                              1 
##                          OTHER               PATCHY DENSE FOG 
##                             52                              3 
##                     PATCHY ICE                   PROLONG COLD 
##                              1                             22 
##              PROLONG COLD/SNOW                 PROLONG WARMTH 
##                              1                              4 
##                 PROLONGED RAIN                           RAIN 
##                              4                             16 
##                   RAIN (HEAVY)                  RAIN AND WIND 
##                              1                              1 
##                    RAIN DAMAGE                      RAIN/SNOW 
##                              1                              5 
##                      RAIN/WIND                      RAINSTORM 
##                              1                              1 
##           RAPIDLY RISING WATER                   RECORD  COLD 
##                              1                              1 
##                    RECORD COLD      RECORD COLD AND HIGH WIND 
##                             67                              1 
##              RECORD COLD/FROST                    RECORD COOL 
##                              2                              5 
##               RECORD DRY MONTH                 RECORD DRYNESS 
##                              1                              2 
##                    RECORD HEAT               RECORD HEAT WAVE 
##                             82                              1 
##                    RECORD HIGH        RECORD HIGH TEMPERATURE 
##                              7                              3 
##       RECORD HIGH TEMPERATURES                     RECORD LOW 
##                              1                              4 
##            RECORD LOW RAINFALL                RECORD MAY SNOW 
##                              2                              1 
##           RECORD PRECIPITATION                RECORD RAINFALL 
##                              1                             14 
##                    RECORD SNOW               RECORD SNOW/COLD 
##                              8                              1 
##                RECORD SNOWFALL             RECORD TEMPERATURE 
##                              6                             16 
##            RECORD TEMPERATURES                    RECORD WARM 
##                              5                              1 
##             RECORD WARM TEMPS.                  RECORD WARMTH 
##                              1                            154 
##             RECORD WINTER SNOW          RECORD/EXCESSIVE HEAT 
##                              3                              3 
##      RECORD/EXCESSIVE RAINFALL              RED FLAG CRITERIA 
##                              1                              2 
##               RED FLAG FIRE WX              REMNANTS OF FLOYD 
##                              2                              2 
##                    RIP CURRENT                   RIP CURRENTS 
##                            470                            304 
##        RIP CURRENTS HEAVY SURF        RIP CURRENTS/HEAVY SURF 
##                              1                              2 
##         RIVER AND STREAM FLOOD                    RIVER FLOOD 
##                              2                            173 
##                 RIVER FLOODING                     ROCK SLIDE 
##                             29                              2 
##                     ROGUE WAVE            ROTATING WALL CLOUD 
##                              1                              5 
##                     ROUGH SEAS                     ROUGH SURF 
##                              3                              4 
##                    RURAL FLOOD                   SAHARAN DUST 
##                              2                              4 
##              SEASONAL SNOWFALL                         SEICHE 
##                              1                             21 
##                    SEVERE COLD            SEVERE THUNDERSTORM 
##                              1                             13 
##      SEVERE THUNDERSTORM WINDS           SEVERE THUNDERSTORMS 
##                              5                             23 
##              SEVERE TURBULENCE                          SLEET 
##                              1                             59 
##          SLEET & FREEZING RAIN                    SLEET STORM 
##                              1                             12 
##            SLEET/FREEZING RAIN                SLEET/ICE STORM 
##                              2                              1 
##                SLEET/RAIN/SNOW                     SLEET/SNOW 
##                              1                              2 
##                     SMALL HAIL                   SMALL STREAM 
##                             53                              1 
##               SMALL STREAM AND   SMALL STREAM AND URBAN FLOOD 
##                              1                              2 
## SMALL STREAM AND URBAN FLOODIN             SMALL STREAM FLOOD 
##                              1                              7 
##          SMALL STREAM FLOODING       SMALL STREAM URBAN FLOOD 
##                              4                              1 
##       SMALL STREAM/URBAN FLOOD                 SML STREAM FLD 
##                              5                              2 
##                          SMOKE                           SNOW 
##                             11                            617 
##    SNOW- HIGH WIND- WIND CHILL              SNOW ACCUMULATION 
##                              1                              2 
##                  SNOW ADVISORY                  SNOW AND COLD 
##                              1                              2 
##            SNOW AND HEAVY SNOW                   SNOW AND ICE 
##                              2                             34 
##             SNOW AND ICE STORM                 SNOW AND SLEET 
##                              1                              5 
##                  SNOW AND WIND                   SNOW DROUGHT 
##                              1                              7 
##             SNOW FREEZING RAIN                   SNOW SHOWERS 
##                             11                              6 
##                     SNOW SLEET                    SNOW SQUALL 
##                              1                             19 
##                   SNOW SQUALLS              SNOW/ BITTER COLD 
##                             22                              1 
##                      SNOW/ ICE              SNOW/BLOWING SNOW 
##                              1                              7 
##                      SNOW/COLD             SNOW/FREEZING RAIN 
##                              2                              6 
##                SNOW/HEAVY SNOW                SNOW/HIGH WINDS 
##                              1                              2 
##                       SNOW/ICE                 SNOW/ICE STORM 
##                              7                             17 
##                      SNOW/RAIN                SNOW/RAIN/SLEET 
##                              1                              1 
##                     SNOW/SLEET       SNOW/SLEET/FREEZING RAIN 
##                             10                              6 
##                SNOW/SLEET/RAIN                     SNOW\\COLD 
##                              1                              1 
##                SNOWFALL RECORD              SNOWMELT FLOODING 
##                              1                              5 
##                      SNOWSTORM                      SOUTHEAST 
##                              1                              1 
##              STORM FORCE WINDS                    STORM SURGE 
##                              1                            261 
##               STORM SURGE/TIDE                STREAM FLOODING 
##                            148                              1 
##                   STREET FLOOD                STREET FLOODING 
##                              3                              3 
##                    STRONG WIND               STRONG WIND GUST 
##                           3569                              2 
##                   STRONG WINDS              SUMMARY AUGUST 10 
##                            204                              2 
##              SUMMARY AUGUST 11              SUMMARY AUGUST 17 
##                              2                              1 
##             SUMMARY AUGUST 2-3              SUMMARY AUGUST 21 
##                              1                              1 
##              SUMMARY AUGUST 28               SUMMARY AUGUST 4 
##                              1                              1 
##               SUMMARY AUGUST 7               SUMMARY AUGUST 9 
##                              1                              1 
##                 SUMMARY JAN 17             SUMMARY JULY 23-24 
##                              1                              1 
##             SUMMARY JUNE 18-19               SUMMARY JUNE 5-6 
##                              1                              1 
##                 SUMMARY JUNE 6            SUMMARY OF APRIL 12 
##                              1                              2 
##            SUMMARY OF APRIL 13            SUMMARY OF APRIL 21 
##                              1                              2 
##            SUMMARY OF APRIL 27           SUMMARY OF APRIL 3RD 
##                              1                              1 
##            SUMMARY OF AUGUST 1             SUMMARY OF JULY 11 
##                              1                              1 
##              SUMMARY OF JULY 2             SUMMARY OF JULY 22 
##                              1                              1 
##             SUMMARY OF JULY 26             SUMMARY OF JULY 29 
##                              1                              1 
##              SUMMARY OF JULY 3             SUMMARY OF JUNE 10 
##                              1                              1 
##             SUMMARY OF JUNE 11             SUMMARY OF JUNE 12 
##                              1                              1 
##             SUMMARY OF JUNE 13             SUMMARY OF JUNE 15 
##                              2                              1 
##             SUMMARY OF JUNE 16             SUMMARY OF JUNE 18 
##                              1                              1 
##             SUMMARY OF JUNE 23             SUMMARY OF JUNE 24 
##                              1                              1 
##              SUMMARY OF JUNE 3             SUMMARY OF JUNE 30 
##                              2                              1 
##              SUMMARY OF JUNE 4              SUMMARY OF JUNE 6 
##                              1                              1 
##            SUMMARY OF MARCH 14            SUMMARY OF MARCH 23 
##                              1                              2 
##            SUMMARY OF MARCH 24         SUMMARY OF MARCH 24-25 
##                              1                              1 
##            SUMMARY OF MARCH 27            SUMMARY OF MARCH 29 
##                              1                              1 
##              SUMMARY OF MAY 10              SUMMARY OF MAY 13 
##                              1                              1 
##              SUMMARY OF MAY 14              SUMMARY OF MAY 22 
##                              1                              1 
##           SUMMARY OF MAY 22 AM           SUMMARY OF MAY 22 PM 
##                              1                              1 
##           SUMMARY OF MAY 26 AM           SUMMARY OF MAY 26 PM 
##                              1                              1 
##           SUMMARY OF MAY 31 AM           SUMMARY OF MAY 31 PM 
##                              1                              1 
##            SUMMARY OF MAY 9-10            SUMMARY SEPT. 25-26 
##                              1                              1 
##           SUMMARY SEPTEMBER 20           SUMMARY SEPTEMBER 23 
##                              1                              2 
##            SUMMARY SEPTEMBER 3            SUMMARY SEPTEMBER 4 
##                              1                              1 
##               SUMMARY: NOV. 16              SUMMARY: NOV. 6-7 
##                              2                              1 
##            SUMMARY: OCT. 20-21            SUMMARY: OCTOBER 31 
##                              1                              1 
##              SUMMARY: SEPT. 18             TEMPERATURE RECORD 
##                              1                             43 
##              THUDERSTORM WINDS            THUNDEERSTORM WINDS 
##                              2                              2 
##            THUNDERESTORM WINDS                    THUNDERSNOW 
##                              1                              1 
##             THUNDERSNOW SHOWER                   THUNDERSTORM 
##                              1                             45 
##            THUNDERSTORM  WINDS            THUNDERSTORM DAMAGE 
##                              7                              2 
##         THUNDERSTORM DAMAGE TO              THUNDERSTORM HAIL 
##                              1                              1 
##            THUNDERSTORM W INDS              THUNDERSTORM WIND 
##                              1                          82564 
##        THUNDERSTORM WIND (G40)           THUNDERSTORM WIND 50 
##                              1                              2 
##           THUNDERSTORM WIND 52           THUNDERSTORM WIND 56 
##                              1                              1 
##           THUNDERSTORM WIND 59       THUNDERSTORM WIND 59 MPH 
##                              1                              1 
##      THUNDERSTORM WIND 59 MPH.       THUNDERSTORM WIND 60 MPH 
##                              1                              4 
##       THUNDERSTORM WIND 65 MPH        THUNDERSTORM WIND 65MPH 
##                              1                              1 
##           THUNDERSTORM WIND 69       THUNDERSTORM WIND 98 MPH 
##                              1                              1 
##          THUNDERSTORM WIND G50          THUNDERSTORM WIND G51 
##                              4                              1 
##          THUNDERSTORM WIND G52          THUNDERSTORM WIND G55 
##                              2                              1 
##          THUNDERSTORM WIND G60          THUNDERSTORM WIND G61 
##                              2                              1 
##        THUNDERSTORM WIND TREES             THUNDERSTORM WIND. 
##                              1                              1 
##        THUNDERSTORM WIND/ TREE       THUNDERSTORM WIND/ TREES 
##                              1                              4 
##       THUNDERSTORM WIND/AWNING         THUNDERSTORM WIND/HAIL 
##                              1                              1 
##    THUNDERSTORM WIND/LIGHTNING             THUNDERSTORM WINDS 
##                              1                          20843 
## THUNDERSTORM WINDS      LE CEN          THUNDERSTORM WINDS 13 
##                              1                              1 
##           THUNDERSTORM WINDS 2          THUNDERSTORM WINDS 50 
##                              1                              1 
##          THUNDERSTORM WINDS 52          THUNDERSTORM WINDS 53 
##                              1                              1 
##          THUNDERSTORM WINDS 60          THUNDERSTORM WINDS 61 
##                              1                              1 
##          THUNDERSTORM WINDS 62      THUNDERSTORM WINDS 63 MPH 
##                              1                              1 
##         THUNDERSTORM WINDS AND THUNDERSTORM WINDS FUNNEL CLOU 
##                              2                              2 
##           THUNDERSTORM WINDS G         THUNDERSTORM WINDS G60 
##                              2                              1 
##        THUNDERSTORM WINDS HAIL  THUNDERSTORM WINDS HEAVY RAIN 
##                             61                              1 
##   THUNDERSTORM WINDS LIGHTNING THUNDERSTORM WINDS SMALL STREA 
##                              7                              1 
## THUNDERSTORM WINDS URBAN FLOOD            THUNDERSTORM WINDS. 
##                              1                              3 
##      THUNDERSTORM WINDS/ FLOOD       THUNDERSTORM WINDS/ HAIL 
##                              2                              1 
## THUNDERSTORM WINDS/FLASH FLOOD    THUNDERSTORM WINDS/FLOODING 
##                              1                              1 
## THUNDERSTORM WINDS/FUNNEL CLOU        THUNDERSTORM WINDS/HAIL 
##                              1                             24 
##  THUNDERSTORM WINDS/HEAVY RAIN           THUNDERSTORM WINDS53 
##                              1                              1 
##         THUNDERSTORM WINDSHAIL            THUNDERSTORM WINDSS 
##                              1                             51 
##              THUNDERSTORM WINS                  THUNDERSTORMS 
##                              1                              4 
##             THUNDERSTORMS WIND            THUNDERSTORMS WINDS 
##                              6                             14 
##                  THUNDERSTORMW               THUNDERSTORMW 50 
##                              1                              1 
##            THUNDERSTORMW WINDS              THUNDERSTORMWINDS 
##                              3                              1 
##              THUNDERSTROM WIND             THUNDERSTROM WINDS 
##                              1                              2 
##              THUNDERTORM WINDS              THUNDERTSORM WIND 
##                              3                              1 
##              THUNDESTORM WINDS              THUNERSTORM WINDS 
##                              2                              1 
##                    TIDAL FLOOD                 TIDAL FLOODING 
##                              1                             25 
##                        TORNADO                 TORNADO DEBRIS 
##                          60652                              1 
##                     TORNADO F0                     TORNADO F1 
##                             19                              4 
##                     TORNADO F2                     TORNADO F3 
##                              3                              2 
##             TORNADO/WATERSPOUT                      TORNADOES 
##                              1                              2 
##     TORNADOES, TSTM WIND, HAIL                       TORNADOS 
##                              1                              1 
##                        TORNDAO                TORRENTIAL RAIN 
##                              1                              1 
##            TORRENTIAL RAINFALL            TROPICAL DEPRESSION 
##                              1                             60 
##                 TROPICAL STORM         TROPICAL STORM ALBERTO 
##                            690                              1 
##            TROPICAL STORM DEAN          TROPICAL STORM GORDON 
##                              2                              1 
##           TROPICAL STORM JERRY                           TSTM 
##                              3                              1 
##                TSTM HEAVY RAIN                      TSTM WIND 
##                              3                         219946 
##               TSTM WIND  (G45)                 TSTM WIND (41) 
##                              1                              1 
##                TSTM WIND (G35)                TSTM WIND (G40) 
##                              1                             10 
##                TSTM WIND (G45)                   TSTM WIND 40 
##                             40                              1 
##                   TSTM WIND 45                   TSTM WIND 50 
##                              1                              1 
##                   TSTM WIND 51                   TSTM WIND 52 
##                              2                              5 
##                   TSTM WIND 55                  TSTM WIND 65) 
##                              3                              1 
##        TSTM WIND AND LIGHTNING               TSTM WIND DAMAGE 
##                              1                              1 
##                  TSTM WIND G45                  TSTM WIND G58 
##                              1                              1 
##                 TSTM WIND/HAIL                     TSTM WINDS 
##                           1028                              6 
##                       TSTM WND                          TSTMW 
##                              1                              1 
##                        TSUNAMI               TUNDERSTORM WIND 
##                             20                              1 
##                        TYPHOON              UNSEASONABLE COLD 
##                             11                              1 
##              UNSEASONABLY COLD              UNSEASONABLY COOL 
##                             23                             12 
##        UNSEASONABLY COOL & WET               UNSEASONABLY DRY 
##                              2                             56 
##               UNSEASONABLY HOT              UNSEASONABLY WARM 
##                             10                            126 
##        UNSEASONABLY WARM & WET      UNSEASONABLY WARM AND DRY 
##                              1                             13 
##         UNSEASONABLY WARM YEAR          UNSEASONABLY WARM/WET 
##                              2                              2 
##               UNSEASONABLY WET            UNSEASONAL LOW TEMP 
##                             19                              2 
##                UNSEASONAL RAIN                 UNUSUAL WARMTH 
##                              2                             10 
##          UNUSUAL/RECORD WARMTH                 UNUSUALLY COLD 
##                              2                              8 
##            UNUSUALLY LATE SNOW                 UNUSUALLY WARM 
##                              1                              4 
##                URBAN AND SMALL         URBAN AND SMALL STREAM 
##                              2                              3 
##   URBAN AND SMALL STREAM FLOOD URBAN AND SMALL STREAM FLOODIN 
##                              3                              6 
##                    URBAN FLOOD          URBAN FLOOD LANDSLIDE 
##                            251                              1 
##                 URBAN FLOODING                   URBAN FLOODS 
##                             99                              3 
##                    URBAN SMALL       URBAN SMALL STREAM FLOOD 
##                              1                              2 
##                    URBAN/SMALL           URBAN/SMALL FLOODING 
##                              2                              1 
##             URBAN/SMALL STREAM      URBAN/SMALL STREAM  FLOOD 
##                              8                              2 
##       URBAN/SMALL STREAM FLOOD    URBAN/SMALL STREAM FLOODING 
##                             30                              4 
##          URBAN/SMALL STRM FLDG           URBAN/SML STREAM FLD 
##                              1                           3392 
##          URBAN/SML STREAM FLDG          URBAN/STREET FLOODING 
##                              1                              3 
##                       VERY DRY                      VERY WARM 
##                              2                              1 
##                            VOG                   VOLCANIC ASH 
##                              1                             23 
##             VOLCANIC ASH PLUME               VOLCANIC ASHFALL 
##                              1                              3 
##              VOLCANIC ERUPTION                  WAKE LOW WIND 
##                              2                              2 
##                     WALL CLOUD        WALL CLOUD/FUNNEL CLOUD 
##                              5                              1 
##            WARM DRY CONDITIONS                   WARM WEATHER 
##                              1                              1 
##                    WATER SPOUT                     WATERSPOUT 
##                              1                           3797 
##                    WATERSPOUT-             WATERSPOUT-TORNADO 
##                             10                              2 
##        WATERSPOUT FUNNEL CLOUD             WATERSPOUT TORNADO 
##                              1                              1 
##                    WATERSPOUT/            WATERSPOUT/ TORNADO 
##                              1                              2 
##             WATERSPOUT/TORNADO                    WATERSPOUTS 
##                              8                             37 
##                    WAYTERSPOUT                  WET MICOBURST 
##                              1                              1 
##                 WET MICROBURST                      WET MONTH 
##                              6                              4 
##                       WET SNOW                    WET WEATHER 
##                              1                              1 
##                       WET YEAR                      WHIRLWIND 
##                              4                              3 
##                     WILD FIRES               WILD/FOREST FIRE 
##                              4                           1457 
##              WILD/FOREST FIRES                       WILDFIRE 
##                              1                           2761 
##                      WILDFIRES                           WIND 
##                              8                            347 
##                  WIND ADVISORY                  WIND AND WAVE 
##                             12                              1 
##                     WIND CHILL           WIND CHILL/HIGH WIND 
##                             18                              1 
##                    WIND DAMAGE                     WIND GUSTS 
##                             31                              3 
##                     WIND STORM                      WIND/HAIL 
##                              1                              1 
##                          WINDS                     WINTER MIX 
##                             36                              3 
##                   WINTER STORM        WINTER STORM HIGH WINDS 
##                          11433                              1 
##         WINTER STORM/HIGH WIND        WINTER STORM/HIGH WINDS 
##                              1                              1 
##                  WINTER STORMS                 WINTER WEATHER 
##                              3                           7045 
##             WINTER WEATHER MIX             WINTER WEATHER/MIX 
##                              6                           1104 
##                    WINTERY MIX                     WINTRY MIX 
##                              2                             94 
##                            WND 
##                              1
```

```r
table(stormData$EVTYPE2)
```

```
## 
##    ASTRONOMICAL LOW TIDE                AVALANCHE                 BLIZZARD 
##                      174                      386                     2719 
##            COASTAL FLOOD          COLD/WIND CHILL              DEBRIS FLOW 
##                      657                      539                        0 
##                DENSE FOG              DENSE SMOKE                  DROUGHT 
##                     1293                       10                     2488 
##               DUST DEVIL               DUST STORM           EXCESSIVE HEAT 
##                      149                      427                     1678 
##  EXTREME COLD/WIND CHILL              FLASH FLOOD                    FLOOD 
##                     1002                    54278                    25327 
##             FROST/FREEZE             FUNNEL CLOUD             FREEZING FOG 
##                     1343                     6844                       46 
##                     HAIL                     HEAT               HEAVY RAIN 
##                   288661                      767                    11742 
##               HEAVY SNOW                HIGH SURF                HIGH WIND 
##                    15708                      734                    20214 
##      HURRICANE (TYPHOON)                ICE STORM         LAKE-EFFECT SNOW 
##                        0                     2006                      636 
##          LAKESHORE FLOOD                LIGHTNING              MARINE HAIL 
##                       23                    15755                      442 
##         MARINE HIGH WIND       MARINE STRONG WIND MARINE THUNDERSTORM WIND 
##                      135                       48                     5812 
##              RIP CURRENT                   SEICHE                    SLEET 
##                      470                       21                       59 
##         STORM SURGE/TIDE              STRONG WIND        THUNDERSTORM WIND 
##                      148                     3569                    82564 
##                  TORNADO      TROPICAL DEPRESSION           TROPICAL STORM 
##                    60652                       60                      690 
##                  TSUNAMI             VOLCANIC ASH               WATERSPOUT 
##                       20                       23                     3797 
##                 WILDFIRE             WINTER STORM           WINTER WEATHER 
##                     2761                    11433                     7045
```


## Results  


## Conclusion

