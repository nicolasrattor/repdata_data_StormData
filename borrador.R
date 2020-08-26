
library(data.table)
library(tidyverse)
library(lubridate)

## Cargar base

download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
              "StormData.csv.bz2", method = "libcurl")

data<-read.csv("StormData.csv.bz2", header = TRUE, sep = ",")


## Transformar datos
summary(data)
names(data)
head(data)
data$BGN_DATE<-mdy_hms(as.character(data$BGN_DATE))
data$BGN_YEAR<-year(data$BGN_DATE)
data$END_DATE<-mdy_hms(as.character(data$END_DATE))


## Preguntas
#* Does the analysis address the question of which types of events are most harmful to population health?
data %>% group_by(EVTYPE) %>% summarise(FATALITIES=sum(FATALITIES),
                                        INJURIES=sum(INJURIES)) %>% arrange(-FATALITIES)

data %>% filter(EVTYPE %in% c("TORNADO","EXCESSIVE HEAT","FLASH FLOOD","HEAT","LIGHTNING","TSTM WIND")) %>% 
  group_by(EVTYPE,BGN_YEAR) %>% summarise(FATALITIES=sum(FATALITIES),
                                 INJURIES=sum(INJURIES)) %>% arrange(-FATALITIES) %>% 
  gather(key = "type", value = "value", -EVTYPE,-BGN_YEAR) %>% 
  ggplot(aes( x = BGN_YEAR, y =value, fill = type, color = type)) + facet_wrap(~EVTYPE) + 
  geom_line() + theme_bw() + 
  labs(x="Year",
       y = "FATALITIES AND INJURIES",
       caption = "National Climatic Data Center Storm Events") + 
  theme(legend.position = "bottom")
  
#* Does the analysis address the question of which types of events have the greatest economic consequences?