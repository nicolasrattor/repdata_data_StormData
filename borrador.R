
library(data.table)
library(tidyverse)
library(lubridate)
library(ggpubr)

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

data$MAG<-as.numeric(data$MAG)
data$FATALITIES<-as.numeric(data$FATALITIES)
data$INJURIES<-as.numeric(data$INJURIES)
data$PROPDMG<-as.numeric(data$PROPDMG)
data$CROPDMG<-as.numeric(data$CROPDMG)
data$PROPDMGEXP<-as.factor(data$PROPDMGEXP)
data$CROPDMGEXP<-as.factor(data$CROPDMGEXP)

data %>% select(FATALITIES,INJURIES,PROPDMG,CROPDMG,MAG) %>% dfSummary() %>% view()

## Preguntas
#* Does the analysis address the question of which types of events are most harmful to population health?
data %>% group_by(EVTYPE) %>% summarise(FATALITIES=sum(FATALITIES),
                                        INJURIES=sum(INJURIES),
                                        PROPDMG=sum(PROPDMG),
                                        CROPDMG=sum(CROPDMG)) %>% arrange(-FATALITIES,
                                                                          -INJURIES,
                                                                          -PROPDMG,
                                                                          -CROPDMG)

a<-data %>% group_by(EVTYPE) %>% summarise(FATALITIES=sum(FATALITIES),
                                        INJURIES=sum(INJURIES),
                                        PROPDMG=sum(PROPDMG),
                                        CROPDMG=sum(CROPDMG),
                                        MAG=sum(MAG)) %>% arrange(-FATALITIES)

g1<-a %>% filter(FATALITIES>200) %>% ggplot(aes(x=reorder(EVTYPE,FATALITIES), y=FATALITIES)) +
  geom_bar(stat="identity", fill="red", colour="black", width=0.8) + 
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme_bw() +  coord_flip() + 
  geom_text(aes(label = format(FATALITIES, big.mark = ",", scientific = FALSE)), hjust=-0.1, colour = "black", size=3.0) +
  labs(title="Fatalities",
       x="EVTYPE", 
       y = "FATALITIES")

g2<-a %>% filter(INJURIES>1000) %>% ggplot(aes(x=reorder(EVTYPE,INJURIES), y=INJURIES)) +
  geom_bar(stat="identity", fill="purple", colour="black", width=0.8) + 
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme_bw() +  coord_flip() + 
  geom_text(aes(label = format(INJURIES, big.mark = ",", scientific = FALSE)), hjust=-0.1, colour = "black", size=3.0) +
  labs(title="Injuries",
       x="EVTYPE", 
       y = "INJURIES")

g3<-a %>% filter(PROPDMG>20000) %>% mutate(PROPDMG=PROPDMG/10) %>% 
  ggplot(aes(x=reorder(EVTYPE,PROPDMG), y=round(PROPDMG))) +
  geom_bar(stat="identity", fill="black", colour="black", width=0.8) + 
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme_bw() +  coord_flip() + 
  geom_text(aes(label = format(round(PROPDMG), big.mark = ",", scientific = FALSE)), hjust=-0.1, colour = "black", size=3.0) +
  labs(title="Property damage (divided into 10)",
       x="EVTYPE", 
       y = "PROPDMG")

g4<-a %>% filter(CROPDMG>5000) %>% mutate(CROPDMG=CROPDMG/10) %>% 
  ggplot(aes(x=reorder(EVTYPE,CROPDMG), y=round(CROPDMG))) +
  geom_bar(stat="identity", fill="#3399FF", colour="black", width=0.8) + 
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme_bw() +  coord_flip() + 
  geom_text(aes(label = format(round(CROPDMG), big.mark = ",", scientific = FALSE)), hjust=-0.1, 
            colour = "black", size=3.0) +
  labs(title="Crop damage (divided into 10)",
       x="EVTYPE", 
       y = "CROPDMG")

g5<-ggarrange(g1,g2,g3,g4,ncol = 2,nrow = 2)
annotate_figure(g5, top=text_grob("The most damaging and economically costly weather events in the United States (1950-2011)"),
                bottom = text_grob("National Climatic Data Center Storm Events",
                                       hjust = 1, x = 1, face = "italic", size = 10))

data %>% filter(EVTYPE %in% c("TORNADO","EXCESSIVE HEAT","FLASH FLOOD",
                              "FLOOD","TSTM WIND",
                              "HAIL")) %>% 
  group_by(EVTYPE,BGN_YEAR) %>% summarise(FATALITIES=sum(FATALITIES),
                                          INJURIES=sum(INJURIES),
                                          PROPDMG=(sum(PROPDMG)/10),
                                          CROPDMG=sum(CROPDMG)/10) %>% 
  gather(key = "type", value = "value", -EVTYPE,-BGN_YEAR) %>% 
  ggplot(aes( x = BGN_YEAR, y =value, fill = type, color = type)) + facet_wrap(~EVTYPE) + 
  geom_line() + theme_bw() + 
  labs(title="The most damaging and economically costly weather events in the United States (1950-2011)",
       x="Year",
       y = "Fatalities, injures, property damage and crop damage",
       caption = "National Climatic Data Center Storm Events") + 
  theme(legend.position = "bottom")
  
#* Does the analysis address the question of which types of events have the greatest economic consequences?