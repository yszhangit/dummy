library(dplyr)
library(magrittr)
library(knitr)

evtcat <- read.csv("evtcat.txt")
eventcat<-sort(toupper((evtcat[,1])))
rm(evtcat)

dat<-read.csv("../repdata%2Fdata%2FStormData.csv", stringsAsFactors = F)
dat<-tbl_df(dat)
dat<-dat %>% mutate(EVTTYPE = toupper(trimws(EVTYPE)))

dat<-dat %>% mutate(PROPDMGEXP = toupper(trimws(PROPDMGEXP)),
                    CROPDMGEXP = toupper(trimws(CROPDMGEXP)),
                    EVTYPE = toupper(trimws(EVTYPE)),
                    event_cat = NA
                    )

assign_event_cat <- function(ev,ec) {
  for ( i in 1:length(ev)) {
    dat1[grep(ev[i],dat1$EVTTYPE),] %<>% mutate(event_cat = gsub(ev[i],ec[i], EVTYPE))
    dat1<<-dat1
  }
}
event_types <- c(
  "TSTM",
  "EXTREME COLD",
  "EXTREME HEAT",
  "WILD/FOREST FIRE",
  "URBAN/SML STREAM FLD",
  "SNOW",
  "FOG",
  "URBAN FLOOD",
  "WIND",
  "EXTREME WINDCHILL",
  "FREEZING RAIN",
  "HEAVY SURF/HIGH SURF",
  "HURRICANE",
  "RIVER FLOOD",
  "STORM SURGE",
  "ASTRONOMICAL LOW TIDE",
  "COLD"
)

event_cats <- c(
  "THUNDERSTORM",
  "EXTREME COLD/WIND CHILL",
  "HEAT",
  "WILDFIRE",
  "FLOOD",
  "HEAVY SNOW",
  "DENSE FOG",
  "FLOOD",
  "HIGH WIND",
  "EXTREME COLD/WIND CHILL",
  "SLEET",
  "HIGH SURF",
  "HURRICANE (TYPHOON)",
  "FLOOD",
  "STORM SURGE/TIDE",
  "ASTRONOMICAL LOW TIDE",
  "COLD/WIND CHILL"
)
assign_event_cat(event_types, event_cats)

for ( e in eventcat) {
  dat1[startsWith(dat1$EVTYPE , e), "event_cat"] <- e
}


dat1$propval <- expval(dat1$PROPDMG, dat1$PROPDMGEXP)
dat1$cropval <- expval(dat1$CROPDMG, dat1$CROPDMGEXP)

# check 
dat1 %>% 
  filter(is.na(event_cat))  %>% 
  group_by(EVTYPE) %>% 
  summarize( 
    cnt = n(), 
    total_fat = sum(FATALITIES), 
    total_inj = sum(INJURIES),
    total_propval =  round(sum(propval)/1000000,2)
  ) %>% 
  arrange(desc(cnt))


