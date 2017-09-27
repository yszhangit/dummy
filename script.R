library(dplyr)

envcat <- read.csv("envcat.txt")
eventcat<-sort(toupper((envcat[,1])))

dat<-read.csv("repdata%2Fdata%2FStormData.csv", stringsAsFactors = F)
dat<-tbl_df(dat)
dat<-dat %>% mutate(EVTTYPE = toupper(trimws(EVTYPE)))

dat1<-dat %>% mutate(PROPDMGEXP = toupper(trimws(PROPDMGEXP)),
                    CROPDMGEXP = toupper(trimws(CROPDMGEXP)),
                    EVTYPE = toupper(trimws(EVTYPE)))

dat1<-mutate(dat1, event_cat = NA)


assign_event_cat <- function(ev,ec) {
  for ( i in 1:length(ev)) {
    dat1[grep(ev[i],dat1$EVTTYPE),] %<>% mutate(event_cat = gsub(ev[i],ec[i], EVTYPE))
    dat1<<-dat1
  }
}


for ( e in eventcat) {
  dat1[startsWith(dat1$EVTYPE , e), "event_cat"] <- e
}

dat1$propval <- expval(dat1$PROPDMG, dat1$PROPDMGEXP)
dat1$cropval <- expval(dat1$CROPDMG, dat1$CROPDMGEXP)

# check 
dat1_sum<-dat1 %>% 
  filter(is.na(event_cat))  %>% 
  group_by(EVTYPE) %>% 
  summarize( 
    cnt = n(), 
    total_fat = sum(FATALITIES), 
    total_inj = sum(INJURIES),
    total_propval =  sum(propval)
  ) %>% 
  arrange(desc(cnt))


