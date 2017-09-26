library(dplyr)

envcat <- read.csv("envcat.txt")
eventcat<-sort(toupper((envcat[,1])))

dat<-read.csv("repdata%2Fdata%2FStormData.csv", stringsAsFactors = F)
dat<-tbl_df(dat)
dat<-dat %>% mutate(EVTTYPE = toupper(trimws(EVTYPE)))


sam<-sample_frac(dat,0.1)
sam<-sam %>% mutate(toupper(trimws(PROPDMGEXP)))
sam<-sam %>% mutate(toupper(trimws(CROPDMGEXP)))
sam<-sam %>% mutate(toupper(trimws(EVTYPE)))
# possible value
# H,h,K,k,M,m,B,b,+,-,?,0,1,2,3,4,5,6,7,8, and blank-character
# if condition of (), you need || instead of | for multi-condition
expval<-function(v,e) {
  if ( is.na (e) || e == ""  ) {
    res <- 0
  } else if (e == 'H' || e == 'h') {
    res <- v*100
  } else if (e == 'K' || e == 'k') {
    res <- v*1000
  } else if (e == 'M' || e == 'm') {
    res <- v*1000000
  } else if (e == 'B' || e == 'b') {
    res <- v*1000000000
  } else if (e %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8")) {
    res <- v*10
  } else if (e == '+' ) {
    res <- v
  } else if (e %in% c("-", "?")) {
    res <- 0
  } else {
    print(paste("invalid expression: >", e, "<"))
    res <- 0
  }
  res
}


sam_val <- sam %>% 
  select(PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) %>% 
  mutate( propval= expval(PROPDMG,PROPDMGEXP),
          cropval= expval(CROPDMG,CROPDMGEXP)
          )

# use unlist or pull to extract a column from data frame if you forgot again how to do it
sam1<-mutate(sam, event_cat = NA)
sam1[sam1$EVTYPE == 'TSTM WIND', "EVTYPE"] <- "THUNDERSTORM WIND"

for ( e in eventcat) {
  sam1[startsWith(sam1$EVTYPE , e), "event_cat"] <- e
}