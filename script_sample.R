# using sample during coding
sam<-sample_frac(dat,0.1)
sam<-sam %>% mutate(PROPDMGEXP = toupper(trimws(PROPDMGEXP)),
                    CROPDMGEXP = toupper(trimws(CROPDMGEXP)),
                    EVTYPE = toupper(trimws(EVTYPE)))

# possible value
# H,h,K,k,M,m,B,b,+,-,?,0,1,2,3,4,5,6,7,8, and blank-character
# if condition of (), you need || instead of | for multi-condition
expval_dont<-function(v,e) {
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
  return(res)
}

# ok better way, THE R way is to vectorlize functions, this is a wrapper function Vectorize
# but this is a fake vector function, i still got same warning message
# Grouping rowwise data frame strips rowwise nature 

# Vexpval<-Vectorize(expval, vectorize.args = c("v","e"))

# apply function to each row, because function take row, mutate will pass list to function call, not individual element
#

# try v and e as vector YO!
#
#     doenst apperas to be any faster
#
#> table(dat$CROPDMGEXP)

#?      0      2      B      k      K      m      M 
#618413      7     19      1      9     21 281832      1   1994 
#> table(dat$PROPDMGEXP)

#-      ?      +      0      1      2      3      4      5      6      7      8      B      h      H      K      m      M 
#465934      1      8      5    216     25     13      4      4     28      4      5      1     40      1      6 424665      7  11330 

expval<-function(v,e) {
  if ( length(v) != length(e)) {
    message("2 vectors not at same length")
    return()
  }
  # the order doesnt matter, all gsub will run anyway,
  # only order matters is number be first, so that 10 will not be replace again
  e <- sub("^[0|1|2|3|4|5|6|7|8]$", "10", e)
  e <- sub("^$", "0", e)
  e <- sub("^[-|?]$", "0", e)
  e <- sub("^\\+$", "1", e)
  e <- sub("^[h|H]$", "100", e)
  e <- sub("^[k|K]$", "1000", e)
  e <- sub("^[m|M]$", "1000000", e)
  e <- sub("^[b|B]$", "1000000000", e)
  e <- as.numeric(e)
  return (v * e)
}

# still slow not matter what function, and alway get rowwrise warning
# sam <- sam %>% 
#  mutate( propval = expval(PROPDMG,PROPDMGEXP),
#          cropval = expval(CROPDMG,CROPDMGEXP)
#          )

sam$propval <- expval(sam$PROPDMG, sam$PROPDMGEXP)
sam$cropval <- expval(sam$CROPDMG, sam$CROPDMGEXP)

# use unlist or pull to extract a column from data frame if you forgot again how to do it


sam<-mutate(sam, event_cat = NA)

# clean up event type

assign_event_cat_sample <- function(ev,ec) {
  for ( i in 1:length(ev)) {
    sam[grep(ev[i],sam$EVTYPE),] %<>% mutate(event_cat = gsub(ev[i],ec[i], EVTYPE))
    sam<<-sam
  }
}
# event_types and event_cats has to match as pairs
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

assign_event_cat_sample(event_types, event_cats)

# AFTER clean up
# some name has unnecessary text afterwards
for ( e in eventcat) {
  sam[startsWith(sam$EVTYPE , e), "event_cat"] <- e
}

# check 
sam_sum<-sam %>% 
  filter(is.na(event_cat))  %>% 
  group_by(EVTYPE) %>% 
  summarize( 
    cnt = n(), 
    total_fat = sum(FATALITIES), 
    total_inj = sum(INJURIES),
    total_propval =  sum(propval)
  ) %>% 
  arrange(desc(cnt))

