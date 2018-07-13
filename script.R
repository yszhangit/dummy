library(dplyr)
library(magrittr)
library(knitr)

dat<-read.csv("repdata%2Fdata%2FStormData.csv", stringsAsFactors = F)
dat<-tbl_df(dat)
# backup
dat1<-dat

# add year
dat$date<-strptime(dat$BGN_DATE, "%m/%d/%Y %H:%M:%S")
dat$year<-dat$date$year+1900
dat$date<-as.character(dat$date)  # this column will not be use again actually
total_Obj<-dim(dat)[1]
quantile(dat$year)
# remove lower 25%
dat<-dat[dat$year>=1995,]
total_obj<-dim(dat)[1]

# damage value
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

# clean up exp cloumns
dat<-dat %>% mutate(PROPDMGEXP = trimws(PROPDMGEXP),
                    CROPDMGEXP = trimws(CROPDMGEXP)
)

# calculate property and crop danmage
dat$propval <- expval(dat$PROPDMG, dat$PROPDMGEXP)
dat$cropval <- expval(dat$CROPDMG, dat$CROPDMGEXP)

# clean up event
source('script_event.R')
dat$event_cat <- events$event_cat

# check 
res<-dat %>% 
#  filter(is.na(event_cat))  %>%  group_by(EVTYPE) %>% 
  group_by(event_cat) %>% 
  summarize( 
    cnt = n(), 
    total_fat = sum(FATALITIES), 
    total_inj = sum(INJURIES),
    total_propval =  round(sum(propval)/1000000,2),
    total_cropval =  round(sum(cropval)/1000000,2)
  ) %>% 
  arrange(desc(cnt))

dat %>% group_by(is.na(event_cat)) %>% summarize(
  cnt=n(),
  total_fat = sum(FATALITIES), 
  total_inj = sum(INJURIES),
  total_propval =  round(sum(propval)/1000000,2),
  total_cropval =  round(sum(cropval)/1000000,2)
  )

# remove un-cat
dat<-dat[!is.na(dat$event_cat), ]

# top event function
# top_fat<-top_env(dat$FATALITIES,20) doesnt work
# top_fat<-top_env("FATALITIES",20)  doesnt work
top_env<-function(cl, topn) {
  total_cnt<-sum(cl)

  d<-dat %>% 
    group_by(event_cat) %>%
    summarise(total=sum(cl)) %>%
    arrange(desc(total)) %>%
    top_n(topn)

  other_cnt <- dat %>% 
    filter(!event_cat %in% d$event_cat) %>% 
    group_by(event_cat) %>% 
    summarise(total=sum(cl)) %>% 
    select(total) %>% 
    sum()
    
  d[nrow(d)+1,] <- list("OTHERS",other_cnt)
  
  d$pct<-round(d$total/total_cnt*100,2)

  return(d)
}


# top fatality
top_fat<-dat %>% 
  group_by(event_cat) %>%
  summarize(total= sum(FATALITIES)) %>% 
  arrange(desc(total)) %>% top_n(20)

total_cnt<-sum(dat$FATALITIES)

other_cnt<-dat %>% 
  filter(!event_cat %in% top_fat$event_cat) %>% 
  group_by(event_cat) %>% 
  summarise(total=sum(FATALITIES)) %>% 
  select(total) %>% 
  sum()
top_fat[nrow(top_fat)+1,] <- list("OTHERS",other_cnt)
top_fat$pct<-round(top_fat$total/total_cnt*100,2)

# top injure
top_inj<-dat %>% 
  group_by(event_cat) %>%
  summarize(total= sum(INJURIES)) %>% 
  arrange(desc(total)) %>% top_n(20)

total_cnt<-sum(dat$INJURIES)

other_cnt<-dat %>% 
  filter(!event_cat %in% top_inj$event_cat) %>% 
  group_by(event_cat) %>% 
  summarise(total=sum(INJURIES)) %>% 
  select(total) %>% 
  sum()
top_inj[nrow(top_inj)+1,] <- list("OTHERS",other_cnt)
top_inj$pct<-round(top_inj$total/total_cnt*100,2)

# top property damange
top_prop<-dat %>% 
  group_by(event_cat) %>%
  summarize(total= round(sum(propval)/1000000)) %>% 
  arrange(desc(total)) %>% top_n(20)

total_cnt<-round(sum(dat$propval)/1000000)

other_cnt<-dat %>% 
  filter(!event_cat %in% top_prop$event_cat) %>% 
  group_by(event_cat) %>% 
  summarise(total=round(sum(propval)/1000000)) %>% 
  select(total) %>% 
  sum()
top_prop[nrow(top_prop)+1,] <- list("OTHERS",other_cnt)
top_prop$pct<-round(top_prop$total/total_cnt*100,2)

# top crop damage
top_crop<-dat %>% 
  group_by(event_cat) %>%
  summarize(total= round(sum(cropval)/1000000)) %>% 
  arrange(desc(total)) %>% top_n(20)

total_cnt<-round(sum(dat$cropval)/1000000)

other_cnt<-dat %>% 
  filter(!event_cat %in% top_crop$event_cat) %>% 
  group_by(event_cat) %>% 
  summarise(total=round(sum(cropval)/1000000)) %>% 
  select(total) %>% 
  sum()
top_crop[nrow(top_crop)+1,] <- list("OTHERS",other_cnt)
top_crop$pct<-round(top_crop$total/total_cnt*100,2)

# treemap
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,1)))
vp_fat<-viewport(layout.pos.col=1, layout.pos.row=1)
treemap(top_fat, index=c("event_cat","total"), vSize="total", vColor="total", type="value", title="top fatality events", vp=vp_fat)
vp_inj<-viewport(layout.pos.col=1, layout.pos.row=2)
treemap(top_inj, index=c("event_cat","total"), vSize="total", vColor="total", type="value", title="top injure event", vp=vp_inj)

grid.newpage()
pushViewport(viewport(layout=grid.layout(2,1)))
vp_prop<-viewport(layout.pos.col=1, layout.pos.row=1)
treemap(top_prop, index=c("event_cat","total"), vSize="total", vColor="total", type="value", title="top property damage events", vp=vp_prop)
vp_crop<-viewport(layout.pos.col=1, layout.pos.row=2)
treemap(top_crop, index=c("event_cat","total"), vSize="total", vColor="total", type="value", title="top crop damage events", vp=vp_crop)




# pie chart
library(plotly)
p_fat<-plot_ly(top_fat[,c("event_cat","total")], labels = ~event_cat, values = ~total, type='pie', showlegend=T) %>%
  layout(title="top 20 event by fatality")
p_inj<-plot_ly(top_inj[,c("event_cat","total")], labels = ~event_cat, values = ~total, type='pie', showlegend=T) %>%
  layout(title="top 20 event by injuries")


plot_ly() %>%
  add_pie(data=top_fat[,c("event_cat","total")], labels = ~event_cat, values = ~total, 
          name="Fatality", domain = list(x = c(0, 0.4), y = c(0.4, 1))) %>%
  add_pie(data=top_fat[,c("event_cat","total")], labels = ~event_cat, values = ~total, 
          name="Injuries", domain = list(x = c(0.6, 1), y = c(0.4, 1))) %>%
  layout(title="top events by fatality and injure", showlegend = F)

### top event by state
state<-read.csv("state.txt",sep = "\t", stringsAsFactors = F)

# https://stackoverflow.com/questions/15069984/replace-values-in-data-frame-based-on-other-data-frame-in-r
dat$state_name<-state$name[match(dat$STATE, state$code)]
# wrost event in each state
# note the group_by order, state_name first, fitler(min_rank) will first on first group column
state_prop <- dat %>% 
  group_by(state_name,event_cat) %>%  
  summarise(total_prop=sum(propval)) %>%
  arrange(desc(total_prop)) %>% 
  filter(row_number(state_name) < 2) %>% 
  mutate(total_prop=round(total_prop/1000000))

# if a state have all 0 across all event, min_rank will not filter any
#state_crop<-dat %>% 
#  group_by(state_name,event_cat) %>% 
#  summarise(total_cropval=round(sum(cropval)/1000000)) %>% 
#  filter(total_cropval>0 & !is.na(total_cropval)) %>% 
#  filter(min_rank(desc(total_cropval)) < 2)
# above statement, the rank will be the same if value is the same, for example SOUTH CAROLINA
# on way to deal with it is to round after filter, but to do it right, dont use rank

state_crop <- dat %>% 
  group_by(state_name,event_cat) %>%  
  summarise(total_crop=sum(cropval)) %>%
  arrange(desc(total_crop)) %>% 
  filter(row_number(state_name) < 2) %>% 
  mutate(total_crop=round(total_crop/1000000))

state_fat <- dat %>% 
  group_by(state_name,event_cat) %>% 
  summarise(total_fat=sum(FATALITIES)) %>%
  arrange(desc(total_fat)) %>%
  filter(row_number(state_name) <2)

state_inj<-dat %>% 
  group_by(state_name,event_cat) %>% 
  summarise(total_inj=sum(INJURIES)) %>% 
  arrange(desc(total_inj)) %>%
  filter(row_number(state_name) <2)

state_tops<-merge(state_fat, state_inj, by="state_name")
state_tops<-merge(state_tops, state_prop, by="state_name")
state_tops<-merge(state_tops, state_crop, by="state_name")

colnames(state_tops)<-c("State",
                        "Event Caused Most Fatality", "Fatality Count", 
                        "Event Caused most Injure", "Injure Count",
                        "Event caused most Property Damage","Damage Value (Million)",
                        "Event Caused Most Crop Damage","Damage Value (Million)"
                        )

dat %>% select(FATALITIES,REMARKS,event_cat, BGN_DATE) %>% arrange(desc(FATALITIES)) %>% filter(min_rank(desc(FATALITIES))<6)
