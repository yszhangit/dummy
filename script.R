dat<-read.csv("repdata%2Fdata%2FStormData.csv")
dat<-tbl_df(dat)
sam<-sample_frac(sam,0.1)
sam<-sam %>% mutate(toupper(trimws(PROPDMGEXP)))

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


sam<-sam %>% mutate(toupper(trimws(PROPDMGEXP)))
sam <- sam %>% select(PROPDMG, PROPDMGEXP) %>% mutate( propval= expval(PROPDMG,PROPDMGEXP))
