a<-data.frame(ID=c("VA","MD","MD","VA","VA","DC"), val=c(1,23,12,1,231,12))
b<-data.frame(code=c("MD","VA","NY"),name=c("maryland","virginia","new york"))
a$state_name<-b$name[match(a$ID,b$code)]


https://stackoverflow.com/questions/15069984/replace-values-in-data-frame-based-on-other-data-frame-in-r