#require  the packages 
setwd("E:/")
library(data.table)
library(ggplot2)

#get the origin datasets and manipulate
data<-fread("data.csv")
str(data)
data$year <- as.numeric(format(as.Date(data$BGN_DATE, format = "%m/%d/%Y %H:%M:%S"), "%Y"))
#show the counts of years
ggplot(data,aes(year))+geom_histogram()

# get the selected datasets we use
year<-as.data.frame(table(data$year))
year$cum<-cumsum(year$Freq)
dim(data)[1]*0.8
#the datasets of storm is what we use next,it's also data.table
storm<-data[data$year>=1992,]


#the Fatalities by Severe Weather  from 1992 to 2011
fata<-storm[,list(all.fata=sum(FATALITIES)),by=EVTYPE]
fata<-as.data.frame(fata)
fata_top15<-fata[order(fata$all.fata,decreasing =T),][1:15,]
fata_top15
ggplot(fata_top15, aes(x=EVTYPE, y=all.fata)) + 
    geom_bar(stat ="identity",width=0.8)+ 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    xlab("Severe Weather Type") + ylab("Number of Fatalities") + 
    ggtitle("Total Fatalities by Severe Weather\n Events in the U.S. from 1992 - 2011")



#the Injuries by Severe Weather  from 1992 to 2011
inj<-storm[,list(all.inj=sum(INJURIES)),by=EVTYPE]
inj<-as.data.frame(inj)
inj_top15<-inj[order(inj$all.inj,decreasing =T),][1:15,]
inj_top15
ggplot(inj_top15, aes(x=EVTYPE, y=all.inj)) + 
    geom_bar(stat ="identity",width=0.9)+ 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    xlab("Severe Weather Type") + ylab("Number of Injuries") + 
    ggtitle("Total Injuries by Severe Weather\n Events in the U.S. from 1992 - 2011")


#the Property damage by Severe Weather  from 1992 to 2011
a<-c("EVTYPE","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")
storm<-as.data.frame(storm)
data2<-storm[,names(storm) %in% a]
data2[data2$PROPDMGEXP=="",]$PROPDMGEXP=0
data2[tolower(data2$PROPDMGEXP)=="h",]$PROPDMGEXP=2
data2[tolower(data2$PROPDMGEXP)=="k",]$PROPDMGEXP=3
data2[tolower(data2$PROPDMGEXP)=="m",]$PROPDMGEXP=6
data2[tolower(data2$PROPDMGEXP)=="b",]$PROPDMGEXP=9
data2$PROPDMGEXP<-as.numeric(data2$PROPDMGEXP)
data2<-data2[complete.cases(data2),]
data2$PRO<-data2$PROPDMG*10^(data2$PROPDMGEXP)
data2<-as.data.table(data2)

pro<-data2[,list(all.pro=sum(PRO)),by=EVTYPE]
pro<-as.data.frame(pro)
pro_top15<-pro[order(pro$all.pro,decreasing =T),][1:15,]
pro_top15
ggplot(pro_top15, aes(x=EVTYPE, y=all.pro)) + 
    geom_bar(stat ="identity",width=0.9)+ 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    xlab("Severe Weather Type") + ylab("Number of Property damage") + 
    ggtitle("Total Property damage by Severe Weather\n Events in the U.S. from 1992 - 2011")


#the crop damage by Severe Weather  from 1992 to 2011
a<-c("EVTYPE","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")
storm<-as.data.frame(storm)
data3<-storm[,names(storm) %in% a]
data3[data3$CROPDMGEXP=="",]$CROPDMGEXP=0
data3[tolower(data3$CROPDMGEXP)=="k",]$CROPDMGEXP=3
data3[tolower(data3$CROPDMGEXP)=="m",]$CROPDMGEXP=6
data3[tolower(data3$CROPDMGEXP)=="b",]$CROPDMGEXP=9
data3$CROPDMGEXP<-as.numeric(data3$CROPDMGEXP)
data3<-data3[complete.cases(data3),]
data3$crop<-data3$CROPDMG*10^(data3$CROPDMGEXP)
data3<-as.data.table(data3)

crop<-data3[,list(all.crop=sum(crop)),by=EVTYPE]
crop<-as.data.frame(crop)
crop_top15<-crop[order(crop$all.crop,decreasing =T),][1:15,]
crop_top15


ggplot(crop_top15, aes(x=EVTYPE, y=all.crop)) + 
    geom_bar(stat ="identity",width=0.9)+ 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    xlab("Severe Weather Type") + ylab("Number of Crop damage") + 
    ggtitle("Total Crop damage by Severe Weather\n Events in the U.S. from 1992 - 2011")

