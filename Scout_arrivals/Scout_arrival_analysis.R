###Script for conducting analyses related to martin first arrivals over time

#Purple Martin Scout Arrival Data can be found at: https://www.purplemartin.org/research/8/scout-arrival-study/

library(lubridate)

#read in scout arrivals by state

#TX
sadtx <- read.csv("Scout_arrivals/TX_arrivals.csv") 
sadtx$NAME <- NULL

#select adults only
sadtx<- sadtx[sadtx$AGE == "Adult",] 

#remove dates after May 31 because birds should arrive by then
nrow(sadtx[grep("^6/", sadtx$DATE),]) #1 date after May 31
sadtx<-sadtx[-grep("^6/", sadtx$DATE),] #remove dates after May 31

#number of arrivals
nrow(sadtx) #9589

sadtx$Month_Day <- format(mdy(sadtx$DATE), "%m-%d")
#getting mean, q1, q3
meanls <- list()
q1ls <- list()
q3ls <- list()
for (a in unique(sadtx$YEAR)) {
  datels <- list()
  for (i in 1:nrow(sadtx)) {
    year <- sadtx$YEAR[i]
    if (year == a) {
      datels <- c(datels, sadtx$DATE[i])
    }
  }
  mean <- as.Date(mean(mdy(unlist(datels))))
  meanls <- c(meanls, as.character(mean))
  q1 <- as.Date(quantile(as.numeric(mdy(unlist(datels))), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)[1], origin="1970-01-01")
  q1ls <- c(q1ls, as.character(q1))
  q3 <-as.Date(quantile(as.numeric(mdy(unlist(datels))), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)[3], origin="1970-01-01")
  q3ls <- c(q3ls, as.character(q3))
}

#adding means to sadtx
dfmeanls <- list()
for(a in meanls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadtx)){
    if(sadtx$YEAR[i] == yr){
      dfmeanls[[i]] <- a
    }
  }
}
sadtx$Mean <- unlist(dfmeanls)
sadtx$Mean_md <- format(ymd(sadtx$Mean), "%m-%d")
#adding q1 to sadtx
dfq1ls <- list()
for(a in q1ls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadtx)){
    if(sadtx$YEAR[i] == yr){
      dfq1ls[[i]] <- a
    }
  }
}
sadtx$Q1 <- unlist(dfq1ls)
sadtx$Q1_md <- format(as.Date(sadtx$Q1), "%m-%d")
#adding q3 to sadtx
dfq3ls <- list()
for(a in q3ls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadtx)){
    if(sadtx$YEAR[i] == yr){
      dfq3ls[[i]] <- a
    }
  }
}
sadtx$Q3 <- unlist(dfq3ls)
sadtx$Q3_md <- format(as.Date(sadtx$Q3), "%m-%d")

#calculate mean day of year for 1999-2021
mean(yday(unlist(meanls))[4:27])
sdtx<-sd(yday(unlist(meanls))[4:27])
meantx<-as.Date(mean(yday(unlist(meanls))[4:27]),origin="2024/12/31")
tx_2022<-as.Date(mean(yday(unlist(meanls))[3]),origin="2024/12/31")
tx_2023<-as.Date(mean(yday(unlist(meanls))[2]),origin="2024/12/31")
tx_2024<-as.Date(mean(yday(unlist(meanls))[1]),origin="2024/12/31")

#minimum arrival
min_arrival_tx=NULL
for (i in levels(as.factor(sadtx$YEAR))) {
	min_arrival_tx<-c(min_arrival_tx,min(yday(as.Date(sadtx$DATE[sadtx$YEAR==i], format="%m/%d/%y"))))
	}
mean(min_arrival_tx[1:24]) #mean min arrival from 1998-2021
min_arrival_tx[25]


#plot the data 
#3 Dec dates (2002 and 2009) need to be plotted separately
#grid::current.viewport() #sometimes this call seems to be necessary to initalize a plot to prevent a depth error being thrown

ggplot(sadtx[-grep("^12/", sadtx$DATE),], aes(y = YEAR, x = as.Date(Month_Day, format = "%m-%d"))) +
  geom_rect(xmin = meantx-(2*sdtx), xmax = meantx+(2*sdtx), ymin = 1997, ymax = 2025, fill = "gray 91") + 
  geom_point(col="dark gray") + 
  geom_point(data=sadtx[grep("^12/", sadtx$DATE),], aes(y = YEAR, x = as.Date(Month_Day, format = "%m-%d")-366), col="gray") + #Dec dates
  geom_vline(xintercept=meantx, col="black")+ 
  geom_point(aes(y=YEAR, x = as.Date(Mean_md, format = "%m-%d")), color = "black", size = 4)+
  geom_point(aes(y=YEAR, x = as.Date(Q1_md, format = "%m-%d")), color = "black", size = 3, shape=3)+
  geom_point(aes(y=YEAR, x = as.Date(Q3_md, format = "%m-%d")), color = "black", size = 3, shape=3)+
  scale_x_date(date_labels = "%m-%d", date_breaks = "20 days") +
  ylim(1998,2024)+
   labs(y = "Year", x = "Date") +
  #ggtitle("TX Scout Arrival Dates") +
    theme(legend.position="none",
  	panel.background=element_rect(fill="white"), 
  	plot.margin=unit(c(1,1,1,1),"cm"), 
  	axis.title.y=element_text(size = 22, margin=margin(0,10,0,0)), 
  	axis.title.x=element_text(size = 22, margin=margin(20,0,0,0)),
  	axis.text.x = element_text(size = 14), 
  	axis.text.y = element_text(size = 14),
  	axis.line=element_line(size=1))

arrival_anomaly<-data.frame(Year = rev(c(1998:2024)), Arrival_anomaly=NA, State="TX")
for (i in 1:27) {
	arrival_anomaly$anomaly[i]<-as.POSIXlt(as.Date(meanls[[i]]), format = "%y%m%d")$yday-as.POSIXlt(meantx, format = "%y%m%d")$yday 
}
write.csv(arrival_anomaly,"TX_anomalous_arrivals.csv", row.names=FALSE)

################################

#LA
sadla <- read.csv("Scout_arrivals/LA_arrivals.csv")
sadla$NAME <- NULL

#select adults only
sadla<- sadla[sadla$AGE == "Adult",]

#remove dates after May 31 because birds should arrive by then
nrow(sadla[grep("^6/", sadla$DATE),]) #0

#number of arrivals
nrow(sadla) #3190

sadla$Month_Day <- format(mdy(sadla$DATE), "%m-%d")
#getting mean, q1, q3
meanls <- list()
q1ls <- list()
q3ls <- list()
for (a in unique(sadla$YEAR)) {
  datels <- list()
  for (i in 1:nrow(sadla)) {
    year <- sadla$YEAR[i]
    if (year == a) {
      datels <- c(datels, sadla$DATE[i])
    }
  }
  mean <- as.Date(mean(mdy(unlist(datels))))
  meanls <- c(meanls, as.character(mean))
  q1 <- as.Date(quantile(as.numeric(mdy(unlist(datels))), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)[1], origin="1970-01-01")
  q1ls <- c(q1ls, as.character(q1))
  q3 <-as.Date(quantile(as.numeric(mdy(unlist(datels))), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)[3], origin="1970-01-01")
  q3ls <- c(q3ls, as.character(q3))
}

#adding means to sadla
dfmeanls <- list()
for(a in meanls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadla)){
    if(sadla$YEAR[i] == yr){
      dfmeanls[[i]] <- a
    }
  }
}
sadla$Mean <- unlist(dfmeanls)
sadla$Mean_md <- format(ymd(sadla$Mean), "%m-%d")
#adding q1 to sadla
dfq1ls <- list()
for(a in q1ls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadla)){
    if(sadla$YEAR[i] == yr){
      dfq1ls[[i]] <- a
    }
  }
}
sadla$Q1 <- unlist(dfq1ls)
sadla$Q1_md <- format(as.Date(sadla$Q1), "%m-%d")
#adding q3 to sadla
dfq3ls <- list()
for(a in q3ls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadla)){
    if(sadla$YEAR[i] == yr){
      dfq3ls[[i]] <- a
    }
  }
}
sadla$Q3 <- unlist(dfq3ls)
sadla$Q3_md <- format(as.Date(sadla$Q3), "%m-%d")

#calculate mean day of year for 1999-2021
mean(yday(unlist(meanls))[4:27])
sdla<-sd(yday(unlist(meanls))[4:27])
meanla<-as.Date(mean(yday(unlist(meanls))[4:27]),origin="2024/12/31")
la_2022<-as.Date(mean(yday(unlist(meanls))[3]),origin="2024/12/31")
la_2023<-as.Date(mean(yday(unlist(meanls))[2]),origin="2024/12/31")
la_2024<-as.Date(mean(yday(unlist(meanls))[1]),origin="2024/12/31")

#minimum arrival
min_arrival_la=NULL
for (i in levels(as.factor(sadla$YEAR))) {
	min_arrival_la<-c(min_arrival_la,min(yday(as.Date(sadla$DATE[sadla$YEAR==i], format="%m/%d/%y"))))
	}
#except Dec arrivals don't work
min_arrival_la[23] <- -3 #2020 min arrival is Dec 29, 2019
min_arrival_la[24] <- -12 #2021 min arrival is Dec 20, 2020

mean(min_arrival_la[1:24]) #mean min arrival from 1998-2021
min_arrival_la[25]

#plot the data vertically
#4 Dec dates to plot separately
ggplot(sadla[-grep("^12/", sadla$DATE),], aes(y = YEAR, x = as.Date(Month_Day, format = "%m-%d"))) +
  geom_rect(xmin = meanla-(2*sdla), xmax = meanla+(2*sdla), ymin = 1997, ymax = 2025, fill = "gray91") + 
  geom_point(col="dark gray") + 
  geom_point(data=sadla[grep("^12/", sadla$DATE),], aes(y = YEAR, x = as.Date(Month_Day, format = "%m-%d")-366), col="gray") + #Dec dates
  geom_vline(xintercept=meanla, col="black")+
  geom_point(aes(y=YEAR, x = as.Date(Mean_md, format = "%m-%d")), color = "black", size = 3)+
  geom_point(aes(y=YEAR, x = as.Date(Q1_md, format = "%m-%d")), color = "black", size = 2, shape=3)+
  geom_point(aes(y=YEAR, x = as.Date(Q3_md, format = "%m-%d")), color = "black", size = 2, shape=3)+
  scale_x_date(date_labels = "%m-%d", date_breaks = "20 days") +
  ylim(1998,2024)+
  labs(y = "Year", x = "Date") +
  #ggtitle("LA Scout Arrival Dates") +
    theme(legend.position="none",
  	panel.background=element_rect(fill="white"), 
  	plot.margin=unit(c(1,1,1,1),"cm"), 
  	axis.title.y=element_text(size = 22, margin=margin(0,10,0,0)), 
  	axis.title.x=element_text(size = 22, margin=margin(20,0,0,0)),
  	axis.text.x = element_text(size = 14), 
  	axis.text.y = element_text(size = 14),
  	axis.line=element_line(size=1))

arrival_anomaly<-data.frame(Year = rev(c(1998:2024)), Arrival_anomaly=NA, State="LA")
for (i in 1:27) {
	arrival_anomaly$anomaly[i]<-as.POSIXlt(as.Date(meanls[[i]]), format = "%y%m%d")$yday-as.POSIXlt(meanla, format = "%y%m%d")$yday 
}
write.csv(arrival_anomaly,"LA_anomalous_arrivals.csv", row.names=FALSE)

###############################################

#MS
sadms <- read.csv("Scout_arrivals/MS_arrivals.csv")
sadms$NAME <- NULL

#select adults only
sadms<- sadms[sadms$AGE == "Adult",]

#remove dates after May 31
nrow(sadms[grep("^6/", sadms$DATE),]) #0

#number of arrivals
nrow(sadms) #1337

sadms$Month_Day <- format(mdy(sadms$DATE), "%m-%d")
#getting mean, q1, q3
meanls <- list()
q1ls <- list()
q3ls <- list()
for (a in unique(sadms$YEAR)) {
  datels <- list()
  for (i in 1:nrow(sadms)) {
    year <- sadms$YEAR[i]
    if (year == a) {
      datels <- c(datels, sadms$DATE[i])
    }
  }
  mean <- as.Date(mean(mdy(unlist(datels))))
  meanls <- c(meanls, as.character(mean))
  q1 <- as.Date(quantile(as.numeric(mdy(unlist(datels))), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)[1], origin="1970-01-01")
  q1ls <- c(q1ls, as.character(q1))
  q3 <-as.Date(quantile(as.numeric(mdy(unlist(datels))), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)[3], origin="1970-01-01")
  q3ls <- c(q3ls, as.character(q3))
}

#adding means to sadms
dfmeanls <- list()
for(a in meanls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadms)){
    if(sadms$YEAR[i] == yr){
      dfmeanls[[i]] <- a
    }
  }
}
sadms$Mean <- unlist(dfmeanls)
sadms$Mean_md <- format(ymd(sadms$Mean), "%m-%d")
#adding q1 to sadms
dfq1ls <- list()
for(a in q1ls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadms)){
    if(sadms$YEAR[i] == yr){
      dfq1ls[[i]] <- a
    }
  }
}
sadms$Q1 <- unlist(dfq1ls)
sadms$Q1_md <- format(as.Date(sadms$Q1), "%m-%d")
#adding q3 to sadms
dfq3ls <- list()
for(a in q3ls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadms)){
    if(sadms$YEAR[i] == yr){
      dfq3ls[[i]] <- a
    }
  }
}
sadms$Q3 <- unlist(dfq3ls)
sadms$Q3_md <- format(as.Date(sadms$Q3), "%m-%d")

#calculate mean day of year for 1999-2021
mean(yday(unlist(meanls))[4:27])
sdms<-sd(yday(unlist(meanls))[4:27])
meanms<-as.Date(mean(yday(unlist(meanls))[4:27]),origin="2024/12/31")
ms_2022<-as.Date(mean(yday(unlist(meanls))[3]),origin="2024/12/31")
ms_2023<-as.Date(mean(yday(unlist(meanls))[2]),origin="2024/12/31")
ms_2024<-as.Date(mean(yday(unlist(meanls))[1]),origin="2024/12/31")

#minimum arrival
min_arrival_ms=NULL
for (i in levels(as.factor(sadms$YEAR))) {
	min_arrival_ms<-c(min_arrival_ms,min(yday(as.Date(sadms$DATE[sadms$YEAR==i], format="%m/%d/%y"))))
	}
mean(min_arrival_ms[1:24]) #mean min arrival from 1998-2021
min_arrival_ms[25]


#plot the data vertically
ggplot(sadms, aes(y = YEAR, x = as.Date(Month_Day, format = "%m-%d"))) +
  geom_rect(xmin = meanms-(2*sdms), xmax = meanms+(2*sdms), ymin = 1996, ymax = 2026, fill = "gray91") + 
  geom_point(col="dark gray") + 
  geom_vline(xintercept=meanms, col="black")+ 
  geom_point(aes(y=YEAR, x = as.Date(Mean_md, format = "%m-%d")), color = "black", size = 3)+
  geom_point(aes(y=YEAR, x = as.Date(Q1_md, format = "%m-%d")), color = "black", size = 2, shape=3)+
  geom_point(aes(y=YEAR, x = as.Date(Q3_md, format = "%m-%d")), color = "black", size = 2, shape=3)+
  scale_x_date(date_labels = "%m-%d", date_breaks = "20 days") +
  ylim(1998,2024)+
  labs(y = "Year", x = "Date") +
  ggtitle("MS Scout Arrival Dates") +
    theme(legend.position="none",
  	panel.background=element_rect(fill="white"), 
  	plot.margin=unit(c(1,1,1,1),"cm"), 
  	axis.title.y=element_text(margin=margin(0,20,0,0)), 
  	axis.title.x=element_text(margin=margin(20,0,0,0)),
  	axis.line=element_line(size=.5))

arrival_anomaly<-data.frame(Year = rev(c(1998:2024)), Arrival_anomaly=NA, State="MS")
for (i in 1:27) {
	arrival_anomaly$anomaly[i]<-as.POSIXlt(as.Date(meanls[[i]]), format = "%y%m%d")$yday-as.POSIXlt(meanms, format = "%y%m%d")$yday 
}
write.csv(arrival_anomaly,"MS_anomalous_arrivals.csv", row.names=FALSE)

#####################################


#AR
sadar <- read.csv("Scout_arrivals/AR_arrivals.csv")
sadar$NAME <- NULL

#select adults only
sadar<- sadar[sadar$AGE == "Adult",]

#remove dates after May 31 because birds should arrive by then
nrow(sadar[grep("^6/", sadar$DATE),]) #3
sadar<-sadar[-grep("^6/", sadar$DATE),]

#number of arrivals
nrow(sadar) #1544

sadar$Month_Day <- format(mdy(sadar$DATE), "%m-%d")
#getting mean, q1, q3
meanls <- list()
q1ls <- list()
q3ls <- list()
for (a in unique(sadar$YEAR)) {
  datels <- list()
  for (i in 1:nrow(sadar)) {
    year <- sadar$YEAR[i]
    if (year == a) {
      datels <- c(datels, sadar$DATE[i])
    }
  }
  mean <- as.Date(mean(mdy(unlist(datels))))
  meanls <- c(meanls, as.character(mean))
  q1 <- as.Date(quantile(as.numeric(mdy(unlist(datels))), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)[1], origin="1970-01-01")
  q1ls <- c(q1ls, as.character(q1))
  q3 <-as.Date(quantile(as.numeric(mdy(unlist(datels))), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)[3], origin="1970-01-01")
  q3ls <- c(q3ls, as.character(q3))
}

#adding means to sadar
dfmeanls <- list()
for(a in meanls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadar)){
    if(sadar$YEAR[i] == yr){
      dfmeanls[[i]] <- a
    }
  }
}
sadar$Mean <- unlist(dfmeanls)
sadar$Mean_md <- format(ymd(sadar$Mean), "%m-%d")
#adding q1 to sadar
dfq1ls <- list()
for(a in q1ls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadar)){
    if(sadar$YEAR[i] == yr){
      dfq1ls[[i]] <- a
    }
  }
}
sadar$Q1 <- unlist(dfq1ls)
sadar$Q1_md <- format(as.Date(sadar$Q1), "%m-%d")
#adding q3 to sadar
dfq3ls <- list()
for(a in q3ls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadar)){
    if(sadar$YEAR[i] == yr){
      dfq3ls[[i]] <- a
    }
  }
}
sadar$Q3 <- unlist(dfq3ls)
sadar$Q3_md <- format(as.Date(sadar$Q3), "%m-%d")

#calculate mean day of year for 1998-2021
mean(yday(unlist(meanls))[4:27])
sdar<-sd(yday(unlist(meanls))[4:27])
meanar<-as.Date(mean(yday(unlist(meanls))[4:27]),origin="2024/12/31")
ar_2022<-as.Date(mean(yday(unlist(meanls))[3]),origin="2024/12/31")
ar_2023<-as.Date(mean(yday(unlist(meanls))[2]),origin="2024/12/31")
ar_2024<-as.Date(mean(yday(unlist(meanls))[1]),origin="2024/12/31")

#minimum arrival
min_arrival_ar=NULL
for (i in levels(as.factor(sadar$YEAR))) {
	min_arrival_ar<-c(min_arrival_ar,min(yday(as.Date(sadar$DATE[sadar$YEAR==i], format="%m/%d/%y"))))
	}
mean(min_arrival_ar[1:24]) #mean min arrival from 1998-2021
min_arrival_ar[25]

#plot the data vertically
ggplot(sadar, aes(y = YEAR, x = as.Date(Month_Day, format = "%m-%d"))) +
  geom_rect(xmin = meanar-(2*sdar), xmax = meanar+(2*sdar), ymin = 1996, ymax = 2026, fill = "gray91") + 
  geom_point(col="dark gray") + 
  geom_vline(xintercept=meanar, col="black")+ 
  geom_point(aes(y=YEAR, x = as.Date(Mean_md, format = "%m-%d")), color = "black", size = 3)+
  geom_point(aes(y=YEAR, x = as.Date(Q1_md, format = "%m-%d")), color = "black", size = 2, shape=3)+
  geom_point(aes(y=YEAR, x = as.Date(Q3_md, format = "%m-%d")), color = "black", size = 2, shape=3)+
  scale_x_date(date_labels = "%m-%d", date_breaks = "20 days") +
  ylim(1998,2024)+
  labs(y = "Year", x = "Date") +
  ggtitle("AR Scout Arrival Dates") +
    theme(legend.position="none",
  	panel.background=element_rect(fill="white"), 
  	plot.margin=unit(c(1,1,1,1),"cm"), 
  	axis.title.y=element_text(margin=margin(0,20,0,0)), 
  	axis.title.x=element_text(margin=margin(20,0,0,0)),
  	axis.line=element_line(size=.5))


arrival_anomaly<-data.frame(Year = rev(c(1998:2024)), Arrival_anomaly=NA, State="AR")
for (i in 1:27) {
	arrival_anomaly$anomaly[i]<-as.POSIXlt(as.Date(meanls[[i]]), format = "%y%m%d")$yday-as.POSIXlt(meanar, format = "%y%m%d")$yday 
}
write.csv(arrival_anomaly,"AR_anomalous_arrivals.csv", row.names=FALSE)

###################################################

#OK
sadok <- read.csv("Scout_arrivals/OK_arrivals.csv")
sadok$NAME <- NULL

#select adults only
sadok<- sadok[sadok$AGE == "Adult",]

#remove dates after May 31 because birds should arrive by then
nrow(sadok[grep("^6/", sadok$DATE),]) #0

#number of arrivals
nrow(sadok) #1946

sadok$Month_Day <- format(mdy(sadok$DATE), "%m-%d")
#getting mean, q1, q3
meanls <- list()
q1ls <- list()
q3ls <- list()
for (a in unique(sadok$YEAR)) {
  datels <- list()
  for (i in 1:nrow(sadok)) {
    year <- sadok$YEAR[i]
    if (year == a) {
      datels <- c(datels, sadok$DATE[i])
    }
  }
  mean <- as.Date(mean(mdy(unlist(datels))))
  meanls <- c(meanls, as.character(mean))
  q1 <- as.Date(quantile(as.numeric(mdy(unlist(datels))), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)[1], origin="1970-01-01")
  q1ls <- c(q1ls, as.character(q1))
  q3 <-as.Date(quantile(as.numeric(mdy(unlist(datels))), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)[3], origin="1970-01-01")
  q3ls <- c(q3ls, as.character(q3))
}

#adding means to sadok
dfmeanls <- list()
for(a in meanls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadok)){
    if(sadok$YEAR[i] == yr){
      dfmeanls[[i]] <- a
    }
  }
}
sadok$Mean <- unlist(dfmeanls)
sadok$Mean_md <- format(ymd(sadok$Mean), "%m-%d")
#adding q1 to sadok
dfq1ls <- list()
for(a in q1ls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadok)){
    if(sadok$YEAR[i] == yr){
      dfq1ls[[i]] <- a
    }
  }
}
sadok$Q1 <- unlist(dfq1ls)
sadok$Q1_md <- format(as.Date(sadok$Q1), "%m-%d")
#adding q3 to sadok
dfq3ls <- list()
for(a in q3ls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadok)){
    if(sadok$YEAR[i] == yr){
      dfq3ls[[i]] <- a
    }
  }
}
sadok$Q3 <- unlist(dfq3ls)
sadok$Q3_md <- format(as.Date(sadok$Q3), "%m-%d")

#calculate mean day of year for 1998-2021
mean(yday(unlist(meanls))[4:27])
sdok<-sd(yday(unlist(meanls))[4:27])
meanok<-as.Date(mean(yday(unlist(meanls))[4:27]),origin="2024/12/31")
ok_2022<-as.Date(mean(yday(unlist(meanls))[3]),origin="2024/12/31")
ok_2023<-as.Date(mean(yday(unlist(meanls))[2]),origin="2024/12/31")
ok_2024<-as.Date(mean(yday(unlist(meanls))[1]),origin="2024/12/31")

#minimum arrival
min_arrival_ok=NULL
for (i in levels(as.factor(sadok$YEAR))) {
	min_arrival_ok<-c(min_arrival_ok,min(yday(as.Date(sadok$DATE[sadok$YEAR==i], format="%m/%d/%y"))))
	}
mean(min_arrival_ok[1:24]) #mean min arrival from 1998-2021
min_arrival_ok[25]

#plot the data vertically
ggplot(sadok, aes(y = YEAR, x = as.Date(Month_Day, format = "%m-%d"))) +
  geom_rect(xmin = meanok-(2*sdok), xmax = meanok+(2*sdok), ymin = 1996, ymax = 2026, fill = "gray91") + 
  geom_point(col="dark gray") + 
  geom_vline(xintercept=meanok, col="black")+ 
  geom_point(aes(y=YEAR, x = as.Date(Mean_md, format = "%m-%d")), color = "black", size = 3)+
  geom_point(aes(y=YEAR, x = as.Date(Q1_md, format = "%m-%d")), color = "black", size = 2, shape=3)+
  geom_point(aes(y=YEAR, x = as.Date(Q3_md, format = "%m-%d")), color = "black", size = 2, shape=3)+
  scale_x_date(date_labels = "%m-%d", date_breaks = "20 days") +
  ylim(1998,2024)+
  labs(y = "Year", x = "Date") +
  ggtitle("OK Scout Arrival Dates") +
    theme(legend.position="none",
  	panel.background=element_rect(fill="white"), 
  	plot.margin=unit(c(1,1,1,1),"cm"), 
  	axis.title.y=element_text(margin=margin(0,20,0,0)), 
  	axis.title.x=element_text(margin=margin(20,0,0,0)),
  	axis.line=element_line(size=.5))

arrival_anomaly<-data.frame(Year = rev(c(1998:2024)), Arrival_anomaly=NA, State="OK")
for (i in 1:27) {
	arrival_anomaly$anomaly[i]<-as.POSIXlt(as.Date(meanls[[i]]), format = "%y%m%d")$yday-as.POSIXlt(meanok, format = "%y%m%d")$yday 
}
write.csv(arrival_anomaly,"OK_anomalous_arrivals.csv", row.names=FALSE)

##################################################

#AL
sadal <- read.csv("Scout_arrivals/AL_arrivals.csv")
sadal$NAME <- NULL

#select adults only
sadal<- sadal[sadal$AGE == "Adult",]

#remove dats after May 31 because birds should arrive by then
nrow(sadal[grep("^6/", sadal$DATE),]) #0

#number of arrivals
nrow(sadal) #2314

sadal$Month_Day <- format(mdy(sadal$DATE), "%m-%d")
#getting mean, q1, q3
meanls <- list()
q1ls <- list()
q3ls <- list()
for (a in unique(sadal$YEAR)) {
  datels <- list()
  for (i in 1:nrow(sadal)) {
    year <- sadal$YEAR[i]
    if (year == a) {
      datels <- c(datels, sadal$DATE[i])
    }
  }
  mean <- as.Date(mean(mdy(unlist(datels))))
  meanls <- c(meanls, as.character(mean))
  q1 <- as.Date(quantile(as.numeric(mdy(unlist(datels))), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)[1], origin="1970-01-01")
  q1ls <- c(q1ls, as.character(q1))
  q3 <-as.Date(quantile(as.numeric(mdy(unlist(datels))), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)[3], origin="1970-01-01")
  q3ls <- c(q3ls, as.character(q3))
}

#adding means to sadal
dfmeanls <- list()
for(a in meanls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadal)){
    if(sadal$YEAR[i] == yr){
      dfmeanls[[i]] <- a
    }
  }
}
sadal$Mean <- unlist(dfmeanls)
sadal$Mean_md <- format(ymd(sadal$Mean), "%m-%d")
#adding q1 to sadal
dfq1ls <- list()
for(a in q1ls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadal)){
    if(sadal$YEAR[i] == yr){
      dfq1ls[[i]] <- a
    }
  }
}
sadal$Q1 <- unlist(dfq1ls)
sadal$Q1_md <- format(as.Date(sadal$Q1), "%m-%d")
#adding q3 to sadal
dfq3ls <- list()
for(a in q3ls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadal)){
    if(sadal$YEAR[i] == yr){
      dfq3ls[[i]] <- a
    }
  }
}
sadal$Q3 <- unlist(dfq3ls)
sadal$Q3_md <- format(as.Date(sadal$Q3), "%m-%d")

#calculate mean day of year for 1998-2021
mean(yday(unlist(meanls))[4:27])
sdal<-sd(yday(unlist(meanls))[4:27])
meanal<-as.Date(mean(yday(unlist(meanls))[4:27]),origin="2024/12/31")
al_2022<-as.Date(mean(yday(unlist(meanls))[3]),origin="2024/12/31")
al_2023<-as.Date(mean(yday(unlist(meanls))[2]),origin="2024/12/31")
al_2024<-as.Date(mean(yday(unlist(meanls))[1]),origin="2024/12/31")

#minimum arrival
min_arrival_al=NULL
for (i in levels(as.factor(sadal$YEAR))) {
	min_arrival_al<-c(min_arrival_al,min(yday(as.Date(sadal$DATE[sadal$YEAR==i], format="%m/%d/%y"))))
	}
mean(min_arrival_al[1:24]) #mean min arrival from 1998-2021
min_arrival_al[25]


#plot the data vertically
ggplot(sadal, aes(y = YEAR, x = as.Date(Month_Day, format = "%m-%d"))) +
  geom_rect(xmin = meanal-(2*sdal), xmax = meanal+(2*sdal), ymin = 1996, ymax = 2026, fill = "gray91") + 
  geom_point(col="dark gray") + 
  geom_vline(xintercept=meanal, col="black")+ 
  geom_point(aes(y=YEAR, x = as.Date(Mean_md, format = "%m-%d")), color = "black", size = 3)+
  geom_point(aes(y=YEAR, x = as.Date(Q1_md, format = "%m-%d")), color = "black", size = 2, shape=3)+
  geom_point(aes(y=YEAR, x = as.Date(Q3_md, format = "%m-%d")), color = "black", size = 2, shape=3)+
  scale_x_date(date_labels = "%m-%d", date_breaks = "20 days") +
  labs(y = "Year", x = "Date") +
  ggtitle("AL Scout Arrival Dates") +
    theme(legend.position="none",
  	panel.background=element_rect(fill="white"), 
  	plot.margin=unit(c(1,1,1,1),"cm"), 
  	axis.title.y=element_text(margin=margin(0,20,0,0)), 
  	axis.title.x=element_text(margin=margin(20,0,0,0)),
  	axis.line=element_line(size=.5))

arrival_anomaly<-data.frame(Year = rev(c(1998:2024)), Arrival_anomaly=NA, State="AL")
for (i in 1:27) {
	arrival_anomaly$anomaly[i]<-as.POSIXlt(as.Date(meanls[[i]]), format = "%y%m%d")$yday-as.POSIXlt(meanal, format = "%y%m%d")$yday 
}
write.csv(arrival_anomaly,"AL_anomalous_arrivals.csv", row.names=FALSE)

###################################################

#GA
sadga <- read.csv("Scout_arrivals/GA_arrivals.csv")
sadga$NAME <- NULL

#select adults only
sadga<- sadga[sadga$AGE == "Adult",]

#remove dates after May 31 because birds should arrive by then
nrow(sadga[grep("^6/", sadga$DATE),]) #2
sadga<-sadga[-grep("^6/", sadga$DATE),]

#number of arrivals
nrow(sadga) #1773

sadga$Month_Day <- format(mdy(sadga$DATE), "%m-%d")
#getting mean, q1, q3
meanls <- list()
q1ls <- list()
q3ls <- list()
for (a in unique(sadga$YEAR)) {
  datels <- list()
  for (i in 1:nrow(sadga)) {
    year <- sadga$YEAR[i]
    if (year == a) {
      datels <- c(datels, sadga$DATE[i])
    }
  }
  mean <- as.Date(mean(mdy(unlist(datels))))
  meanls <- c(meanls, as.character(mean))
  q1 <- as.Date(quantile(as.numeric(mdy(unlist(datels))), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)[1], origin="1970-01-01")
  q1ls <- c(q1ls, as.character(q1))
  q3 <-as.Date(quantile(as.numeric(mdy(unlist(datels))), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)[3], origin="1970-01-01")
  q3ls <- c(q3ls, as.character(q3))
}

#adding means to sadga
dfmeanls <- list()
for(a in meanls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadga)){
    if(sadga$YEAR[i] == yr){
      dfmeanls[[i]] <- a
    }
  }
}
sadga$Mean <- unlist(dfmeanls)
sadga$Mean_md <- format(ymd(sadga$Mean), "%m-%d")
#adding q1 to sadga
dfq1ls <- list()
for(a in q1ls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadga)){
    if(sadga$YEAR[i] == yr){
      dfq1ls[[i]] <- a
    }
  }
}
sadga$Q1 <- unlist(dfq1ls)
sadga$Q1_md <- format(as.Date(sadga$Q1), "%m-%d")
#adding q3 to sadga
dfq3ls <- list()
for(a in q3ls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadga)){
    if(sadga$YEAR[i] == yr){
      dfq3ls[[i]] <- a
    }
  }
}
sadga$Q3 <- unlist(dfq3ls)
sadga$Q3_md <- format(as.Date(sadga$Q3), "%m-%d")

#calculate mean day of year for 1998-2021
mean(yday(unlist(meanls))[4:27])
sdga<-sd(yday(unlist(meanls))[4:27])
meanga<-as.Date(mean(yday(unlist(meanls))[4:27]),origin="2024/12/31")
ga_2022<-as.Date(mean(yday(unlist(meanls))[3]),origin="2024/12/31")
ga_2023<-as.Date(mean(yday(unlist(meanls))[2]),origin="2024/12/31")
ga_2024<-as.Date(mean(yday(unlist(meanls))[1]),origin="2024/12/31")

#minimum arrival
min_arrival_ga=NULL
for (i in levels(as.factor(sadga$YEAR))) {
	min_arrival_ga<-c(min_arrival_ga,min(yday(as.Date(sadga$DATE[sadga$YEAR==i], format="%m/%d/%y"))))
	}
mean(min_arrival_ga[1:24]) #mean min arrival from 1998-2021
min_arrival_ga[25]

#plot the data vertically
ggplot(sadga, aes(y = YEAR, x = as.Date(Month_Day, format = "%m-%d"))) +
  geom_rect(xmin = meanga-(2*sdga), xmax = meanga+(2*sdga), ymin = 1996, ymax = 2026, fill = "gray91") + 
  geom_point(col="dark gray") + 
  geom_vline(xintercept=meanga, col="black")+ 
  geom_point(aes(y=YEAR, x = as.Date(Mean_md, format = "%m-%d")), color = "black", size = 3)+
  geom_point(aes(y=YEAR, x = as.Date(Q1_md, format = "%m-%d")), color = "black", size = 2, shape=3)+
  geom_point(aes(y=YEAR, x = as.Date(Q3_md, format = "%m-%d")), color = "black", size = 2, shape=3)+
  scale_x_date(date_labels = "%m-%d", date_breaks = "20 days") +
  labs(y = "Year", x = "Date") +
  ggtitle("GA Scout Arrival Dates") +
    theme(legend.position="none",
  	panel.background=element_rect(fill="white"), 
  	plot.margin=unit(c(1,1,1,1),"cm"), 
  	axis.title.y=element_text(margin=margin(0,20,0,0)), 
  	axis.title.x=element_text(margin=margin(20,0,0,0)),
  	axis.line=element_line(size=.5))


arrival_anomaly<-data.frame(Year = rev(c(1998:2024)), Arrival_anomaly=NA, State="GA")
for (i in 1:27) {
	arrival_anomaly$anomaly[i]<-as.POSIXlt(as.Date(meanls[[i]]), format = "%y%m%d")$yday-as.POSIXlt(meanga, format = "%y%m%d")$yday 
}
write.csv(arrival_anomaly,"GA_anomalous_arrivals.csv", row.names=FALSE)

##################################################

#FL
sadfl <- read.csv("Scout_arrivals/FL_arrivals.csv")
sadfl$NAME <- NULL

#select adults only
sadfl<- sadfl[sadfl$AGE == "Adult",]

#remove dates after May 31 because birds should arrive by then
nrow(sadfl[grep("^6/", sadfl$DATE),]) #2
sadfl<-sadfl[-grep("^6/", sadfl$DATE),]

#number of arrivals
nrow(sadfl) #3569

sadfl$Month_Day <- format(mdy(sadfl$DATE), "%m-%d")
#getting mean, q1, q3
meanls <- list()
q1ls <- list()
q3ls <- list()
for (a in unique(sadfl$YEAR)) {
  datels <- list()
  for (i in 1:nrow(sadfl)) {
    year <- sadfl$YEAR[i]
    if (year == a) {
      datels <- c(datels, sadfl$DATE[i])
    }
  }
  mean <- as.Date(mean(mdy(unlist(datels))))
  meanls <- c(meanls, as.character(mean))
  q1 <- as.Date(quantile(as.numeric(mdy(unlist(datels))), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)[1], origin="1970-01-01")
  q1ls <- c(q1ls, as.character(q1))
  q3 <-as.Date(quantile(as.numeric(mdy(unlist(datels))), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)[3], origin="1970-01-01")
  q3ls <- c(q3ls, as.character(q3))
}

#adding means to sadfl
dfmeanls <- list()
for(a in meanls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadfl)){
    if(sadfl$YEAR[i] == yr){
      dfmeanls[[i]] <- a
    }
  }
}
sadfl$Mean <- unlist(dfmeanls)
sadfl$Mean_md <- format(ymd(sadfl$Mean), "%m-%d")
#adding q1 to sadfl
dfq1ls <- list()
for(a in q1ls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadfl)){
    if(sadfl$YEAR[i] == yr){
      dfq1ls[[i]] <- a
    }
  }
}
sadfl$Q1 <- unlist(dfq1ls)
sadfl$Q1_md <- format(as.Date(sadfl$Q1), "%m-%d")
#adding q3 to sadfl
dfq3ls <- list()
for(a in q3ls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadfl)){
    if(sadfl$YEAR[i] == yr){
      dfq3ls[[i]] <- a
    }
  }
}
sadfl$Q3 <- unlist(dfq3ls)
sadfl$Q3_md <- format(as.Date(sadfl$Q3), "%m-%d")

#calculate mean day of year for 1998-2021
mean(yday(unlist(meanls))[4:27])
sdfl<-sd(yday(unlist(meanls))[4:27])
meanfl<-as.Date(mean(yday(unlist(meanls))[4:27]),origin="2024/12/31")
fl_2022<-as.Date(mean(yday(unlist(meanls))[3]),origin="2024/12/31")
fl_2023<-as.Date(mean(yday(unlist(meanls))[2]),origin="2024/12/31")
fl_2024<-as.Date(mean(yday(unlist(meanls))[1]),origin="2024/12/31")


#minimum arrival
min_arrival_fl=NULL
for (i in levels(as.factor(sadfl$YEAR))) {
	min_arrival_fl<-c(min_arrival_fl,min(yday(as.Date(sadfl$DATE[sadfl$YEAR==i], format="%m/%d/%y"))))
	}
#except Dec arrivals don't work
min_arrival_fl[2] <- -9 #1999 min arrival is Dec 23, 1998
min_arrival_fl[3] <- -12 #2000 min arrival is Dec 20, 1999
min_arrival_fl[5] <- -9 #2002 min arrival is Dec 23, 2001
min_arrival_fl[6] <- -20 #2003 min arrival is Dec 12, 2002
min_arrival_fl[7] <- -8 #2004 min arrival is Dec 24, 2003
min_arrival_fl[8] <- -7 #2005 min arrival is Dec 25, 2004
min_arrival_fl[9] <- -4 #2006 min arrival is Dec 28, 2005
min_arrival_fl[10] <- -21 #2007 min arrival is Dec 11, 2006
min_arrival_fl[11] <- -10 #2008 min arrival is Dec 22, 2007
min_arrival_fl[12] <- -11 #2009 min arrival is Dec 21, 2008
min_arrival_fl[13] <- -9 #2010 min arrival is Dec 23, 2009
min_arrival_fl[16] <- -13 #2013 min arrival is Dec 19, 2012
min_arrival_fl[17] <- -4 #2014 min arrival is Dec 28, 2013
min_arrival_fl[18] <- -9 #2015 min arrival is Dec 23, 2014
min_arrival_fl[19] <- -2 #2016 min arrival is Dec 30, 2015
min_arrival_fl[20] <- -8 #2017 min arrival is Dec 24, 2016
min_arrival_fl[22]  <- -3 #2019 min arrival is Dec 29, 2018
min_arrival_fl[23] <- -5 #2020 min arrival is Dec 27, 2019
min_arrival_fl[24] <- -9 #2021 min arrival is Dec 23, 2020
min_arrival_fl[25] <- -5 #2022 min arrival is Dec 27, 2021
min_arrival_fl[26] <- -15 #2022 min arrival is Dec 17, 2022
min_arrival_fl[27] <- -15 #2023 min arrival is Dec 17, 2023

mean(min_arrival_fl[1:24]) #mean min arrival from 1998-2021

#plot the data vertically
ggplot(sadfl[-grep("^12/", sadfl$DATE),], aes(y = YEAR, x = as.Date(Month_Day, format = "%m-%d"))) +
  geom_rect(xmin = meanfl-(2*sdfl), xmax = meanfl+(2*sdfl), ymin = 1996, ymax = 2026, fill = "gray91") + 
  geom_point(col="dark gray") + 
  geom_point(data=sadfl[grep("^12/", sadfl$DATE),], aes(y = YEAR, x = as.Date(Month_Day, format = "%m-%d")-366), col="gray") + #Dec dates
  geom_vline(xintercept=meanfl, col="black")+ 
  geom_point(aes(y=YEAR, x = as.Date(Mean_md, format = "%m-%d")), color = "black", size = 3)+
  geom_point(aes(y=YEAR, x = as.Date(Q1_md, format = "%m-%d")), color = "black", size = 2, shape=3)+
  geom_point(aes(y=YEAR, x = as.Date(Q3_md, format = "%m-%d")), color = "black", size = 2, shape=3)+
  scale_x_date(date_labels = "%m-%d", date_breaks = "20 days") +
  ylim(1998,2024) +
  labs(y = "Year", x = "Date") +
  ggtitle("FL Scout Arrival Dates") +
    theme(legend.position="none",
  	panel.background=element_rect(fill="white"), 
  	plot.margin=unit(c(1,1,1,1),"cm"), 
  	axis.title.y=element_text(margin=margin(0,20,0,0)), 
  	axis.title.x=element_text(margin=margin(20,0,0,0)),
  	axis.line=element_line(size=.5))


arrival_anomaly<-data.frame(Year = rev(c(1998:2024)), Arrival_anomaly=NA, State="FL")
for (i in 1:27) {
	arrival_anomaly$anomaly[i]<-as.POSIXlt(as.Date(meanls[[i]]), format = "%y%m%d")$yday-as.POSIXlt(meanfl, format = "%y%m%d")$yday 
}
write.csv(arrival_anomaly,"FL_anomalous_arrivals.csv", row.names=FALSE)

#####################################################
  
#SC
sadsc <- read.csv("Scout_arrivals/SC_arrivals.csv")
sadsc$NAME <- NULL

#select adults only
sadsc<- sadsc[sadsc$AGE == "Adult",]

#remove dates after May 31 because birds should arrive by then
nrow(sadsc[grep("^6/", sadsc$DATE),]) #1
sadsc<-sadsc[-grep("^6/", sadsc$DATE),]

#number of arrivals
nrow(sadsc) #1732

sadsc$Month_Day <- format(mdy(sadsc$DATE), "%m-%d")
#getting mean, q1, q3
meanls <- list()
q1ls <- list()
q3ls <- list()
for (a in unique(sadsc$YEAR)) {
  datels <- list()
  for (i in 1:nrow(sadsc)) {
    year <- sadsc$YEAR[i]
    if (year == a) {
      datels <- c(datels, sadsc$DATE[i])
    }
  }
  mean <- as.Date(mean(mdy(unlist(datels))))
  meanls <- c(meanls, as.character(mean))
  q1 <- as.Date(quantile(as.numeric(mdy(unlist(datels))), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)[1], origin="1970-01-01")
  q1ls <- c(q1ls, as.character(q1))
  q3 <-as.Date(quantile(as.numeric(mdy(unlist(datels))), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)[3], origin="1970-01-01")
  q3ls <- c(q3ls, as.character(q3))
}

#adding means to sadsc
dfmeanls <- list()
for(a in meanls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadsc)){
    if(sadsc$YEAR[i] == yr){
      dfmeanls[[i]] <- a
    }
  }
}
sadsc$Mean <- unlist(dfmeanls)
sadsc$Mean_md <- format(ymd(sadsc$Mean), "%m-%d")
#adding q1 to sadsc
dfq1ls <- list()
for(a in q1ls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadsc)){
    if(sadsc$YEAR[i] == yr){
      dfq1ls[[i]] <- a
    }
  }
}
sadsc$Q1 <- unlist(dfq1ls)
sadsc$Q1_md <- format(as.Date(sadsc$Q1), "%m-%d")
#adding q3 to sadsc
dfq3ls <- list()
for(a in q3ls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadsc)){
    if(sadsc$YEAR[i] == yr){
      dfq3ls[[i]] <- a
    }
  }
}
sadsc$Q3 <- unlist(dfq3ls)
sadsc$Q3_md <- format(as.Date(sadsc$Q3), "%m-%d")

#calculate mean day of year for 1998-2021
mean(yday(unlist(meanls))[4:27])
sdsc<-sd(yday(unlist(meanls))[4:27])
meansc<-as.Date(mean(yday(unlist(meanls))[4:27]),origin="2024/12/31")
sc_2022<-as.Date(mean(yday(unlist(meanls))[3]),origin="2024/12/31")
sc_2023<-as.Date(mean(yday(unlist(meanls))[2]),origin="2024/12/31")
sc_2024<-as.Date(mean(yday(unlist(meanls))[1]),origin="2024/12/31")


#minimum arrival
min_arrival_sc=NULL
for (i in levels(as.factor(sadsc$YEAR))) {
	min_arrival_sc<-c(min_arrival_sc,min(yday(as.Date(sadsc$DATE[sadsc$YEAR==i], format="%m/%d/%y"))))
	}
mean(min_arrival_sc[1:24]) #mean min arrival from 1998-2021
min_arrival_sc[25]

#plot the data vertically
ggplot(sadsc, aes(y = YEAR, x = as.Date(Month_Day, format = "%m-%d"))) +
  geom_rect(xmin = meansc-(2*sdsc), xmax = meansc+(2*sdsc), ymin = 1996, ymax = 2026, fill = "gray91") + 
  geom_point(col="dark gray") + 
  geom_vline(xintercept=meansc, col="black")+ 
  geom_point(aes(y=YEAR, x = as.Date(Mean_md, format = "%m-%d")), color = "black", size = 3)+
  geom_point(aes(y=YEAR, x = as.Date(Q1_md, format = "%m-%d")), color = "black", size = 2, shape=3)+
  geom_point(aes(y=YEAR, x = as.Date(Q3_md, format = "%m-%d")), color = "black", size = 2, shape=3)+
  scale_x_date(date_labels = "%m-%d", date_breaks = "20 days") +
  ylim(1998,2024) +
  labs(y = "Year", x = "Date") +
  ggtitle("SC Scout Arrival Dates") +
    theme(legend.position="none",
  	panel.background=element_rect(fill="white"), 
  	plot.margin=unit(c(1,1,1,1),"cm"), 
  	axis.title.y=element_text(margin=margin(0,20,0,0)), 
  	axis.title.x=element_text(margin=margin(20,0,0,0)),
  	axis.line=element_line(size=.5))  


arrival_anomaly<-data.frame(Year = rev(c(1998:2024)), Arrival_anomaly=NA, State="SC")
for (i in 1:27) {
	arrival_anomaly$anomaly[i]<-as.POSIXlt(as.Date(meanls[[i]]), format = "%y%m%d")$yday-as.POSIXlt(meansc, format = "%y%m%d")$yday 
}
write.csv(arrival_anomaly,"SC_anomalous_arrivals.csv", row.names=FALSE)
  
####################################################### 
 
#TN
sadtn <- read.csv("Scout_arrivals/TN_arrivals.csv")
sadtn$NAME <- NULL

#select adults only
sadtn<- sadtn[sadtn$AGE == "Adult",]

#remove dates after May 31 because birds should arrive by then
nrow(sadtn[grep("^6/", sadtn$DATE),]) #1
sadtn<-sadtn[-grep("^6/", sadtn$DATE),]

#number of arrivals
nrow(sadtn) #2266

sadtn$Month_Day <- format(mdy(sadtn$DATE), "%m-%d")
#getting mean, q1, q3
meanls <- list()
q1ls <- list()
q3ls <- list()
for (a in unique(sadtn$YEAR)) {
  datels <- list()
  for (i in 1:nrow(sadtn)) {
    year <- sadtn$YEAR[i]
    if (year == a) {
      datels <- c(datels, sadtn$DATE[i])
    }
  }
  mean <- as.Date(mean(mdy(unlist(datels))))
  meanls <- c(meanls, as.character(mean))
  q1 <- as.Date(quantile(as.numeric(mdy(unlist(datels))), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)[1], origin="1970-01-01")
  q1ls <- c(q1ls, as.character(q1))
  q3 <-as.Date(quantile(as.numeric(mdy(unlist(datels))), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)[3], origin="1970-01-01")
  q3ls <- c(q3ls, as.character(q3))
}

#adding means to sadtn
dfmeanls <- list()
for(a in meanls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadtn)){
    if(sadtn$YEAR[i] == yr){
      dfmeanls[[i]] <- a
    }
  }
}
sadtn$Mean <- unlist(dfmeanls)
sadtn$Mean_md <- format(ymd(sadtn$Mean), "%m-%d")
#adding q1 to sadtn
dfq1ls <- list()
for(a in q1ls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadtn)){
    if(sadtn$YEAR[i] == yr){
      dfq1ls[[i]] <- a
    }
  }
}
sadtn$Q1 <- unlist(dfq1ls)
sadtn$Q1_md <- format(as.Date(sadtn$Q1), "%m-%d")
#adding q3 to sadtn
dfq3ls <- list()
for(a in q3ls){
  yr <- format(as.Date(a), "%Y")
  for(i in 1:nrow(sadtn)){
    if(sadtn$YEAR[i] == yr){
      dfq3ls[[i]] <- a
    }
  }
}
sadtn$Q3 <- unlist(dfq3ls)
sadtn$Q3_md <- format(as.Date(sadtn$Q3), "%m-%d")

#calculate mean day of year for 1998-2021
mean(yday(unlist(meanls))[4:27])
sdtn<-sd(yday(unlist(meanls))[4:27])
meantn<-as.Date(mean(yday(unlist(meanls))[4:27]),origin="2024/12/31")
tn_2022<-as.Date(mean(yday(unlist(meanls))[3]),origin="2024/12/31")
tn_2023<-as.Date(mean(yday(unlist(meanls))[2]),origin="2024/12/31")
tn_2024<-as.Date(mean(yday(unlist(meanls))[1]),origin="2024/12/31")

#minimum arrival
min_arrival_tn=NULL
for (i in levels(as.factor(sadtn$YEAR))) {
	min_arrival_tn<-c(min_arrival_tn,min(yday(as.Date(sadtn$DATE[sadtn$YEAR==i], format="%m/%d/%y"))))
	}
mean(min_arrival_tn[1:24]) #mean min arrival from 1998-2021
min_arrival_tn[25]

#plot the data vertically
ggplot(sadtn, aes(y = YEAR, x = as.Date(Month_Day, format = "%m-%d"))) +
  geom_rect(xmin = meantn-(2*sdtn), xmax = meantn+(2*sdtn), ymin = 1996, ymax = 2026, fill = "gray91") + 
  geom_point(col="dark gray") + 
  geom_vline(xintercept=meantn, col="black")+ 
  geom_point(aes(y=YEAR, x = as.Date(Mean_md, format = "%m-%d")), color = "black", size = 3)+
  geom_point(aes(y=YEAR, x = as.Date(Q1_md, format = "%m-%d")), color = "black", size = 2, shape=3)+
  geom_point(aes(y=YEAR, x = as.Date(Q3_md, format = "%m-%d")), color = "black", size = 2, shape=3)+
  scale_x_date(date_labels = "%m-%d", date_breaks = "20 days") +
  ylim(1998,2024) +
  labs(y = "Year", x = "Date") +
  ggtitle("TN Scout Arrival Dates") +
    theme(legend.position="none",
  	panel.background=element_rect(fill="white"), 
  	plot.margin=unit(c(1,1,1,1),"cm"), 
  	axis.title.y=element_text(margin=margin(0,20,0,0)), 
  	axis.title.x=element_text(margin=margin(20,0,0,0)),
  	axis.line=element_line(size=.5))  
  	

arrival_anomaly<-data.frame(Year = rev(c(1998:2024)), Arrival_anomaly=NA, State="TN")
for (i in 1:27) {
	arrival_anomaly$anomaly[i]<-as.POSIXlt(as.Date(meanls[[i]]), format = "%y%m%d")$yday-as.POSIXlt(meantn, format = "%y%m%d")$yday 
}
write.csv(arrival_anomaly,"TN_anomalous_arrivals.csv", row.names=FALSE)

######################################################

#############
#plot deviations from the mean for 2022 by state (Figure 2c)
devs<-data.frame(states=rep(factor(c("LA", "TX", "MS", "AL", "AR", "TN", "OK", "GA", "SC", "FL"), c("LA", "TX", "MS", "AL", "AR", "TN", "OK", "GA", "SC", "FL")),3),
devs= c(as.numeric(la_2022-meanla),as.numeric(tx_2022-meantx),as.numeric(ms_2022-meanms),as.numeric(al_2022-meanal),as.numeric(ar_2022-meanar),as.numeric(tn_2022-meantn),as.numeric(ok_2022-meanok),as.numeric(ga_2022-meanga),as.numeric(sc_2022-meansc),as.numeric(fl_2022-meanfl), as.numeric(la_2023-meanla),as.numeric(tx_2023-meantx),as.numeric(ms_2023-meanms),as.numeric(al_2023-meanal),as.numeric(ar_2023-meanar),as.numeric(tn_2023-meantn),as.numeric(ok_2023-meanok),as.numeric(ga_2023-meanga),as.numeric(sc_2023-meansc),as.numeric(fl_2023-meanfl),as.numeric(la_2024-meanla),as.numeric(tx_2024-meantx),as.numeric(ms_2024-meanms),as.numeric(al_2024-meanal),as.numeric(ar_2024-meanar),as.numeric(tn_2024-meantn),as.numeric(ok_2024-meanok),as.numeric(ga_2024-meanga),as.numeric(sc_2024-meansc),as.numeric(fl_2024-meanfl)),year=c(rep(2022,10),rep(2023,10),rep(2024,10)))


ggplot(devs, aes(x=states, y=devs, group=year, fill=factor(year))) + 
	geom_col(position = "dodge") +
	geom_hline(yintercept=0)+
	scale_fill_grey(start = 0.2, end = 0.8) +
	labs(x = "State", y = "Deviation from the Long-term Mean (days)") +
	theme(panel.background=element_rect(fill="white"), plot.margin=unit(c(1,1,1,1),"cm"), 
  	axis.title.y=element_text(size = 14, margin=margin(0,10,0,0)), 
  	axis.title.x=element_text(size = 14, margin=margin(20,0,0,0)),
  	axis.text.x = element_text(size = 12), 
  	axis.text.y = element_text(size = 12),
  	axis.line=element_line(size=1))
 

