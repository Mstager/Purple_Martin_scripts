#R script to generate Figure 4a and associated analysis regarding anomalous arrivals

install.packages(c("lme4","MuMIn","nlme","MASS","AICcmodavg","r2glmm","ggplot2")) #if not already installed
library(lme4)
library(MuMIn)
library(nlme)
library(MASS)
library(AICcmodavg)
library(r2glmm)
library(ggplot2)

#arrival data files generated in Scout_arrival_analysis.R
OK_arr<-read.csv(file="OK_anomalous_arrivals.csv")
LA_arr<-read.csv(file="LA_anomalous_arrivals.csv")
TX_arr<-read.csv(file="TX_anomalous_arrivals.csv")
MS_arr<-read.csv(file="MS_anomalous_arrivals.csv")
AL_arr<-read.csv(file="AL_anomalous_arrivals.csv")
FL_arr<-read.csv(file="FL_anomalous_arrivals.csv")
GA_arr<-read.csv(file="GA_anomalous_arrivals.csv")
AR_arr<-read.csv(file="AR_anomalous_arrivals.csv")
SC_arr<-read.csv(file="SC_anomalous_arrivals.csv")
TN_arr<-read.csv(file="TN_anomalous_arrivals.csv")

#Merge arrival datasets
arrival<-rbind(OK_arr,LA_arr,TX_arr,MS_arr,AL_arr,FL_arr,GA_arr,AR_arr,SC_arr,TN_arr) 

#Create t-1 year
arrival$Year_tminus1<-(arrival$Year-1)


#monthly weather anomaly data for each state from the U.S. National Centers for Environmental Information database from 1998-2024
#(downloaded from: https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/divisional/mapping) and combined into a single .csv
weather<-read.csv(file="weather_anomaly.csv")

#convert degrees F to degrees C
weather$Longterm_mean_F <- weather$Mean - weather$Anomaly
weather$Longterm_mean_C <- (weather$Longterm_mean_F-32)*5/9
weather$Mean_C <- (weather$Mean-32)*5/9
weather$Anomaly_C <- weather$Mean_C-weather$Longterm_mean_C 



#merge weather and arrival datasets
Jan <- weather[weather$Month=="1",]
all_Jan<-merge(Jan, arrival, by.x=c("Year", "State"), by.y=c("Year_tminus1", "State"))
Feb <- weather[weather$Month=="2",]
all_Feb<-merge(Feb, arrival, by.x=c("Year", "State"), by.y=c("Year_tminus1", "State"))

#Mixed models with arrival ~ temp anomaly during winter months with state as random effect accounting for temporal autocorrelation; phi > 0
mod1<-glmmPQL(Arrival_anomaly~Anomaly_C, random = ~ 1|State, family=gaussian, correlation = corCAR1(form = ~ Year|State), data=all_Jan[all_Jan$Year<2021,]); summary(mod1)
mod2<-glmmPQL(Arrival_anomaly~Anomaly_C, random = ~ 1|State, family=gaussian, correlation = corCAR1(form = ~ Year|State), data=all_Feb[all_Feb$Year<2021,]); summary(mod2)

#comparing R-squared values
r2beta(mod1); r2beta(mod2)


#Plot Figure 4a
new_Feb<-all_Feb[all_Feb$Year<2021,][order(all_Feb$Anomaly_C[all_Feb$Year<2021]),]
State<-"TX"
Anomaly_C = seq(-3.555556, by = 0.04282408, length.out = nrow(new_Feb))
newdat <- data.frame(Anomaly_C = Anomaly_C, State = State)
TX.prediction<-predictSE(mod2, newdata=newdat, se.fit=TRUE)
TX.prediction$lower <- TX.prediction$fit - 1.96*TX.prediction$se.fit
TX.prediction$upper <- TX.prediction$fit + 1.96*TX.prediction$se.fit
TX.prediction$Anomaly_C<-newdat$Anomaly_C
pred <- data.frame(Anomaly_C = TX.prediction$Anomaly_C, upper = TX.prediction$upper, lower = TX.prediction$lower, fit = TX.prediction$fit)

ggplot(pred, aes(x=Anomaly_C, y=fit)) + 
	geom_point(aes(x=new_Feb$Anomaly_C, y=new_Feb$Arrival_anomaly), size=2) + 
	geom_line(col="#4A6FE3", lwd=2) +
	geom_ribbon(aes(ymin = upper, ymax = lower), linetype=3, alpha = 0.2)+
	xlab("Temperature Anomaly (°C)")+
	ylab("Arrival Anomaly (Days)")+
	theme_minimal()+
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
	legend.position = "none", 
	axis.text.x=element_text(size=14, colour="black"), 
	axis.text.y=element_text(size=14, colour="black"), 
	axis.title.x=element_text(size=22, colour="black", margin=margin(20,0,0,0)), 
	axis.title.y=element_text(size=22, colour="black", margin=margin(0,20,0,0)), 
	axis.ticks.x=element_line(colour="black"), 
	axis.ticks.y=element_line(colour="black"), 
	axis.line=element_line(size=1))


##############################################################################
install.packages(c("lubridate","rptR","data.table","dplyr","lme4","lmerTest","ggplot2"))#if not already installed

library(lubridate)
library(rptR)
library(data.table)
library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)

#Texas data
tx<-read.csv(file="TX_arrivals_edited.csv")
tx<-tx[tx$AGE!="Subadult",]

#generate yeardays
tx$date <- as.Date(tx$DATE, format="%m/%d/%Y")
tx$yday<-yday(tx$date)

#concatenate NAME and CITY
tx$site <- paste(tx$NAME, tx$CITY, sep="_")

#convert to upper case
tx$site<-toupper(tx$site)

#Investigate Temporal Trends in arrival date
tx_early<-tx[tx$yday<160,]
tx_long_term<-tx_early %>% count(site)
tx_long_term<-tx_long_term[tx_long_term$n>10,]
tx_temporal<-merge(tx_early, tx_long_term, by="site")
mod1<-lmer(yday~YEAR+(1|site), data=tx_temporal); summary(mod1)

#repeatability of yday
rpt(yday ~ (1|site), grname="site", data = tx, nboot=0, npermut=0, datatype = "Poisson")

tx_late<-tx[tx$yday<160 & tx$YEAR<2021,]
tx_test<-tx_late %>% count(site)
tx_test<-tx_test[tx_test$n>1,]
tx_rpt<-merge(tx_late, tx_test, by="site")
tx_rpt$site[order(tx_rpt$site)]

#repeatability of yday only at sites with repeated measures
rpt(yday ~ (1|site), grname="site", data = tx_rpt, nboot=0, npermut=0, datatype = "Poisson")
length(levels(as.factor(tx_rpt$site)))


#yday statistics by site
tx_site_mean<-aggregate(tx_rpt$yday, by=list(tx_rpt$site), na.rm=TRUE, mean)
tx_site_min<-aggregate(tx_rpt$yday, by=list(tx_rpt$site), na.rm=TRUE, min)
tx_site_max<-aggregate(tx_rpt$yday, by=list(tx_rpt$site), na.rm=TRUE, max)
tx_stats<-merge(tx_site_mean, tx_site_min, by="Group.1")
tx_st<-merge(tx_stats, tx_site_max, by="Group.1")
names(tx_st)<-c("site", "mean", "min", "max")
tx_rpts<-tx_st[tx_st$min<50,]
tx_rpts$range<-(tx_rpts$max-tx_rpts$min)
mean(tx_rpts$range)
sd(tx_rpts$range)

#storm estimates
tx_s<-tx[tx$YEAR>2020 & tx$YEAR<2023,]
tx_test2<-tx_s %>% count(site)
tx_test2<-tx_test2[tx_test2$n>1,]
tx_storm<-merge(tx_s, tx_test2, by="site")
tx_storm$site[order(tx_storm$site)]
tx_2021<-tx_storm[tx_storm$YEAR==2021,]
tx_2022<-tx_storm[tx_storm$YEAR==2022,]
tx_storms<-merge(tx_2021, tx_2022, by="site")
tx_early<-tx_storms[tx_storms$yday.x<52,]
length(levels(as.factor(tx_early$site)))
tx_early$dif<-(tx_early$yday.y-tx_early$yday.x)
tx_survivors<-tx_early[tx_early$dif<19,]
length(levels(as.factor(tx_survivors$site)))


#WEATHER DATA
tx_feb<-weather[weather$Month=="2" & weather$State=="TX",]
tx_mar<-weather[weather$Month=="3" & weather$State=="TX",]
tx_apr<-weather[weather$Month=="4" & weather$State=="TX",]
tx_may<-weather[weather$Month=="5" & weather$State=="TX",]
tx_jun<-weather[weather$Month=="6" & weather$State=="TX",]

#Weather Trend Analysis
tx_late<-tx[tx$yday<160 & tx$YEAR<2021,]
tx_test<-tx_late %>% count(site)
tx_test<-tx_test[tx_test$n>1,]
tx_rpt<-merge(tx_late, tx_test, by="site")
tx_rpt$site[order(tx_rpt$site)]


mod_tx_feb<-lm(Mean_C~Year, data=tx_feb); summary(mod_tx_feb)
mod_tx_mar<-lm(Mean_C~Year, data=tx_mar); summary(mod_tx_mar)
mod_tx_apr<-lm(Mean_C~Year, data=tx_apr); summary(mod_tx_apr)
mod_tx_may<-lm(Mean_C~Year, data=tx_may); summary(mod_tx_may)
mod_tx_jun<-lm(Mean_C~Year, data=tx_jun); summary(mod_tx_jun)

ci95 <- predict(mod_tx_jun, tx_jun, interval = "confidence", level = 0.95)

tx_jun <- cbind(tx_jun, ci95)

#plot Figure 4d
new<-merge(tx_temporal, tx_jun, by.x="YEAR", by.y="Year")

ggplot(data=new, aes(y=yday, x=YEAR)) + 
	geom_point(color="#ABB4E2")+
	stat_smooth(method="glm", col="#4A6FE3")+
	geom_line(aes(y=upr*7, x=YEAR), lwd=1, col="#E5A5B1", lty=2) + #upper limit of 95% CI
	geom_line(aes(y=lwr*7, x=YEAR), lwd=1, col="#E5A5B1", lty=2) +  #lower limit of 95% CI
	geom_line(aes(y=fit*7, x=YEAR), lwd=2, col="#D33F6A") + 
	geom_point(aes(y=(Mean_C*7), x=YEAR)) +
	labs(y="Arrival Date", x="Year") +
	scale_x_continuous(breaks=c(2000,2005,2010,2015,2020))+
	scale_y_continuous(limits = c(8, 175), sec.axis = sec_axis(~./7)) + 
    theme(panel.background=element_rect(fill="white"), 
  	legend.position = "none", 
	plot.margin=unit(c(1,1,1,1),"cm"), 
  	axis.text.x=element_text(size=14, colour="black"), 
	axis.text.y=element_text(size=14, colour="black"), 
	axis.title.x=element_text(size=22, colour="black", margin=margin(20,0,0,0)), 
	axis.title.y=element_text(size=22, colour="black", margin=margin(0,10,0,0)), 
	axis.line=element_line(size=1))


#Louisiana Data
la<-read.csv(file="LA_arrivals_edited.csv")
la<-la[la$AGE!="SubadultM" & la$AGE!="Subadult",]

#generate yeardays
la$date <- as.Date(la$DATE, format="%m/%d/%Y")
la$yday<-yday(la$date)

#concatenate NAME and CITY
la$site <- paste(la$NAME, la$CITY, sep="_")

#convert to upper case
la$site<-toupper(la$site)

#Investigate Temporal Trends in arrival date
la_early<-la[la$yday<160,]
la_long_term<-la_early %>% count(site)
la_long_term<-la_long_term[la_long_term$n>10,]
la_temporal<-merge(la_early, la_long_term, by="site")
mod2<-lmer(yday~YEAR+(1|site), data=la_temporal); summary(mod2)

#repeatability of yday
rpt(yday ~ (1|site), grname="site", data = la, nboot=0, npermut=0, datatype = "Poisson")



#identify repeated measures
la_late<-la[la$yday<100 & la$YEAR<2021,]
la_test<-la_late %>% count(site)
la_test<-la_test[la_test$n>1,]
la_rpt<-merge(la_late, la_test, by="site")
la_rpt$site[order(la_rpt$site)]

#repeatability of yday only at sites with repeated measures
rpt(yday ~ (1|site), grname="site", data = la_rpt, nboot=0, npermut=0, datatype = "Poisson")
length(levels(as.factor(la_rpt$site)))


#yday statistics by site
la_site_mean<-aggregate(la_rpt$yday, by=list(la_rpt$site), na.rm=TRUE, mean)
la_site_min<-aggregate(la_rpt$yday, by=list(la_rpt$site), na.rm=TRUE, min)
la_site_max<-aggregate(la_rpt$yday, by=list(la_rpt$site), na.rm=TRUE, max)
la_stats<-merge(la_site_mean, la_site_min, by="Group.1")
la_st<-merge(la_stats, la_site_max, by="Group.1")
names(la_st)<-c("site", "mean", "min", "max")
la_rpts<-la_st[la_st$min<38,]
la_rpts$range<-(la_rpts$max-la_rpts$min)
mean(la_rpts$range)
sd(la_rpts$range)

#storm estimates
la_s<-la[la$YEAR>2020 & la$YEAR<2023,]
la_test2<-la_s %>% count(site)
la_test2<-la_test2[la_test2$n>1,]
la_storm<-merge(la_s, la_test2, by="site")
la_storm$site[order(la_storm$site)]
la_2021<-la_storm[la_storm$YEAR==2021,]
la_2022<-la_storm[la_storm$YEAR==2022,]
la_storms<-merge(la_2021, la_2022, by="site")
la_early<-la_storms[la_storms$yday.x<52,]
length(levels(as.factor(la_early$site)))
la_early$dif<-(la_early$yday.y-la_early$yday.x)
la_survivors<-la_early[la_early$dif<24,]
length(levels(as.factor(la_survivors$site)))

#####Weather Data
la_feb<-weather[weather$Month=="2" & weather$State=="LA",]
la_mar<-weather[weather$Month=="3" & weather$State=="LA",]
la_apr<-weather[weather$Month=="4" & weather$State=="LA",]
la_may<-weather[weather$Month=="5" & weather$State=="LA",]
la_jun<-weather[weather$Month=="6" & weather$State=="LA",]

#Weather Trend Analysis
mod_la_feb<-lm(Mean_C~Year, data=la_feb); summary(mod_la_feb)
mod_la_mar<-lm(Mean_C~Year, data=la_mar); summary(mod_la_mar)
mod_la_apr<-lm(Mean_C~Year, data=la_apr); summary(mod_la_apr)
mod_la_may<-lm(Mean_C~Year, data=la_may); summary(mod_la_may)
mod_la_jun<-lm(Mean_C~Year, data=la_jun); summary(mod_la_jun)

ci95 <- predict(mod_la_jun, la_jun, interval = "confidence", level = 0.95)

la_jun <- cbind(la_jun, ci95)

#plot Figure S5
new_la<-merge(la_temporal, la_jun, by.x="YEAR", by.y="Year")

ggplot(data=new_la, aes(y=yday, x=YEAR)) + 
	geom_point(color="#ABB4E2")+
	stat_smooth(method="glm", col="#4A6FE3")+
	geom_line(aes(y=upr*7, x=YEAR), lwd=1, col="#E5A5B1", lty=2) + #upper limit of 95% CI
	geom_line(aes(y=lwr*7, x=YEAR), lwd=1, col="#E5A5B1", lty=2) +  #lower limit of 95% CI
	geom_line(aes(y=fit*7, x=YEAR), lwd=2, col="#D33F6A") + 
	geom_point(aes(y=(Mean_C*7), x=YEAR)) +
	labs(y="Arrival Date", x="Year") +
	scale_x_continuous(breaks=c(2000,2005,2010,2015,2020))+
	scale_y_continuous(limits = c(8, 175), sec.axis = sec_axis(~./7)) + 
    theme(panel.background=element_rect(fill="white"), 
  	legend.position = "none", 
	plot.margin=unit(c(1,1,1,1),"cm"), 
  	axis.text.x=element_text(size=14, colour="black"), 
	axis.text.y=element_text(size=14, colour="black"), 
	axis.title.x=element_text(size=22, colour="black", margin=margin(20,0,0,0)), 
	axis.title.y=element_text(size=22, colour="black", margin=margin(0,10,0,0)), 
	axis.line=element_line(size=1))
