#R script to generate Figure 4a and associated analysis regarding anomalous arrivals

library(climwin)
library(lme4)
library(MuMIn)
library(nlme)
library(MASS)
library(ggplot2)
library(RColorBrewer)

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

#Arrival ~ temp anomaly during winter months; state not significant as random effect
mod1<-lm(Arrival_anomaly~Anomaly_C, subset=Year<2021,  data=all_Feb); summary(mod1); AIC(mod1)
mod3<-lm(Arrival_anomaly~Anomaly_C, subset=Year<2021, data=all_Jan); summary(mod3); AIC(mod3)
mod4<-lm(Arrival_anomaly~1, subset=Year<2021, data=all_Jan); summary(mod4); AIC(mod4)

#Model accounting for temporal autocorrelation; don't see any evidence 
mod5<-glmmPQL(Arrival_anomaly~Anomaly_C, random = ~ 1|State, family=gaussian, correlation = corCAR1(form = ~ Year|State),  subset=Year<2021, data=all_Feb); summary(mod5); AIC(mod5)

#Plot Figure 4a
pal <- hcl.colors(n = 11, palette = "Blue-Red2")
ggplot(all_Feb[all_Feb$Year<2021,], aes(y=Arrival_anomaly, x=Anomaly_C))+ 
	geom_point(size=2)+
	xlab("Temperature Anomaly (°C)")+
	ylab("Arrival Anomaly (Days)")+
	stat_smooth(method="glm", col=pal[1], lwd=2)+
	xlim(-4,7)+
	scale_linetype_manual(values=c('solid','solid'))+
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
