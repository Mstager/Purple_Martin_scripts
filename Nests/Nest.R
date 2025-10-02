#PMCA Nest data

install.packages(c("dplyr","lubridate","broom","ggplot2")) #if not already installed

library(dplyr)
library(lubridate)
library(broom)
library(ggplot2)

nests<-read.csv("PMW_Nestdata.csv")
nests$laydate<-yday(as.Date(nests$X1st.egg, format="%m/%d/%y"))
nests$Cust.. <- as.factor(nests$Cust..)

#Add nest success (whether or not at least one young fledged from nest)
nests$nest_success <- 0
nests$nest_success[nests$Fledge > 0] <- 1
nests$nest_success<-as.factor(nests$nest_success)

#Add egg success for life table analysis
nests$egg_success<- nests$Fledge/nests$Eggs

###########################################
#TX nests
TX_nests <- nests %>% filter(State=="TX")

TX_nests %>% count(Year)

TX_nests <- TX_nests[TX_nests$Year>1997,]

#sample sizes:
nrow(TX_nests)
length(which(TX_nests$laydate<80))
length(which(TX_nests$laydate>212))
TX_nests$laydate[TX_nests$laydate<80 | TX_nests$laydate > 212] <- NA
nrow(TX_nests[!is.na(TX_nests$laydate),])
nrow(TX_nests[TX_nests$Male!="",])

#nest success ~ laydate
mod<-glm(nest_success~laydate, TX_nests, family="binomial")
summary(mod)


plot_df <- augment(mod, type.predict = "response")
plot_df$nest_success <- as.integer(as.character(plot_df$nest_success))

fam <- family(mod)
fam
str(fam)

ilink <- fam$linkinv
ilink

TX_nests <- bind_cols(TX_nests, setNames(as_tibble(predict(mod, TX_nests, se.fit = TRUE)[1:2]), c('fit_link','se_link')))

TX_nests <- mutate(TX_nests, fit_resp = ilink(fit_link), right_upr = ilink(fit_link + (2*se_link)), right_lwr = ilink(fit_link - (2*se_link)))

#Plot Figure 4c
a<-ggplot(plot_df, aes(x = laydate)) +
  geom_line(aes(y = .fitted), color = "#4A6FE3", lwd=2) +
  labs(x = "First Egg Date (day of year)", y = "Nest Success") +
  geom_point(aes(y = nest_success), alpha = 0.2) +
  theme(legend.position="none",
  	panel.background=element_rect(fill="white"), 
  	plot.margin=unit(c(1,1,1,1),"cm"), 
  	axis.title.y=element_text(size = 22, margin=margin(0,10,0,0)), 
  	axis.title.x=element_text(size = 22, margin=margin(20,0,0,0)),
  	axis.text.x = element_text(size = 14), 
  	axis.text.y = element_text(size = 14),
  	axis.line=element_line(size=1))

a + geom_ribbon(data = TX_nests,
                  aes(ymin = right_lwr, ymax = right_upr),
                  alpha = 0.1)



#getting mean, q1, q3 laydate
meanls <- list()
q1ls <- list()
q3ls <- list()
for (a in unique(TX_nests$Year)) {
  datels <- list()
  for (i in 1:nrow(TX_nests)) {
    year <- TX_nests$Year[i]
    if (year == a) {
      datels <- c(datels, TX_nests$laydate[i])
    }
  }
  mean <- mean(as.numeric(unlist(datels)), na.rm=TRUE)
  meanls <- c(meanls, mean)
  q1 <- quantile(as.numeric(unlist(datels)), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)[1]
  q1ls <- c(q1ls, q1)
  q3 <- quantile(as.numeric(unlist(datels)), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)[3]
  q3ls <- c(q3ls, q3)
}

#adding means to TX_nests
dfmeanls <- list()
for(a in 1:length(meanls)){
  yr = unique(TX_nests$Year)[a]
  for(i in 1:nrow(TX_nests)){
    if(TX_nests$Year[i] == yr){
      dfmeanls[[i]] <- meanls[[a]]
    }
  }
}
TX_nests$Mean <- unlist(dfmeanls)
#adding q1 to TX_nests
dfq1ls <- list()
for(a in 1:length(q1ls)){
    yr = unique(TX_nests$Year)[a]
	for(i in 1:nrow(TX_nests)){
    if(TX_nests$Year[i] == yr){
      dfq1ls[[i]] <- q1ls[[a]]
    }
  }
}
TX_nests$Q1 <- unlist(dfq1ls)
#adding q3 to TX_nests
dfq3ls <- list()
for(a in 1:length(q3ls)){
    yr = unique(TX_nests$Year)[a]
	for(i in 1:nrow(TX_nests)){
    if(TX_nests$Year[i] == yr){
      dfq3ls[[i]] <- q3ls[[a]]
    }
  }
}
TX_nests$Q3 <- unlist(dfq3ls)


#calculate mean laydate for 1998-2020
meantx<-mean(unlist(meanls)[1:23])
sdtx<-sd(unlist(meanls)[1:23])

q1tx<-mean(unlist(q1ls)[1:23])
q1tx_sd<-sd(unlist(q1ls)[1:23])
q3tx<-mean(unlist(q3ls)[1:23])
q3tx_sd<-sd(unlist(q3ls)[1:23])

#difference in days between laydate in 2021 and long-term mean
meanls[[24]] - meantx

TX_success<-data.frame(TX_nests %>% group_by(Year) %>% count(nest_success))
TX_success_year<-data.frame(TX_success %>% group_by(Year) %>% summarize(sum(n)))
TX_nest_success<-data.frame(year = TX_success_year$Year, prop_fledge=TX_success$n[TX_success$nest_success==1]/TX_success_year$sum.n)
TX_nest_success
mean(TX_nest_success$prop_fledge[1:23])
sd(TX_nest_success$prop_fledge[1:23])


#plot the data vertically
ggplot(TX_nests, aes(y = Year, x = laydate)) +
  geom_rect(xmin = meantx-(2*sdtx), xmax = meantx+(2*sdtx), ymin = 1997, ymax = 2024, fill = "gray 91") + 
  geom_point(col="dark gray") + 
  geom_point(aes(y=Year, x = Mean), color = "black", size = 4)+
  geom_point(aes(y=Year, x = Q1), color = "black", size = 3, shape=3)+
  geom_point(aes(y=Year, x = Q3), color = "black", size = 3, shape=3)+
  annotate("rect", xmin=80, xmax=205, ymin=2020.5, ymax=2021.5, alpha=0.15, fill="red") +   
  geom_vline(xintercept=round(meantx), col="black")+ 
  ylim(1998,2023)+
  xlim(80,205) +
  labs(y = "Year", x = "Date") +
  ggtitle("TX Lay Dates") +
    theme(legend.position="none",
  	panel.background=element_rect(fill="white"), 
  	plot.margin=unit(c(1,1,1,1),"cm"), 
  	axis.title.y=element_text(size = 22, margin=margin(0,10,0,0)), 
  	axis.title.x=element_text(size = 22, margin=margin(20,0,0,0)),
  	axis.text.x = element_text(size = 14), 
  	axis.text.y = element_text(size = 14),
  	axis.line=element_line(size=1))


#nest success ~ male age class using nests w/ males of known age
summary(glm(nest_success~Male, TX_nests[TX_nests$Male!="",], family="binomial"))


#egg success for life table
TX_eggsuccess<-data.frame(TX_nests %>% group_by(Year) %>% summarize(mean(egg_success, na.rm=TRUE))) 
names(TX_eggsuccess)=c("Year", "Egg_success")
mean(TX_eggsuccess$Egg_success)


#ratios of adults:subadults
TX_nests_age<-TX_nests %>% count(Year, Male) 
TX_nests_age<-TX_nests_age[TX_nests_age$Male!="",] #remove males of unknown age
TX_nests_year<-data.frame(TX_nests_age %>% group_by(Year) %>% summarize(sum(n)))

TX_nests_subadults <- data.frame(year = TX_nests_year$Year, prop_adult=TX_nests_age$n[TX_nests_age$Male=="SY"]/TX_nests_year$sum.n)

range(TX_nests_subadults$prop_adult) 
mean(TX_nests_subadults$prop_adult[1:23]) #mean subadult males as proportion of total from 1998-2020
sd(TX_nests_subadults$prop_adult[1:23])


#laydate differences among subadults and adults
#Sample size exceeds allowable size for Shapiro Wilk test for normality
hist(TX_nests$laydate[TX_nests$Male!="ASY"])
hist(TX_nests$laydate[TX_nests$Male!="SY"])

#test for equal variance
var.test(laydate~Male, TX_nests[TX_nests$Male!="",], alternative="two.sided")

#t-test assumptions are met
t.test(laydate~Male, TX_nests[TX_nests$Male!="",], var.equal=FALSE)


#Comparing 2021 and 2022 age ratios
TX_nests_2122 <- TX_nests %>% filter(Year==2021 | Year==2022)
TX_nests_2122 <- TX_nests_2122 %>% filter(Male!="")

#customers with data from 2021 and 2022 only
x<-data.frame(TX_nests_2122[TX_nests_2122$Year == 2021 | TX_nests_2122$Year==2022,] %>% group_by(Cust.., Year) %>% count(as.factor(Cust..)))
x$Cust..[duplicated(x$Cust..)]

TX_nests_2122_dups <- NA
for (i in x$Cust..[duplicated(x$Cust..)]){
	print(i)
	TX_nests_2122_dups <- rbind(TX_nests_2122_dups, TX_nests_2122[TX_nests_2122$Cust..==i,])
}
TX_nests_2122_dups <- TX_nests_2122_dups[-1,]

y<-data.frame(TX_nests_2122_dups %>% group_by(Cust..,Year) %>% count(Male))

TX_nests_2122_dups_num <- data.frame(y %>% group_by(Cust.., Year) %>% summarize(sum(n)))

TX_nests_2122_adults <- data.frame(year = TX_nests_2122_dups_num$Year, prop_adult=y$n[y$Male=="ASY"]/TX_nests_2122_dups_num$sum.n)

t.test(prop_adult~year, TX_nests_2122_adults, paired=TRUE) #change from 2021 to 2022 in proportion of adult breeders


####################################

#LA nests
LA_nests <- nests %>% filter(State=="LA")

LA_nests %>% count(Year)

LA_nests <- LA_nests[LA_nests$Year>1997,]
LA_nests <- LA_nests[LA_nests$Year!=2005 & LA_nests$Year!=2007,]

#sample sizes:
nrow(LA_nests)
length(which(LA_nests$laydate<80))
length(which(LA_nests$laydate>212))
LA_nests$laydate[LA_nests$laydate<80 | LA_nests$laydate > 212] <- NA
nrow(LA_nests[!is.na(LA_nests$laydate),])
nrow(LA_nests[LA_nests$Male!="",])

#nest success ~ laydate
modLA<-glm(nest_success~laydate, LA_nests, family="binomial")
summary(modLA)

plot_df <- augment(modLA, type.predict = "response")
plot_df$nest_success <- as.integer(as.character(plot_df$nest_success))

fam <- family(modLA)
fam
str(fam)

ilink <- fam$linkinv
ilink

LA_nests <- bind_cols(LA_nests, setNames(as_tibble(predict(modLA, LA_nests, se.fit = TRUE)[1:2]), c('fit_link','se_link')))

LA_nests <- mutate(LA_nests, fit_resp = ilink(fit_link), right_upr = ilink(fit_link + (2*se_link)), right_lwr = ilink(fit_link - (2*se_link)))

#Plot Figure S4
a<-ggplot(plot_df, aes(x = laydate)) +
  geom_line(aes(y = .fitted), color = "#4A6FE3", lwd=2) +
  labs(x = "First Egg Date (day of year)", y = "Nest Success") +
  geom_point(aes(y = nest_success), alpha = 0.2) +
  theme(legend.position="none",
  	panel.background=element_rect(fill="white"), 
  	plot.margin=unit(c(1,1,1,1),"cm"), 
  	axis.title.y=element_text(size = 22, margin=margin(0,10,0,0)), 
  	axis.title.x=element_text(size = 22, margin=margin(20,0,0,0)),
  	axis.text.x = element_text(size = 14), 
  	axis.text.y = element_text(size = 14),
  	axis.line=element_line(size=1))

a + geom_ribbon(data = LA_nests,
                  aes(ymin = right_lwr, ymax = right_upr),
                  alpha = 0.1)

#getting mean, q1, q3 laydate
meanls <- list()
q1ls <- list()
q3ls <- list()
for (a in unique(LA_nests$Year)) { #no 2008 or 2012
  datels <- list()
  for (i in 1:nrow(LA_nests)) {
    year <- LA_nests$Year[i]
    if (year == a) {
      datels <- c(datels, LA_nests$laydate[i])
    }
  }
  mean <- mean(as.numeric(unlist(datels)), na.rm=TRUE)
  meanls <- c(meanls, mean)
  q1 <- quantile(as.numeric(unlist(datels)), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)[1]
  q1ls <- c(q1ls, q1)
  q3 <- quantile(as.numeric(unlist(datels)), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)[3]
  q3ls <- c(q3ls, q3)
}

#adding means to LA_nests
dfmeanls <- list()
for(a in 1:length(meanls)){
  yr = unique(LA_nests$Year)[a]
  for(i in 1:nrow(LA_nests)){
    if(LA_nests$Year[i] == yr){
      dfmeanls[[i]] <- meanls[[a]]
    }
  }
}
LA_nests$Mean <- unlist(dfmeanls)
#adding q1 to LA_nests
dfq1ls <- list()
for(a in 1:length(q1ls)){
    yr = unique(LA_nests$Year)[a]
	for(i in 1:nrow(LA_nests)){
    if(LA_nests$Year[i] == yr){
      dfq1ls[[i]] <- q1ls[[a]]
    }
  }
}
LA_nests$Q1 <- unlist(dfq1ls)
#adding q3 to LA_nests
dfq3ls <- list()
for(a in 1:length(q3ls)){
    yr = unique(LA_nests$Year)[a]
	for(i in 1:nrow(LA_nests)){
    if(LA_nests$Year[i] == yr){
      dfq3ls[[i]] <- q3ls[[a]]
    }
  }
}
LA_nests$Q3 <- unlist(dfq3ls)


#calculate mean day of year for 1998-2020 without 2005, 2007, 2008, or 2012
meanLA<-mean(unlist(meanls)[1:19])
sdLA<-sd(unlist(meanls)[1:19])

q1LA<-mean(unlist(q1ls)[1:19])
q1LA_sd<-sd(unlist(q1ls)[1:19])
q3LA<-mean(unlist(q3ls)[1:19])
q3LA_sd<-sd(unlist(q3ls)[1:19])

#difference in days between laydate in 2021 and long-term mean
meanls[[20]] - meanLA


LA_success<-data.frame(LA_nests %>% group_by(Year) %>% count(nest_success))
LA_success_year<-data.frame(LA_success %>% group_by(Year) %>% summarize(sum(n)))
LA_nest_success<-data.frame(year = LA_success_year$Year, prop_fledge=LA_success$n[LA_success$nest_success==1]/LA_success_year$sum.n)
mean(LA_nest_success$prop_fledge[1:19])
sd(LA_nest_success$prop_fledge[1:19])
range(LA_nest_success$prop_fledge[1:19])
LA_nest_success$prop_fledge[20] #nest success in 2021

#plot the data vertically
ggplot(LA_nests, aes(y = Year, x = laydate)) +
  geom_rect(xmin = meanLA-(2*sdLA), xmax = meanLA+(2*sdLA), ymin = 1997, ymax = 2024, fill = "gray 91") + 
  geom_point(col="dark gray") + 
  geom_point(aes(y=Year, x = Mean), color = "black", size = 3)+
  geom_point(aes(y=Year, x = Q1), color = "black", size = 2, shape=3)+
  geom_point(aes(y=Year, x = Q3), color = "black", size = 2, shape=3)+
  geom_point(aes(y=2021, x = meanls[[20]]), color = "red", size = 3)+
  geom_point(aes(y=2021, x = q1ls[[20]]), color = "red", size = 2, shape=3)+
  geom_point(aes(y=2021, x = q3ls[[20]]), color = "red", size = 2, shape=3)+
  geom_vline(xintercept=meanLA, col="black")+ 
  ylim(1997,2024)+
  labs(y = "Year", x = "Date") +
  ggtitle("LA Lay Dates") +
    theme(legend.position="none",
  	panel.background=element_rect(fill="white"), 
  	plot.margin=unit(c(1,1,1,1),"cm"), 
  	axis.title.y=element_text(margin=margin(0,20,0,0)), 
  	axis.title.x=element_text(margin=margin(20,0,0,0)),
  	axis.line=element_line(size=.5))


#nest success ~ male age class using nests w/ males of known age
summary(glm(nest_success~Male, LA_nests[LA_nests$Male!="",], family="binomial"))


#egg success for life table
LA_eggsuccess<-data.frame(LA_nests %>% group_by(Year) %>% summarize(mean(egg_success, na.rm=TRUE))) 
names(LA_eggsuccess)=c("Year", "Egg_success")
mean(LA_eggsuccess$Egg_success)


#ratios of adults:subadults
LA_nests_age<-LA_nests[LA_nests$Male!="",] %>% count(Year, Male) %>% spread(Male, n, fill = 0) %>% gather(Male, n, -1)
LA_nests_year<-data.frame(LA_nests_age %>% group_by(Year) %>% summarize(sum(n)))

LA_nests_adults <- data.frame(year = LA_nests_year$Year, prop=LA_nests_age$n[LA_nests_age$Male=="ASY"]/LA_nests_year$sum.n)
LA_nests_subadults <- data.frame(year = LA_nests_year$Year, prop=LA_nests_age$n[LA_nests_age$Male=="SY"]/LA_nests_year$sum.n)

mean(LA_nests_subadults$prop[1:19])
sd(LA_nests_subadults$prop[1:19])


#laydate differences among subadults and adults
#test for normality
shapiro.test(LA_nests$laydate[LA_nests$Male!="SY"])
shapiro.test(LA_nests$laydate[LA_nests$Male!="ASY"])

#test for equal variance
var.test(laydate~Male, LA_nests[LA_nests$Male!="",], alternative="two.sided")

#test for differences among the age classes
wilcox.test(laydate~Male, LA_nests[LA_nests$Male!="",]) 


#Comparing 2021 and 2022 age ratios
LA_nests_2122 <- LA_nests %>% filter(Year==2021 | Year==2022)
LA_nests_2122 <- LA_nests_2122 %>% filter(Male!="")

#observers with data from 2021 and 2022 only
x<-data.frame(LA_nests_2122[LA_nests_2122$Year == 2021 | LA_nests_2122$Year==2022,] %>% group_by(Cust.., Year) %>% count(as.factor(Cust..)))
x$Cust..[duplicated(x$Cust..)] 

LA_nests_2122_dups <- NA
for (i in x$Cust..[duplicated(x$Cust..)]){
	print(i)
	LA_nests_2122_dups <- rbind(LA_nests_2122_dups, LA_nests_2122[LA_nests_2122$Cust..==i,])
}
LA_nests_2122_dups <- LA_nests_2122_dups[-1,]

y<-data.frame(LA_nests_2122_dups %>% group_by(Cust..,Year) %>% count(Male))

LA_nests_2122_dups_num <- data.frame(y %>% group_by(Cust.., Year) %>% summarize(sum(n)))

LA_nests_2122_adults <- data.frame(year = LA_nests_2122_dups_num$Year, prop_adult=y$n[y$Male=="ASY"]/LA_nests_2122_dups_num$sum.n)

t.test(prop_adult~year, LA_nests_2122_adults, paired=TRUE)

#both states together
t.test(prop_adult~year, rbind(TX_nests_2122_adults,LA_nests_2122_adults), paired=TRUE)
