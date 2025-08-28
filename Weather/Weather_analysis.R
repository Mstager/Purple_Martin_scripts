#Analyses of weather during the 2021 winter storm event

#read table of sampling sites, included in /Supp_Tables
sites <- read.csv("Table_S5.csv")

#downloading 25 years of data (1995 to 2020) for each unique site and storing in location25ls
install.packages(c("daymetr","ggplot2","cowplot") #if not already installed
library(daymetr)
location25ls <- list()
for(i in 1:nrow(sites)){
  clim25 <- download_daymet(site = sites$Location[i],
                          lat = sites$Latitude[i],
                          lon = sites$Longitude[i],
                          start = 1995,
                          end = 2020,
                          internal = TRUE)
  #select only Feb dates
  location25ls <- c(location25ls, list(clim25$data[clim25$data$yday>31&clim25$data$yday<60,]))
}


#finding mean and standard deviation for each climate variable per location
meanls <- list()
sdls <- list()
climvarls <- list()
minls <- list()
maxls <- list()
siteclimls <- list()
for (i in 1:length(location25ls)){
  climatedf <- location25ls[[i]]
  for(j in 3:ncol(climatedf)){
    mean <- mean(climatedf[[j]])
    meanls <- c(meanls, mean)
    sd <- sd(climatedf[[j]])
    sdls <- c(sdls, sd)
    min <- (mean - (2.5*sd))
    minls <- c(minls, min)
    max <- (mean + (2.5*sd))
    maxls <- c(maxls, max)
    climvarls <- c(climvarls, colnames(climatedf)[j])
  }
#storing in clim25df
  clim25df <- data.frame(Variables = unlist(climvarls),
                         Mean = unlist(meanls),
                         S.D. = unlist(sdls),
                         Min = unlist(minls),
                         Max = unlist(maxls))
  siteclimls <- c(siteclimls, list(clim25df))
  meanls <- list()
  sdls <- list()
  minls <-  list()
  maxls <- list()
  climvarls <- list()
}

#getting climate data of only february 2021 and storing in febclimdata
febclimdata <- list()
for(i in 1:nrow(sites)){
	clim2021 <- download_daymet(site = sites$Location[i],
                          lat = sites$Latitude[i],
                          lon = sites$Longitude[i],
                          start = 2021,
                          end = 2021,
                          internal = TRUE)
  #select only Feb dates
  febclimdata <- c(febclimdata, list(clim2021$data[clim2021$data$yday>31&clim2021$data$yday<60,]))
}

#calculate distance from mean for feb 2021 in terms of standard deviation and store in sdplotdf
sdtminls <- list()
sdtmaxls <- list()
sdprcpls <- list()
datels <- list()
locationls <- list()
latitudels <- list()
longitudels <- list()


for(a in 1:length(febclimdata)){
  for(i in 1:nrow(febclimdata[[a]])){
    locationls <- c(locationls, sites$Location[a])
    latitudels <- c(latitudels, sites$Latitude[a])
	longitudels <- c(longitudels, sites$Longitude[a])
	datels <- c(datels, febclimdata[[a]]$yday[i])
    #tmin
    tmin <- (febclimdata[[a]]$tmin..deg.c.[i] - siteclimls[[i]][6,2])/siteclimls[[i]][6,3]
    sdtminls <- c(sdtminls, tmin)
    #tmax
    tmax <- (febclimdata[[a]]$tmax..deg.c.[i] -  siteclimls[[i]][5,2])/siteclimls[[i]][5,3]
    sdtmaxls <- c(sdtmaxls, tmax)
    #prcp
    prcp <- (febclimdata[[a]]$prcp..mm.day.[i] - siteclimls[[i]][2,2])/siteclimls[[i]][2,3]
    sdprcpls <- c(sdprcpls, prcp)
  }
}
sdplotdf <- data.frame(Location = unlist(locationls), 
					State = sub(".*, ", "", unlist(locationls)),
					Latitude = unlist(latitudels), 
					Longitude = unlist(longitudels),
                    Date = unlist(datels),
                    tmin.sd = unlist(sdtminls),
                	tmax.sd = unlist(sdtmaxls),
                    prcp.sd = unlist(sdprcpls))

#######################################
#Plot Figure 1b

library(ggplot2); library(cowplot)
#plotting the standard deviations from mean for tmin
#plot only Feb 10-21
sdplotdf <- sdplotdf[sdplotdf$Date > 40 & sdplotdf$Date < 54,]

#Dec 31 as the origin so that Jan 1 is yday = 1
p1<-ggplot(data = sdplotdf, mapping = aes(x = factor(Date), y = tmin.sd))+
  geom_hline(yintercept=0, col="gray")+
  geom_point(aes(color = Latitude, shape=State))+
  scale_color_gradient(low = "gray80", high = "black")+
  scale_x_discrete(labels = c(format(as.Date(sdplotdf$Date, origin = "2020-12-31"), "%d"))) +
  labs(x = "February", y = "Standard Deviations from Mean")+
  theme(panel.background=element_rect(fill="white"), 
  	plot.margin=unit(c(1,1,1,1),"cm"), 
  	axis.title.y=element_text(margin=margin(0,10,0,0)), 
  	axis.title.x=element_text(margin=margin(10,0,0,0)),
  	axis.line=element_line(size=.5), 
  	legend.position="none")
 

#plotting the standard deviations from mean for tmax
p2<-ggplot(data = sdplotdf, mapping = aes(x = factor(Date), y = tmax.sd))+
  geom_hline(yintercept=0, col="gray")+
  geom_point(aes(color = Latitude, shape=State))+
  scale_color_gradient(low = "gray80", high = "black")+
  scale_x_discrete(labels = c(format(as.Date(sdplotdf$Date, origin = "2020-12-31"), "%d"))) +
  labs(x = "February")+
  theme(panel.background=element_rect(fill="white"), 
  	plot.margin=unit(c(1,1,1,1),"cm"), 
  	axis.title.y = element_blank(), 
  	axis.title.x=element_text(margin=margin(10,0,0,0)),
  	axis.line=element_line(size=.5), 
  	legend.position="none")
  	
  	
#plotting the standard deviations from mean for prcp
p3<-ggplot(data = sdplotdf, mapping = aes(x = factor(Date), y = prcp.sd))+
  geom_hline(yintercept=0, col="gray")+
  geom_point(aes(color = Latitude, shape=State))+
  scale_color_gradient(low = "gray80", high = "black")+
  scale_x_discrete(labels = c(format(as.Date(sdplotdf$Date, origin = "2020-12-31"), "%d"))) +
  labs(x = "February")+
  theme(panel.background=element_rect(fill="white"), 
  	plot.margin=unit(c(1,1,1,1),"cm"), 
  	axis.title.y = element_blank(),
  	axis.title.x=element_text(margin=margin(10,0,0,0)),
  	axis.line=element_line(size=.5), 
  	legend.box.spacing=unit(5, units="pt"),
  	legend.margin=margin(0,0,0,0))

plot_grid(p1, p2, p3, nrow=1, rel_widths=c(1.1,1,1.18), labels=c("Minimum Temperature", "Maximum Temperature", "Precipitation"))

