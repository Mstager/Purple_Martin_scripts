#life table response analysis for PUMA

install.packages("ggplot2") #if not already installed
library (ggplot2)


#annual survival rates from Brown, C. R., D. A. Airola, and S. Tarof (2021). Purple Martin (Progne subis), version 2.0. In Birds of the World (P. G. Rodewald, Editor). Cornell Lab of Ornithology, Ithaca, NY, USA. https://doi-org.silk.library.umass.edu/10.2173/bow.purmar.02
surv_adult = 0.63 #annual survival of adults age 2-5 years
surv_subadult = 0.48 #annual survival of adults age 1 (subadults)
surv_fledge = 0.27 #annual survival of fledglings to first year

#clutch sizes in N. Texas from Brown, C. R. (1978c). Clutch size and reproductive success of adult and subadult Purple Martins. Southwestern Naturalist 23: 597–604.
clutch_adult = 4.97
clutch_subadult = 4.11

#egg success (probability of egg surviving to chick fledging) 
egg_success = 0.71 #average for TX and for LA from PMCA data across all years
egg_success_TX_storm = 0.67 #average egg success in 2021 for TX
egg_success_LA_storm = 0.53 #average egg success in 2021 for LA

###################################################################
###to maintain a stable population given average adult survival

#number of time steps to simulate
timesteps = 100

#initiate an empty data frame to store output
life.table = data.frame(time = 0:timesteps, n_adults = NA, n_subadults = NA, n_fledge = NA, n_eggs = NA) 

#starting adult population size
n_adult_start = 1000 
life.table$n_adults[1] <- n_adult_start

for (i in 0:timesteps) {
	#number of adults at time t=i:
		n_adults = life.table$n_adults[life.table$time==i] 
	#annual attrition in adults is equal to number of adults at time t=i minus the number of adults that survive, given annual survival rate:
		n_adult_deaths = n_adults - (n_adults * surv_adult) 
	#number of subadults needed to compensate for adult mortality given annual survival of subadults:
		n_subadults = n_adult_deaths / surv_subadult 
	#number of fledglings needed to compensate for subadult mortality given annual survival of fledglings:
		n_fledge = n_subadults / surv_fledge 
	#number of eggs produced given the # of adult and subadult pairs and their respective clutch sizes:
		n_eggs = ((n_adults/2)* clutch_adult) + ((n_subadults/2)* clutch_subadult) 
	#number of individuals in each age class at time t=i:
		life.table[life.table$time==i,] <- c(i, n_adults, n_subadults, n_fledge, n_eggs)
	#number of adults at time t=i+1 is equal to number of adults at time t=i x survival of adults plus number of subadults at t=i x survival of subadults:
		life.table$n_adults[life.table$time==i+1] <- (life.table$n_adults[life.table$time==i]* surv_adult) + (life.table$n_subadults[life.table$time==i]* surv_subadult)
}

life.table #this just demonstrates the number of individuals required in each age class to maintain a stable population over time

####################################################################################################

####to maintain a stable/growing population in TX, incorporating avereage egg success

#initiate an empty data frame to store output
life.table.TX = data.frame(time = 0:timesteps, n_adults = NA, n_subadults = NA, n_fledge = NA, n_eggs = NA) 

#now time t=1 is equal to the values needed to maintain a stable population as they were just calculated
life.table.TX[1,] <- life.table[1,] 

for (i in 1:timesteps) {
	#number of adults at time t=i is equal to number of adults at time t=i-1 x survival of adults plus number of subadults at t=i-1 x survival of subadults:
		life.table.TX$n_adults[life.table.TX$time==i] = (life.table.TX$n_adults[life.table.TX$time==i-1]* surv_adult) + (life.table.TX$n_subadults[life.table.TX$time==i-1]* surv_subadult)
	#number of subadults at time t=i is equal to number of fledglings at time t=i-1 x survival of fledglings:
		life.table.TX$n_subadults[life.table.TX$time==i] = life.table.TX$n_fledge[life.table.TX$time==i-1]* surv_fledge
	#number of eggs at time t=i is equal to number of adults at time t=i x clutch size of adults plus number of subadults at time t=i x clutch size of subadults :
		life.table.TX$n_eggs[life.table.TX$time==i] = ((life.table.TX$n_adults[life.table.TX$time==i]/2)* clutch_adult) + ((life.table.TX$n_subadults[life.table.TX$time==i]/2)* clutch_subadult) #number of eggs produced given the # of adults and 1st years and their respective clutch sizes
	#number of fledglings at time t=i is equal to number of eggs at time t=i x egg success:
		life.table.TX$n_fledge[life.table.TX$time==i] = life.table.TX$n_eggs[life.table.TX$time==i]* egg_success
}

####################################################################################################

####storm decrement in TX w/ reduced adult survival and nest success in year 1 followed by average egg success

#initiate an empty data frame to store output of time to recovery for different simulated values of adult mortality
TX_recovery <- data.frame(time=rep(NA,26), mortality=rep(NA,26))

#initiate an empty data frame to store output for each instance of adult mortality
life.table.TX.storm = data.frame(time = 0:timesteps, n_adults = NA, n_subadults = NA, n_fledge = NA, n_eggs = NA)

#now time t=1 is equal to the values needed to maintain a stable population in the initial scenario
life.table.TX.storm[1,] <- life.table.TX[1,]

for (j in 1:26) { #for 26 values of adult mortality rate ranging from 0 to 0.25
	#simulated value of adult mortality for this iteration:
		adult_storm_mortality = (j-1)*0.01
		for (i in 1:timesteps) {
			#the storm occurs in timestep t=1
			if (i==1) {
				#adult mortality is reduced at time t=1 such that the number of adults at time t=i is equal to number of adults at time t=i-1 x (annual survival of adults - mortality rate) plus number of subadults at t=i-1 x survival of subadults:
					life.table.TX.storm$n_adults[life.table.TX.storm$time==i] = (life.table.TX.storm$n_adults[life.table.TX.storm$time==i-1]* (surv_adult-adult_storm_mortality)) + (life.table.TX.storm$n_subadults[life.table.TX.storm$time==i-1]* surv_subadult)
			}
			else {
				#in all other timesteps, the number of adults at time t=i is equal to number of adults at time t=i-1 x survival of adults plus number of subadults at t=i-1 x survival of subadults:
					life.table.TX.storm$n_adults[life.table.TX.storm$time==i] = (life.table.TX.storm$n_adults[life.table.TX.storm$time==i-1]* surv_adult) + (life.table.TX.storm$n_subadults[life.table.TX.storm$time==i-1]* surv_subadult)
			}
			#number of subadults at time t=i is equal to number of fledglings at time t=i-1 x survival of fledglings:
				life.table.TX.storm$n_subadults[life.table.TX.storm$time==i] = life.table.TX.storm$n_fledge[life.table.TX.storm$time==i-1]* surv_fledge
			#number of eggs at time t=i is equal to number of adults at time t=i x clutch size of adults plus number of subadults at time t=i x clutch size of subadults :	
				life.table.TX.storm$n_eggs[life.table.TX.storm$time==i] = ((life.table.TX.storm$n_adults[life.table.TX.storm$time==i]/2)* clutch_adult) + ((life.table.TX.storm$n_subadults[life.table.TX.storm$time==i]/2)* clutch_subadult) #number of eggs produced given the # of adults and 1st years and their respective clutch sizes
			if (i==1) {
				#egg success is reduced at time t=1 such that number of fledglings at time t=i is equal to number of eggs at time t=i x egg success in TX in 2021:
				life.table.TX.storm$n_fledge[life.table.TX.storm$time==i] = life.table.TX.storm$n_eggs[life.table.TX.storm$time==i]* egg_success_TX_storm
			}
			else {
				#for all other timesteps, the number of fledglings at time t=i is equal to number of eggs at time t=i x egg success:
					life.table.TX.storm$n_fledge[life.table.TX.storm$time==i] = life.table.TX.storm$n_eggs[life.table.TX.storm$time==i]* egg_success
			}
		}
	#store the minimum number of time steps required to return to initial adult population size
		TX_recovery$time[j]<-min(life.table.TX.storm$time[which(life.table.TX.storm$n_adults>=n_adult_start & life.table.TX.storm$time>2)])-1
	#store the mortality rate for this iteration
		TX_recovery$mortality[j]<- adult_storm_mortality
}

############################################################################

###storm decrement in TX w/ reduced adult survival and nest success in year 1 followed by average egg success + second event in year 11 
TX_recovery2 <- data.frame(time=rep(NA,26), mortality=rep(NA,26))

life.table.TX.storm2 = data.frame(time = 0:timesteps, n_adults = NA, n_subadults = NA, n_fledge = NA, n_eggs = NA)
life.table.TX.storm2[1,] <- life.table.TX[1,]

for (j in 1:26) {
	adult_storm_mortality = (j-1)*0.01
	for (i in 1:timesteps) {
		if (i ==1 | i==11) {
			life.table.TX.storm2$n_adults[life.table.TX.storm2$time==i] = (life.table.TX.storm2$n_adults[life.table.TX.storm2$time==i-1]* (surv_adult-adult_storm_mortality)) + (life.table.TX.storm2$n_subadults[life.table.TX.storm2$time==i-1]* surv_subadult)
		}
		else {
			life.table.TX.storm2$n_adults[life.table.TX.storm2$time==i] = (life.table.TX.storm2$n_adults[life.table.TX.storm2$time==i-1]* surv_adult) + (life.table.TX.storm2$n_subadults[life.table.TX.storm2$time==i-1]* surv_subadult)
		}
		life.table.TX.storm2$n_subadults[life.table.TX.storm2$time==i] = life.table.TX.storm2$n_fledge[life.table.TX.storm2$time==i-1]* surv_fledge
		life.table.TX.storm2$n_eggs[life.table.TX.storm2$time==i] = ((life.table.TX.storm2$n_adults[life.table.TX.storm2$time==i]/2)* clutch_adult) + ((life.table.TX.storm2$n_subadults[life.table.TX.storm2$time==i]/2)* clutch_subadult) #number of eggs produced given the # of adults and 1st years and their respective clutch sizes
		if (i ==1 | i ==11) {
			life.table.TX.storm2$n_fledge[life.table.TX.storm2$time==i] = life.table.TX.storm2$n_eggs[life.table.TX.storm2$time==i]* egg_success_TX_storm
			}
		else {
			life.table.TX.storm2$n_fledge[life.table.TX.storm2$time==i] = life.table.TX.storm2$n_eggs[life.table.TX.storm2$time==i]* egg_success
			}
	}
TX_recovery2$time[j]<-min(life.table.TX.storm2$time[which(life.table.TX.storm2$n_adults>=n_adult_start & life.table.TX.storm2$time>2)])-1
TX_recovery2$mortality[j]<- adult_storm_mortality
}

TX_recovery2$time[TX_recovery2$time<10] <- NA




#######################################################
#to maintain a stable/growing population in LA
life.table.LA = data.frame(time = 0:timesteps, n_adults = NA, n_1styr = NA, n_fledge = NA, n_eggs = NA)
life.table.LA[1,] <- life.table[1,]

for (i in 1:timesteps) {
	life.table.LA$n_adults[life.table.LA$time==i] = (life.table.LA$n_adults[life.table.LA$time==i-1]* surv_adult) + (life.table.LA$n_1styr[life.table.LA$time==i-1]* surv_1styr)
	life.table.LA$n_1styr[life.table.LA$time==i] = life.table.LA$n_fledge[life.table.LA$time==i-1]* surv_fledge
	life.table.LA$n_eggs[life.table.LA$time==i] = ((life.table.LA$n_adults[life.table.LA$time==i]/2)* clutch_adult) + ((life.table.LA$n_1styr[life.table.LA$time==i]/2)* clutch_1styr) #maximum number of eggs produced given the number of adults and 1st years in the population and their respective clutch sizes
	life.table.LA$n_fledge[life.table.LA$time==i] = life.table.LA$n_eggs[life.table.LA$time==i]* egg_success
}


#storm decrement in LA w/ reduced adult survival
LA_recovery <- data.frame(time=rep(NA,21), mortality=rep(NA,21))

life.table.LA.storm = data.frame(time = 0:timesteps, n_adults = NA, n_1styr = NA, n_fledge = NA, n_eggs = NA)
life.table.LA.storm[1,] <- life.table.LA[1,]

for (j in 1:21) {
	adult_storm_mortality = (j-1)*0.01
	for (i in 1:timesteps) {
		if (i ==1) {
			life.table.LA.storm$n_adults[life.table.LA.storm$time==i] = (life.table.LA.storm$n_adults[life.table.LA.storm$time==i-1]* (surv_adult-adult_storm_mortality)) + (life.table.LA.storm$n_1styr[life.table.LA.storm$time==i-1]* surv_1styr)
		}
		else {
			life.table.LA.storm$n_adults[life.table.LA.storm$time==i] = (life.table.LA.storm$n_adults[life.table.LA.storm$time==i-1]* surv_adult) + (life.table.LA.storm$n_1styr[life.table.LA.storm$time==i-1]* surv_1styr)
		}
		life.table.LA.storm$n_1styr[life.table.LA.storm$time==i] = life.table.LA.storm$n_fledge[life.table.LA.storm$time==i-1]* surv_fledge
		life.table.LA.storm$n_eggs[life.table.LA.storm$time==i] = ((life.table.LA.storm$n_adults[life.table.LA.storm$time==i]/2)* clutch_adult) + ((life.table.LA.storm$n_1styr[life.table.LA.storm$time==i]/2)* clutch_1styr) #maximum number of eggs produced given the number of adults and 1st years in the population and their respective clutch sizes
		if (i ==1) {
			life.table.LA.storm$n_fledge[life.table.LA.storm$time==i] = life.table.LA.storm$n_eggs[life.table.LA.storm$time==i]* egg_success_LA_storm
			}
		else {
			life.table.LA.storm$n_fledge[life.table.LA.storm$time==i] = life.table.LA.storm$n_eggs[life.table.LA.storm$time==i]* egg_success
			}
	}
LA_recovery$time[j]<-min(life.table.LA.storm$time[which(life.table.LA.storm$n_adults>=n_adult_start & life.table.LA.storm$time>2)])-1
LA_recovery$mortality[j]<- adult_storm_mortality
}


#storm decrement in LA w/ reduced adult survival + second event in year 11 
LA_recovery2 <- data.frame(time=rep(NA,21), mortality=rep(NA,21))

timesteps = 120
life.table.LA.storm2 = data.frame(time = 0:timesteps, n_adults = NA, n_1styr = NA, n_fledge = NA, n_eggs = NA)
life.table.LA.storm2[1,] <- life.table.LA[1,]

for (j in 1:21) {
	adult_storm_mortality = (j-1)*0.01
	for (i in 1:timesteps) {
		if (i ==1 | i ==11) {
			life.table.LA.storm2$n_adults[life.table.LA.storm2$time==i] = (life.table.LA.storm2$n_adults[life.table.LA.storm2$time==i-1]* (surv_adult-adult_storm_mortality)) + (life.table.LA.storm2$n_1styr[life.table.LA.storm2$time==i-1]* surv_1styr)
		}
		else {
			life.table.LA.storm2$n_adults[life.table.LA.storm2$time==i] = (life.table.LA.storm2$n_adults[life.table.LA.storm2$time==i-1]* surv_adult) + (life.table.LA.storm2$n_1styr[life.table.LA.storm2$time==i-1]* surv_1styr)
		}
		life.table.LA.storm2$n_1styr[life.table.LA.storm2$time==i] = life.table.LA.storm2$n_fledge[life.table.LA.storm2$time==i-1]* surv_fledge
		life.table.LA.storm2$n_eggs[life.table.LA.storm2$time==i] = ((life.table.LA.storm2$n_adults[life.table.LA.storm2$time==i]/2)* clutch_adult) + ((life.table.LA.storm2$n_1styr[life.table.LA.storm2$time==i]/2)* clutch_1styr) #maximum number of eggs produced given the number of adults and 1st years in the population and their respective clutch sizes
		if (i ==1 | i ==11) {
			life.table.LA.storm2$n_fledge[life.table.LA.storm2$time==i] = life.table.LA.storm2$n_eggs[life.table.LA.storm2$time==i]* egg_success_LA_storm
			}
		else {
			life.table.LA.storm2$n_fledge[life.table.LA.storm2$time==i] = life.table.LA.storm2$n_eggs[life.table.LA.storm2$time==i]* egg_success
			}
	}
LA_recovery2$time[j]<-min(life.table.LA.storm2$time[which(life.table.LA.storm2$n_adults>=n_adult_start & life.table.LA.storm2$time>2)])-1
LA_recovery2$mortality[j]<- adult_storm_mortality
}





###############################
#plotting results

recovery <- merge(TX_recovery, TX_recovery2, by="mortality")
recovery <- merge(recovery, LA_recovery, by="mortality")
recovery <- merge(recovery, LA_recovery2, by="mortality")
names(recovery) <- c("mortality", "TX_time", "TX_time2", "LA_time", "LA_time2")

TX_col = "#E66100" #UT colors
LA_col = "#5D3A9B" #LSU colors

ggplot(recovery, aes(y=TX_time, x=mortality))+ 
	geom_point(size=3, col=TX_col, alpha=0.7)+ 
	geom_point(aes(y=TX_time2, x=mortality),size=2.5, col=TX_col, alpha=0.7, shape = 21, stroke=1)+
	geom_point(aes(y=LA_time, x=mortality), size=3, col=LA_col, alpha=0.7)+ 
	geom_point(aes(y=LA_time2, x=mortality),size=2.5, col=LA_col, alpha=0.7, shape = 21, stroke=1)+
	geom_hline(yintercept=10, lty=2, color="gray") +
	xlab("Storm Decrement to Adult Survival (%)")+
	ylab("Recovery Time (years)")+
	scale_y_continuous(breaks=seq(0,110,20)) +
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
