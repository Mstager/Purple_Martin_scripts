#life table for PUMA

#annual survival rates from Brown, C. R., D. A. Airola, and S. Tarof (2021). Purple Martin (Progne subis), version 2.0. In Birds of the World (P. G. Rodewald, Editor). Cornell Lab of Ornithology, Ithaca, NY, USA. https://doi-org.silk.library.umass.edu/10.2173/bow.purmar.02
surv_adult = 0.63 #annual survival of adults age 2-5 years
surv_1styr = 0.48 #annual survival of adults age 1
surv_fledge = 0.27 #annual survival of fledglings to first year

#clutch sizes in N. Texas from Brown, C. R. (1978c). Clutch size and reproductive success of adult and subadult Purple Martins. Southwestern Naturalist 23: 597–604.
clutch_adult = 4.97
clutch_1styr = 4.11

#egg success (probability of egg surviving to chick fledging) 
egg_success = 0.71 #average for TX and for LA from PMCA data across all years
max_egg_success = 0.82 #maximum for TX from PMCA data across all years
egg_success_TX_storm = 0.67 #average egg success in 2021 for TX
egg_success_LA_storm = 0.53 #average egg success in 2021 for LA


#to maintain a stable population
timesteps = 10
life.table = data.frame(time = 0:timesteps, n_adults = NA, n_1styr = NA, n_fledge = NA, n_eggs = NA)
life.table$n_adults[1] <- 100

for (i in 0:timesteps) {
	n_adults = life.table$n_adults[life.table$time==i]
	n_adult_replacements = n_adults - (n_adults * surv_adult) #number of 1st yrs needed to replace annual attrition in adults
	n_1styr = n_adult_replacements / surv_1styr #number of 1st yrs needed to reach adult replacement given annual survival
	n_fledge = n_1styr / surv_fledge #number of fledgies needed to replace 1st yrs annually
	n_eggs = ((n_adults/2)* clutch_adult) + ((n_1styr/2)* clutch_1styr) #number of eggs produced given the # of adults and 1st years and their respective clutch sizes
	life.table[life.table$time==i,] <- c(i, n_adults, n_1styr, n_fledge, n_eggs)
	life.table$n_adults[life.table$time==i+1] <- (life.table$n_adults[life.table$time==i]* surv_adult) + (life.table$n_1styr[life.table$time==i]* surv_1styr)
}

####################################################################################################

#to maintain a stable/growing population in TX
timesteps = 10
life.table.TX = data.frame(time = 0:timesteps, n_adults = NA, n_1styr = NA, n_fledge = NA, n_eggs = NA)
life.table.TX[1,] <- life.table[1,]

for (i in 1:timesteps) {
	life.table.TX$n_adults[life.table.TX$time==i] = (life.table.TX$n_adults[life.table.TX$time==i-1]* surv_adult) + (life.table.TX$n_1styr[life.table.TX$time==i-1]* surv_1styr)
	life.table.TX$n_1styr[life.table.TX$time==i] = life.table.TX$n_fledge[life.table.TX$time==i-1]* surv_fledge
	life.table.TX$n_eggs[life.table.TX$time==i] = ((life.table.TX$n_adults[life.table.TX$time==i]/2)* clutch_adult) + ((life.table.TX$n_1styr[life.table.TX$time==i]/2)* clutch_1styr) #number of eggs produced given the # of adults and 1st years and their respective clutch sizes
	life.table.TX$n_fledge[life.table.TX$time==i] = life.table.TX$n_eggs[life.table.TX$time==i]* egg_success
}


#storm decrement in TX w/ reduced adult survival and nest success in year 1 followed by average egg success
TX_recovery <- data.frame(time=rep(NA,21), mortality=rep(NA,21))

egg_success_TX_storm = 0.67
timesteps=50
life.table.TX.storm = data.frame(time = 0:timesteps, n_adults = NA, n_1styr = NA, n_fledge = NA, n_eggs = NA)
life.table.TX.storm[1,] <- life.table.TX[1,]

for (j in 1:21) {
	adult_storm_mortality = (j-1)*0.01
	for (i in 1:timesteps) {
		if (i ==1) {
			life.table.TX.storm$n_adults[life.table.TX.storm$time==i] = (life.table.TX.storm$n_adults[life.table.TX.storm$time==i-1]* (surv_adult-adult_storm_mortality)) + (life.table.TX.storm$n_1styr[life.table.TX.storm$time==i-1]* surv_1styr)
		}
		else {
			life.table.TX.storm$n_adults[life.table.TX.storm$time==i] = (life.table.TX.storm$n_adults[life.table.TX.storm$time==i-1]* surv_adult) + (life.table.TX.storm$n_1styr[life.table.TX.storm$time==i-1]* surv_1styr)
		}
		life.table.TX.storm$n_1styr[life.table.TX.storm$time==i] = life.table.TX.storm$n_fledge[life.table.TX.storm$time==i-1]* surv_fledge
		life.table.TX.storm$n_eggs[life.table.TX.storm$time==i] = ((life.table.TX.storm$n_adults[life.table.TX.storm$time==i]/2)* clutch_adult) + ((life.table.TX.storm$n_1styr[life.table.TX.storm$time==i]/2)* clutch_1styr) #number of eggs produced given the # of adults and 1st years and their respective clutch sizes
		if (i ==1) {
			life.table.TX.storm$n_fledge[life.table.TX.storm$time==i] = life.table.TX.storm$n_eggs[life.table.TX.storm$time==i]* egg_success_TX_storm
			}
		else {
			life.table.TX.storm$n_fledge[life.table.TX.storm$time==i] = life.table.TX.storm$n_eggs[life.table.TX.storm$time==i]* egg_success
			}
	}
TX_recovery$time[j]<-min(life.table.TX.storm$time[which(life.table.TX.storm$n_adults>=100 & life.table.TX.storm$time>2)])-1
TX_recovery$mortality[j]<- adult_storm_mortality
}





########################################
#to maintain a stable/growing population in LA
timesteps = 10
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

egg_success_LA_storm = 0.53
timesteps=60
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
LA_recovery$time[j]<-min(life.table.LA.storm$time[which(life.table.LA.storm$n_adults>=100 & life.table.LA.storm$time>2)])-1
LA_recovery$mortality[j]<- adult_storm_mortality
}

recovery <- merge(TX_recovery, LA_recovery, by="mortality")
names(recovery) <- c("mortality", "TX_time", "LA_time")

library (ggplot2)
ggplot(recovery, aes(y=TX_time, x=mortality))+ 
	geom_point(size=3, col="#E66100", alpha=0.7)+
	geom_point(aes(y=LA_time, x=mortality), size=3, col="#5D3A9B", alpha=0.7)+
	xlab("Storm Decrement to Adult Survival (%)")+
	ylab("Recovery Time (years)")+
	ylim(0,55)+
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
