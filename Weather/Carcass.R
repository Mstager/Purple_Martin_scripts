#Analyses on PUMA carcass morphometrics and body composition

install.packages(c("tidyr","dplyr","ggplot2")) #if not already installed

library(tidyr)
library(dplyr)
library(ggplot2)

puma_df <- read.csv("Supp_Tables/Table_S3.csv")

#PUMA May 2021 banding data
banding<-read.csv("Supp_Tables/Table_S4.csv")

#split coordinates column
banding <-banding  %>% separate(Coordinates, sep=",", into=c("Latitude","Longitude"), convert=TRUE)


#select only sites with banding data
puma_subset<-puma_df[puma_df$Location=="Fate, TX" | puma_df$Location=="Natalia, TX" | puma_df$Location=="Gonzalez, TX" | puma_df$Location=="Liberty, TX" | puma_df$Location=="South Lake, TX" | puma_df$Location=="Trinidad, TX" | puma_df$Location=="San Antonio, TX" | puma_df$Location=="Austin, TX" | puma_df$Location=="Giddings, TX" | puma_df$Location=="New Braunfels, TX",]

#test for normality
shapiro.test(banding$Wing.Chord..mm.)
shapiro.test(puma_subset$Wing.Chord..mm.)

#test for equal variance
var.test(banding$Wing.Chord..mm., puma_subset$Wing.Chord..mm., alternative="two.sided")

#t-test assumptions are met
t.test(banding$Wing.Chord..mm., puma_subset$Wing.Chord..mm., var.equal=TRUE)

mean(as.numeric(banding$Bird.Mass..g.), na.rm=TRUE)
mean(puma_subset$Mass); sd(puma_subset$Mass)


#Calculate proportional fat and lean mass
puma_subset$Prop.Fat<- puma_subset$Fat.Mass..g./puma_subset$Mass..g.     
puma_subset$Prop.Lean<-puma_subset$Lean.Mass..g./puma_subset$Mass..g.     

mean(puma_subset$Prop.Fat, na.rm=TRUE)
mean(puma_subset$Prop.Lean, na.rm=TRUE)

#compare proportional lean mass among sexes
#test for normality fails
shapiro.test(puma_subset$Prop.Lean[puma_subset$Sex=="M"])
shapiro.test(puma_subset$Prop.Lean[puma_subset$Sex=="F"])

#Mann-Whitney test
wilcox.test(Prop.Lean~Sex, puma_subset, alternative="two.sided")

#compare proportional fat mass among sexes
#test for normality fails
shapiro.test(puma_subset$Prop.Fat[puma_subset$Sex=="M"])
shapiro.test(puma_subset$Prop.Fat[puma_subset$Sex=="F"])

#Mann-Whitney test
wilcox.test(Prop.Fat~Sex, puma_subset, alternative="two.sided")



#Plot Figure 1d
ggplot(puma_subset, aes(Prop.Fat))+ geom_histogram(color="black", fill="grey", bins=20) + geom_vline(xintercept=mean(puma_subset$Prop.Fat, na.rm=TRUE), linetype=2) + xlab("Fat Mass (proportion of body mass)") + theme_classic()

#Plot Figure S1
ggplot(puma_subset, aes(Prop.Lean))+ geom_histogram(color="black", fill="grey", bins=20) + geom_vline(xintercept=mean(puma_subset$Prop.Lean, na.rm=TRUE), linetype=2) +  xlab("Lean Mass (as proportion of body mass)") + theme_classic()
