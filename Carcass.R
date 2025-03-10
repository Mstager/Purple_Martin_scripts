#Analyses on PUMA carcass morphometrics and body composition

library(tidyr)
library(dplyr)
library(ggplot2)

puma_df <- read.csv("Table_S3.csv")

#PUMA May 2021 banding data
banding<-read.csv("Table_S5.csv")

#split coordinates column
banding <-banding  %>% separate(Coordinates, sep=",", into=c("Latitude","Longitude"), convert=TRUE)


#select only sites with banding data
puma_subset<-puma_df[puma_df$Location=="Fate, TX" | puma_df$Location=="Natalia, TX" | puma_df$Location=="Gonzalez, TX" | puma_df$Location=="Liberty, TX" | puma_df$Location=="South Lake, TX" | puma_df$Location=="Trinidad, TX" | puma_df$Location=="San Antonio, TX" | puma_df$Location=="Austin, TX" | puma_df$Location=="Giddings, TX" | puma_df$Location=="New Braunfels, TX",]

t.test(band_subset$Wing.Chord..mm., puma_subset$Wing..mm.)

mean(as.numeric(band_subset$Bird.Mass..g.), na.rm=TRUE)
mean(puma_subset$Mass); sd(puma_subset$Mass)


#Calculate proportional fat and lean mass
puma_subset$Prop.Fat<- puma_subset$FatMass..g./puma_subset$Mass..g.     
puma_subset$Prop.Lean<-puma_subset$Lean.Mass..g./puma_subset$Mass..g.     

mean(puma_subset$Prop.Fat, na.rm=TRUE)
mean(puma_subset$Prop.Lean, na.rm=TRUE)

#Plot Figure 1d
ggplot(puma_subset, aes(Prop.Fat))+ geom_histogram(color="black", fill="grey", bins=20) + geom_vline(xintercept=mean(puma_subset$Prop.Fat, na.rm=TRUE), linetype=2) + xlab("Fat Mass (proportion of body mass)") + theme_classic()

#Plot Figure S1
ggplot(puma_subset, aes(Prop.Lean))+ geom_histogram(color="black", fill="grey", bins=20) + geom_vline(xintercept=mean(puma_subset$Prop.Lean, na.rm=TRUE), linetype=2) +  xlab("Lean Mass (as proportion of body mass)") + theme_classic()
