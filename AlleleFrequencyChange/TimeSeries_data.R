library(ggplot2)

NonOut<-read.table("~/Dropbox/Stager_Lab/PUMA_genomics/PUMA_nonoutlier_100k_subset_sorted.bed",header=TRUE)
NonOut$A<-abs(NonOut$during - NonOut$before)
NonOut$B<-abs(NonOut$after - NonOut$during)
NonOut$C<-abs(NonOut$after - NonOut$before)

maxA<-max(NonOut$A, na.rm=TRUE)
maxB<-max(NonOut$B, na.rm=TRUE)
maxC<-max(NonOut$C, na.rm=TRUE)

A95<-unname(quantile(NonOut$A, 0.95, na.rm=TRUE))
B95<-unname(quantile(NonOut$B, 0.95, na.rm=TRUE))
C95<-unname(quantile(NonOut$C, 0.95, na.rm=TRUE))

PUMAout<-read.csv("~/Dropbox/Stager_Lab/PUMA_genomics/OutlierSNPS_23Dec.csv", header=TRUE)

PUMAout$A<-abs(PUMAout$during - PUMAout$before)
PUMAout$B<-abs(PUMAout$after - PUMAout$during)
PUMAout$C<-abs(PUMAout$after - PUMAout$before)

write.csv(PUMAout,"~/Dropbox/Stager_Lab/PUMA_genomics/All_SNPs.csv")


Aout<-subset(PUMAout, A>A95 & B>B95)

#Aout<-subset(PUMAout, A<A95 & B>B95 & C>C95)
write.csv(Aout,"~/Dropbox/Stager_Lab/PUMA_genomics/After_Storm_change_SNPs.csv")



Aout$A1<-Aout$during - Aout$before
Aout$B1<-Aout$after - Aout$during
Aout$C1<-Aout$after - Aout$before


AoutPositive<-subset(Aout, A1>0)
AoutNegative<-subset(Aout, A1<0)

write.csv(AoutPositive,"~/Dropbox/Stager_Lab/PUMA_genomics/StormIncreaseSNPs.csv")
write.csv(AoutNegative,"~/Dropbox/Stager_Lab/PUMA_genomics/StormDecreaseSNPs.csv")

Aout$POS<-paste(Aout$CHROM, Aout$START, sep="_")
newA<-Aout[,c(6:12)]
headers=Aout$POS
newAt<-as.data.frame(t(newA))
colnames(newAt)<-headers
newAt$pool<-c(3,1,2,25,35,40,50)

TimePointA<-subset(newAt,pool<5)

lenA<-nrow(Aout)
print(lenA)
TimePointA<-TimePointA[order(TimePointA$pool),]
pdf("~/Dropbox/Stager_Lab/PUMA_genomics/TimeSeriesDuringChange.pdf")
plot(TimePointA$pool,TimePointA[,1], ylim=c(0,1), xlim=c(1,3), type='n', ylab="Allele frequency", xlab="Time point")
for (i in seq(1:lenA)) {
	lines(TimePointA$pool, TimePointA[,i],col="gray")
}

#intersect<-read.csv("~/Dropbox/Stager_Lab/PUMA_genomics/TimeLatIntersect.csv",header=TRUE)

pool<-c(1,2,3)
Lat1<-c(0.3, 0.666667, 0.407407)
Lat2<-c(0.2, 0.613636, 0.181818)
intersect<-as.data.frame(cbind(pool,Lat1,Lat2))

for (n in seq(1:2)){
	lines(intersect$pool, intersect[,n+1], col="black", lwd=4)
}

peakSnp<-read.csv("~/Dropbox/Stager_Lab/PUMA_genomics/PeakSNPs.csv", header=TRUE)
for (k in seq(1:21)){
	lines(peakSnp$pool, peakSnp[,k+1], col="blue", lwd=1)
}

for (x in seq(1:8)){
	lines(peakSnp$pool, peakSnp[,x+1], col="yellow", lwd=1)
}

dev.off()
