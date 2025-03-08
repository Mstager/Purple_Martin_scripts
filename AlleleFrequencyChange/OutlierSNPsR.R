PUMA<-read.csv("~/Dropbox/Stager_Lab/PUMA_genomics/OutlierSNPS_23Dec.csv", header=TRUE)
PUMA$POS<-paste(PUMA$CHROM, PUMA$START, sep="_")
newPUMA<-PUMA[,c(6:12)]
headers=PUMA$POS
newPUMAt<-as.data.frame(t(newPUMA))
colnames(newPUMAt)<-headers
newPUMAt$pool<-c(3,1,2,25,35,40,50)

TimePoint<-subset(newPUMAt,pool<5)
latitude<-subset(newPUMAt,pool>20)

end<-ncol(newPUMAt)-1

plot(latitude $pool, latitude[,1], ylim=c(0,1), type='n')
pval=c()
for (i in seq(1:end)) {
	lines(latitude$pool, latitude[,i],col="gray")
	lm1<-lm(latitude[,i]~latitude$pool)
	
	p<-summary(lm1)$coefficients["latitude$pool","Pr(>|t|)"]
	print(p)
	pval=c(pval,p)
}

snpPvals<-cbind(headers,pval)
colnames(snpPvals)<-c("SNP","Pval")
print(snpPvals)

pval2<-as.data.frame(na.omit(snpPvals))
pval3<-subset(pval2, Pval<0.05)

for (row in 1:nrow(pval3)){
	pos<-as.character(pval3[row,]$SNP)
	#newDF<-select(latitude,pool,pos)
	lines(latitude[,"pool"], latitude[,pos],col="red")
	}

write.csv(pval3, "~/Dropbox/Stager_Lab/PUMA_genomics/Latitude_OutlierSigSNPs.csv")