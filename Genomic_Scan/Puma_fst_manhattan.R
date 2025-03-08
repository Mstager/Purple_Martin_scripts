#!/usr/bin/Rscript
library(ggplot2)
#script for plotting FST by chromosome order
setwd("/Users/phredbenham/Dropbox/PUMA_final_analyses/final_analyses/")

fst <- read.table("PsubisFst_25Sept2024.windowedScaf.weir.fst", header=T) #fst data
scafs <- read.csv("purple_scafs_ordered_by_chicken.csv") #scaffold/chr info
scafs$id <- 1:nrow(scafs)
scafs$purple_scaf <- gsub("Q","",scafs$purple_scaf)

#making new columns for adding chromosome info
fst$best_chick_chr <- as.character(NA)
fst$best_chick_id <- as.numeric(0)
fst$orientation <- as.character(NA)

#adding chr by matching scaffold name, this next chunk takes a while to run
for (i in 1:length(fst$CHROM)){
  for (j in 1:length(scafs$chr_num)){
    
    if (fst$CHROM[i] == scafs$purple_scaf[j]){
      fst$best_chick_chr[i] <- as.character(scafs$chr_num[j])
      fst$best_chick_id[i] <- as.numeric(scafs$id[j])
      fst$orientation[i] <- as.character(scafs$ori[j])
   }
  }
}

write.csv(fst,'Storm_PostStorm.windowed.weir.fst.orderedchr.csv')

######### after ordering chromosomes############################
fst$pos <- (((fst$BIN_START + fst$BIN_END)+1) /2) #this step necessary for windowed fsts with bin start and end
fst$best_chick_chr[is.na(fst$best_chick_chr)] <- 34
Autosome1<-subset(fst, best_chick_chr!="Z")
Autosome3<-subset(fst, best_chick_chr!="W")


Autosome3$best_chick_chr <- as.numeric(Autosome3$best_chick_chr)
Autosome2<-subset(Autosome3, best_chick_chr<34)
Autosome2$SNP<-paste(Autosome2$CHROM, Autosome2$BIN_START, sep="_")

#quick plot weighted fst
plot(Autosome2$pos, Autosome2$WEIGHTED_FST, main="Storm vs. Post-Storm windowed FST")

write.csv(Autosome3,'PUMA_fst_orderedchr_26Sept.csv')

##########################manhattan plot for FST#############################
library(qqman)

pumaAutosomes<-read.csv("PUMA_fst_orderedchr_26Sept.csv", header=TRUE)

sdFst<-sd(pumaAutosomes$WEIGHTED_FST)
meanFst<-mean(pumaAutosomes$WEIGHTED_FST)
thresh<-meanFst+(5*sdFst)

pdf(file="PUMA_manhattan_plotFst_12Feb.pdf", height=3, width=12)
manhattan(pumaAutosomes, chr="best_chick_chr", bp="pos", p="WEIGHTED_FST", snp="SNP", logp=FALSE, ylab="Weir and Cockerham Fst",  ylim = c(0, 0.12), cex.axis=0.9, suggestiveline = F, genomewideline = thresh)
dev.off()

pdf(file="PUMA_manhattan_plotFst_Chr4_12Feb.pdf", height=3, width=5)
manhattan(subset(pumaAutosomes, best_chick_chr==4), chr="best_chick_chr", bp="pos", p="WEIGHTED_FST", snp="SNP", logp=FALSE, ylab="Weir and Cockerham Fst",  ylim = c(0, 0.12), xlim = c(0,3e6), cex.axis=0.9, suggestiveline = F, genomewideline = thresh)
dev.off()

FstChr4Plot<-ggplot(subset(pumaAutosomes, best_chick_chr==4), aes(x=pos, y=MEAN_FST)) + geom_point() + theme_bw() + xlim(0,3e6) +   ylim(0, 0.12) + xlab("Chromosome 4 position") + ylab("Weir and Cockerham Fst") + geom_hline(yintercept=thresh,linetype="dashed",color="red") + annotate("rect", xmin=1232501, xmax=1432501, ymin=0,ymax=Inf, alpha=0.2, fill="blue") + theme(legend.position="none") + theme(axis.text.y=element_blank())

ggsave(filename="PUMA_manhattan_plotFst_Chr4_12Feb.pdf", plot= FstChr4Plot, height=3, width=5, units=c("in"))

FstChr24Plot<-ggplot(subset(pumaAutosomes, best_chick_chr==24), aes(x=pos, y=MEAN_FST)) + geom_point() + theme_bw() + xlab("Chromosome 24 position") + ylim(0, 0.12) + ylab("Weir and Cockerham Fst") + geom_hline(yintercept=thresh,linetype="dashed", color="red")  + annotate("rect", xmin=5412501, xmax=5562501, ymin=0,ymax=Inf, alpha=0.2, fill="yellow") + theme(legend.position="none") + theme(axis.text.y=element_blank())

ggsave(filename="PUMA_manhattan_plotFst_Chr24_12Feb.pdf", plot=FstChr24Plot, height=3, width=5, units=c("in"))


pdf(file="PUMA_manhattan_plotFst_Chr24_12Feb.pdf", height=3, width=5)
manhattan(subset(pumaAutosomes, best_chick_chr==24), chr="best_chick_chr", bp="pos", p="WEIGHTED_FST", snp="SNP", logp=FALSE, ylab="Weir and Cockerham Fst",  ylim = c(0, 0.12), cex.axis=0.9, suggestiveline = F, genomewideline = thresh)
dev.off()

pdf(file="PUMA_manhattan_plotFst_ChrZ_12Feb.pdf", height=3, width=5)
manhattan(subset(pumaAutosomes, best_chick_chr==34), chr="best_chick_chr", bp="pos", p="WEIGHTED_FST", snp="SNP", logp=FALSE, ylab="Weir and Cockerham Fst",  ylim = c(0, 0.12), cex.axis=0.9, suggestiveline = F, genomewideline = thresh)
dev.off()

##########################plot for Pi ##########################################################
Pi <- read.csv("PUMA_storm_25sept_pi.windowed.pi.csv", header=T) # pi data
scafs <- read.csv("purple_scafs_ordered_by_chicken.csv") #scaffold/chr info
scafs$id <- 1:nrow(scafs)
scafs$purple_scaf <- gsub("Q","",scafs$purple_scaf)

#making new columns for adding chromosome info
Pi$best_chick_chr <- as.character(NA)
Pi$best_chick_id <- as.numeric(0)
Pi$orientation <- as.character(NA)

#adding chr by matching scaffold name, this next chunk takes a while to run
for (i in 1:length(Pi$CHROM)){
	for (j in 1:length(scafs$chr_num)){
    
    if (Pi$CHROM[i] == scafs$purple_scaf[j]){
     Pi$best_chick_chr[i] <- as.character(scafs$chr_num[j])
      Pi$best_chick_id[i] <- as.numeric(scafs$id[j])
      Pi$orientation[i] <- as.character(scafs$ori[j])
    }
  }
}

Pi$pos <- (((Pi$BIN_START + Pi$BIN_END)+1) /2) #this step necessary for windowed fsts with bin start and end
Pi$best_chick_chr[is.na(Pi$best_chick_chr)] <- 34
PiAutosome1<-subset(Pi, best_chick_chr!="Z")
PiAutosome3<-subset(PiAutosome1, best_chick_chr!="W")


PiAutosome3$best_chick_chr <- as.numeric(PiAutosome3$best_chick_chr)
PiAutosome2<-subset(PiAutosome3, best_chick_chr<34)
PiAutosome2$SNP<-paste(PiAutosome2$CHROM, PiAutosome2$BIN_START, sep="_")

#write.csv(PiAutosome2,'PUMA_PI_orderedchr_26Sept.csv')
library(ggplot2)

PIorder<-read.csv("PUMA_PI_orderedchr_26Sept.csv",header=TRUE)

PIorder4<-subset(PIorder, best_chick_chr==4)
PiChr4Plot<-ggplot(PIorder4, aes(x=pos, y=STORM_PI, color=POP)) + geom_line() + theme_bw() +  ylim(0, 0.004) + xlim(0,3e6) + xlab("Chromosome 4 position") + ylab("Nucleotide diversity") + annotate("rect", xmin=1232501, xmax=1432501, ymin=0,ymax=Inf, alpha=0.2, fill="blue") + theme(legend.position="none") + theme(axis.text.y=element_blank())


ggsave(filename="PUMA_manhattan_plotPI_Chr4_1october.pdf", plot=PiChr4Plot, height=3, width=5, units=c("in"))

PIorder24<-subset(PIorder, best_chick_chr==24)
PiChr24Plot<-ggplot(PIorder24, aes(x=pos, y=STORM_PI, color=POP)) + geom_line() + theme_bw() + xlab("Chromosome 24 position") + ylab("Nucleotide diversity") + ylim(0, 0.004) + annotate("rect", xmin=5412501, xmax=5562501, ymin=0,ymax=Inf, alpha=0.2, fill="yellow") + theme(legend.position="none") + theme(axis.text.y=element_blank())

ggsave(filename="PUMA_manhattan_plotPI_Chr24_1october.pdf", plot=PiChr24Plot, height=3, width=5, units=c("in"))


##########################plot for TajD ##########################################################
tajd <- read.csv("PUMA_TajD_results.csv", header=T) # tajd data
scafs <- read.csv("purple_scafs_ordered_by_chicken.csv") #scaffold/chr info
scafs$id <- 1:nrow(scafs)
scafs$purple_scaf <- gsub("Q","",scafs$purple_scaf)

#making new columns for adding chromosome info
tajd$best_chick_chr <- as.character(NA)
tajd$best_chick_id <- as.numeric(0)
tajd$orientation <- as.character(NA)

#adding chr by matching scaffold name, this next chunk takes a while to run
for (i in 1:length(tajd$CHROM)){
	for (j in 1:length(scafs$chr_num)){
    
    if (tajd$CHROM[i] == scafs$purple_scaf[j]){
      tajd$best_chick_chr[i] <- as.character(scafs$chr_num[j])
      tajd$best_chick_id[i] <- as.numeric(scafs$id[j])
      tajd$orientation[i] <- as.character(scafs$ori[j])
    }
  }
}

#Pi$pos <- (((Pi$BIN_START + Pi$BIN_END)+1) /2) #this step necessary for windowed fsts with bin start and end
tajd$best_chick_chr[is.na(tajd$best_chick_chr)] <- 34
tajdAutosome1<-subset(tajd, best_chick_chr!="Z")
tajdAutosome3<-subset(tajdAutosome1, best_chick_chr!="W")


tajdAutosome3$best_chick_chr <- as.numeric(tajdAutosome3$best_chick_chr)
tajdAutosome2<-subset(tajdAutosome3, best_chick_chr<34)
tajdAutosome2$SNP<-paste(tajdAutosome2$CHROM, tajdAutosome2$BIN_START, sep="_")

tajdAutosome2$deltaTAJD<-tajdAutosome2$TajimaD - tajdAutosome2$psTajimaD
tajdAutosome2$BIN_END <- tajdAutosome2$BIN_START + 24999
write.csv(tajdAutosome2,'PUMA_tajd_orderedchr_27Sept.csv')



TAJDorder<-read.csv("PUMA_tajd_orderedchr_edited_27Sept.csv",header=TRUE)

Chr4<-subset(TAJDorder, best_chick_chr==4)

TajdChr1Plot<-ggplot(Chr4, aes(x=POS, y=TajimaD, color=POP)) + geom_line() + theme_bw() + xlim(0,2000000)
print(TajdChr1Plot)

Thresh95<-quantile(tajdAutosome2$deltaTAJD, 0.95, na.rm=TRUE)
deltaTAJDoutlier<-subset(tajdAutosome2, deltaTAJD>=Thresh95)

write.csv(deltaTAJDoutlier, "deltaTajdOutliers.csv")


TajdChr4Plot<-ggplot(Chr4, aes(x=POS, y=TajimaD, color=POP)) + geom_line() + theme_bw() + xlim(0,3e6) + ylim(-2.5, 2.5) + xlab("Chromosome 4 position") + ylab("Tajima's D")  + annotate("rect", xmin=1232501, xmax=1432501, ymin=-2.5,ymax=Inf, alpha=0.2, fill="blue")  + theme(legend.position="none") + theme(axis.text.y=element_blank())

ggsave(filename="PUMA_manhattan_plotTAJD_Chr4_1october.pdf", plot=TajdChr4Plot, height=3, width=5, units=c("in"))

Chr24<-subset(TAJDorder, best_chick_chr==24)
TajdChr24Plot<-ggplot(Chr24, aes(x=POS, y= TajimaD, color=POP)) + geom_line() + theme_bw() + ylim(-2.5, 2.5) + xlab("Chromosome 24 position") + ylab("Tajima's D") + annotate("rect", xmin=5412501, xmax=5562501, ymin=-2.5,ymax=Inf, alpha=0.2, fill="yellow") + theme(legend.position="none") + theme(axis.text.y=element_blank())

ggsave(filename="PUMA_manhattan_plotTAJD_Chr24_1october.pdf", plot=TajdChr24Plot, height=3, width=5, units=c("in"))
