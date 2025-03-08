import pandas as pd
import os

#import grenedalf.frequency.csv

Puma_af = open("PUMA_perSnpFst.weir.fst","r")
OutFile = open("PUMA_perSnpFst.weir.bed", 'a')
for line in Puma_af:
	line=line.strip('\n')
	lineList = line.split('\t')
	
	if lineList[0] == 'CHROM':
		print(lineList)
	
	else:
		END_POS = str(int(lineList[1]) + 1)
		lineList.insert(2, END_POS)
		finLine = '\t'.join(lineList)
		print(finLine, file=OutFile) 
Puma_af.close()	
OutFile.close()

#then export file as a tab delimited bed file
OutBed = "grenedalf.frequency.bed"
Puma_af.to_csv(OutBed, na_rep='NaN', sep='\t', index=False, header=False)

#intersect af.bed with Fst outliers from previous analysis using bedtools
os.system("source activate Popgen")

bedtools_cmd = "bedtools intersect -wb -a grenedalf.frequency.bed -b Puma_Fst_25Sept_sorted.bed > PUMA_AF_Fst_intersect.bed" % (OutBed, "Puma_Fst_25Sept_sorted.bed", "PUMA_AF_Fst_intersect.bed")
os.system(bedtools_cmd)
