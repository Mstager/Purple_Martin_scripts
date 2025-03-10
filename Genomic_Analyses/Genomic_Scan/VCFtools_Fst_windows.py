import numpy as np
import scipy
import pandas as pd
import argparse
import os
import matplotlib.pyplot as plt

Usage="""
Summary stats on windowed nucleotide diversity output from VCFtools
"""

parser = argparse.ArgumentParser(description = Usage)
parser.add_argument("-i", "--input", required=True, help="windowed pi from vcftools")
parser.add_argument("-o", "--output", required=True, help="output file prefix")
parser.add_argument("-g", "--gff", required=True, help="gff file showing gene windows for overlap")
args=parser.parse_args()

VCFout=args.input
OutPre=args.output
GFF=args.gff

FstWindows = pd.read_csv(VCFout, sep="\t")
print(len(FstWindows))
#FstWindows.sort_values(by=['CHROM', 'BIN_START'])
print(FstWindows.head())
FstWindows[FstWindows['WEIGHTED_FST']<0] = 0

FstWindows.sort_values(by=['CHROM', 'BIN_START'])

Fst_mean=np.nanmean(FstWindows['WEIGHTED_FST'])
Fst_max=np.nanmax(FstWindows['WEIGHTED_FST'])
Fst_sd=np.nanstd(FstWindows['WEIGHTED_FST'])
Fst_thresh=Fst_mean + 5*Fst_sd
FstWindows3=FstWindows[FstWindows['WEIGHTED_FST']>Fst_thresh]
print(Fst_mean,Fst_max,Fst_thresh, len(FstWindows3), sep='\t')

#print(FstWindows3)
OutBed = OutPre + ".bed"
FstWindows3.to_csv(OutBed, na_rep='NaN', sep='\t', index=False, header=False)

InBed1=OutPre + "_sorted.bed"
InBed2=OutPre + "_annotated.txt"
os.system("bedtools sort -i %s > %s" % (OutBed, InBed1))

#os.system("bedtools slop -i %s -g bPasSan1.fna.gff -b 100 >%s" % (InBed1, InBed2))

#then use resulting bed file for intersect
os.system("bedtools intersect -wb -a %s -b %s > %s" % (InBed1, GFF, InBed2))


# How to plot gene vs. -log10(pvalue) and colour it by chromosome?
FstWindows['window'] = range(len(FstWindows))
df_grouped = FstWindows.groupby(('CHROM'))

# manhattan plot
fig = plt.figure(figsize=(16, 6)) # Set the figure size
ax = fig.add_subplot(111)
colors = ['black','gray']

x_labels = []
x_labels_pos = []
for num, (name, group) in enumerate(df_grouped):
    group.plot(kind='scatter', x='window', y='WEIGHTED_FST',color=colors[num % len(colors)], ax=ax, legend=None)
    x_labels.append(name)
    x_labels_pos.append((group['window'].iloc[-1] - (group['window'].iloc[-1] - group['window'].iloc[0])/2))
ax.set_xticks(x_labels_pos)
ax.set_xticklabels(x_labels)

# set axis limits
ax.set_xlim([0, len(FstWindows)])
plt.axhline(y = Fst_mean, color = 'blue', linestyle = '--')
plt.axhline(y = Fst_thresh, color = 'red', linestyle = '--')

# x axis label
ax.set_xlabel('Scaffold')

# show the graph
PlotOut = OutPre + "Fst_Manhattan_plot.pdf"
fig.savefig(PlotOut, dpi=150)
