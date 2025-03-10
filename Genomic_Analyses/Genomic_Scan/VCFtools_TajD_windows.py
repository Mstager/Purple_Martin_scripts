import numpy as np
import scipy
import pandas as pd
import argparse
import os
import matplotlib.pyplot as plt

Usage="""
Summary stats on windowed Tajima's D from VCFtools
"""

parser = argparse.ArgumentParser(description = Usage)
parser.add_argument("-i", "--input", required=True, help="windowed TajimaD from vcftools")
parser.add_argument("-o", "--output", required=True, help="output file prefix")
parser.add_argument("-g", "--gff", required=True, help="gff file showing gene windows for overlap")
args=parser.parse_args()

VCFout=args.input
OutPre=args.output
GFF=args.gff

TajDwindows = pd.read_csv(VCFout, sep="\t")



print(len(TajDwindows))
TajDwindows.sort_values(by=['CHROM', 'BIN_START'])

TajDwindows['BIN_END'] =  TajDwindows['BIN_START'] + 24999
TajDwindows = TajDwindows.iloc[:,[0,1,4,2,3]]
print(TajDwindows.head())

TajimaD_mean=np.nanmean(TajDwindows['TajimaD'])
TajimaD_min=np.nanmin(TajDwindows['TajimaD'])
TajimaD_sd=np.nanstd(TajDwindows['TajimaD'])
TajimaD_99th=np.nanpercentile(TajDwindows['TajimaD'], 1)
TajimaD_outliers=TajDwindows[TajDwindows['TajimaD']<TajimaD_99th]
print(TajimaD_mean,TajimaD_min,TajimaD_sd,TajimaD_99th, sep='\t')
print(len(TajimaD_outliers))


OutBed=OutPre + ".bed"
TajimaD_outliers.to_csv(OutBed, na_rep='NaN', sep='\t', index=False, header=False)

InBed1=OutPre + "_sorted.bed"
InBed2=OutPre + "_annotated.txt"
os.system("bedtools sort -i %s > %s" % (OutBed, InBed1))
#os.system("bedtools slop -i %s -g bPasSan1.fna.gff -b 100 >%s" % (InBed1, InBed2))

#then use resulting bed file for intersect
os.system("bedtools intersect -wb -a %s -b %s > %s" % (InBed1, GFF, InBed2))


# How to plot gene vs. -log10(pvalue) and colour it by chromosome?
TajDwindows['window'] = range(len(TajDwindows))
df_grouped = TajDwindows.groupby(('CHROM'))

# manhattan plot
fig = plt.figure(figsize=(16, 6)) # Set the figure size
ax = fig.add_subplot(111)
colors = ['black','gray']

x_labels = []
x_labels_pos = []
for num, (name, group) in enumerate(df_grouped):
    group.plot(kind='line', x='window', y='TajimaD',color=colors[num % len(colors)], ax=ax, legend=None)
    x_labels.append(name)
    x_labels_pos.append((group['window'].iloc[-1] - (group['window'].iloc[-1] - group['window'].iloc[0])/2))
ax.set_xticks(x_labels_pos)
ax.set_xticklabels(x_labels)

# set axis limits
ax.set_xlim([0, len(TajDwindows)])
#plt.axhline(y = Fst_mean, color = 'blue', linestyle = '--')
plt.axhline(y = TajimaD_99th, color = 'red', linestyle = '--')

# x axis label
ax.set_xlabel('Scaffold')

# show the graph
PlotOut= OutPre + "Tajd_Manhattan_plot.pdf"
fig.savefig(PlotOut, dpi=150)
