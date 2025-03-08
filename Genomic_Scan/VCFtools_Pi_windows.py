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

PiWindows = pd.read_csv(VCFout, sep="\t")
print(PiWindows.head())

print(len(PiWindows))
PiWindows.sort_values(by=['CHROM', 'BIN_START'])

Pi_mean=np.nanmean(PiWindows['PI'])
Pi_min=np.nanmin(PiWindows['PI'])
Pi_sd=np.nanstd(PiWindows['PI'])
Pi_95th=np.percentile(PiWindows['PI'], 5)
Pi_outliers=PiWindows[PiWindows['PI']<Pi_95th]
print(Pi_mean,Pi_min,Pi_sd,Pi_95th, sep='\t')
print(len(Pi_outliers))


OutBed=OutPre + ".bed"
Pi_outliers.to_csv(OutBed, na_rep='NaN', sep='\t', index=False, header=False)

InBed1=OutPre + "_sorted.bed"
InBed2=OutPre + "_annotated.txt"
os.system("bedtools sort -i %s > %s" % (OutBed, InBed1))
#os.system("bedtools slop -i %s -g bPasSan1.fna.gff -b 100 >%s" % (InBed1, InBed2))

#then use resulting bed file for intersect
os.system("bedtools intersect -wb -a %s -b %s > %s" % (InBed1, GFF, InBed2))


# How to plot gene vs. -log10(pvalue) and colour it by chromosome?
PiWindows['window'] = range(len(PiWindows))
df_grouped = PiWindows.groupby(('CHROM'))

# manhattan plot
fig = plt.figure(figsize=(16, 6)) # Set the figure size
ax = fig.add_subplot(111)
colors = ['black','gray']

x_labels = []
x_labels_pos = []
for num, (name, group) in enumerate(df_grouped):
    group.plot(kind='line', x='window', y='PI',color=colors[num % len(colors)], ax=ax, legend=None)
    x_labels.append(name)
    x_labels_pos.append((group['window'].iloc[-1] - (group['window'].iloc[-1] - group['window'].iloc[0])/2))
ax.set_xticks(x_labels_pos)
ax.set_xticklabels(x_labels)

# set axis limits
ax.set_xlim([0, len(PiWindows)])
#plt.axhline(y = Fst_mean, color = 'blue', linestyle = '--')
plt.axhline(y = Pi_95th, color = 'red', linestyle = '--')

# x axis label
ax.set_xlabel('Scaffold')

# show the graph
PlotOut= OutPre + "Manhattan_plot.pdf"
fig.savefig(PlotOut, dpi=150)