import numpy as np
import scipy
import pandas
import argparse
import os
import matplotlib as mpl
import matplotlib.pyplot as plt
import seaborn as sns
import PCA_functions as pcfunc
sns.set_style('white')
sns.set_style('ticks')
sns.set_context('notebook')
import h5py
import allel; print('scikit-allel', allel.__version__)


Usage = """
Conduct PC analysis on SNP data in vcf file (converted to hdf5) using scikit allele (). 
Will remove singletons and prune SNPs in linkage if desired. Produces a figure and file with PC1-4 values
appended.
Last update by Phred M. Benham on 10 February 2023
"""

parser = argparse.ArgumentParser(description = Usage)
parser.add_argument("-i","--input", required=True, help="input vcf or hdf5 file")
parser.add_argument("-o","--output", required=True, help="output file names")
parser.add_argument("-s","--sample_data", required=True, help="csv file with sample info, samples must be in same order as vcf file")
parser.add_argument("-a","--category1", nargs='?', const=1, type=str, default=None, help="1st category to color pca points, use column from sample data (optional)")
parser.add_argument("-b","--category2", nargs='?', const=1, type=str, default=None, help="2nd category to make pca points different shapes, use column from sample data (optional)")
parser.add_argument("-c","--plot34", nargs='?', const=1, type=int, default=1, help="1 yes or 0 no, if 1 will plot PC1 vs PC2 and PC3 vs PC4, default=1")
parser.add_argument("-d","--output_pcs", nargs='?', const=1, type=int, default=1, help="1 yes or 0 no, if 1 will append PCs to sample file and output with output suffix, [default 1]")
parser.add_argument("-e","--Plot_subsets", nargs='?', const=1, type=int, default=0, help="1 yes or 0 no, if 1 will subset clusters based on column in input file and perform PCA, define clusters with -f [default 0]")
parser.add_argument("-f","--Plot_subset_category",  nargs='?', const=1, type=str, default=None, help="use with -e to subset data and plot separate PCA results")
parser.add_argument("-l","--ld_prune",  nargs='?', const=1, type=int, default=0, help="1 yes or 0 no, if 1 will prune SNPs based on user expectations can specify with -w, -k, -t, -r [default 0]")
parser.add_argument("-w","--window", nargs='?', const=1, type=int, default=2500, help="integer for window size for LD pruning [default 2500]")
parser.add_argument("-k","--step", nargs='?', const=1, type=int,default=250, help="integer specificying step size [default 250]")
parser.add_argument("-t","--threshold", nargs='?', const=1, type=float, default=0.25, help="float specifying ld threshold for pruning[default 0.25]")
parser.add_argument("-r","--n_iterations", nargs='?', const=1, type=int, default=5, help="integer specifying number of rounds of ld pruning [default 5]")

args = parser.parse_args()

VCFfile=args.input
OutFilePrefix=args.output
SampleData=args.sample_data
ColCategory=args.category1
ShapeCategory=args.category2
TwoPlots=args.plot34
PCvals_out=args.output_pcs
ClusterPlot=args.Plot_subsets
ClustCategory=args.Plot_subset_category
LdPrune=args.ld_prune
Window=args.window
Step=args.step
Threshold=args.threshold
iters=args.n_iterations

print(LdPrune)

#input data, check if h5 file has already been created. if not create h5 file and import.
VCF_path = VCFfile
h5file = VCF_path + '.h5'
if not os.path.exists(h5file):
	allel.vcf_to_hdf5(VCF_path, h5file, fields='*', overwrite=True)

#load hdf5 file with genotype calls and sample data
Capture_callset = h5py.File(h5file, mode='r')

#load csv sample file. Order of samples in CSV must match order in the VCF file!!!
samples = pandas.read_csv(SampleData)
print(samples.head())

#create a genotype array from callset. 2nd create allele counts file from genotype array
gt = allel.GenotypeChunkedArray(Capture_callset['calldata/GT'])

#filter SNP dataset to remove singletons, multiallelic SNPs, etc. will also perform basic LD pruning.
if LdPrune==0:
	gn1 =  pcfunc.gt_filtering_noPrune(gt)

else:
	#print(LdPrune)
	gn1 = pcfunc.gt_filtering(gt, LdPrune, Window, Step,Threshold,iters)


coords1, model1 = allel.pca(gn1, n_components=10)
samples['PC1'] = coords1[:,0]
samples['PC2'] = coords1[:,1]
samples['PC3'] = coords1[:,2]
samples['PC4'] = coords1[:,3]

#output csv file with values for PC1-4 appended
if int(PCvals_out)==1:
	SampleOutput = OutFilePrefix + ".csv"
	samples.to_csv(SampleOutput)

#Plot PC results, colors and shapes of points can be specified above.
if int(TwoPlots)==0:
	#palette={"Callipepla_californica":"#FF7B84"; "Callipepla_gambelii": "#04B8BC"; "hybrid": "#FF930B"}
	#calquail: #FF7B84
	#GamQuail: #04B8BC
		
	pcfunc.fig_pca1(samples,model1,OutFilePrefix, Hue=ColCategory, Style=ShapeCategory)

else:
	pcfunc.fig_pca2(samples,model1,OutFilePrefix, Hue=ColCategory, Style=ShapeCategory)	
	
#Subset data based on user defined category (e.g. subspecies/pop. gen clusters) and plot PC1 vs. PC2 for each data subset. 
#This second part will be more useful after an initial exploration of data. 
if ClusterPlot==1:
	variants = allel.VariantChunkedTable(Capture_callset['variants'], names=['POS', 'REF', 'ALT', 'DP', 'MQ'])
	idx = np.arange(0,len(variants),1)

	Clusters = samples[ClustCategory].unique()
	for cat in Clusters:
		print(cat)
		samples_select = samples[ClustCategory].isin({cat}).values
		samples_Selected = samples[samples_select]
		samples_Selected.reset_index(drop=True, inplace=True)
		gt_sub = gt.subset(idx, samples_select)
		gn2 = pcfunc.gt_filtering_noPrune(gt_sub)
		coords1, model1 = allel.pca(gn2, n_components=10)
		samples_Selected['PC1'] = coords1[:,0]
		samples_Selected['PC2'] = coords1[:,1]
		OutFile1 = OutFilePrefix + '_' + cat
		SampleOutFile = OutFilePrefix + '_' + cat + ".csv"
		samples_Selected.to_csv(SampleOutFile)
		pcfunc.fig_pca1(samples_Selected,model1,OutFile1, Hue=ShapeCategory)