import numpy as np
import scipy
import pandas
import argparse
import os
import matplotlib as mpl
import matplotlib.pyplot as plt
import seaborn as sns
sns.set_style('white')
sns.set_style('ticks')
sns.set_context('notebook')
import h5py
import allel

########################################################################################
def fig_pca1(samples,model,OutFile, Hue=None, palette=None, Style=None):
	"""
	samples=dataframe to be used for plotting
	model=PCA results
	OutFile=name and path to output file
	Hue=color category from sample dataframe
	Style=shape category from sample dataframe
	"""
	OutFig=OutFile + ".pdf"
	fig,ax = plt.subplots(figsize=(7, 5))
	sns.scatterplot(x='PC1',y='PC2',hue=Hue, palette=palette, style=Style, edgecolor='black',s=100, alpha=1, data=samples)
	ax.legend(bbox_to_anchor=(1, 1), loc='upper left', fontsize='medium')
	ax.set_xlabel('PC1 (%.1f%%)' % (model.explained_variance_ratio_[0]*100))
	ax.set_ylabel('PC2 (%.1f%%)' % (model.explained_variance_ratio_[1]*100))
	
	fig.savefig(OutFig, dpi=300, bbox_inches='tight')

########################################################################################

def fig_pca2(samples,model,OutFile, Hue=None, Style=None):
	"""
	samples=dataframe to be used for plotting
	model=PCA results
	OutFile=name and path to output file
	Hue=color category from sample dataframe
	Style=shape category from sample dataframe
	"""
	OutFig=OutFile+".pdf"
	fig,(ax1,ax2) = plt.subplots(1,2,figsize=(12, 5))
	#fig, ax = plt.subplots(figsize=(7, 5))
	sns.scatterplot(ax=ax1, x='PC1',y='PC2',hue=Hue, style=Style, edgecolor='black',s=100, alpha=1, legend=False, data=samples)
	ax1.set_xlabel('PC1 (%.1f%%)' % (model.explained_variance_ratio_[0]*100))
	ax1.set_ylabel('PC2 (%.1f%%)' % (model.explained_variance_ratio_[1]*100))


	sns.scatterplot(ax=ax2, x='PC3',y='PC4',hue=Hue, style=Style, edgecolor='black', s=100, alpha=1, data=samples)
	ax2.legend(bbox_to_anchor=(1, 1), loc='upper left', fontsize='medium')
	ax2.set_xlabel('PC3 (%.1f%%)' % (model.explained_variance_ratio_[2]*100))
	ax2.set_ylabel('PC4 (%.1f%%)' % (model.explained_variance_ratio_[3]*100))
	fig.savefig(OutFig, dpi=300, bbox_inches='tight')

########################################################################################
#LD prune function, size of window, step, threshold and n_iter all user specified.
def ld_prune(gn, size, step, threshold, n_iter):
    for i in range(n_iter):
        loc_unlinked = allel.locate_unlinked(gn, size=size, step=step, threshold=threshold)
        n = np.count_nonzero(loc_unlinked)
        n_remove = gn.shape[0] - n
        print('iteration', i+1, 'retaining', n, 'removing', n_remove, 'variants')
        gn = gn.compress(loc_unlinked, axis=0)
    return gn            
########################################################################################
def gt_filtering(gt,LdPrune, size, step,threshold,n_iter=5):
	ac = gt.count_alleles()[:]
	mult_allele = np.count_nonzero(ac.max_allele() > 1)
	singleton = np.count_nonzero((ac.max_allele() == 1) & ac.is_singleton(1))
	print("No. multiallelic snps", mult_allele, "No. singleton snps", singleton, sep='\t')
	flt = (ac.max_allele() == 1) & (ac[:, :2].min(axis=1) > 1)
	gf = gt.compress(flt, axis=0)
	print(len(gf))
	gn = gf.to_n_alt()
	gnu = ld_prune(gn, size=size, step=step, threshold=threshold, n_iter=n_iter)
	return gnu
########################################################################################
########################################################################################
def gt_filtering_noPrune(gt):
	#gt = genotype.take(samples, axis=1)
	ac = gt.count_alleles()[:]
	mult_allele = np.count_nonzero(ac.max_allele() > 1)
	singleton = np.count_nonzero((ac.max_allele() == 1) & ac.is_singleton(1))
	print("No. multiallelic snps", mult_allele, "No. singleton snps", singleton, sep='\t')
	flt = (ac.max_allele() == 1) & (ac[:, :2].min(axis=1) > 1)
	gf = gt.compress(flt, axis=0)
	print(len(gf))
	gn = gf.to_n_alt()
	return gn
########################################################################################