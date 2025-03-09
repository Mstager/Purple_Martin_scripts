# Purple_Martin_scripts

## Data and code for: Stager et al. _in review_ "Immediate and long-term consequences of a storm-induced mass mortality event"


### Genomic analyses
1) **PUMA Notebook.md** \
  overview of pipeline to clean, map, and genotype raw reads for whole genome sequencing and poolseq

2) **Psubis_filter_24Sept24.log** \
  commands to produce final filtered vcf using vcftools \
  removed 5 individuals that showed high degree of relatedness (>= to full siblings or parent-offspring)

3) **PUMA_WGS_sample_info_NoInbred.csv** \
   List of samples (related birds excluded), file important for generating PCA.

4) *PCA* directory: \
	4a) **PUMA_PCA.py** \
			Code for performing principal components analysis that also removes singletons 
			and performs LD pruning to generate unlinked SNP dataset. \
	4b) functions for performing and plotting PCA analyses with PUMA_PCA.py \
	
5) *Genomc_scan* directory: \
	5a) **vcftools_logs**: \
		log files showing commands to estimate Fst, nucleotide diversity (Pi), and Tajima's D (tajd)
		in 25Kb non-overlapping windows in VCFtools. \
	
	5b) *VCFtools_output* directory: \
		Vcftools output files showing Fst, pi, and tajd values in 25kb windows. Pi and Tajd
		files are in .csv format that combined storm and post-storm results into single file
		for plotting. \
		
	5c) **PUMAlift_genes_sorted.gff** \
		Genome annotations for the Purple Martin genome. Derived from a liftover of gene annotations
		from the Barn Swallow genome (NCBI) to Purple Martin. A Purple Martin annotation is available,
		but has significantly fewer genes. \
		
	5d) **VCFtools_Fst_windows.py**, **VCFtools_Pi_windows.py**, **VCFtools_TajD_windows.py** \
		Python scripts to identify outlier windows from VCFtools_output for each statistic (5b) and provide
		annotations for the windows based on .gff file (5c). \
		
	5e) **purple_scafs_ordered_by_chicken.csv** \
		Map showing alignment of Purple Martin scaffolds to chicken chromosomes \
	
	5f) **Puma_fst_manhattan.R** \
		R script for assigning windows to chicken chromosomes and generating manhattan plots
		for Fst, Pi, and Tajima's D. PUMA_fst_orderedchr_26Sept.csv is an intermediate file
		that required some manual sorting of windows along each chicken chromosome. \

6) *AlleleFrequencyChange* directory: \
	Scripts for analysis of POOLSEQ dataset. \
	
	6a) **PoolseqAFparse.py** \
		Python script to intersect SNPs from poolseq analysis with Fst outliers \
	
	6b) **OutlierSNPS_23Dec.csv** \
		Allele frequencies for Fst outlier SNPs. For input to R \
	
	6c) **OutlierSNPsR** \
		Rscript to identify SNPs varying significantly with latitude. \
	
	6d) **PUMA_nonoutlier_100k_subset_sorted.bed** \
		Non-outlier SNPs for input to 6e and used to determine threshold for signifcant
		AF change. \	
	
	6e) **TimeSeries_data.R** \
		Rscript to determine SNPs that significantly change across time points in Texas. \


### Weather analyses

7) *Weather* directory: \
   	7a) **Figure1a.r** \
  		R script for generating Figure 1a using data in **Table_S4.csv** and **2021_scout-arrival-data-pre-Feb20.csv** \
	7b) **Weather.r** \
   		R script for conducting weather anomaly analyses and generating Figures 1b using data in **Table_S4.csv** \

### Morphological analyses

9) 		

#### Body composition analyses
	
10) **QMR.r** \
    R script for conducting body composition analyses using Quantititave Magnetic Resonance data from carcasses \

### Scout arrival 

11) *Scout_arrivals* directory: \
	11a) **Scout_arrival.r** \
		Analyses of trends in Purple Martin Conservation Association Scout-Arrival Study data and plots for Figures 2a, 2b, and S2. \
	11b) Scout arrival data for each of the 10 focal states from 1998 through 2024 in .csv format (redacted: without observer names). Data can also be obtained directly from https://www.purplemartin.org/research/8/scout-arrival-study/.
