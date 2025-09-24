## This repository has scripts used for analyses in the following publication: 

[Genetic ancestry and population structure in the All of Us Research Program cohort](https://www.nature.com/articles/s41467-025-59351-8) in _Nature Communications_ 

**Cite this work:** Sharma, S., Nagar, S.D., Pemu, P. et al. Genetic ancestry and population structure in the All of Us Research Program cohort. Nat Commun 16, 4123 (2025). https://doi.org/10.1038/s41467-025-59351-8. 

## Overview of scripts and results

### **Step 1:** Population structure and genetic ancestry in the All of Us Research Program
+ _01_PopGen_merginingAndHarmonization.r_ - An R script that uses Plink to harmonize All of Us genotype data with 1000 Genome WGS data. It checks for allele consistencies and strand alignment, tries to fix mismatcing alleles and produces one single genetic dataset.
+ _01_PopGen_plotPCA.r_ - Results of PCA are visualized here with different settings (check **Step 2** for detailed unsupervised analyses of population structure).
+ _01_PopGenp_plotAdmixturePlots.V3.r_ -  Script to visualized ancestry distribution in the All of Us Research Program

<img src="https://media.springernature.com/full/springer-static/image/art%3A10.1038%2Fs41467-025-59351-8/MediaObjects/41467_2025_59351_Fig2_HTML.png?as=webp" width=50% height=50%>


### **Step 2:** Unupervised clustering using density-based clustering methods
+ _02_UnsupervisedClustering_performUnsupervisedClustering.r_ - Uses dbSCAN, HDBSCAN, and kernal density function for inferring clusters in the PCA results.
+ _02_UnsupervisedClustering_plotUMAP.V1.r_ - Extends the analysis to UMAPs.

<img src="https://media.springernature.com/full/springer-static/image/art%3A10.1038%2Fs41467-025-59351-8/MediaObjects/41467_2025_59351_Fig1_HTML.png?as=webp" width=50% height=50%>

### **Step 3:** Generates US geographical maps for ancestry visualizations (Tracts data obtained from: https://www2.census.gov/geo/tiger/TIGER2022/ZCTA520/)
+ _03_Geography_prepareGeographicalData.r_ - Uses US census tracts to summarise genetic ancestry across US states inferred using participants in the All of Us
+ _03_Geography_plotCollapsed.r_ - Collapses data to 2 or 3 digit summaries.

<img src="https://media.springernature.com/full/springer-static/image/art%3A10.1038%2Fs41467-025-59351-8/MediaObjects/41467_2025_59351_Fig4_HTML.png?as=webp" width=50% height=50%>
 ![Figure2-AncestryByGeography](https://github.com/user-attachments/assets/82c66017-30aa-410b-a51b-d81504b80b28)
