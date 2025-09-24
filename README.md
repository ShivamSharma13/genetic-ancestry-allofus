## This repository has scripts used for analyses in the following publication: 

[Genetic ancestry and population structure in the All of Us Research Program cohort](https://www.nature.com/articles/s41467-025-59351-8) in _Nature Communications_ 

**Cite this work:** Sharma, S., Nagar, S.D., Pemu, P. et al. Genetic ancestry and population structure in the All of Us Research Program cohort. Nat Commun 16, 4123 (2025). https://doi.org/10.1038/s41467-025-59351-8. 

## Overview of scripts and results

1. **Step 1:** Population structure and genetic ancestry in the All of Us Research Program
   + 01_PopGen_merginingAndHarmonization.r - An R script that uses Plink to harmonize All of Us genotype data with 1000 Genome WGS data. It checks for allele consistencies and strand alignment, tries to fix mismatcing alleles and produces one single genetic dataset.
   + 01_PopGen_plotPCA.r - Results of PCA are visualized here with different settings (check **Step 2** for detailed unsupervised analyses of population structure).
   + 01_PopGenp_plotAdmixturePlots.V3.r -  Script to visualized ancestry distribution in the All of Us Research Program

![](https://media.springernature.com/full/springer-static/image/art%3A10.1038%2Fs41467-025-59351-8/MediaObjects/41467_2025_59351_Fig2_HTML.png?as=webp)

4. 
