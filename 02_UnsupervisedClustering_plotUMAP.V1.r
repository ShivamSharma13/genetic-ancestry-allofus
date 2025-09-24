rm(list=ls())
set.seed(13)

options(scipen=100, digits=3)

library('dplyr')
library('ggplot2')
library('umap')
library('data.table')
library("dbscan")


options(scipen=100, digits=3)
options(repr.plot.width=20, repr.plot.height=16)

all_pcs = as.data.frame(fread("Data/Reviews/PCA/extractedChrAllPruned.PCA.30.eigenvec"))

#Remove AOU samples.
aou_pcs = all_pcs[grep("AOU", all_pcs$`#FID`), ]

nrow(aou_pcs)
head(aou_pcs)

head(aou_pcs[,3:18])

#Define UMAP parameters.
custom.config <- umap.defaults
custom.config$n_epocs = 100
custom.config$spread = 2
custom.config$min_dist = 0.1
custom.config$random_state = 13

aou_pcs.umap <- umap(aou_pcs[,3:18], config=custom.config)

df = data.frame(aou_pcs.umap$layout)

write.table(x = df, file = "Data/Reviews/Clustering/UMAP.100EPOCS.2Spread.0.1Dist.AOUOnly.tsv", 
            row.names = F, sep = "\t", quote = F)


df = as.data.frame(fread("Data/Reviews/Clustering/UMAP.100EPOCS.2Spread.0.1Dist.AOUOnly.tsv"))

merged_dataset = cbind(aou_pcs[,1:2], df)

nrow(merged_dataset)
head(merged_dataset)

names(merged_dataset) = c("Pop", "SampleID", "UMAP1", "UMAP2")
nrow(merged_dataset)
head(merged_dataset)

#Write the merged data into a file.
write.table(x = merged_dataset, file = "Data/Reviews/Clustering/UMAP.100EPOCS.2Spread.0.1Dist.AOUOnly.Merged.tsv", 
            row.names = F, sep = "\t", quote = F)


#Load the Demographic file and check it out.
demographics <- read.table('Data/Metadata/DemographicDataSIRE.tsv', header = T, sep = '\t')
demographics = demographics %>% select(person_id, SelfReportedRaceEthnicity)

head(demographics)

final_dataset = merge(merged_dataset, demographics, by.x = "SampleID", by.y = "person_id")
nrow(final_dataset)
head(final_dataset)

pca_colors = c("Asian" = "#B71C1C", 
          "Black or African American" = "#283593",
          "Native Hawaiian or Other Pacific Islander" = "#E040FB",
          "Middle Eastern or North African" = "#80461B", 
          "Hispanic or Latino" = "#41C9F8",
          "White" = "#FFA000", 
          "More than one population" = "#9E9E9E",
          "No information" = "#e8e8e8")

g = ggplot(final_dataset,
       aes(x=UMAP1, y=UMAP2, fill=SelfReportedRaceEthnicity)) + 
    scale_fill_manual(values = pca_colors, guide = "none") +
    labs(x = "UMAP1", y = "UMAP2") +
    geom_point(pch = 21, color = "black", stroke = 0.1) + 
    theme_classic(base_size = 24)

ggsave("Data/Reviews/Figures/Clustering/UMAP.SIRE.V2.pdf", g, width = 9, height = 8, 
       dpi = 100, units = "in", device='pdf')
ggsave("Data/Reviews/Figures/Clustering/UMAP.SIRE.V2.png", g, width = 9, height = 8, 
       dpi = 600, units = "in", device='png')

g

from sklearn.preprocessing import StandardScaler
from sklearn.cluster import DBSCAN
from sklearn.neighbors import NearestNeighbors
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import hdbscan

np.random.seed(13)

umap = pd.read_csv('Data/Reviews/Clustering/UMAP.100EPOCS.2Spread.0.1Dist.AOUOnly.Merged.tsv', delimiter='\t')
umap = umap.rename(columns={'#FID': 'Pop', 'IID': 'SampleID'})

print(umap.shape)
umap.head()

#Create the HDBSCAn object.
hdbscan_clusterer = hdbscan.HDBSCAN(core_dist_n_jobs = 16, 
                                    min_cluster_size = 2500, 
                                    min_samples = 2000)

#Fit the model.
hdbscan_clusterer.fit(umap.iloc[:, 2:])

### Outliers are coded as -1.
# Shift all cluster labels by 1. Cluster 0 is now the outlier cluster - like they do in R.
hdbscan_predictions = hdbscan_clusterer.labels_
hdbscan_predictions = hdbscan_predictions + 1

#Check the unique clusters.
print("Number of unique clusters (0 is an outlier): ", np.unique(hdbscan_predictions))

#Count the numnber of members in each cluster.
hdbscan_count_matrix = np.bincount(hdbscan_predictions)
print("Number of All of Us participants in each cluster: ", hdbscan_count_matrix)

umap["HDBScanPredictions"] = hdbscan_predictions
umap.head()

# Assign colors to different labels
color_dict = {0: '#dbdbdb', 1: '#FF5733', 2: '#33FF57', 3: '#3366FF',
              4: '#FF33C7', 5: '#33FFFF', 6: '#FFA933', 7: '#B233FF',
             8:'#FF33FF', 9:'#33FFB2', 10:'#FF3366', 11: '#d1e1ff',
             12: '#d37ed4', 13: '#8ecab5'}  # Define colors for each label



# Create a scatter plot and color by labels
plt.figure(figsize=(12, 12))

for label in umap['HDBScanPredictions'].unique():
    if label == 0:
        plt.scatter(umap[umap['HDBScanPredictions'] == label]['UMAP1'], 
                    umap[umap['HDBScanPredictions'] == label]['UMAP2'],
                    label=label, c=color_dict[label], s = 3, alpha=0.5)


for label in umap['HDBScanPredictions'].unique():
    if label != 0:
        plt.scatter(umap[umap['HDBScanPredictions'] == label]['UMAP1'], 
                    umap[umap['HDBScanPredictions'] == label]['UMAP2'],
                    label=label, c=color_dict[label], s = 3, alpha=0.5)

plt.title('Scatter Plot Colored by Labels')
plt.xlabel('X-axis')
plt.ylabel('Y-axis')
plt.legend()
plt.show()

umap.to_csv("Data/Reviews/Clustering/UMAP.100EPOCS.2Spread.0.1Dist.AOUOnly.Merged.Clustered.tsv",sep = "\t",index = False)



rm(list=ls())
set.seed(13)

options(scipen=100, digits=3)

library('dplyr')
library('ggplot2')
library('umap')
library('data.table')
library("dbscan")
library('ggforce')
library('concaveman')



options(scipen=100, digits=3)
options(repr.plot.width=20, repr.plot.height=16)

hdbscan_clusters_umap = as.data.frame(fread("Data/Reviews/Clustering/UMAP.100EPOCS.2Spread.0.1Dist.AOUOnly.Merged.Clustered.tsv"))
names(hdbscan_clusters_umap)[5] = "HDBScanPredictions.UMAP"

#Load the Demographic file and check it out.
demographics <- read.table('Data/Metadata/DemographicDataSIRE.tsv', header = T, sep = '\t')
demographics = demographics %>% select(person_id, SelfReportedRaceEthnicity)

hdbscan_clusters_umap = merge(hdbscan_clusters_umap, demographics, by.x = "SampleID", by.y = "person_id")
nrow(hdbscan_clusters_umap)
head(hdbscan_clusters_umap)

hdbscan_clusters_pca <- as.data.frame(fread('Data/Reviews/Clustering/HDBScan.5PC.2000MinSamples.2500MinCLusterSize.tsv', 
                               header = T, 
                               sep = '\t')) %>% 
                                select(SampleID, HDBScanPredictions)

hdbscan_clusters_umap = merge(hdbscan_clusters_umap, hdbscan_clusters_pca, by = "SampleID")

dim(hdbscan_clusters_umap)
head(hdbscan_clusters_umap)


cluster_colors = c("0" = "#9E9E9E", 
                  "1" = "#0c6ceb",
                  "2" = "#c70493",
                  "3" = "#3aec22", 
                  "4" = "#2B328D",
                  "5" = "#D4C928", 
                  "6" = "#175c34",
                  "7" = "#ab1513")

all_clusters = hdbscan_clusters_umap %>% filter(HDBScanPredictions.UMAP %in% c(1:13))

outlier_clusters = hdbscan_clusters_umap %>% filter(HDBScanPredictions.UMAP %in% c(0))

g = ggplot(all_clusters, 
       aes(x = UMAP1, y = UMAP2, fill = as.factor(HDBScanPredictions))) +
    geom_point(data = outlier_clusters, 
               aes(x = UMAP1, y = UMAP2), 
               stroke = 0.25, alpha = 0.75, pch = 21, size = 2, fill = "#a9a9a9", color = "black") +  
    geom_point(stroke = 0.25, alpha = 1, pch = 21, size = 2) + 
    geom_mark_hull(data = all_clusters,
                   expand = unit(0.9495, "mm"),radius = unit(1, "mm"),
                   size = 1,label.fontsize = 20, con.cap = 0,con.size = 1,alpha = 0,
                   aes(fill = factor(HDBScanPredictions.UMAP), label = factor(HDBScanPredictions.UMAP))) +
    labs(x = "UMAP1", y = "UMAP2") +
    scale_fill_manual(values = cluster_colors, guide = "none") +
    theme_bw(base_size=32) +
    theme(legend.position = "bottom")

#ggsave("Data/Reviews/Figures/Clustering/UMAP.HDBSCANClustering.Labels.pdf", g, width = 9, height = 8, 
#       dpi = 100, units = "in", device='pdf')
ggsave("Data/Reviews/Figures/Clustering.V2/UMAP.HDBSCANClustering.V2.png", g, width = 12, height = 12, 
       dpi = 600, units = "in", device='png')

hdbscan_clusters_pca = as.data.frame(fread("Data/Reviews/Clustering/HDBScan.5PC.2000MinSamples.2500MinCLusterSize.tsv"))
names(hdbscan_clusters_pca)[8] = "HDBScanPredictions.PCA"

final_dataset = merge(hdbscan_clusters_umap, hdbscan_clusters_pca, by = "SampleID")

dim(final_dataset)
head(final_dataset)


dim(final_dataset)
final_dataset = final_dataset %>% filter(HDBScanPredictions.UMAP != 0 & HDBScanPredictions.PCA != 0)
dim(final_dataset)


### Use this for renaming the clusters.

#ggplot(final_dataset %>% sample_n(2000), aes(x = PC1, y = PC2, label = HDBScanPredictions.PCA))+
#    geom_point() +
#    geom_text(vjust = -1, hjust = 0.5) +
#    theme_classic()

# Rename HDBScan clusters based on PCA. (left to right)
confusion_dataset = final_dataset %>% select(SampleID, HDBScanPredictions.UMAP, HDBScanPredictions.PCA)
confusion_dataset = confusion_dataset %>% 
                    mutate(HDBScanPredictions.PCA.LeftRightRename = case_when(HDBScanPredictions.PCA == 1 ~ 4,
                                                                             HDBScanPredictions.PCA == 2 ~ 3,
                                                                             HDBScanPredictions.PCA == 3 ~ 5,
                                                                             HDBScanPredictions.PCA == 4 ~ 7,
                                                                             HDBScanPredictions.PCA == 5 ~ 1,
                                                                             HDBScanPredictions.PCA == 6 ~ 6,
                                                                             HDBScanPredictions.PCA == 7 ~ 2)) %>%
                    select(-HDBScanPredictions.PCA)
head(confusion_dataset)

cluster_comparison_counts = as.data.frame(table(confusion_dataset %>% 
                                                select(HDBScanPredictions.PCA.LeftRightRename,HDBScanPredictions.UMAP)))

cluster_comparison_counts = cluster_comparison_counts %>%
                            group_by(HDBScanPredictions.PCA.LeftRightRename) %>% 
                            mutate(Percent = Freq/sum(Freq)) %>%
                            arrange(HDBScanPredictions.PCA.LeftRightRename)

head(cluster_comparison_counts,15)

#Plot the confusion matrix between ancestry & SIRE
confusion_matrix_plot = ggplot(cluster_comparison_counts, 
                               aes(x = as.factor(HDBScanPredictions.UMAP), 
                                             y = as.factor(HDBScanPredictions.PCA.LeftRightRename))) +
    geom_tile(aes(fill = Percent), colour = "white") +
    geom_text(aes(label = paste(round(Percent,2)*100, "%", sep = ""), color = (Percent > 0.4)), size = 7) +
    scale_fill_gradient(low = "yellow1", high = "magenta4") +
    scale_color_manual(values=c("black", "white")) +
    theme_classic(base_size = 24) + 
    labs(x = "UMAP", y = "PCA")+
    theme(legend.position = "bottom", legend.title = element_blank())

confusion_matrix_plot

#ggsave("Data/Reviews/Figures/Clustering/UMAP.SIRE.V3.ClusteringComparison.pdf", confusion_matrix_plot, width = 16, height = 10, 
#       dpi = 100, units = "in", device='pdf')
ggsave("Data/Reviews/Figures/Clustering.V2/UMAP.SIRE.V4.ClusteringComparison.pdf", confusion_matrix_plot, width = 16, height = 10, 
       dpi = 600, units = "in", device='pdf')


