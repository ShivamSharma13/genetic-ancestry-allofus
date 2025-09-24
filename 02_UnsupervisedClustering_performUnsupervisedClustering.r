rm(list=ls())
set.seed(13)

library('dplyr')
library('ggplot2')
library('tidyr')
library('data.table')
library('plotly')
library('reticulate')
library('reshape2')
library('RColorBrewer')
library('ggforce')
library('concaveman')
library('FNN')


#For plotly exports
reticulate::py_run_string("import sys")
reticulate::py_run_string("import plotly")


options(scipen=100, digits=3)
options(repr.plot.width=20, repr.plot.height=16)

#Load the eigenvec file and check it out.
eigenvec <- as.data.frame(fread('Data/Reviews/PCA/extractedChrAllPruned.PCA.30.eigenvec', header = T, sep = '\t'))

#Get the samples names and pops out.
eigenvec_left = eigenvec[,1:2]
colnames(eigenvec_left) = c("Pop", "SampleID")

#Get the PCs values out.
eigenvec_right = eigenvec[,3:ncol(eigenvec)]
colnames(eigenvec_right) <- paste('PC', c(1:30), sep = '')
eigenvec = cbind(eigenvec_left, eigenvec_right)

eigenvec = eigenvec[,1:32]

#Select top 16 PCs to be consistent with Rye.
eigenvec = eigenvec %>% select(Pop, SampleID, c(paste('PC', c(1:16), sep = '')))

#Sample out only the All of Us samples.
eigenvec = eigenvec %>% filter(Pop == "AOU")

#Check the dataframe
dim(eigenvec)
head(eigenvec)

kd <- with(eigenvec, MASS::kde2d(PC1, PC2, n = 24))

fig <- plot_ly(x = kd$x, y = kd$y, z = kd$z, 
                contours = list(
                z = list(show = TRUE, start = round(min(kd$z),-2),
                            end = round(max(kd$z),-2), 
                            size = 4000)),
                colorscale = list(c(0, 0.1, 0.25, 0.5, 0.75, 1), 
                             c("#FCF55F", "#8a57c0", "#6f449c", "#54327a", "#3b205a", "#24103b")),
                width = 1000,height = 1000) %>%
    add_surface() %>%
    hide_colorbar() %>%
    layout(scene = list(camera = list(eye = list(x=-2, y=0, z = 0.75)),
                      xaxis = list(showgrid = F, autotick = F), 
                      yaxis = list(showgrid = F, autotick = F), 
                      zaxis = list(showgrid = F, autotick = F))) %>%
    config(
    toImageButtonOptions = list(
      format = "png",
      filename = "Data/Reviews/Figures/Clustering.V2/DensityPlot.KDE3D.V2",
      width = 10000,
      height = 10000
    )
  )

fig
#save_image(fig, "Data/Reviews/Figures/Clustering.V2/DensityPlot.KDE3D.V3.pdf", width = 8000, height = 8000)


kd <- with(eigenvec, MASS::kde2d(PC1, PC3, n = 18))

fig <- plot_ly(x = kd$x, y = kd$y, z = kd$z, 
                contours = list(
                z = list(show = TRUE, start = round(min(kd$z),-2),
                            end = round(max(kd$z),-2), 
                            size = 4000)),
                colorscale = list(c(0, 0.1, 0.25, 0.5, 0.75, 1), 
                             c("#FCF55F", "#8a57c0", "#6f449c", "#54327a", "#3b205a", "#24103b")),
                width = 1000,height = 1000) %>%
    add_surface() %>%
    hide_colorbar() %>%
    layout(scene = list(camera = list(eye = list(x=-2, y=0, z = 0.75)),
                      xaxis = list(showgrid = T, autotick = F), 
                      yaxis = list(showgrid = T, autotick = F), 
                      zaxis = list(showgrid = T, autotick = F))) %>%
    config(
    toImageButtonOptions = list(
      format = "png",
      filename = "Data/Reviews/Figures/Clustering/DensityPlot.KDE3D.PC13.V2",
      width = 10000,
      height = 10000
    )
  )

fig
#save_image(fig, "Data/Reviews/Figures/Clustering.V2//DensityPlot.KDE3D.PC13.V1.pdf", width = 8000, height = 8000)


#ggplot(eigenvec, aes(x = PC1, y = PC2)) +
#    geom_point(alpha = 0.05, pch=21, fill = "black") +  # Scatter plot of points
#    geom_density_2d(n=24) +        # Contour lines indicating density
#    labs(title = "Density of Data Points", x = "X-axis", y = "Y-axis")+
#    theme_bw(base_size=24)

#eigenvec_dbscan = dbscan(eigenvec %>% select(PC1, PC2, PC3, PC4, PC5), 
#       eps = 0.0002, 
#       minPts = 250, 
#       borderPoints = TRUE)

#eigenvec$DBScanCluster = eigenvec_dbscan$cluster
#head(eigenvec)

#eigenvec_dbscan

hdbscan <- as.data.frame(fread('Data/Reviews/Clustering/HDBScan.5PC.2000MinSamples.2500MinCLusterSize.tsv', 
                               header = T, 
                               sep = '\t'))

dim(hdbscan)
head(hdbscan)

cluster_colors = c("0" = "#9E9E9E", 
                  "1" = "#0c6ceb",
                  "2" = "#c70493",
                  "3" = "#3aec22", 
                  "4" = "#2B328D",
                  "5" = "#D4C928", 
                  "6" = "#175c34",
                  "7" = "#ab1513")

main_clusters = hdbscan %>% filter(HDBScanPredictions %in% c(1,2,4,5,6,7))
main_clusters$PC1 = main_clusters$PC1 * -1

secondary_clusters = hdbscan %>% filter(HDBScanPredictions %in% c(3))
secondary_clusters$PC1 = secondary_clusters$PC1 * -1

dim(secondary_clusters)
head(secondary_clusters)

all_clusters = hdbscan %>% filter(HDBScanPredictions %in% c(1,2,3,4,5,6,7))
all_clusters$PC1 = all_clusters$PC1 * -1

outlier_clusters = hdbscan %>% filter(HDBScanPredictions %in% c(0))
outlier_clusters$PC1 = outlier_clusters$PC1 * -1

g = ggplot(all_clusters, 
       aes(x = PC1, y = PC2, fill = as.factor(HDBScanPredictions))) +
    geom_point(data = outlier_clusters, 
               aes(x = PC1, y = PC2), stroke = 0, alpha = 0.25, pch = 21, size = 2, fill = "#a9a9a9") +  
    geom_point(stroke = 0, alpha = 0.5, pch = 21, size = 2) + 
    geom_mark_hull(data = main_clusters,
                   expand = unit(0.75, "mm"),radius = unit(2, "mm"),
                   aes(fill = factor(HDBScanPredictions, levels = c(6,7,5,1,2,4)))) +
    geom_mark_hull(data = secondary_clusters,
                   expand = unit(0.75, "mm"),radius = unit(2, "mm"),
                   aes(fill = as.factor(HDBScanPredictions))) +
    labs(x = "PC1 (68.8%)", y = "PC2 (18.58%)") +
    scale_fill_manual(values = cluster_colors, guide = "none") +
    theme_bw(base_size=32) +
    theme(legend.position = "none")

ggsave("Data/Reviews/Figures/Clustering.V2/HDBScanResults.ClusterColors.PC12.V1.png", g, 
       width = 12, height = 12, dpi = 600, units = "in", device='png')
#ggsave("Data/Reviews/Figures/Clustering.V2/HDBScanResults.ClusterColors.PC12.V1.pdf", g, 
#       width = 12, height = 12, dpi = 100, units = "in", device='pdf')

g

g = ggplot(all_clusters, 
       aes(x = PC1, y = PC3, fill = as.factor(HDBScanPredictions))) +
    geom_point(data = outlier_clusters, 
               aes(x = PC1, y = PC3), stroke = 0, alpha = 0.25, pch = 21, size = 2, fill = "#a9a9a9") +  
    geom_point(stroke = 0, alpha = 0.5, pch = 21, size = 2) + 
    geom_mark_hull(data = main_clusters,
                   expand = unit(0.75, "mm"),radius = unit(2, "mm"),
                   aes(fill = factor(HDBScanPredictions, levels = c(6,7,5,1,2,4)))) +
    geom_mark_hull(data = secondary_clusters,
                   expand = unit(0.75, "mm"),radius = unit(2, "mm"),
                   aes(fill = as.factor(HDBScanPredictions))) +
    labs(x = "PC1 (68.8%)", y = "PC3 (6.18%)") +
    scale_fill_manual(values = cluster_colors, guide = "none") +
    theme_bw(base_size=32) +
    theme(legend.position = "none")

ggsave("Data/Reviews/Figures/Clustering.V2/HDBScanResults.ClusterColors.PC13.V1.png", g, 
       width = 12, height = 12, dpi = 600, units = "in", device='png')
#ggsave("Data/Reviews/Figures/Clustering.V2/HDBScanResults.ClusterColors.PC13.V1.pdf", g, 
#       width = 12, height = 12, dpi = 100, units = "in", device='pdf')

g

# Standardize the PC columns
scaled_data <- scale(eigenvec[, c("PC1", "PC2", "PC3", "PC4", "PC5")])
dim(scaled_data)
head(scaled_data)

# Choose the value of k (number of neighbors)
k <- 100

# Compute distances
distances <- knn.dist(scaled_data, k = k)

distances = distances %>% as.data.frame() 
distances$Sums = rowSums(distances < 0.1)

ress = cbind(eigenvec, distances %>% select(Sums))

#Plot PC1 and PC2.
g = ggplot(ress, aes(x = PC1*-1, y = PC2)) +
    geom_point(alpha = 0.5, stroke = 0.05, color = "black", fill = "grey", pch = 21, size = 2) +
    theme_bw(base_size = 32) +
    labs(x = "PC1 (68.8%)", y = "PC2 (18.58%)") +
    theme(legend.position = "none")

ggsave("Data/Reviews/Figures/Clustering.V2/HDBScanResults.Greyed.PC12.V1.png", g, 
       width = 12, height = 12, dpi = 600, units = "in", device='png')

distances = distances %>% as.data.frame() 
distances$Sums = rowSums(distances < 0.1)

ress = cbind(eigenvec, distances %>% select(Sums))
# Create histogram using ggplot
ggplot(ress, aes(x = Sums)) +
    geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
    theme_bw()

#Plot PC1 and PC2.
g = ggplot(ress, aes(x = PC1*-1, y = PC2, fill = Sums)) +
    geom_point(alpha = 0.5, stroke = 0.05, color = "black", pch = 21, size = 2) +
    scale_fill_gradient(low = "yellow", high = "purple")  +
    theme_bw(base_size = 32) +
    labs(x = "PC1 (68.8%)", y = "PC2 (18.58%)") +
    theme(legend.position = "none")

ggsave("Data/Reviews/Figures/Clustering.V2/HDBScanResults.Neighbors.PC12.V1.png", g, 
       width = 12, height = 12, dpi = 600, units = "in", device='png')
#ggsave("Data/Reviews/Figures/Clustering.V2/HDBScanResults.Neighbors.PC12.V1.pdf", g, 
#       width = 12, height = 12, dpi = 100, units = "in", device='pdf')

#Plot the legend scale.
#g = g+theme(legend.position = "bottom")
#ggsave("Data/Reviews/Figures/Clustering.V2/HDBScanResults.Neighbors.PC12.V1.Legend.png", g, 
#       width = 2, height = 2, dpi = 200, units = "in", device='png')

#Plot PC1 and PC3.
g = ggplot(ress, aes(x = PC1*-1, y = PC3, fill = Sums)) +
    geom_point(alpha = 0.5, stroke = 0.05, color = "black", pch = 21, size = 2) +
    scale_fill_gradient(low = "yellow", high = "purple")  +
    theme_bw(base_size = 32) +
    labs(x = "PC1 (68.8%)", y = "PC3 (6.18%)") +
    theme(legend.position = "none")

#ggsave("Data/Reviews/Figures/Clustering.V2/HDBScanResults.Neighbors.PC13.V1.png", g, 
#       width = 12, height = 12, dpi = 600, units = "in", device='png')
#ggsave("Data/Reviews/Figures/Clustering.V2/HDBScanResults.Neighbors.PC13.V1.pdf", g, 
#       width = 12, height = 12, dpi = 100, units = "in", device='pdf')


citation("MASS")



































#Get prediction counts stratified by SIRE per K-cluster
count(hdbscan, HDBScanPredictions)
hdbscan_counts = hdbscan %>% group_by(HDBScanPredictions) %>% count(SelfReportedRaceEthnicity)
names(hdbscan_counts) = c("HDBScanPredictions", "SelfReportedRaceEthnicity", "Counts")

dim(hdbscan_counts)
head(hdbscan_counts,10)

hdbscan_counts = tidyr::complete(hdbscan_counts,  
                       SelfReportedRaceEthnicity = c("Asian", "Black or African American", 
                                                     "Hispanic or Latino", 
                                                     "Middle Eastern or North African", 
                                                     "More than one population", 
                                                     "Native Hawaiian or Other Pacific Islander", 
                                                     "No information", "White"), 
                       fill = list(Counts = 0))

dim(hdbscan_counts)
head(hdbscan_counts,10)


#Get total per group.
hdbscan_counts_total_label_counts = hdbscan %>% count(HDBScanPredictions)
names(hdbscan_counts_total_label_counts) = c("HDBScanPredictions", "TotalCounts")

#Merge data
hdbscan_counts = merge(hdbscan_counts, 
                               hdbscan_counts_total_label_counts, 
                               by = "HDBScanPredictions")

#Get fraction representation for each SIRE group for each K-cluster.
hdbscan_counts$SIREFractionWithinLabel = (hdbscan_counts$Counts / hdbscan_counts$TotalCounts) * 100

nrow(hdbscan_counts)
head(hdbscan_counts)

#Check the SIRE distribution for cluster number 5. This is just to have a look at how the data looks like.
hdbscan_counts[hdbscan_counts$HDBScanPredictions == "0",
                       c("SelfReportedRaceEthnicity", "Counts")]


colors = c("Asian" = "#B71C1C", 
          "Black or African American" = "#283593",
          "Native Hawaiian or Other Pacific Islander" = "#E040FB",
          "Middle Eastern or North African" = "#80461B", 
          "Hispanic or Latino" = "#41C9F8",
          "White" = "#FFA000", 
          "More than one population" = "#9E9E9E",
          "No information" = "#e8e8e8")

### Replace 0 with "Outlier" label.
hdbscan_counts$HDBScanPredictions = ifelse(hdbscan_counts$HDBScanPredictions == 0, "Outliers", hdbscan_counts$HDBScanPredictions) 
hdbscan_counts$HDBScanPredictionLabels = paste0(hdbscan_counts$HDBScanPredictions, 
                                               " (n = ",
                                               hdbscan_counts$TotalCounts,
                                               ")")
head(hdbscan_counts)

g = ggplot(#data=hdbscan_counts %>% filter(HDBScanPredictions !=0),
        data=hdbscan_counts,
           aes(x=as.factor(HDBScanPredictions), 
           y=SIREFractionWithinLabel, 
           fill=SelfReportedRaceEthnicity)) +
    geom_bar(stat="identity", position=position_dodge(), color = "black", size = 0.5) +
    facet_wrap(~ HDBScanPredictionLabels, scales = "free_x", ncol = 4) +
    ylim(c(0,100)) +
    scale_fill_manual(values = colors, guide = "none") +
    labs(x = "", y = "Self-identified race and ethnicity percentages") +
    theme_classic(base_size = 24) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
            panel.spacing = unit(0.1, "lines"),
            panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
            strip.background = element_rect(color = "black", size = 1),
            strip.text.x = element_text(size = 12),
             axis.line = element_line(size = 0.5))


ggsave("Data/Reviews/Figures/Clustering/SIREDistribution.V1.pdf", g, width = 9, height = 8, 
       dpi = 600, units = "in", device='pdf')
ggsave("Data/Reviews/Figures/Clustering/SIREDistribution.V1.png", g, width = 9, height = 8, 
       dpi = 600, units = "in", device='png')

g

head(eigenvec)

cluster_colors = c("0" = "#9E9E9E", 
                  "1" = "#137C3E",
                  "2" = "#B84398",
                  "3" = "#EC8F22", 
                  "4" = "#2B328D",
                  "5" = "#D4C928", 
                  "6" = "#05C1F2",
                  "7" = "#ED2624")

g = ggplot(eigenvec, 
       aes(x = PC1*-1, y = PC2, fill = as.factor(HDBScanPredictions))) +
    geom_point(data = hdbscan %>% filter(HDBScanPredictions == 0), 
               aes(x = PC1*-1, y = PC2), stroke = 0, alpha = 0.1, pch = 21, size = 2, fill = "#a9a9a9") +  
    geom_point(stroke = 0, alpha = 0.1, pch = 21, size = 2) +  
    labs(x = "PC1", y = "PC2") +
    scale_fill_manual(values = cluster_colors, guide = "none") +
    theme_bw(base_size=40) +
    theme(legend.position = "none")

ggsave("Data/Reviews/Figures/Clustering/HDBScanResults.SIREColors.V1.tiff", g, 
       width = 9, height = 8, dpi = 600, units = "in", device='tiff')
ggsave("Data/Reviews/Figures/Clustering/HDBScanResults.SIREColors.V1.pdf", g, 
       width = 9, height = 8, dpi = 100, units = "in", device='pdf')

g
