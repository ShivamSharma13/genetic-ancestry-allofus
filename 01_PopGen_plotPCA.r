rm(list=ls())
set.seed(13)

options(scipen=100, digits=4)

library('dplyr')
library('ggplot2')
library('tidyr')
library('data.table')


#Load the Eignevector file and check it out.
eigenvec <- as.data.frame(fread('Data/Reviews//PCA/extractedChrAllPruned.PCA.30.eigenvec', header = T))
head(eigenvec)
nrow(eigenvec)

#Get the samples names and pops out.
eigenvec_left = eigenvec[,1:2]
colnames(eigenvec_left) = c("Pop", "SampleID")

#Get the PCs values out.
eigenvec_right = eigenvec[,3:ncol(eigenvec)]
colnames(eigenvec_right) <- paste('PC', c(1:30), sep = '')
eigenvec = cbind(eigenvec_left, eigenvec_right)

eigenvec = eigenvec[,1:32]

#Check the dataframe
head(eigenvec)

##Read rye.
sire = as.data.frame(fread("Data//Reviews/Rye/RyeEstimates.16From30PCS.UnrelatedParticipantsV7.Continental.7.Q")) %>%
        select(SampleID, SelfReportedRaceEthnicity)

head(sire)

merged = merge(eigenvec, sire, by = "SampleID")


dim(merged)
head(merged)

colors = c("Asian" = "#B71C1C", 
          "Black or African American" = "#283593",
          "Native Hawaiian or Other Pacific Islander" = "#E040FB",
          "Middle Eastern or North African" = "#80461B", 
          "Hispanic or Latino" = "#41C9F8",
          "White" = "#FFA000", 
          "More than one population" = "#9E9E9E",
          "No information" = "#e8e8e8")

g = ggplot(merged %>% filter(SelfReportedRaceEthnicity %in% c("Asian","Black or African American","Native Hawaiian or Other Pacific Islander",
                                                             "Middle Eastern or North African","White")),
       aes(x = PC1*-1, y = PC2, fill = as.factor(SelfReportedRaceEthnicity))) +
    geom_point(data = merged %>% filter(SelfReportedRaceEthnicity %in% c("More than one population", "No information" )), 
               aes(x = PC1*-1, y = PC2), stroke = 0.5, pch = 21, size = 1.5, fill = "#e8e8e8", color = "black") +  
    geom_point(data = merged %>% filter(SelfReportedRaceEthnicity %in% c("More than one population")), 
               aes(x = PC1*-1, y = PC2), stroke = 0.5, pch = 21, size = 1.5, fill = "#9E9E9E", color = "black") +  
    geom_point(data = merged %>% filter(SelfReportedRaceEthnicity %in% c("Hispanic or Latino")), 
               aes(x = PC1*-1, y = PC2), stroke = 0.5, pch = 21, size = 1.5, fill = "#41C9F8", color = "black") +  
    geom_point(stroke = 0.5, pch = 21, size = 1.5, color = "black") +  
    labs(x = "PC1", y = "PC2") +
    scale_fill_manual(values = colors, guide = "none") +
    theme_bw(base_size=24) +
    theme(legend.position = "none")
    
ggsave("Data/Reviews/Figures/PCA/PCA.SIRE.V2.png", g, 
       width = 9, height = 8, dpi = 600, units = "in", device='png')
ggsave("Data/Reviews/Figures/PCA/PCA.SIRE.V2.pdf", g, 
       width = 9, height = 8, dpi = 100, units = "in", device='pdf')

g

#Create labels for Continenetal Populations.
eigenvec = within(eigenvec, ContinentalPopulation <- 
            ifelse(Pop %in% c("forReferenceGIH","forReferenceSTU","forReferenceITU"), "SouthIndian", 
            ifelse(Pop %in% c("forReferenceCHB","forReferenceDai","forReferenceJPT","forReferenceKHV", "forReferenceShe", "forReferenceTujia"), "EastAsian", 
            ifelse(Pop %in% c("forReferenceBedouin","forReferenceDruze","forReferencePalestinian"), "WestAsian",
            ifelse(Pop %in% c("forReferenceBrahui","forReferenceMakrani","forReferenceBalochi"), "NorthIndian", 
            ifelse(Pop %in% c("forReferenceFIN","forReferenceGBR","forReferenceIBS", "forReferenceTSI", "forReferenceTuscan"), "European",
            ifelse(Pop %in% c("forReferenceESN","forReferenceGWD","forReferenceLWK", "forReferenceMSL", "forReferenceYRI"), "African",
            ifelse(Pop %in% c("forReferenceBougainville","forReferencePapuanHighlands","forReferencePapuanSepik"), "Oceanian",
            ifelse(Pop %in% c("AOU"), "AllOfUs",
            ifelse(Pop %in% c("forReferenceColombian", "forReferenceKaritiana", "forReferenceMaya", "forReferencePEL", "forReferencePima", "forReferenceSurui"), "NativeAmerican", "NotSelected"))))))))))

#Check the DF.
head(eigenvec)

colors = data.frame("EthnicGroup" = c("SouthIndian", "EastAsian", "WestAsian", "NorthIndian", 
                                      "European", "African", "NativeAmerican", "Oceanian",
                                        "AllOfUs"),
                    "Color" = c("#B71C1C", "seagreen4", "#80461B", "purple", 
                                "#FFA000", "#283593", "#41C9F8", "#E040FB", "gray60"))

eigenvec_pca = eigenvec[eigenvec$Pop != "NotSelected",]
eigenvec_pca = merge(eigenvec_pca, colors, by.x = "ContinentalPopulation", by.y = "EthnicGroup")
eigenvec_pca <- rbind(eigenvec_pca[eigenvec_pca$Color == "gray60", ], eigenvec_pca[eigenvec_pca$Color != "gray60",])

#Plot PCA for top two PCs.
pca_ref_only_plot = ggplot(eigenvec_pca[eigenvec_pca$Pop != "AOU",],
       aes(x=PC1*-1, y=PC2)) + 
    labs(subtitle = "",
         x = "PC1", y = "PC2") +
    geom_point(size = 2, pch = 21, color = "black", stroke = 0.25, fill = eigenvec_pca[eigenvec_pca$Pop != "AOU",]$Color) + 
    theme_classic(base_size = 24)

ggsave("Data/Reviews/Figures/PCA/PCA.ContinentalAncestry.ReferenceOnly.png", pca_ref_only_plot, width = 9, height = 8, dpi = 600, units = "in", device='png')


#Plot PCA for top two PCs.
pca_global_plot = ggplot(eigenvec_pca,
       aes(x=PC1*-1, y=PC2)) + 
    labs(subtitle = "",
         x = "PC1", y = "PC2") +
    geom_point(size = 2, pch = 21, color = "black", stroke = 0.25, fill = eigenvec_pca$Color) + 
    theme_classic(base_size = 24)

ggsave("Data/Reviews/Figures/PCA/PCA.ContinentalAncestry.ReferenceOverlap.png", pca_global_plot, width = 9, height = 8, dpi = 600, units = "in", device='png')


colors = c("Asian" = "#B71C1C", 
          "Black or African American" = "#283593",
          "Native Hawaiian or Other Pacific Islander" = "#E040FB",
          "Middle Eastern or North African" = "#80461B", 
          "Hispanic or Latino" = "#41C9F8",
          "White" = "#FFA000", 
          "More than one population" = "#9E9E9E",
          "No information" = "#e8e8e8")

g = ggplot(#data=hdbscan_counts %>% filter(HDBScanPredictions !=0),
        data=merged,
           aes(x=PC1*-1, 
           y=PC2, 
           fill=as.factor(SelfReportedRaceEthnicity))) +
    geom_point(stroke = 0.5, pch = 21, size = 1.5, color = "black") +  
    facet_wrap(~ factor(SelfReportedRaceEthnicity, levels = c("Asian","Black or African American","Hispanic or Latino",
                                                           "Middle Eastern or North African","Native Hawaiian or Other Pacific Islander",
                                                           "White","More than one population","No information")), 
               scales = "free_x", ncol = 4) +
    scale_fill_manual(values = colors, guide = "none") +
    labs(x = "PC1", y = "PC2") +
    theme_classic(base_size = 24) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
            panel.spacing = unit(0.1, "lines"),
            panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
            strip.background = element_rect(color = "black", size = 1),
            strip.text.x = element_text(size = 10),
             axis.line = element_line(size = 0.5))


ggsave("Data/Reviews/Figures/PCA/PCA.SIRE.Facet.pdf", g, width = 14, height = 8, 
       dpi = 600, units = "in", device='pdf')
ggsave("Data/Reviews/Figures/PCA/PCA.SIRE.Facet.png", g, width = 14, height = 8, 
       dpi = 600, units = "in", device='png')

g


