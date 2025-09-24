rm(list=ls())
set.seed(13)

library('ggplot2')
library('dplyr')
library('data.table')
library('RColorBrewer')
library('ggpubr')
library('stringr')
library('parallel')

#Load the Demographic file and check it out.
hdb_clusters <- as.data.frame(fread('Data/Reviews/Clustering/HDBScan.5PC.2000MinSamples.2500MinCLusterSize.tsv', header = T, sep = '\t'))

#Get relevant columns.
hdb_clusters = hdb_clusters %>% select(SampleID, Pop, HDBScanPredictions)

nrow(hdb_clusters)
head(hdb_clusters)
count(hdb_clusters, HDBScanPredictions)


#Load the Demographic file and check it out.
rye <- as.data.frame(fread('Data/Reviews/Rye/RyeEstimates.16From30PCS.UnrelatedParticipants-16.13.Q', 
                           header = T, 
                           sep = '\t'))

nrow(rye)
head(rye)

#Drop down samples which do not have demographic information.
aou_dataset = merge(rye, hdb_clusters, by.x = "SampleID", by.y = "SampleID")
nrow(aou_dataset)
head(aou_dataset)

#Get some aggregate information.
print(paste("Total samples = ", nrow(aou_dataset), sep = " "))

print("Sample count breakdown:")
count(aou_dataset, HDBScanPredictions)

#Collapse the 14 sub-gropus into 7 continental groups.
continental_dataset = aou_dataset
continental_dataset$African = continental_dataset$WesternAfrican +
                                continental_dataset$SeneGambianAfrican +
                                continental_dataset$EastAfrican

continental_dataset$European = continental_dataset$NorthernEuropean +
                                continental_dataset$NorthWesternEuropean +
                                continental_dataset$IberianEuropean +
                                continental_dataset$ItalianEuropean

continental_dataset$SouthAsian = continental_dataset$NorthIndian + 
                                continental_dataset$SouthIndian

continental_dataset = continental_dataset %>% select(SampleID, HDBScanPredictions, 
                                           African, European, SouthAsian, EastAsian,
                                           NativeAmerican, Oceania, WestAsian)

multiply <- function(x, na.rm = FALSE) (x*100)
continental_dataset = continental_dataset %>% 
  mutate_at(vars(African:WestAsian), multiply)

head(continental_dataset)

#Define colors for admixture plots.
continental_ancestry_colors = c("SouthAsian" = "#B71C1C", "EastAsian" = "seagreen4", "WestAsian" = "#80461B", 
                               "European" = "#FFA000", "African" = "#283593",
                               "NativeAmerican" = "#41C9F8", "Oceania" = "#E040FB")

plot_admixture <- function(this_hdb_cluster){
    #Get the race for this dataset.
    this_cluster_dataset = continental_dataset %>% filter(HDBScanPredictions == this_hdb_cluster) %>% as.data.frame()

    #Get the mean ancestry value for each column.
    means = as.data.frame.list(colMeans(this_cluster_dataset %>% 
                                select(African, European, SouthAsian, EastAsian,
                                       NativeAmerican, Oceania, WestAsian)))
    
    #Get the max ancestry column index.
    max_ancestry_column_index = max.col(means)
    
    #Get max ancestry column name.
    max_ancestry_column_name = colnames(means)[max_ancestry_column_index]
    
    #Sort the dataset by max ancestry column.
    this_cluster_dataset_sorted = this_cluster_dataset[order(this_cluster_dataset[,max_ancestry_column_name], decreasing = TRUE),]

    row.names(this_cluster_dataset_sorted) <- NULL
    
    #Melt the dataset and preserve the index for the admixture plot.
    this_cluster_dataset_sorted$index= as.numeric(rownames(this_cluster_dataset_sorted))
    
    this_cluster_dataset_sorted_melt = reshape2::melt(data = this_cluster_dataset_sorted, 
                                        id.vars = c('SampleID', 'HDBScanPredictions', 'index'), 
                                        measure.vars = c('European', 'African', 
                                                          'NativeAmerican', 'WestAsian', 'SouthAsian',
                                                         'EastAsian', 'Oceania'))
    colnames(this_cluster_dataset_sorted_melt)[4] <- 'Ancestry'
    plot_width = log10(nrow(this_cluster_dataset_sorted)) * 4

    
    #Make the ggplot.
    rye_continental_admixture_plot = ggplot(data = this_cluster_dataset_sorted_melt,
                            aes(x=index, y=value, fill=Ancestry)) +
    geom_bar(stat="summary", width=1.05) + 
    scale_fill_manual(values=continental_ancestry_colors) + 
    theme_classic() +
    theme(line = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), 
          axis.text=element_blank(), axis.ticks=element_blank(),  
         legend.position = "none")
    
    #Save the plot.
    ggsave(paste("Data/Reviews/Figures/Rye/HDBCluster/AdmixtureV1", 
                 gsub(" ", "_", this_hdb_cluster, fixed = TRUE), "tiff", sep = "."),
           rye_continental_admixture_plot, width = plot_width, height = 8, dpi = 450, 
           units = "in", device='tiff', limitsize = FALSE)
    
    
    
    #Make the ggplot for axes labels.
    rye_continental_admixture_plot = ggplot(data = this_prediction_dataset_sorted_melt,
                            aes(x=index, y=value, fill=Ancestry)) +
    geom_bar(stat="summary", width=1.05) + 
    scale_fill_manual(values=continental_ancestry_colors) + 
    labs(title = "") +
    theme_classic() +
    theme(axis.title = element_blank(),legend.position = "none")

    #Save the plot.
    ggsave(paste("Data/Reviews/Figures/Rye/HDBCluster/AdmixtureV1.Legend", 
                 gsub(" ", "_", this_hdb_cluster, fixed = TRUE), "pdf", sep = "."),
           rye_continental_admixture_plot, width = plot_width, height = 2, dpi = 100, 
           units = "in", device='pdf', limitsize = FALSE)
    
    return(means)
}


predictions = unique(continental_dataset$HDBScanPredictions)

#Check the function you wish to call.
prediction_averages = mclapply(predictions, function(i) plot_admixture(i), mc.cores = 1)

predictions = unique(continental_dataset$HDBScanPredictions)
predictions

library("tidyr")

#Summarize the continental estimates by SIRE info.
continental_dataset_summarized <- continental_dataset %>% 
        group_by(HDBScanPredictions) %>% 
        summarise_at(vars(African,European,SouthAsian,EastAsian,NativeAmerican,WestAsian,Oceania), mean)

head(data.frame(continental_dataset_summarized), n = 20)

aggregated_ancestry_melt = continental_dataset_summarized %>%
                        pivot_longer(!HDBScanPredictions, names_to = "AncestryGroup", values_to = "Fraction")
head(aggregated_ancestry_melt)

#Define colors for admixture plots.
continental_ancestry_colors = c("SouthAsian" = "#B71C1C", "EastAsian" = "seagreen4", "WestAsian" = "#80461B", 
                               "European" = "#FFA000", "African" = "#283593",
                               "NativeAmerican" = "#41C9F8", "Oceania" = "#E040FB")

#Plot the stacked bar charts.
StackedBarChart = ggplot(aggregated_ancestry_melt, aes(fill=AncestryGroup, y=Fraction, x=HDBScanPredictions)) + 
    geom_bar(position="stack", stat="identity") + 
    scale_fill_manual(values = continental_ancestry_colors, name = "Continental Ancestry") +
    coord_flip() +
    labs(y = "", x = "SIRE Group") +
    theme_classic(base_size = 16) 

ggsave(paste0("Data//Reviews//Figures/Rye/HDBCluster/StackedBarChart.ContinentalAncestry.HDB.pdf"), StackedBarChart, width = 12, height = 4, dpi = 300, units = "in", device='pdf')
ggsave(paste0("Data//Reviews//Figures/Rye/HDBCluster/StackedBarChart.ContinentalAncestry.HDB.png"), StackedBarChart, width = 12, height = 4, dpi = 300, units = "in", device='png')

StackedBarChart





















#Load the Demographic file and check it out.
demographics <- as.data.frame(fread('Data/Metadata/DemographicDataSIRE.tsv', header = T, sep = '\t'))

#Get relevant columns.
demographics = demographics %>% select(person_id, SelfReportedRaceEthnicity)

nrow(demographics)
head(demographics)

#Load the Demographic file and check it out.
rye <- as.data.frame(fread('Data/Reviews/Rye/RyeEstimates.16From30PCS.UnrelatedParticipants-16.13.Q', 
                           header = T, 
                           sep = '\t'))

nrow(rye)
head(rye)

#Drop down samples which do not have demographic information.
aou_dataset = merge(rye, demographics, by.x = "SampleID", by.y = "person_id")
nrow(aou_dataset)
head(aou_dataset)

#Get some aggregate information.
print(paste("Total samples = ", nrow(aou_dataset), sep = " "))

print("Sample count breakdown:")
count(aou_dataset, SelfReportedRaceEthnicity)

head(aou_dataset)

#Collapse the 14 sub-gropus into 7 continental groups.
continental_dataset = aou_dataset
continental_dataset$African = continental_dataset$WesternAfrican +
                                continental_dataset$SeneGambianAfrican +
                                continental_dataset$EastAfrican

continental_dataset$European = continental_dataset$NorthernEuropean +
                                continental_dataset$NorthWesternEuropean +
                                continental_dataset$IberianEuropean +
                                continental_dataset$ItalianEuropean

continental_dataset$SouthAsian = continental_dataset$NorthIndian + 
                                continental_dataset$SouthIndian

continental_dataset = continental_dataset %>% select(SampleID, SelfReportedRaceEthnicity, 
                                           African, European, SouthAsian, EastAsian,
                                           NativeAmerican, Oceania, WestAsian)

multiply <- function(x, na.rm = FALSE) (x*100)
continental_dataset = continental_dataset %>% 
  mutate_at(vars(African:WestAsian), multiply)

head(continental_dataset)

#Define colors for admixture plots.
continental_ancestry_colors = c("SouthAsian" = "#B71C1C", "EastAsian" = "seagreen4", "WestAsian" = "#80461B", 
                               "European" = "#FFA000", "African" = "#283593",
                               "NativeAmerican" = "#41C9F8", "Oceania" = "#E040FB")

plot_admixture <- function(dataset){
    #Get the race for this dataset.
    dataset = dataset %>% as.data.frame()
    this_dataset_race = unique(dataset$SelfReportedRaceEthnicity)

    #Get the mean ancestry value for each column.
    means = as.data.frame.list(colMeans(dataset %>% 
                                select(African, European, SouthAsian, EastAsian,
                                       NativeAmerican, Oceania, WestAsian)))
    
    #Get the max ancestry column index.
    max_ancestry_column_index = max.col(means)
    
    #Get max ancestry column name.
    max_ancestry_column_name = colnames(means)[max_ancestry_column_index]
    
    #Sort the dataset by max ancestry column.
    dataset_sorted = dataset[order(dataset[,max_ancestry_column_name], decreasing = TRUE),]
    #dataset_sorted = dataset[order(dataset[,"European"], decreasing = TRUE),]

    row.names(dataset_sorted) <- NULL
    
    #Melt the dataset and preserve the index for the admixture plot.
    dataset_sorted$index= as.numeric(rownames(dataset_sorted))
    
    dataset_sorted_melt = reshape2::melt(data = dataset_sorted, 
                                        id.vars = c('SampleID', 'SelfReportedRaceEthnicity', 'index'), 
                                        measure.vars = c('European', 'African', 'NativeAmerican',
                                                   'SouthAsian', 'EastAsian', 'Oceania', 'WestAsian'))
    colnames(dataset_sorted_melt)[4] <- 'Ancestry'
    dataset_sorted_melt = dataset_sorted_melt %>% arrange(index)

    #Make the ggplot.
    rye_continental_admixture_plot = ggplot(data = dataset_sorted_melt,
                            aes(x=index, y=value, fill=Ancestry)) +
    geom_bar(stat="summary", width=1.05) + 
    scale_fill_manual(values=continental_ancestry_colors) + 
    labs(title = "") + theme_classic() +
    theme(line = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), 
          axis.text=element_blank(), axis.ticks=element_blank(),  
         legend.position = "none")

    #Every plot will have log(sample_size_of_SIRE_group) inches of width.
    plot_width = log10(nrow(dataset_sorted)) * 4
    
    #Save the plot.
    #ggsave(paste("Data//Reviews/Figures/Rye/AdmixtureV1", gsub(" ", "_", this_dataset_race, fixed = TRUE), "tiff", sep = "."),
    #       rye_continental_admixture_plot, width = plot_width, height = 20, dpi = 600, 
    #       units = "in", device='tiff', limitsize = FALSE)

    
    return(means)
}

#Plot the ADMIXTURE plot, one SIRE group at a time.
continental_dataset_sub = continental_dataset %>% filter(SelfReportedRaceEthnicity == "White") %>% as.data.frame()
confusion_matrix = continental_dataset %>%
   group_by(SelfReportedRaceEthnicity) %>%
   do(plot_admixture(.))

head(confusion_matrix)

confusion_matrix_melted = reshape2::melt(data = confusion_matrix, 
                    id.vars = c('SelfReportedRaceEthnicity'), 
                    measure.vars = c('European', 'African', 'NativeAmerican',
                               'SouthAsian','EastAsian','Oceania', 'WestAsian'))

confusion_matrix_melted = confusion_matrix_melted[order(as.character(confusion_matrix_melted$variable)), ]
head(confusion_matrix_melted, 25)

#Plot the confusion matrix between ancestry & SIRE
confusion_matrix_plot = ggplot(confusion_matrix_melted, 
                               mapping = aes(x = factor(variable, levels = c("EastAsian","SouthAsian","African", "NativeAmerican", "WestAsian", "Oceania", "European")), 
                                             y = factor(SelfReportedRaceEthnicity, levels = c("Asian",
                                                                                             "Black or African American",
                                                                                             "Hispanic or Latino",
                                                                                             "Middle Eastern or North African",
                                                                                             "Native Hawaiian or Other Pacific Islander",
                                                                                             "White",
                                                                                             "More than one population",
                                                                                             "No information")))) +
    geom_tile(aes(fill = value), colour = "white") +
    geom_text(aes(label = paste(round(value,2), "%", sep = ""), color = (value > 40)), size = 5) +
    scale_fill_gradient(low = "yellow1", high = "magenta4") +
    scale_color_manual(values=c("black", "white")) +
    scale_y_discrete(limits=rev, labels = function(x) str_wrap(x, width = 20)) +
    theme_classic(base_size = 24) + 
    theme(legend.position = "none",
         axis.text.y = element_text(angle = 45, hjust = 0.9),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_blank())

ggsave("Data/Reviews/Figures/Rye//SIREvsAncestry.png",
    confusion_matrix_plot, width = 9, height = 9, dpi = 300, 
    units = "in", device='png', limitsize = FALSE)
ggsave("Data/Reviews/Figures/Rye//SIREvsAncestry.pdf",
    confusion_matrix_plot, width = 9, height = 9, dpi = 300, 
    units = "in", device='pdf', limitsize = FALSE)

confusion_matrix_plot

#Summarize the continental estimates by SIRE info.
continental_dataset_summarized <- continental_dataset %>% 
        group_by(SelfReportedRaceEthnicity) %>% 
        summarise_at(vars(African,European,SouthAsian,EastAsian,NativeAmerican,WestAsian,Oceania), mean)

head(data.frame(continental_dataset_summarized), n = 20)

#Aggregate the average ancestry information.
aggregated_ancestry = continental_dataset_summarized %>% 
                            group_by(SelfReportedRaceEthnicity) %>%
                            summarise_at(vars(African,European,SouthAsian,EastAsian,NativeAmerican,WestAsian,Oceania), mean)
aggregated_ancestry_melt = reshape2::melt(aggregated_ancestry)
head(aggregated_ancestry_melt)

#Define colors for admixture plots.
continental_ancestry_colors = c("SouthAsian" = "#B71C1C", "EastAsian" = "seagreen4", "WestAsian" = "#80461B", 
                               "European" = "#FFA000", "African" = "#283593",
                               "NativeAmerican" = "#41C9F8", "Oceania" = "#E040FB")

#Plot the stacked bar charts.
StackedBarChart = ggplot(aggregated_ancestry_melt, aes(fill=variable, y=value, x=SelfReportedRaceEthnicity)) + 
    geom_bar(position="stack", stat="identity") + 
    scale_fill_manual(values = continental_ancestry_colors, name = "Continental Ancestry") +
    coord_flip() +
    labs(y = "", x = "SIRE Group") +
    theme_classic(base_size = 16) 

ggsave(paste0("Data//Reviews//Figures/Rye/StackedBarChart.ContinentalAncestry.RaceAndEthnicity.pdf"), StackedBarChart, width = 12, height = 4, dpi = 300, units = "in", device='pdf')
ggsave(paste0("Data//Reviews//Figures/Rye/StackedBarChart.ContinentalAncestry.RaceAndEthnicity.png"), StackedBarChart, width = 12, height = 4, dpi = 300, units = "in", device='png')

StackedBarChart


