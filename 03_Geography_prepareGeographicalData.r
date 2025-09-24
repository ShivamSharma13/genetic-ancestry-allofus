rm(list=ls())
set.seed(13)

options(scipen=100, digits=3)

library('dplyr')
library('ggplot2')
library('data.table')
library("rgdal")
library('parallel')
library("sf")

options(repr.plot.width=20, repr.plot.height=12)

rye_estimates <- as.data.frame(fread('Data//Reviews/Rye/RyeEstimates.16From30PCS.UnrelatedParticipants-16.13.Q', header = T, sep = '\t'))

dim(rye_estimates)
head(rye_estimates)

demographics <- as.data.frame(fread('Data/Metadata/DemographicDataSIRE.tsv', header = T, sep = '\t')) %>% select(person_id, SelfReportedRaceEthnicity)

dim(demographics)
head(demographics)

rye_estimates = merge(rye_estimates, demographics, by.x = "SampleID", by.y = "person_id")
dim(rye_estimates)
head(rye_estimates)



#Collapse the 14 sub-gropus into 7 continental groups.
continental_dataset = rye_estimates
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

dim(continental_dataset)
head(continental_dataset)



write.table(x = continental_dataset, 'Data//Reviews/Rye/RyeEstimates.16From30PCS.UnrelatedParticipantsV7.Continental.7.Q', 
            sep = '\t', quote = F, row.names = F)


zip_codes = as.data.frame(fread('Data/Reviews/Metadata/ZipCodeData.tsv'))

dim(zip_codes)
head(zip_codes)

nrow(unique(zip_codes['zip_code']))

zip_codes = zip_codes %>% select(person_id, zip_code)

head(zip_codes)

dataset = merge(continental_dataset, zip_codes, by.x = "SampleID", by.y = "person_id")

nrow(dataset)

dataset$ZipCode2Digits = substr(dataset$zip_code, 1, 2)
dataset$ZipCode3Digits = substr(dataset$zip_code, 1, 3)

dim(dataset)
head(dataset)

#Get the summary at 3-digit resolution. This is the maximum resolution All of Us provides.
summarized_zips = dataset %>% 
   group_by(ZipCode3Digits) %>%
   summarize_at(vars(African,European,SouthAsian,EastAsian,NativeAmerican,WestAsian,Oceania), mean)

summarized_zips_sample_counts = dataset %>% group_by(ZipCode3Digits) %>%
                    summarize(SampleCounts = n())

summarized_zips = merge(summarized_zips, 
                                     summarized_zips_sample_counts, 
                                     by = "ZipCode3Digits")

dim(summarized_zips)
head(summarized_zips)

zip_to_state = as.data.frame(fread('Data/Reviews/Maps/ReferenceFiles//Zip5ToState.tsv', colClasses = c("character", "character")))
zip_to_state$ZipCode3Digits = substr(zip_to_state$ZipCode5Digits, 1, 3)

zip3_to_state = unique(zip_to_state %>% select(ZipCode3Digits, State))

dim(zip3_to_state)
head(zip3_to_state)

#These zips overlap between two or more states.
zips_to_remove = c("205 MD", "205 VA", "726 MO", "739 TX", "834 WY", "967 AS",
                  "969 GU","969 PW","969 FM","969 MP","969 MH", "063 NY")
zip3_to_state$MergedZip = paste(zip3_to_state$ZipCode3Digits, zip3_to_state$State)
zip3_to_state = zip3_to_state %>% filter(!(MergedZip %in% zips_to_remove))
zip3_to_state = zip3_to_state %>% select(ZipCode3Digits, State)

dim(zip3_to_state)
head(zip3_to_state)


#Any zip code that is assigned to more than one state?
zip3_to_state %>% count(ZipCode3Digits) %>% arrange(-n) %>% filter(n>1)

dim(dataset)
head(dataset)

dataset = merge(dataset, zip3_to_state, by = "ZipCode3Digits")
dim(dataset)
head(dataset)

count(dataset, State) %>% arrange(n) %>% head(20)
count(dataset, zip_code) %>% arrange(-n) %>% head(10)

#Define colors for admixture plots.
continental_ancestry_colors = c("SouthAsian" = "#B71C1C", "EastAsian" = "seagreen4", "WestAsian" = "#80461B", 
                               "European" = "#FFA000", "African" = "#283593",
                               "NativeAmerican" = "#41C9F8", "Oceania" = "#E040FB")

plot_admixture <- function(this_state){
    #Get the race for this dataset.
    this_state_dataset = dataset %>% filter(State == this_state)

    #Get the mean ancestry value for each column.
    means = as.data.frame.list(colMeans(this_state_dataset %>% 
                                select(African, European, SouthAsian, EastAsian,
                                       NativeAmerican, Oceania, WestAsian)))
    
    #Get the max ancestry column index.
    max_ancestry_column_index = max.col(means)
    
    #Get max ancestry column name.
    max_ancestry_column_name = colnames(means)[max_ancestry_column_index]
    
    #Sort the dataset by max ancestry column.
    this_state_dataset_sorted = this_state_dataset[order(this_state_dataset[,max_ancestry_column_name], decreasing = TRUE),]

    row.names(this_state_dataset_sorted) <- NULL
    
    #Melt the dataset and preserve the index for the admixture plot.
    this_state_dataset_sorted$index= as.numeric(rownames(this_state_dataset_sorted))
    
    this_state_dataset_sorted_melt = reshape2::melt(data = this_state_dataset_sorted, 
                                        id.vars = c('SampleID', 'State', 'index'), 
                                        measure.vars = c('European', 'African', 'NativeAmerican',
                                                   'SouthAsian', 'EastAsian', 'Oceania', 'WestAsian'))
    colnames(this_state_dataset_sorted_melt)[4] <- 'Ancestry'
    
    #Make the ggplot.
    rye_continental_admixture_plot = ggplot(data = this_state_dataset_sorted_melt,
                            aes(x=as.factor(index), y=value, fill=Ancestry)) +
    geom_bar(stat="summary", width=1) + 
    scale_fill_manual(values=continental_ancestry_colors) + 
    labs(title = "") +
    theme(line = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), 
          axis.text=element_blank(), axis.ticks=element_blank(),  
         legend.position = "none")

    #Every plot will have log(sample_size_of_SIRE_group) inches of width.
    plot_width = log10(nrow(this_state_dataset_sorted)) * 4

    #Save the plot.
    ggsave(paste("Data//Reviews/Figures//Maps//AdmixturePlots/StateAdmixtureV1", gsub(" ", "_", this_state, fixed = TRUE), "png", sep = "."),
           rye_continental_admixture_plot, width = 8, height = 8, dpi = 600, 
           units = "in", device='png', limitsize = FALSE)
    ggsave(paste("Data//Reviews/Figures/Maps//AdmixturePlots/StateAdmixtureV1", gsub(" ", "_", this_state, fixed = TRUE), "pdf", sep = "."),
           rye_continental_admixture_plot, width = 8, height = 8, dpi = 600, 
           units = "in", device='pdf', limitsize = FALSE)

    
    return(means)
}

#states = c("RI", "ME", "MS")
states = unique(dataset$State)

state_averages = mclapply(states, function(i) plot_admixture(i), mc.cores = 16)

unique(dataset$State)








