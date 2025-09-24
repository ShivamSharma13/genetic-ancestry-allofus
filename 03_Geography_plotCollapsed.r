rm(list=ls())
set.seed(13)

options(scipen=100, digits=3)

library("ggplot2")
library("rgdal")
library("sf")

setwd("~/Dropbox (GaTech)/AllOfUs/Shivam-DemoProject2/")

#########################################Work on the zip codes.###############################################
#Read the zip code file (2 & 3 digits) with ancestry sumarization.
zip_code_2digits = read.table('Data/Summarized/ZipCode2DigitAncestryCollapsed.tsv', header = T, sep = '\t')
zip_code_3digits = read.table('Data/Summarized/ZipCode3DigitAncestryCollapsed.tsv', header = T, sep = '\t')

#Read the mainland zip codes only.
nonmainland_zip_codes = read.table('Data/Summarized/NonMainlandZipCodes.tsv', header = T, sep = '\t', colClasses = 'character')

#Remove the non-mainland zip codes.
zip_code_2digits = zip_code_2digits[!(zip_code_2digits$State %in% c("AK", "HI")),]
zip_code_3digits = zip_code_3digits[!(zip_code_3digits$State %in% c("AK", "HI")),]

#Read the shape file for Zip codes.
shp_zip_file = "Data/MapData/tl_2022_us_zcta520/tl_2022_us_zcta520.shp"
zip_entries = st_read(shp_zip_file)

zip_entries$ZipCode3Digits = substr(zip_entries$ZCTA5CE20, 1, 3)
zip_entries$ZipCode2Digits = substr(zip_entries$ZCTA5CE20, 1, 2)

#Remove non-mainland zip codes from the shape file.
zip_entries = zip_entries[!(zip_entries$ZipCode3Digits %in% nonmainland_zip_codes$ZipCode3Digits),]

#Merge ancestries with zip code shape file.
merged_zip2_ancestries = merge(zip_entries, zip_code_2digits, by.x = "ZipCode2Digits", by.y = "ZipCode2Digits", all = TRUE)
merged_zip3_ancestries = merge(zip_entries, zip_code_3digits, by.x = "ZipCode3Digits", by.y = "ZipCode3Digits")

#########################################Work on states#######################################################
shp_state_file = "Data/MapData//tl_2022_us_state/tl_2022_us_state.shp"
state_entries = st_read(shp_state_file)
state_entries = state_entries[!(state_entries$NAME == "Puerto Rico" | 
                                    state_entries$NAME == "American Samoa" | 
                                    state_entries$NAME == "Alaska" | 
                                    state_entries$NAME == "Guam" | 
                                    state_entries$NAME == "Commonwealth of the Northern Mariana Islands" | 
                                    state_entries$NAME == "United States Virgin Islands" | 
                                    state_entries$NAME == "Hawaii"),]

#Plot for African ancestry.
g = ggplot() + 
    geom_sf(data = state_entries, color="black", fill="grey60", lwd = 0) +
    geom_sf(data = merged_zip2_ancestries, mapping = aes(fill = African, geometry = geometry), lwd = 0) +
    geom_sf(data = merged_zip3_ancestries, mapping = aes(fill = African, geometry = geometry), lwd = 0) +
    geom_sf(data = state_entries, color="black", fill="transparent", lwd = 1) +
    scale_fill_gradient(low = "white", high = "#283593", na.value = "grey60", limits = c(0, 85)) +
    theme_classic(base_size = 60) + theme(line = element_blank(),
                                          axis.text = element_blank(),
                                          title = element_blank(),
                                          legend.position = "bottom",
                                          legend.key.width = unit(4, 'in'),
                                          legend.key.height= unit(1, 'in'))

ggsave("~/Dropbox (GaTech)/AllOfUs/Shivam-DemoProject2/Figures/FinalManuscriptFigures//Raw/Maps/African.png", g, width = 24, height = 24, dpi = 300, units = "in", device='png')
#ggsave("~/Dropbox (GaTech)/AllOfUs/Shivam-DemoProject2/Figures/FinalManuscriptFigures//Raw/Maps/African.pdf", g, width = 24, height = 24, dpi = 300, units = "in", device='pdf')


#Plot for European ancestry.
g = ggplot() + 
    geom_sf(data = state_entries, color="black", fill="grey60", lwd = 0) +
    geom_sf(data = merged_zip2_ancestries, mapping = aes(fill = European, geometry = geometry), lwd = 0) +
    geom_sf(data = merged_zip3_ancestries, mapping = aes(fill = European, geometry = geometry), lwd = 0) +
    geom_sf(data = state_entries, color="black", fill="transparent", lwd = 1) +
    scale_fill_gradient(low = "white", high = "#FFA000", na.value = "grey60", limits = c(0, 100)) +
    theme_classic(base_size = 60) + theme(line = element_blank(),
                                          axis.text = element_blank(),
                                          title = element_blank(),
                                          legend.position = "bottom",
                                          legend.key.width = unit(4, 'in'),
                                          legend.key.height= unit(1, 'in'))

ggsave("~/Dropbox (GaTech)/AllOfUs/Shivam-DemoProject2/Figures/FinalManuscriptFigures//Raw/Maps/European.png", g, width = 24, height = 24, dpi = 300, units = "in", device='png')
#ggsave("~/Dropbox (GaTech)/AllOfUs/Shivam-DemoProject2/Figures/FinalManuscriptFigures//Raw/Maps/European.pdf", g, width = 24, height = 24, dpi = 300, units = "in", device='pdf')


#Plot for Native American ancestry.
g = ggplot() + 
    geom_sf(data = state_entries, color="black", fill="grey60", lwd = 0) +
    geom_sf(data = merged_zip2_ancestries, mapping = aes(fill = NativeAmerican, geometry = geometry), lwd = 0) +
    geom_sf(data = merged_zip3_ancestries, mapping = aes(fill = NativeAmerican, geometry = geometry), lwd = 0) +
    geom_sf(data = state_entries, color="black", fill="transparent", lwd = 1) +
    scale_fill_gradient(low = "white", high = "#41C9F8", na.value = "grey60", limits = c(0, 45)) +
    theme_classic(base_size = 60) + theme(line = element_blank(),
                                          axis.text = element_blank(),
                                          title = element_blank(),
                                          legend.position = "bottom",
                                          legend.key.width = unit(4, 'in'),
                                          legend.key.height= unit(1, 'in'))

ggsave("~/Dropbox (GaTech)/AllOfUs/Shivam-DemoProject2/Figures/FinalManuscriptFigures//Raw/Maps/NativeAmerican.png", g, width = 24, height = 24, dpi = 300, units = "in", device='png')
#ggsave("~/Dropbox (GaTech)/AllOfUs/Shivam-DemoProject2/Figures/FinalManuscriptFigures//Raw/Maps/NativeAmerican.pdf", g, width = 24, height = 24, dpi = 300, units = "in", device='pdf')



































