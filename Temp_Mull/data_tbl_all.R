# Description: 
# Author: Alice Ziegler
# Date: 2017-09-06 13:58:50

rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#Packages: 

#Sources: 
inpath <- "E:/Projekte/Kili/data/diverse_tabellen/"
outpath <- "E:/Projekte/Kili/data/"

########################################################################################
###Settings
########################################################################################

bird_trait <- read.table(file = paste0(inpath, "cwm_bird_traits.csv"), sep = ";", header = T, dec = ",")
SR <- read.table(file = paste0(inpath, "Biodiversity_Data_Marcel.csv"), sep = ";", header = T, 
                     na.strings = "NA", dec = ",")
anm <- read.table(file = paste0(inpath, "animals_plotIDcomplete_Syn1.csv"), sep = ";", header = T, dec = ",")
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################
