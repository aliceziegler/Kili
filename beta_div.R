# Description: get turnover, nestedness, 
#betaPart
#ac = beta div zu welchem teil to und ne
# nmds = aus 3 maßen - viele achsen, auf erste beiden sagen wir vorher. 
# Author: Alice Ziegler
# Date: 2018-05-14 15:10:59
# to do: 
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#Packages: 

#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "/"
inpath <- "../data/"
outpath <- paste0("../data/", sub)

########################################################################################
###Settings
########################################################################################
beta_plnt <- read.csv(paste0(inpath, "taxa_plants_TO_NE_AC_NMDS_sites.csv"), dec = ",", sep = ";")
beta_anm <- read.csv(paste0(inpath, "taxa_animals_moths_TO_NE_AC_NMDS_sites.csv"), dec = ",", sep = ";")
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################
