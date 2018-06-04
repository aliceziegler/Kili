# Description: 
# Author: Alice Ziegler
# Date: 2017-10-10 13:42:18

rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#Packages: 

#Sources: 
inpath <- "E:/Projekte/Kili/data/"
outpath <- "E:/Projekte/Kili/data/"

########################################################################################
###Settings
########################################################################################
load(paste0(inpath, "ldr_SR_str.RData"))
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################
colnames(ldr_SR_str)

str_var_belly <- c("BE_ELEV_SLOPE", "BE_FHD", "BE_H_KURTOSIS", "BE_H_MAX", "BE_H_MEAN", "BE_H_MEDIAN", "BE_H_P10", 
                 "BE_H_P20", "BE_H_P30", "BE_H_P40", "BE_H_P60", "BE_H_P70", "BE_H_P80", "BE_H_P90", "BE_H_SD", 
                 "BE_H_VAR", "BE_PR_CAN", "BE_PR_REG", "BE_PR_UND", "BE_RD_CAN", "BE_RD_UND", "chm_height_max", 
                 "chm_height_mean", "chm_height_sd", "dsm_elevation_sd", "dtm_slope_max", "dtm_slope_sd", 
                 "pulse_returns_max", "pulse_returns_sd", "max_hght", "sd", "max_rtrn", "sd_rtrn_1", "sd_nmbr_rtrn", 
                 "qntl_25", "qntl_50", "qntl_75", "qntl_100", "qntl_rng")
save(str_var_belly, file = paste0(outpath, "list_str_var_belly.RData"))

SR_belly <- c("SRmammals", "spiders", "SRmillipedes", "SRparasitoids", "SRothercoleoptera", "SRcollembola", 
              "SRsyrphids", "SRsnails", "SRlycopodiopsida", "SRconifers", "SRferns", "SRmagnoliids")
