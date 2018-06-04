# Description: 
# Author: Alice Ziegler
# Date: 2017-10-16 11:04:34

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
load(paste0(inpath, "ldr_SR.RData"))
load(paste0(inpath, "ldr_str_pnts.RData"))
load(paste0(inpath, "ldr_str_chm.RData"))
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################
ldr_SR_pnts <- merge(ldr_SR, ldr_str_pnts, by.x = "plotID", by.y = "plotID")

ldr_SR_str <- merge(ldr_SR_pnts, ldr_str_chm[,c("plotID", "gap_frac")], by.x = "plotID", by.y = "plotID")

#save(ldr_SR_str, file = paste0(inpath, "ldr_SR_str.RData"))
