# Description: 
# Author: Alice Ziegler
# Date: 2018-02-26 16:29:08
# to do: wie julianes und Marcels datensatz zusammenbringen? Ist das überhaupt sinnvoll? Erstmal nur marcels!
#mit inpath und outpath anpassen - gennerelle Dateistruktur anlegen und Master sachen einstricken
#rausfinden, was die einzelnen Lidar Strukturparameter überhaupt sind
#NA Werte in Marcels tabelle Füllen (Marcels Vorschlag aus Email)
rm(list=ls())
rm(list=ls())
########################################################################################
###Presettings
########################################################################################
#Packages: 
# library(rPointDB)
# library(plyr)
###trophsum integrieren
#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "sep18/"
inpath <- "../data/"
outpath <- paste0("../data/", sub)

###Bits and pieces for preprocessing dat_SR from Marcels Data csv
raw_dat <- as.data.frame(read.table(file = paste0(inpath, "Biodiversity_Data_Marcel.csv"), 
                     sep = ";", header = T, na.strings = "NA", dec = ","))
ldr_plots <- readRDS(file = paste0(inpath, "ldr_plots.rds"))


### filter raw_dat for only plots that were coverd by lidar mission
dat_filt <- raw_dat[which(raw_dat$plotID %in% ldr_plots),]
# colnames(dat_filt)
for (i in colnames(dat_filt)[c(which(colnames(dat_filt) == "SRmammals") : 
                                     which(colnames(dat_filt) == "SRallplants"))]){
  dat_filt[,i] <- as.numeric(dat_filt[,i])
}

#add 
dat_filt$dstrb <- ifelse(dat_filt$luidich == "Disturbed", 1, 0)
dat_filt$dstrb <- as.logical(dat_filt$dstrb)
dat_filt$elevsq <- (dat_filt$elevation)^2
dat_SR <- dat_filt[,c(which(colnames(dat_filt) == "plotID") : 
                          which(colnames(dat_filt) == "elevation"),
                      which(colnames(dat_filt) == "elevsq"), 
                      which(colnames(dat_filt) == "easting") : 
                        which(colnames(dat_filt) == "lui"),
                        which(colnames(dat_filt) == "dstrb"), 
                        which(colnames(dat_filt) == "SRmammals") : 
                          which(colnames(dat_filt) == "SRallplants"))]



save(dat_SR, file = paste0(outpath, "dat_SR.RData"))

