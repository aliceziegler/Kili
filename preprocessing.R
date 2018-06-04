# Description: 
# Author: Alice Ziegler
# Date: 2018-02-26 16:29:08
# to do: wie julianes und Marcels datensatz zusammenbringen? Ist das überhaupt sinnvoll? Erstmal nur marcels!
#mit inpath und outpath anpassen - gennerelle Dateistruktur anlegen und Master sachen einstricken
#rausfinden, was die einzelnen Lidar Strukturparameter überhaupt sind
#NA Werte in Marcels tabelle Füllen (Marcels Vorschlag aus Email)
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#Packages: 
library(rPointDB)
library(plyr)

#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "mar18/"
inpath <- "../data/"
outpath <- paste0("../data/", sub)

########################################################################################
###Settings
########################################################################################
get_lidar <- T
requ_ldr <- c("kili2", "kili")
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################

###used Datasets
dat_SR <- read.table(file = paste0(inpath, "Biodiversity_Data_Marcel.csv"), sep = ";", header = T, na.strings = "NA",
          dec = ",")
tec_crdnt <- read.csv(paste0(inpath,"tec_crdnt.csv"), header=T, sep=",") # plots with info
colnames(tec_crdnt)[colnames(tec_crdnt) == "slp"] <- "slp_dem"
colnames(tec_crdnt)[colnames(tec_crdnt) == "asp"] <- "asp_dem"
tec_sngl <- tec_crdnt[!duplicated(tec_crdnt$plotID), -which(names(tec_crdnt) %in% c("plot_rnd", "date_coll_insct", "rnd"))]

if (get_lidar == T){
  ####get lidar_indices # von Stephan
  remotesensing <- RemoteSensing$new("http://192.168.191.183:8081") # open remote sensing database connection
  subset_arg_poi_square <- function(pois, group, edge=10) {
    poisFull <- paste0(group, "/", pois)
    poisFinal <- paste0(poisFull, collapse="&")
    r <- paste0("square(poi(", poisFinal, "),", edge, ")")
    return(r)
  }
  ldr_tbl <- data.frame()
  for (i in requ_ldr){
    pointdb <- remotesensing$lidar(i) #open one lidar layer
    poi_group <- "kili"
    pois <- remotesensing$poi_group(poi_group)
    poi_names <- tec_sngl$plotID
    areas <- mapply(function(name, x, y) {return(extent_diameter(x=x, y=y, d=10))}, pois$name, pois$x, pois$y)
    #areas <- subset_arg_poi_square(pois=poi_names, group=poi_group, edge=10)
    #script <- c("BE_H_MAX","BE_H_MEAN")
    fun_nm <- pointdb$processing_functions$name
    ldr_tbl_lay <- pointdb$process(areas=areas, functions=fun_nm)
    ldr_tbl <- rbind(ldr_tbl, ldr_tbl_lay)
  }
  colnames(ldr_tbl)[colnames(ldr_tbl) == "name"] <- "plotID"
  #sort levels of plots ($plotID) to get dataframe sorted from low to high altitude (roughly)
  ldr_tbl$plotID <- factor(ldr_tbl$plotID, levels = c("mai1", "mai2", "mai3", "mai4", "mai5", "sav1", "sav2", "sav3",
                                                      "sav4", "sav5", "cof1", "cof2", "cof3", "cof4", "cof5", "hom1",
                                                      "hom2", "hom3", "hom4", "hom5", "gra1", "gra2", "gra3", "gra4",
                                                      "gra5", "flm1", "flm2", "flm3", "flm4", "foc1", "foc2", "foc3",
                                                      "foc4", "foc5", "fod1", "fod2", "fod3", "fod4", "fod5", "fpo1",
                                                      "fpo2", "fpo3", "fpo4", "fpo5", "fpd1", "fpd2", "fpd3", "fpd4",
                                                      "fpd5", "fer0", "fer1", "fer3", "fer4", "hel1", "hel2", "hel3",
                                                      "hel4"))
  ldr_tbl <- ldr_tbl[order(ldr_tbl$plotID),]
    ### write out lidar table
  write.csv(ldr_tbl, file = paste0(outpath, "ldr_tbl.csv"), row.names = F)
  save(ldr_tbl, file = paste0(outpath, "ldr_tbl.RData")) #um levels zu erhalten
}else{
  #ldr_tbl <- read.csv(paste0(inpath, "ldr_tbl.csv"), header = T, sep = ",")
  load(paste0(inpath, "ldr_tbl.RData"))
}

###merge field data and lidar table
ldr_SR_raw <- join(ldr_tbl, dat_SR, by = "plotID", match = "first") #join, weil merge Reihenfolge alphabetisch sortiert
ldr_SR <- ldr_SR_raw[, c(which(names(ldr_SR_raw) %in% c("plotID")),
                         (which(names(ldr_SR_raw) %in% c("cat")): which(names(ldr_SR_raw) %in% c("lat"))),
                         (which(names(ldr_SR_raw) %in% c("area")): which(names(ldr_SR_raw) %in% c("bbox_area"))),
                         (which(names(ldr_SR_raw) %in% c("BE_ELEV_ASPECT")):
                            which(names(ldr_SR_raw) %in% c("BE_RD_UND"))),
                         (which(names(ldr_SR_raw) %in% c("chm_height_max")):
                            which(names(ldr_SR_raw) %in% c("surface_intensity_sd"))),
                         (which(names(ldr_SR_raw) %in% c("lui_biomass_removal")):
                            which(names(ldr_SR_raw) %in% c("SRallplants")))
                         )]
ldr_SR$cat <- factor(ldr_SR$cat, levels = c("mai", "sav", "cof", "hom", "gra", "flm",
                                            "foc", "fod", "fpo", "fpd", "fer", "hel"))
###remove(ldr_SR_raw)
#write.csv(ldr_SR, file = paste0(inpath, "ldr_SR.csv"), row.names = F)
#save(ldr_SR, file = paste0(inpath, "ldr_SR.RData"))
