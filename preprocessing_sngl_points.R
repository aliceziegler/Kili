###settings###

msc_inpath <- "H:/Masterarbeit/data_div/"
inpath <- "H:/Kili/data/"
requ_ldr <- c("kili_new", "kili")
ldr_radius = 20
###setup###
library(rPointDB)
library(plyr)
source("H://Kili/src/inkili/R/ldr_query.R")
###used Datasets
dat_SR <- read.table(file = paste0(inpath, "Biodiversity_Data_Marcel.csv"), sep = ";", header = T, na.strings = "NA", 
                     dec = ",")
#dat_jljn <- read.csv(paste0(msc_path, "anm_SR.csv"), header=T, sep=",")
tec_crdnt <- read.csv(paste0(msc_inpath,"tec_crdnt.csv"), header=T, sep=",") # plots with info
colnames(tec_crdnt)[colnames(tec_crdnt) == "slp"] <- "slp_dem"
colnames(tec_crdnt)[colnames(tec_crdnt) == "asp"] <- "asp_dem"
tec_sngl <- tec_crdnt[!duplicated(tec_crdnt$plotID), -which(names(tec_crdnt) %in% c("plot_rnd", "date_coll_insct", "rnd"))]



ldr_stats_norm <- ldr_query(plotID = tec_crdnt$plotID, crdnt_x = tec_crdnt$x_pnt,
                            crdnt_y = tec_crdnt$y_pnt, radius = ldr_radius, 
                            height = F, filter = NULL, dens = F, normalise = norm)

##height above sea level
ldr_hght_asl <- ldr_query(plotID = tec_crdnt$plotID, crdnt_x = tec_crdnt$x_pnt,
                          crdnt_y = tec_crdnt$y_pnt, radius = 1, height = T)
#average point density for each plot with ldr radius
ldr_pnt_dnst <- ldr_query(plotID = tec_crdnt$plotID, crdnt_x = tec_crdnt$x_pnt,
                          crdnt_y = tec_crdnt$y_pnt, radius = ldr_radius, height = F, dens = T)


#ldr_stats_all <- merge(ldr_stats_norm, ldr_hght_asl, by = c("plotID", "crdnt_x", "crdnt_y"))
ldr_stats_all <- cbind(ldr_stats_norm, ldr_hght_asl[which(colnames(ldr_hght_asl) == "hght_asl")], ldr_pnt_dnst[which(colnames(ldr_pnt_dnst)== "pnt_dnst")])

ldr_stats <- rdc_by_ldr(dataframe_plts = ldr_stats_all, dataframe_crdnt = tec_crdnt)
###write out table
write.table(ldr_stats, file=paste0(outpath, "/", mod_date, "ldr_stats.csv"),
            row.names=F, sep = ",")
