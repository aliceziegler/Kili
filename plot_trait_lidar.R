# Description: Plotting bird traits agains LiDAR parameters
# Author: Alice Ziegler
# Date: 2017-11-30 08:45:33

rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#Packages: 
library(ggplot2)
#Sources: 
inpath <- "E:/Projekte/Kili/data/"
outpath <- "E:/Projekte/Kili/out/plot_trait_ldr/"

########################################################################################
###Settings
########################################################################################

load(paste0(inpath, "dat_ldr_mrg.RData"))
mrg_tbl$cat <- factor(mrg_tbl$cat, levels = c("mai", "sav", "cof", "hom", "gra", "flm", "fod", 
                                   "foc", "fpo", "fpd", "fed", "fer", "hel"))
nm_ldr <- colnames(mrg_tbl)[c(41:68, 71:90, 94:97, 101:104, 110:129, 131:134)]
nm_trait <- colnames(mrg_tbl)[135:143]
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################

for (i in seq(nm_trait)){
  print(i)
  for (j in seq(nm_ldr)){
    ggplot(mrg_tbl, aes(x=mrg_tbl[nm_ldr[j]], y=mrg_tbl[nm_trait[i]], color = mrg_tbl$cat)) + 
      labs(x = nm_ldr[j], y = nm_trait[i], color = "landuse") +
      geom_point(shape = 16)
    ggsave(filename = paste0(outpath, nm_trait[i], "_by_", nm_ldr[j], ".pdf"))
  }
}




