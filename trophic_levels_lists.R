# Description: 
# Author: Alice Ziegler
# Date: 2018-05-14 11:28:30
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
trophic_tbl <- as.data.frame(read.csv(paste0(inpath, "trophic_tbl.csv"), sep = ";"))
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################
trophic_tbl$Taxon <- as.character(trophic_tbl$Taxon)
trophic_tbl$diet <- factor(trophic_tbl$diet, levels = c("predator", 
                                                        "generalist", 
                                                        "decomposer", 
                                                        "herbivore", 
                                                        "plant", 
                                                        "summary", 
                                                        "trait"
                                                        ))

save(trophic_tbl, file = paste0(outpath, "trophic_tbl.RData"))
