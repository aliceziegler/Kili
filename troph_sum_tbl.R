# Description:
# Author: Alice Ziegler
# Date: 2018-08-20 17:46:58
####
rm(list=ls())
##2Do write in function and call within LiDARtools_wrap from inkili!
########################################################################################
###Presettings
########################################################################################
#Packages:


#Sources:
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "aug18/"
inpath <- paste0("../data/", sub)
inpath_general <- "../data/"
outpath <- paste0("../out/", sub)
if (file.exists(outpath)==F){
  dir.create(file.path(outpath), recursive = T)
}
########################################################################################
###Settings
########################################################################################
mrg_tbl <- get(load(paste0(inpath, "dat_ldr_mrg.RData")))
trophic_tbl <- get(load(paste0(inpath_general, "trophic_tbl.RData")))

#pseudocode
#mrg_tbl alpha nach trophic table zusammenzählen. 
alpha_tbl <- mrg_tbl[, c(which(colnames(mrg_tbl) == "plotID"), which(colnames(mrg_tbl) == "plotUnq"), 
                         which(colnames(mrg_tbl) == "SRmammals") : which(colnames(mrg_tbl) == "SRallplants"))]



#for (x in (colnames(alpha_tbl)[2:ncol(alpha_tbl)])){
troph_pred <- lapply(colnames(alpha_tbl)[3:ncol(alpha_tbl)], function(x){
  trop <- NA
  for (i in trophic_tbl$Taxon){
    match <- grep(i, x, value=TRUE)
    if (length(match) != 0){
      trop <- i
    }
  }
  return(c(predictor = x, Taxon = trop))
})
lookup <- merge(trophic_tbl, as.data.frame(do.call(rbind, troph_pred)), by = "Taxon")
lookup$diet <- factor(lookup$diet, levels = c("generalist", 
                                              "predator", 
                                              "herbivore", 
                                              "decomposer", 
                                              "plant", 
                                              "birds", 
                                              "bats", 
                                              "summary", 
                                              "trait"))



troph_sum <- data.frame(plotID = alpha_tbl$plotID, plotUnq = alpha_tbl$plotUnq)
for (i in levels(trophic_tbl$diet)){
  troph_lst <- lookup$predictor[which(lookup$diet == i)]
  summed <- rowSums(alpha_tbl[, which(colnames(alpha_tbl) %in% 
                                        troph_lst), drop = F], na.rm = T) 
  #drop = F, damit birds und bats mit nur einer spalte trotzdem weiter in die nächste Tabelle geschrieben werden
  troph_sum$summed <- summed
  colnames(troph_sum)[which(colnames(troph_sum) == "summed")] <- paste0("sum_", i, "_N", length(troph_lst))
}


save(troph_sum, file = paste0(inpath, "troph_sum.RData"))
save(troph_sum, file = paste0(inpath_general, "troph_sum.RData"))
# mrg_tbl_all <- merge(mrg_tbl, sum_tbl, by = "plotUnq")
# mrg_tbl <- data.frame(plotID = mrg_tbl_all$plotID.x, plotUnq = mrg_tbl_all$plotUnq, 
#                       mrg_tbl_all[,c(which(colnames(mrg_tbl_all) == "cat") : ncol(mrg_tbl_all))])
# save(mrg_tbl, file = paste0(inpath, "dat_ldr_mrg_sum.RData"))
