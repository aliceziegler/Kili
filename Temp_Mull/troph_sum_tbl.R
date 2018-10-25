# Description:
# Author: Alice Ziegler
# Date: 2018-08-20 17:46:58
####
rm(list=ls())
##2Do lookuop tbl müsste nciht hier erstellt werden. kann in csv eingetragen werden
########################################################################################
###Presettings
########################################################################################
#Packages:


#Sources:
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "okt18/"
inpath <- paste0("../data/", sub)
inpath_general <- "../data/"
outpath <- paste0("../out/", sub)
if (file.exists(outpath)==F){
  dir.create(file.path(outpath), recursive = T)
}

########################################################################################
###Settings
########################################################################################
dat_SR <- readRDS(file = paste0(inpath, "dat_SR.rds"))
trophic_tbl <- as.data.frame(read.csv(paste0(inpath_general, "trophic_tbl.csv"), sep = ";"))
lvl <- c("predator", "generalist", "decomposer", "herbivore", "plant", "birds", "bats", 
            "summary", "trait")
trophic_tbl$Taxon <- as.character(trophic_tbl$Taxon)
trophic_tbl$diet <- factor(trophic_tbl$diet, levels = lvl)

#dat_SR alpha nach trophic table zusammenzählen. 
alpha_tbl <- dat_SR[, c(which(colnames(dat_SR) == "plotID"), 
                        which(colnames(dat_SR) == "SRmammals") : 
                          which(colnames(dat_SR) == "SRallplants"))]

##heteroptera wird hier mit zur summe gerechnet, auch wenn heteroptera nachher im 
##Modell eventuell einzeln nicht berechnet wird, weil es zu wenige Daten gibt
#wie folgt könnte es rausgenommen werden: 
#alpha_tbl <- alpha_tbl[,!grepl("heteroptera", colnames(alpha_tbl))]

troph_pred <- lapply(colnames(dat_SR)[which(colnames(dat_SR) == "SRmammals") : 
                                        which(colnames(dat_SR) == "SRallplants")], function(x){
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
lookup$diet <- factor(lookup$diet, levels = lvl)

troph_sum <- data.frame(plotID = dat_SR$plotID)
for (i in levels(trophic_tbl$diet)){
  troph_lst <- lookup$predictor[which(lookup$diet == i)]
  summed <- rowSums(dat_SR[, which(colnames(dat_SR) %in% 
                                        troph_lst), drop = F], na.rm = T) 
  # drop = F, muss sein, weil es sonst für level mit nur einer Spalte (bats/birds) 
  # fehlermeldung gibt, so können sie trotzdemtrotzdem weiter in die nächste Tabelle 
  # geschrieben werden
  troph_sum$summed <- summed
  colnames(troph_sum)[which(colnames(troph_sum) == "summed")] <- paste0("sum_", i, "_N", length(troph_lst))
}
troph_sum <- troph_sum[!duplicated(troph_sum$plotID),]
saveRDS(troph_sum, file = paste0(inpath, "troph_sum.rds"))



