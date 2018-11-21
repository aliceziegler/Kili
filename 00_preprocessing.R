# Description: Marcels Daten einlesen, 
## SR: 
# plots filtern auf die plots, die tatsächlich überflogen wurden, 
# elevation square anfügen
# Tabelle ausschreiben
## Traits: 
# manipulation
## troph_sum
# 
# Author: Alice Ziegler
# Date: 2018-02-26 16:29:08
# to do: 
# 
rm(list=ls())
########################################################################################
###Presettings
########################################################################################
#Packages: 
library(caret)
library(plyr)

#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "nov18/"
inpath_general <- "../data/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../data/", sub)
if (file.exists(outpath)==F){
  dir.create(file.path(outpath))
}
########################################################################################
###Bits and pieces for preprocessing dat_SR from Marcels Data csv
raw_dat <- as.data.frame(read.table(file = paste0(inpath_general, "Biodiversity_Data_Marcel.csv"), 
                     sep = ";", header = T, na.strings = "NA", dec = ","))
ldr_plots <- readRDS(file = paste0(inpath_general, "ldr_plots.rds"))
trait <- read.csv(paste0(inpath_general, "cwm_bird_traits_ja.csv"), header = T, sep = ";", dec = ",")


### filter raw_dat for only plots that were coverd by lidar mission
dat_filt <- raw_dat[which(raw_dat$plotID %in% ldr_plots),]
# colnames(dat_filt)
for (i in colnames(dat_filt)[c(which(colnames(dat_filt) == "SRmammals") : 
                                     which(colnames(dat_filt) == "SRallplants"))]){
  dat_filt[,i] <- as.numeric(dat_filt[,i])
}

#add 
# dat_filt$dstrb <- ifelse(dat_filt$luidich == "Disturbed", 1, 0)
# dat_filt$dstrb <- as.logical(dat_filt$dstrb)
dat_filt$elevsq <- (dat_filt$elevation)^2
dat_filt$selID <- as.numeric(substr(as.character(dat_filt$plotID), 4, 4))
dat_filt$elevation_scale <- scale(dat_filt$elevation, center = T, scale = T)
dat_filt$elevsq_scale <- scale(dat_filt$elevsq, center = T, scale = T)
dat_SR <- dat_filt[,c(which(colnames(dat_filt) == "plotID"),
                      which(colnames(dat_filt) == "selID"), 
                      which(colnames(dat_filt) == "cat"), 
                      which(colnames(dat_filt) == "elevation"),
                      which(colnames(dat_filt) == "elevation_scale"),
                      which(colnames(dat_filt) == "elevsq"), 
                      which(colnames(dat_filt) == "elevsq_scale"),
                      which(colnames(dat_filt) == "easting") : 
                        which(colnames(dat_filt) == "lui"),
                      # which(colnames(dat_filt) == "dstrb"), 
                      which(colnames(dat_filt) == "SRmammals") : 
                        which(colnames(dat_filt) == "SRallplants"))]

saveRDS(dat_SR, file = paste0(outpath, "dat_SR.rds"))

########################################################################################
####manipulationg trait data
colnames(trait)[1] <- "plotID"
saveRDS(trait, file = paste0(outpath, "traits.rds"))

#########################################################################################
####trophic sum data frame erstellen und ausschreiben
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
saveRDS(troph_sum, file = paste0(outpath, "troph_sum.rds"))

###################################################################################################
# create outs list
ind_nums <- sort(unique(dat_SR$selID))
ind_nums <- ind_nums[ind_nums>0]
cats <- unique(dat_SR$cat)

outs_lst <- lapply(ind_nums, function(k){
  out_sel <- dat_SR[which(dat_SR$selID == k),]
  miss <- cats[!(cats %in% out_sel$cat)]
  df_miss <- dat_SR[dat_SR$cat %in% as.vector(miss),]
  set.seed(k)
  out_miss <- ddply(df_miss, .(cat), function(x){
    x[sample(nrow(x), 1), ]
  })
  out <- rbind(out_sel, out_miss)
})

saveRDS(outs_lst, file = paste0(outpath, "outs_lst.rds"))

Index_out <- lapply(outs_lst, function(i){
  # out_res <- as.integer(rownames(outs_lst[[i]]))
  out_res <- which(dat_SR$plotID %in% i$plotID)
})
  
Index <- lapply(outs_lst, function(i){
  res <- which(!(dat_SR$plotID %in% i$plotID))
})
## index und index put dürfen hier nciht ausgeschrieben und weiter verwendet werden, 
## da durch foc1 die liste länger wird, sobald lidar dataen dazukommen!

###################################################################################################
####Residuals
tbl <- merge(dat_SR, troph_sum, by = "plotID") 
##need to be merged, so residuals of troph sums can be calculated
tbl_res <- tbl
tbl_pred_res <- tbl
#ifelse abfrage, falls alle einträge 0 sind, wie bei trait summary
for (i in c(which(colnames(tbl) == "SRmammals") : ncol(tbl))){
  print(colnames(tbl)[i])
  if(length(unique(tbl[,i])) > 1){
    resp_pls <- tbl[!is.na(tbl[,i]),i]
    pred_pls <- data.frame(elevation_scale = tbl[!is.na(tbl[,i]),"elevation_scale"],
                           elevsq_scale = tbl[!is.na(tbl[,i]),"elevsq_scale"])
    mod_pls_trn <- train(x = pred_pls, 
                         y = resp_pls, 
                         method = "pls", 
                         metric = "RMSE",
                         tuneGrid = expand.grid(ncomp = c(1,2)),
                         trControl = trainControl(method = "cv", index = Index, indexOut = Index_out))
    # newdat_pls <- pred_pls
    prdct_pls_trn <- predict(object = mod_pls_trn, newdata =  pred_pls)
    
    tbl_res[!is.na(tbl_res[,i]),i] <- tbl_res[!is.na(tbl_res[,i]),i] - prdct_pls_trn ##was vorher NA war, bleibt auch jetz NA
    colnames(tbl_res)[i] <- paste0("resid", colnames(tbl_res)[i])
    tbl_pred_res[!is.na(tbl_pred_res[,i]),i] <- prdct_pls_trn ##was vorher NA war, bleibt auch jetz NA
  }else{
    tbl_res[!is.na(tbl_res[,i]),i] <- NA ##was vorher NA war, bleibt auch jetz NA
    colnames(tbl_res)[i] <- paste0("resid", colnames(tbl_res)[i])
    tbl_pred_res[!is.na(tbl_pred_res[,i]),i] <- NA ##was vorher NA war, bleibt auch jetz NA
  }
  
}
# tbl_res <- tbl_res[,-which(colnames(tbl_res) %in% c("selID", "cat", "elevation", "elevation_scale", "elevsq", "elevsq_scale"))]
tbl_res <- tbl_res[,-c(which(colnames(tbl_res) == "selID") : 
                         which(colnames(tbl_res) == "lui"))]
tbl_pred_res <- tbl_pred_res[,-c(which(colnames(tbl_pred_res) == "selID") : 
                                   which(colnames(tbl_pred_res) == "lui"))]
saveRDS(tbl_res, file = paste0(outpath, "tbl_res.rds"))
saveRDS(tbl_pred_res, file = paste0(outpath, "tbl_pred_res.rds"))
