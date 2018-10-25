# Description:
# Author: Alice Ziegler
# Date: 2018-02-28 11:52:57
# to do:
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#Packages:
# library(mgcv)
# library(pls)
library(plyr)
library(caret)
#Sources:
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "okt18/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../data/", sub)
outpath_general <- paste0("../data/")

########################################################################################
###Settings
########################################################################################

########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################

dat_SR <- readRDS(paste0(inpath, "dat_SR.rds"))
#beta <- get(load(paste0(inpath, "beta_anm_plnt.RData")))
troph_sum <- readRDS(paste0(inpath, "troph_sum.rds"))

######Residuals SR data
tbl <- data.frame(plotID = dat_SR$plotID,
                  selID = as.numeric(substr(as.character(dat_SR$plotID), 4, 4)),
                  cat = substr(as.character(dat_SR$plotID), 1, 3), 
                  elevation = dat_SR$elevation, 
                  elevation_scale = scale(dat_SR$elevation, center = T, scale = T), 
                  elevsq = dat_SR$elevsq, 
                  elevsq_scale = scale(dat_SR$elevsq, center = T, scale = T), 
                  dat_SR[,c(which(colnames(dat_SR) %in% "SRmammals") :
                              which(colnames(dat_SR) %in% "SRallplants"))])
# tbl <- merge(tbl, troph_sum[,c(which(colnames(troph_sum) == "plotID"):
#                                  which(colnames(troph_sum) == "sum_trait_N0"))], by = "plotID")
tbl <- merge(tbl, troph_sum, by = "plotID")

cats <- unique(tbl$cat)

ind_nums <- sort(unique(tbl$selID))
ind_nums <- ind_nums[ind_nums>0]

outs_lst <- lapply(ind_nums, function(k){
  out_sel <- tbl[which(tbl$selID == k),]
  miss <- cats[!(cats %in% out_sel$cat)]
  df_miss <- tbl[tbl$cat %in% as.vector(miss),]
  set.seed(k)
  out_miss <- ddply(df_miss, .(cat), function(x){
    x[sample(nrow(x), 1), ]
  })
  out <- rbind(out_sel, out_miss)
})

Index_out <- lapply(seq(length(outs_lst)), function(i){
  out_res <- as.integer(rownames(outs_lst[[i]]))
})
Index <- lapply(outs_lst, function(i){
  res <- which(!(tbl$plotID %in% i$plotID))
})

tbl_res <- tbl
for (i in c(which(colnames(tbl) == "SRmammals") : ncol(tbl))){
  #print(i)
  #print(colnames(tbl)[i])
  resp_pls <- tbl[!is.na(tbl[,i]),i]
  #print(length(unique(resp_pls)))
  pred_pls <- data.frame(elevation_scale = tbl[!is.na(tbl[,i]),"elevation_scale"], 
                         elevsq_scale = tbl[!is.na(tbl[,i]),"elevsq_scale"])
  # mod <- gam(tbl[,i] ~ s(elev$elevation, k = 5))
  # mod <- plsr(tbl[,i] ~ (elev$elevation_scale + elevsq$elevsq_scale))
  #df_scl <- df_scl[complete.cases(df_scl),]
  mod_pls_trn <- train(x = pred_pls, y = resp_pls, method = "pls", metric = "RMSE", 
                       tuneGrid = expand.grid(ncomp = c(1,2)), 
                       trControl = trainControl(method = "cv", index = Index, indexOut = Index_out))
  newdat_pls <- pred_pls
  prdct_pls_trn <- predict(object = mod_pls_trn, newdata =  newdat_pls)
  
  #predict
  #differencce obs and pred
  #write in df
  tbl_res[!is.na(tbl[,i]),i] <- tbl[!is.na(tbl[,i]),i] - prdct_pls_trn######
  colnames(tbl_res)[i] <- paste0("resid", colnames(tbl_res)[i])
}
tbl_res <- tbl_res[,-which(colnames(tbl_res) %in% c("selID", "cat", "elevation", "elevation_scale", "elevsq", "elevsq_scale"))]
saveRDS(tbl_res, file = paste0(outpath, "tbl_res.rds"))

