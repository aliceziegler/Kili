# Description:
# Author: Alice Ziegler
# Date: 2018-11-22 20:16:26
# to do:
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#Packages:

#Sources:
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "nov18_test/"
inpath_general <- "../data/" # only original files
inpath <- paste0("../data/", sub)
########################################################################################
###Settings
########################################################################################
set <- c("frst", "nofrst", "allplts")
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
##########################################################################################################################################################

##112218###write function for residual calculation
#???cats <- unique(tbl$cat)


for (i in set){
  if (all_plts == F){
    if (frst == T){
      cat <- c("fer", "flm", "foc", "fod", "fpd", "fpo", "hom")
    }else if (frst == F){
      cat <- c("cof", "gra", "hel", "mai", "sav")
    }
    tbl <- tbl[which(tbl$cat %in% cat),]
  }
  
  ### outs list ausschreiben
  ### cv Index ausschreiben
  ### SR berechnen
  ### Datensatz für frst, nofrst, allplts ausschreiben
  
  ### ...
  ### 1_model_run anpassen und so verändern, dass je nach flag forest/allplots anderer Datensatz eingeladen wird. 
  ### Rest in 1_model anpassen und nofrst laufen lassen 
  
  
}

#read datasets
dat_ldr_mrg <- readRDS(file = paste0(inpath, "dat_ldr_mrg.rds"))


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
## index und index out dürfen hier nicht ausgeschrieben und weiter verwendet werden, 
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
    tbl_pred_res[!is.na(tbl_pred_res[,i]),i] <- prdct_pls_trn ##was vorher NA war, bleibt auch jetzt NA
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
