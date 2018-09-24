# Description: 
# Author: Alice Ziegler
# Date: 2018-0919 15:30
# to do: ###
#########
rm(list=ls())
########################################################################################
###Presettings
########################################################################################
setwd("D:/Uni/Projekte/Kili/data/sep18/")
#Packages: 
library(plyr)
library(caret)

dat_SR <- get(load("dat_SR.RData"))
dat_SR$selID <- as.numeric(substr(dat_SR$plotID, 4, 4))
dat_SR$elevation_scale <- scale(dat_SR$elevation, center = T, scale = T)
dat_SR$elevsq_scale <- scale(dat_SR$elevsq, center = T, scale = T)
cats <- unique(dat_SR$cat)
ind_nums <- sort(unique(dat_SR$selID))
ind_nums <- ind_nums[ind_nums>0]


###‰uﬂere unabh‰ngige Schleife
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

prediction_cv <- lapply(colnames(dat_SR)[c(which(colnames(dat_SR) == "SRmammals"):
                                     which(colnames(dat_SR) == "SRmagnoliids"))], function(i){
                                       print(i)
  # for (i in colnames(dat_SR)[c(which(colnames(dat_SR) == "SRmammals"): 
  #                                which(colnames(dat_SR) == "SRmagnoliids"))]){

  pls_elev_cv_prdct <- lapply(seq(length(outs_lst)), function(k){
    #print(k)
    out_plt <- outs_lst[[k]]$plotID
    dat_SR$elevation_scale <- scale(dat_SR$elevation, center = T, scale = T)
    dat_SR$elevsq_scale <- scale(dat_SR$elevsq, center = T, scale = T)
    
    tbl_in <- dat_SR[-which(dat_SR$plotID %in% out_plt),]
    tbl_out <- dat_SR[which(dat_SR$plotID %in% out_plt),]
    
    cvind_num <- unique(sort(tbl_in$selID))
    cvind_num <- cvind_num[which(cvind_num >0)]
    cvouts_lst <- lapply(cvind_num, function(k){
      out_sel <- tbl_in[which(tbl_in$selID == k),]
      miss <- cats[!(cats %in% out_sel$cat)]
      df_miss <- tbl_in[tbl_in$cat %in% as.vector(miss),]
      set.seed(k)
      out_miss <- ddply(df_miss, .(cat), function(x){
        x[sample(nrow(x), 1), ]
      })
      out <- rbind(out_sel, out_miss)
    })
    cvIndex <- lapply(cvouts_lst, function(i){
      res <- which(!(tbl_in$plotID %in% i$plotID))
    })
    
    dat <- data.frame("plotID" = tbl_in$plotID, 
                      "elevation_scale"= tbl_in$elevation_scale,
                      "elevsq_scale" = tbl_in$elevsq_scale,
                      "response"= tbl_in[,grepl(paste0("^", i, "$"),
                                                colnames(tbl_in))])
    resp_NA <- dat$plotID[which(is.na(dat$response))]
    
    pred_pls <- data.frame(elevation_scale = dat[!is.na(dat[,"response"]),"elevation_scale"], 
                           elevsq_scale = dat[!is.na(dat[,"response"]),"elevsq_scale"])
    resp_pls <- dat[!is.na(dat[,"response"]),"response"]
    mod_pls_trn <- train(x = pred_pls, y = resp_pls, method = "pls", 
                         tuneGrid = expand.grid(ncomp = c(1,2)), 
                         trControl = trainControl(method = "cv", index = cvIndex))  
    
    #mod_pls_cv <- plsr(response ~ (elevation_scale + elevsq_scale), data = dat)
    #saveRDS(mod_pls_cv, file = paste0(outpath, "pls_cv_mod_", resp, "_", run, ".rds"))
    newdat <- data.frame("elevation_scale" = tbl_out$elevation_scale,
                         "elevsq_scale" = tbl_out$elevsq_scale)
    
    prdct <- predict(object = mod_pls_trn, newdata = newdat)
  
    ################################################################################################hier werden 2 ncomps in datenframe geschrieben
    prdct_df <- data.frame(plotID = tbl_out$plotID, 
                           elevation = tbl_out$elevation,
                           resp = tbl_out[,which(colnames(tbl_out) == i)], 
                           prdct= prdct)
    # colnames(prdct_df)[which(colnames(prdct_df) == "prdct")] <- paste0(j, "_", "run_", i)
    colnames(prdct_df) <- c("plotID", "elevation", i, paste0("prd_", i, "_", "run_", k))
    return(prdct_df = prdct_df)
  })
  return(pls_elev_cv_prdct)
  
  
})##¸ber arten

prdct_lst <- do.call(rbind, prediction_cv)

pls_elev_cv_df_mrg_lst <- lapply(seq(nrow(prdct_lst)), function(k){
  pls_elev_cv_prdct <- prdct_lst[k,]
  pls_elev_cv_df_mrg <- Reduce(function(x, y) merge(x, y, by="plotID", all = T), pls_elev_cv_prdct)
  pls_elev_cv_df_mrg <- pls_elev_cv_df_mrg[, !grepl("\\.", colnames(pls_elev_cv_df_mrg))]
  return(pls_elev_cv_df_mrg)
})

pls_elev_prdct_cv_df <- Reduce(function(x, y) merge(x, y, by="plotID"), pls_elev_cv_df_mrg_lst)
pls_elev_prdct_cv_df <- pls_elev_prdct_cv_df[, !grepl("\\.", colnames(pls_elev_prdct_cv_df))]
pls_elev_prdct_cv_df <- data.frame(plotID = pls_elev_prdct_cv_df$plotID, 
                                     #elevation = pls_elev_prdct_cv_df$elevation, 
                                     #pls_elev_prdct_cv_df[,grepl("run", colnames(pls_elev_prdct_cv_df))]) 
                                     pls_elev_prdct_cv_df[,-(which(colnames(pls_elev_prdct_cv_df) == "plotID"))])
pls_elev_prdct_cv_df <- pls_elev_prdct_cv_df[,colSums(is.na(pls_elev_prdct_cv_df)) < nrow(pls_elev_prdct_cv_df)]
pls_elev_rlvnt <- colnames(pls_elev_prdct_cv_df)[grepl("run", colnames(pls_elev_prdct_cv_df))]
pls_elev_rlvnt_Unq <- unique(substr(pls_elev_rlvnt, 5, nchar(pls_elev_rlvnt)-6))
pls_elev_prdct_cv_df <- pls_elev_prdct_cv_df[,c(which(colnames(pls_elev_prdct_cv_df) =="plotID"), 
                                                    which(colnames(pls_elev_prdct_cv_df) =="plotUnq"), 
                                                    which(grepl("run", colnames(pls_elev_prdct_cv_df)))#, 
                                                    #which(colnames(pls_elev_prdct_cv_df) %in% pls_elev_rlvnt_Unq)
)]
saveRDS(object = pls_elev_prdct_cv_df, file = "pls_elev_prdct_cv_df.rds")

