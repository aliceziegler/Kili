# Description: 
# Author: Alice Ziegler
# Date: 2018-02-09 15:05:58
###to do: subdir ist bedingt durch moddir weiter unten...unschön
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#Packages: 
library(caret)
library(CAST)
library(doParallel)
library(foreach)
library(parallel)
library(plyr)
library(Rsenal)
#setwd for folder with THIS script (only possible within Rstudio)
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "aug18/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../data/", sub)


########################################################################################
###Settings
########################################################################################
###
#DATAFRAME manipulation
###
###choose dataframe and load dataframe
tbl_nm <- "dat_ldr_mrg.RData"
###choose relevant columns
tbl_rw <- get(load(paste0(inpath, tbl_nm)))
# ohne slope und aspect und ohne h?he
tbl_cols <- c(which(colnames(tbl_rw) %in% "plotID") : which(colnames(tbl_rw) %in% "lat"), 
              which(colnames(tbl_rw) %in% "SRmammals") : which(colnames(tbl_rw) %in% "residrosids_jac_NMDS2"), 
              which(colnames(tbl_rw) %in% "sum_predator_N5"): which(colnames(tbl_rw) %in% "sum_bats_N1"), 
              which(colnames(tbl_rw) %in% "plotUnq"), 
              which(colnames(tbl_rw) %in% "AGB"), 
              which(colnames(tbl_rw) %in% "BE_FHD") : which(colnames(tbl_rw) %in% "LAI"), 
              which(colnames(tbl_rw) %in% "chm_surface_ratio"), 
              which(colnames(tbl_rw) %in% "pulse_returns_max") : which(colnames(tbl_rw) %in% "pulse_returns_mean"), 
              which(colnames(tbl_rw) %in% "pulse_returns_sd"), 
              which(colnames(tbl_rw) %in% "vegetation_coverage_01m") : which(colnames(tbl_rw) %in% "vegetation_coverage_10m"), 
              which(colnames(tbl_rw) %in% "mdn_rtrn"), 
              which(colnames(tbl_rw) %in% "sd_rtrn_1"),
              which(colnames(tbl_rw) %in% "gap_frac"))
###
# tbl_rw <- tbl_rw[which(duplicated(tbl_rw$plotID) == F),] ##################dauerhaft sollte das anders gelöst werden 
###
tbl <- tbl_rw[,tbl_cols]
#saveRDS(tbl, file = paste0(outpath, "mrg_tbl_relevant_cols.RDS"))

#^ and $ means only to look for this expression and not for resid_SRmammals
nm_resp <- colnames(tbl)[seq(grep("^SRmammals$", names(tbl)), 
                             grep("sum_bats_N1", names(tbl)))]
nm_pred <- colnames(tbl)[seq(grep("AGB", names(tbl)),
                             grep("gap_frac", names(tbl)))]
nm_meta <- c("plotID", "selID", "cat", "plotUnq")
###selectors
tbl$selID <- as.integer(substr(tbl$plotID, 4, 4))

###
#MODEL CONTROL 
###
type <- "ffs"
method <- "pls" # or other caret methods
tuneLength = 10
sizes <- seq(2, length(nm_pred), 10)
rfe_cntrl <- rfeControl(functions = caretFuncs, method = "LOOCV")
###DOCUMENTATION options
#comment for explenatory filename
comm <- "_ffs_indxINOUT"
ind_num <- max(tbl$selID)
all_plts <- F
frst <- F # set true if model should only be done for forested plots


modDir <- paste0(outpath, Sys.Date(), "_", type, "_", method, comm)
if (file.exists(modDir)==F){
  dir.create(file.path(modDir))
}
#modDir <- "../data/mar18_50m_resid_nrmlz/2018-02-28_ffs_pls"
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################
#choose which plots are beeing used and delete responses, that have less that have values for less 15 plots #########verbessern: relativ gestalten und dann f�r alle nicht nur forest
if (all_plts == F){
  if (frst == T){
    cat <- c("fer", "flm", "foc", "fod", "fpd", "fpo")
  }else if (frst == F){
    cat <- c("cof", "gra", "hel", "hom", "mai", "sav")
  }
  tbl <- tbl[which(tbl$cat %in% cat),]
  #tbl <- tbl[which(tbl$cat %in% cat),which(colSums(is.na(tbl)) < 15)]
}


# choose which columns are beeing used for training, testing, val
df_pred_all <- tbl[, c(which(colnames(tbl) %in% nm_pred))]
## remove all variables where 90% of the entries are the same 
## (mostly 0 for lidar variables in heights where most plots dont have any points)
df_pred <- df_pred_all
for (i in colnames(df_pred_all)){
  frq <- table(df_pred_all[i])
  if (max(frq) > floor(nrow(df_pred_all) * 0.5)){
    df_pred <- df_pred[, !(colnames(df_pred) %in% i)]
  }
}
df_resp <- tbl[, c(which(colnames(tbl) %in% nm_resp))]
df_resp <- as.data.frame(sapply(df_resp, function(x) as.numeric(x)))
df_resp <- cbind(df_resp[,grep("resid", colnames(df_resp))], df_resp[,-grep("resid", colnames(df_resp))])
df_meta <- tbl[, c(which(colnames(tbl) %in% nm_meta))]
#df <- cbind(df_meta, df_pred, df_resp)
cats <- unique(df_meta$cat)

#scaling data to mean 0 and sd 1
scl_lst <- lapply(df_pred, function(i){
  if (class(i) == "numeric"){
    scale(i, center = T, scale = T)
  }else if (class(i) == "integer"){
    scale(as.numeric(i), center = T, scale = T)
  }else{
    i <- i #non numeric or integer columns stay as they are
  }
})
df_scl_pred <- do.call(data.frame, scl_lst)

########################################################################################
###resampling by group = Für jede Landnutzungsform einen Plot je Durchlauf raus
###nehmen (nach Plotnummer, restliche zufällig)
########################################################################################
# outs_lst <- lapply(seq(ind_num), function(k){
#   out_sel <- df_meta[which(df_meta$selID == k),]
#   miss <- cats[!(cats %in% out_sel$cat)]
#   df_miss <- df_meta[df_meta$cat %in% as.vector(miss),]
#   set.seed(k)
#   out_miss <- ddply(df_miss, .(cat), function(x){
#     x[sample(nrow(x), 1), ]
#   })
#   out <- rbind(out_sel, out_miss)
# })
# save(outs_lst, file = paste0(modDir, "/outs_lst.RData"))

pdf(paste0(outpath, "plots_tunelength_R2.pdf"))

for (i in c("residSRbirds", "residSRsnails", "SRsnails", "SRferns", "SRbirds", 
            "birds_jtu_NMDS1", "birds_jtu_NMDS2", "birds_jne_NMDS1", "birds_jne_NMDS2", 
            "birds_jac_NMDS1", "birds_jac_NMDS2", "sum_predator_N5")){
  
  
  #                     "tuneLength", "modDir", "type", "i"))
  ########################################################################################
  ###create and filter dataframe with all predictors and one response
  ########################################################################################
  df_scl <- cbind(df_meta, df_resp[,c(which(colnames(df_resp) == i))], df_scl_pred)
  colnames(df_scl)[grepl("df_resp",colnames(df_scl))] <- i
  df_scl <- Filter(function(x)(length(unique(x))>1), df_scl)
  df_scl <- df_scl[complete.cases(df_scl),]
  save(df_scl, file = paste0(modDir, "/df_scl_", i, "_filtered.RData"))
  
  ###check outs list
  outs_lst <- lapply(seq(ind_num), function(k){
    out_sel <- df_scl[which(df_scl$selID == k),]
    miss <- cats[!(cats %in% out_sel$cat)]
    df_miss <- df_scl[df_scl$cat %in% as.vector(miss),]
    set.seed(k)
    out_miss <- ddply(df_miss, .(cat), function(x){
      x[sample(nrow(x), 1), ]
    })
    out <- rbind(out_sel, out_miss)
  })
  #save(outs_lst, file = paste0(modDir, "/outs_lst.RData"))
  

  # for (j in seq(ind_num)){
  #   df_test <- df_scl[which(df_scl$plotID %in% outs_lst[[j]]$plotID),]
  #   df_train <- df_scl[!(df_scl$plotID %in% df_test$plotID),]
    
    # resp <- df_train[,which(colnames(df_train) == i)]
    # pred <- df_train[,c(which(colnames(df_train) %in% nm_pred))]
  resp <- df_scl[,which(colnames(df_scl) == i)]
  pred <- df_scl[,c(which(colnames(df_scl) %in% nm_pred))]
   
    #set indices for cv in ffs 
    cvind_num <- unique(df_scl$selID)
    cvind_num <- cvind_num[which(cvind_num >0)]
    cvouts_lst <- lapply(cvind_num, function(k){
      out_sel <- df_scl[which(df_scl$selID == k),]
      miss <- cats[!(cats %in% out_sel$cat)]
      df_miss <- df_scl[df_scl$cat %in% as.vector(miss),]
      set.seed(k)
      out_miss <- ddply(df_miss, .(cat), function(x){
        x[sample(nrow(x), 1), ]
      })
      out <- rbind(out_sel, out_miss)
    })

    cvIndex_out <- lapply(seq(length(cvouts_lst)), function(i){# #######wie übergeben
      out_res <- as.integer(rownames(cvouts_lst[[i]]))
    })
    cvIndex <- lapply(cvouts_lst, function(i){
      res <- which(!(df_scl$plotID %in% i$plotID))
    })
testfit <- train(pred, resp, method = "pls", tuneLength=30,
                 trControl = trainControl(#method="cv",
                   #indexOut = as.integer(rownames(outs_lst[[1]])), 
                   index = cvIndex, returnResamp = "all", savePredictions = TRUE))

#pdf(paste0(outpath, "plots_", i, ".pdf"))
#print(plot(testfit, main = i))
#dev.off()


# pdf(paste0(outpath, "plots_", i, ".pdf"))
print(plotModelCV(testfit,tuningValue = "ncomp",metric="Rsquared"))
# dev.off()
}#}
dev.off()


#################################
#################################
#################################
#################################
# 
# testfit <- train(pr, re, method = "pls", tuneLength=30,
#                  trControl = trainControl(#method="cv",
#                    #indexOut = as.integer(rownames(outs_lst[[1]])), 
#                    index = cvIndex, returnResamp = "all", savePredictions = TRUE))
# plot(testfit)
# 
# library(Rsenal)
# plotModelCV(testfit,tuningValue = "ncomp",metric="Rsquared")
# 
