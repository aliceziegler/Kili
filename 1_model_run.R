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
#setwd for folder with THIS script (only possible within Rstudio)
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
#setwd("/media/memory02/users/aziegler/src")
sub <- "jul18_50m/"
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
tbl_cols <- c(which(colnames(tbl_rw) %in% "plotID") : which(colnames(tbl_rw) %in% "lat"), 
              which(colnames(tbl_rw) %in% "SRmammals") : which(colnames(tbl_rw) %in% "BE_ELEV_ASPECT"), 
              which(colnames(tbl_rw) %in% "BE_ELEV_SLOPE") : which(colnames(tbl_rw) %in% "TCH"), 
              which(colnames(tbl_rw) %in% "chm_height_max") : which(colnames(tbl_rw) %in% "chm_surface_ratio"), 
              which(colnames(tbl_rw) %in% "dtm_aspect_mean") : which(colnames(tbl_rw) %in% "dtm_aspect_unweighted_mean"), 
              which(colnames(tbl_rw) %in% "dtm_elevation_sd") : which(colnames(tbl_rw) %in% "dtm_surface_ratio"), 
              which(colnames(tbl_rw) %in% "pulse_returns_max") : which(colnames(tbl_rw) %in% "pulse_returns_mean"), 
              which(colnames(tbl_rw) %in% "pulse_returns_sd"), 
              which(colnames(tbl_rw) %in% "vegetation_coverage_01m") : which(colnames(tbl_rw) %in% "vegetation_coverage_10m"), 
              which(colnames(tbl_rw) %in% "mdn_rtrn") : which(colnames(tbl_rw) %in% "LAI.y"),
              which(colnames(tbl_rw) %in% "gap_frac"))
###
# tbl_rw <- tbl_rw[which(duplicated(tbl_rw$plotID) == F),] ##################dauerhaft sollte das anders gelöst werden 
###
tbl <- tbl_rw[,tbl_cols]
#saveRDS(tbl, file = paste0(outpath, "mrg_tbl_relevant_cols.RDS"))

#^ and $ means only to look for this expression and not for resid_SRmammals
nm_resp <- colnames(tbl)[seq(grep("^SRmammals$", names(tbl)), 
                             grep("abundance", names(tbl)))]
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
comm <- "_cv_onlyForest"
ind_nums <- sort(unique(tbl$selID))
frst <- F # set true if model should onlybe done for forested plots

modDir <- paste0(outpath, Sys.Date(), "_", type, "_", method, comm)
if (file.exists(modDir)==F){
  dir.create(file.path(modDir))
}
#modDir <- "../data/mar18_50m_resid_nrmlz/2018-02-28_ffs_pls"
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################
#choose which plots are beeing used
if (frst == T){
  frst_cat <- c("fer", "flm", "foc", "fod", "fpd", "fpo")
  tbl <- tbl[which(tbl$cat %in% frst_cat),]
}

# choose which columns are beeing used for training, testing, val
df_pred_all <- tbl[, c(which(colnames(tbl) %in% nm_pred))]
## remove all variables where 90% of the entries are the same 
## (mostly 0 for lidar variables in heights where most plots dont have any points)
df_pred <- df_pred_all
for (i in colnames(df_pred_all)){
  frq <- table(df_pred_all[i])
  if (max(frq) > floor(nrow(df_pred_all) * 0.9)){
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


cl <- 15
registerDoParallel(cl)

# cl <- makePSOKcluster(10L)
# jnk <- clusterEvalQ(cl, library(c(caret, CAST, plyr)))

# model <- foreach(i = c("resid_SRmammals", "resid_SRdungbeetles", "resid_SRspiders", "resid_SRmillipedes", "resid_SRotheraculeata",
#                      "resid_SRparasitoids", "resid_SRcollembola", "resid_SRothercoleoptera", "resid_SRsyrphids", "resid_SRorthoptera",
#                      "resid_SRbats", "resid_SRants", "resid_SRbees", "resid_SRmoths", "resid_SRbirds", "resid_SRsnails",
#                      "resid_SRanimals", "resid_SRrosids", "resid_SRasterids", "resid_SRmonocots", "resid_SReudicots",
#                      "resid_SRlycopodiopsida", "resid_SRconifers", "resid_SRferns", "resid_SRmagnoliids", "resid_SRallplants",
#                      "SRmammals", "SRdungbeetles", "SRspiders", "SRmillipedes", "SRotheraculeata", "SRparasitoids", "SRcollembola",
#                      "SRothercoleoptera", "SRsyrphids", "SRorthoptera", "SRbats", "SRants", "SRbees", "SRmoths", "SRbirds",
#                      "SRsnails", "SRanimals", "SRrosids", "SRasterids", "SRmonocots", "SReudicots", "SRlycopodiopsida",
#                      "SRconifers", "SRferns", "SRmagnoliids", "SRallplants"), .packages=c("caret", "CAST", "plyr"))%dopar%{

# model <- foreach(i = colnames(df_resp), .packages=c("caret", "CAST", "plyr"))%dopar%{ ###all
# model <- foreach(i = (colnames(df_resp)[which(colnames(df_resp) %in% "ants_jtu_NMDS1"): length(colnames(df_resp))]), .packages=c("caret", "CAST", "plyr"))%dopar%{
  # clusterExport(cl, c("ind_num", "df_scl", "outs_lst", "method", "rfe_cntrl",
  #                     "tuneLength", "modDir", "type", "i"))
# model <- foreach(i = (colnames(df_resp)[1:floor(length(colnames(df_resp))/2)]), .packages=c("caret", "CAST", "plyr"))%dopar%{
#model <- foreach(i = (colnames(df_resp)[ceiling(length(colnames(df_resp))/2): length(colnames(df_resp))]), .packages=c("caret", "CAST", "plyr"))%dopar%{
  model <- foreach(i = (colnames(df_resp)[28:159]), .packages=c("caret", "CAST", "plyr"))%dopar%{
  ########################################################################################
  ###create and filter dataframe with all predictors and one response
  ########################################################################################
  df_scl <- cbind(df_meta, df_resp[,c(which(colnames(df_resp) == i))], df_scl_pred)
  colnames(df_scl)[grepl("df_resp",colnames(df_scl))] <- i
  df_scl <- Filter(function(x)(length(unique(x))>1), df_scl)
  df_scl <- df_scl[complete.cases(df_scl),]
  save(df_scl, file = paste0(modDir, "/df_scl_", i, "_filtered.RData"))
  
  ###check outs list
  outs_lst <- lapply(ind_nums, function(k){
    out_sel <- df_scl[which(df_scl$selID == k),]
    miss <- cats[!(cats %in% out_sel$cat)]
    df_miss <- df_scl[df_scl$cat %in% as.vector(miss),]
    set.seed(k)
    out_miss <- ddply(df_miss, .(cat), function(x){
      x[sample(nrow(x), 1), ]
    })
    out <- rbind(out_sel, out_miss)
  })
  save(outs_lst, file = paste0(modDir, "/outs_lst.RData"))
  
  
  
  for (j in seq(ind_nums)){
    # #if some plot in outs_lst is now filtered, fill void with other plot of 
    # #that landuse by chance (problem with toolittle plot subset (eg. only forest))
    # for(k in seq(nrow(outs_lst[[j]]))){
    #   if(!(outs_lst[[j]]$plotID[[k]] %in% df_scl$plotID)){
    #     df_miss <- df_scl[df_scl$cat %in% as.vector(outs_lst[[j]]$cat[[k]]),c(1:3)]
    #     set.seed(k)
    #     outs_lst[[j]][k,] <- ddply(df_miss, .(cat), function(x){
    #       x[sample(nrow(x), 1), ]
    #     })
    #   }
    # }
    # save(outs_lst, file = paste0(modDir, "/outs_lst_", i, ".RData"))
  
    
    df_test <- df_scl[which(df_scl$plotID %in% outs_lst[[j]]$plotID),]
    df_train <- df_scl[!(df_scl$plotID %in% df_test$plotID),]
    
    resp <- df_train[,which(colnames(df_train) == i)]
    pred <- df_train[,c(which(colnames(df_train) %in% nm_pred))]
    
    ################
    ################
    ################
    # df_test$selID
    cvind_num <- unique(sort(df_train$selID))
    cvouts_lst <- lapply(cvind_num, function(k){
      out_sel <- df_train[which(df_train$selID == k),]
      miss <- cats[!(cats %in% out_sel$cat)]
      df_miss <- df_train[df_train$cat %in% as.vector(miss),]
      set.seed(k)
      out_miss <- ddply(df_miss, .(cat), function(x){
        x[sample(nrow(x), 1), ]
      })
      out <- rbind(out_sel, out_miss)
    })
    ################
    ################
    ################
    

    
    
    cvIndex_out <- lapply(seq(length(cvouts_lst)), function(i){# #######wie übergeben
      out_res <- as.integer(rownames(cvouts_lst[[i]]))
    })
    

    
    cvIndex <- lapply(cvouts_lst, function(i){
      res <- which(!(df_train$plotID %in% i$plotID))
    })
    
    
    if (type == "rfe"){
      mod <- rfe(pred, resp, method = method,
                 rfeControl = rfe_cntrl, tuneLength = tuneLength)
    }else if (type == "ffs"){
      mod <- ffs(pred, resp, method = method, tuneGrid = expand.grid(ncomp = 1),###
          trControl = trainControl(index = cvIndex, 
                                   allowParallel = F)) ##########PLATZHALTER###########))
    }
    # mod <- train(pred, resp, method = method, tuneGrid = expand.grid(ncomp = 1),
    # trControl = trainControl(index = cvIndex, allowParallel = F))
    nm <- gsub("_", "", i)
    # nm_lst <- strsplit(x = i, split = "_")
    # if (grepl("NMDS", nm_lst[[1]][3])){
    #   nm <- paste0(nm_lst[[1]][1], nm_lst[[1]][2], nm_lst[[1]][3])
    # } else{
    #   nm <- paste0(nm_lst[[1]][1], nm_lst[[1]][2])
    # }
    # save(mod, file = paste0(modDir, "/indv_model_run", j, "_", type, "_", method, "_", 
    #                         nm, ".RData"))
    save(mod, file = paste0(modDir, "/indv_model_run", j, "_", type, "_", method, "_", 
                            nm, ".RData"))
    print(paste0("DONE: ", modDir, "model", type, "_", method, "_", 
                 i, "___________run", j))
    
  }
  
}


# for (i in colnames(df_resp)){ ####problem mit heteroptera
# for (i in c("resid_SRmammals", "resid_SRdungbeetles", "resid_SRspiders", "resid_SRmillipedes", "resid_SRotheraculeata", 
#             "resid_SRparasitoids", "resid_SRcollembola", "resid_SRothercoleoptera", "resid_SRsyrphids", "resid_SRorthoptera", 
#             "resid_SRbats", "resid_SRants", "resid_SRbees", "resid_SRmoths", "resid_SRbirds", "resid_SRsnails", 
#             "resid_SRanimals", "resid_SRrosids", "resid_SRasterids", "resid_SRmonocots", "resid_SReudicots", 
#             "resid_SRlycopodiopsida", "resid_SRconifers", "resid_SRferns", "resid_SRmagnoliids", "resid_SRallplants", 
#             "SRmammals", "SRdungbeetles", "SRspiders", "SRmillipedes", "SRotheraculeata", "SRparasitoids", "SRcollembola", 
#             "SRothercoleoptera", "SRsyrphids", "SRorthoptera", "SRbats", "SRants", "SRbees", "SRmoths", "SRbirds", 
#             "SRsnails", "SRanimals", "SRrosids", "SRasterids", "SRmonocots", "SReudicots", "SRlycopodiopsida", 
#             "SRconifers", "SRferns", "SRmagnoliids", "SRallplants")){
#   
#   # clusterExport(cl, c("ind_num", "df_scl", "outs_lst", "method", "rfe_cntrl", 
#   #                     "tuneLength", "modDir", "type", "i"))
#   ########################################################################################
#   ###create and filter dataframe with als predictors and one response
#   ########################################################################################
#   df_scl <- cbind(df_meta, df_resp[,c(which(colnames(df_resp) == i))], df_scl_pred)
#   colnames(df_scl)[grepl("df_resp",colnames(df_scl))] <- i
#   df_scl <- Filter(function(x)(length(unique(x))>1), df_scl)
#   df_scl <- df_scl[complete.cases(df_scl),]
#   save(df_scl, file = paste0(modDir, "/df_scl_", i, "_filtered.RData"))
#   
#   
#   for (j in seq(ind_num)){
#     
#     df_test <- df_scl[which(df_scl$plotID %in% outs_lst[[j]]$plotID),]
#     df_train <- df_scl[!(df_scl$plotID %in% df_test$plotID),]
#     
#     resp <- df_train[,which(colnames(df_train) == i)]
#     pred <- df_train[,c(which(colnames(df_train) %in% nm_pred))]
#     
#     
#     
#     mod <- rfe(pred, resp, method = method,
#                rfeControl = rfe_cntrl, tuneLength = tuneLength)
#     
#     save(mod, file = paste0(modDir, "/indv_model_run", j, "_", type, "_", method, "_", 
#                             i, ".RData"))
#     print(paste0("DONE: ", modDir, "model", type, "_", method, "_", 
#                  i, "___________run", j))
#     
#   }
#   
# }


save(nm_pred, file = paste0(modDir, "/nm_pred.RData"))
save(nm_resp, file = paste0(modDir, "/nm_resp.RData"))

