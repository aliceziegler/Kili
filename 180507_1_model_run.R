# Description: 
# Author: Alice Ziegler
# Date: 2018-02-09 15:05:58

rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#Packages: 
library(caret)
library(CAST)
library(doParallel)
library(plyr)
#setwd for folder with THIS script (only possible within Rstudio)
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "mai18_50m_resid_nrmlz_forest/"
inpath <- paste0("../data/")
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
              which(colnames(tbl_rw) %in% "mdn_rtrn") : which(colnames(tbl_rw) %in% "LAI"),
              which(colnames(tbl_rw) %in% "gap_frac"))
###
tbl_rw <- tbl_rw[which(duplicated(tbl_rw$plotID) == F),] ##################dauerhaft sollte das anders gelöst werden und rausfinden warum 4 und nicht nur 2 foc1
###
tbl <- tbl_rw[,tbl_cols]

#^ and $ means only to look for this expression and not for resid_SRmammals
nm_resp <- colnames(tbl)[seq(grep("^SRmammals$", names(tbl)), 
                             grep("resid_SRallplants", names(tbl)))]
nm_pred <- colnames(tbl)[seq(grep("AGB", names(tbl)),
                             grep("gap_frac", names(tbl)))]
nm_meta <- c("plotID", "selID", "cat")
###selectors
#tbl$selID <- paste0("id_", substr(tbl$plotID, 4, 4))
tbl$selID <- as.integer(substr(tbl$plotID, 4, 4))

###
#MODEL CONTROL 
###
type <- "rfe"
method <- "pls" # or other caret methods
tuneLength = 20
sizes <- seq(2, length(nm_pred), 10)
rfe_cntrl <- rfeControl(functions = caretFuncs, method = "LOOCV")
###DOCUMENTATION options
#comment for explenatory filename
comm <- "_unabh_repeat_only_for"
ind_num <- 10
frst <- T # set true if model should onlybe done for forested plots

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
df_pred <- tbl[, c(which(colnames(tbl) %in% nm_pred))]
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
outs_lst <- lapply(seq(ind_num), function(k){
  out_sel <- df_meta[which(df_meta$selID == k),]
  miss <- cats[!(cats %in% out_sel$cat)]
  df_miss <- df_meta[df_meta$cat %in% as.vector(miss),]
  set.seed(k)
  out_miss <- ddply(df_miss, .(cat), function(x){
    x[sample(nrow(x), 1), ]
  })
  out <- rbind(out_sel, out_miss)
})
save(outs_lst, file = paste0(modDir, "/outs_lst.RData"))    
cl <- 10
registerDoParallel(cl)

# cl <- makePSOKcluster(10L)
# jnk <- clusterEvalQ(cl, library(c(caret, CAST, plyr)))

model <- foreach(i = c("resid_SRmammals", "resid_SRdungbeetles", "resid_SRspiders", "resid_SRmillipedes", "resid_SRotheraculeata", 
                       "resid_SRparasitoids", "resid_SRcollembola", "resid_SRothercoleoptera", "resid_SRsyrphids", "resid_SRorthoptera", 
                       "resid_SRbats", "resid_SRants", "resid_SRbees", "resid_SRmoths", "resid_SRbirds", "resid_SRsnails", 
                       "resid_SRanimals", "resid_SRrosids", "resid_SRasterids", "resid_SRmonocots", "resid_SReudicots", 
                       "resid_SRlycopodiopsida", "resid_SRconifers", "resid_SRferns", "resid_SRmagnoliids", "resid_SRallplants", 
                       "SRmammals", "SRdungbeetles", "SRspiders", "SRmillipedes", "SRotheraculeata", "SRparasitoids", "SRcollembola", 
                       "SRothercoleoptera", "SRsyrphids", "SRorthoptera", "SRbats", "SRants", "SRbees", "SRmoths", "SRbirds", 
                       "SRsnails", "SRanimals", "SRrosids", "SRasterids", "SRmonocots", "SReudicots", "SRlycopodiopsida", 
                       "SRconifers", "SRferns", "SRmagnoliids", "SRallplants"), .packages=c("caret", "CAST", "plyr"))%dopar%{
                         
                         # clusterExport(cl, c("ind_num", "df_scl", "outs_lst", "method", "rfe_cntrl", 
                         #                     "tuneLength", "modDir", "type", "i"))
                         ########################################################################################
                         ###create and filter dataframe with all predictors and one response
                         ########################################################################################
                         df_scl <- cbind(df_meta, df_resp[,c(which(colnames(df_resp) == i))], df_scl_pred)
                         colnames(df_scl)[grepl("df_resp",colnames(df_scl))] <- i
                         df_scl <- Filter(function(x)(length(unique(x))>1), df_scl)
                         df_scl <- df_scl[complete.cases(df_scl),]
                         save(df_scl, file = paste0(modDir, "/df_scl_", i, "_filtered.RData"))
                         ##create filtered outs_lst
                         #outs_lst <- outs_lst_all[which(outs_lst_all$plotID %in% df_scl$plotID),]
                         
                         for (j in seq(ind_num)){
                           
                           df_test <- df_scl[which(df_scl$plotID %in% outs_lst[[j]]$plotID),]
                           df_train <- df_scl[!(df_scl$plotID %in% df_test$plotID),]
                           
                           resp <- df_train[,which(colnames(df_train) == i)]
                           pred <- df_train[,c(which(colnames(df_train) %in% nm_pred))]
                           
                           
                           
                           mod <- rfe(pred, resp, method = method,
                                      rfeControl = rfe_cntrl, tuneLength = tuneLength)
                           
                           save(mod, file = paste0(modDir, "/indv_model_run", j, "_", type, "_", method, "_", 
                                                   i, ".RData"))
                           print(paste0("DONE: ", modDir, "model", type, "_", method, "_", 
                                        i, "___________run", j))
                           
                         }
                         
                       }


save(nm_pred, file = paste0(modDir, "/nm_pred.RData"))
save(nm_resp, file = paste0(modDir, "/nm_resp.RData"))

