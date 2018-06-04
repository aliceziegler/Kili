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
sub <- "mar18_50m_resid_nrmlz/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../data/", sub)
parll <- T

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
comm <- "_unabh"
ind_num <- 10

modDir <- paste0(outpath, Sys.Date(), "_", type, "_", method, comm)
if (file.exists(modDir)==F){
  dir.create(file.path(modDir))
}
#modDir <- "../data/mar18_50m_resid_nrmlz/2018-02-28_ffs_pls"
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################
# choose which columns are beeing used for training, testing, val
df_pred <- tbl[, c(which(colnames(tbl) %in% nm_pred))]
df_resp <- tbl[, c(which(colnames(tbl) %in% nm_resp))]
df_meta <- tbl[, c(which(colnames(tbl) %in% nm_meta))]
df <- cbind(df_meta, df_pred, df_resp)

#data preparation ###do within loop for each species for not loosing too many plots
df <- Filter(function(x)(length(unique(x))>1), df) # 
df <- df[complete.cases(df),]
#scaling data to mean 0 and sd 1
scl_lst <- lapply(df[,c(which(colnames(df) %in% nm_pred))], function(i){
  if (class(i) == "numeric"){
    scale(i, center = T, scale = T)
  }else if (class(i) == "integer"){
    scale(as.numeric(i), center = T, scale = T)
  }else{
    i <- i #non numeric or integer columns stay as they are
  }
})
df_scl_pred <- do.call(data.frame, scl_lst)
df_scl_all <- cbind(df[,c(which(colnames(df) %in% nm_meta))], df_scl_pred, df[,c(which(colnames(df) %in% nm_resp))])

########################################################################################
###resampling by group = Für jede Landnutzungsform einen Plot je durchlauf (zufällig rausnehmen)
########################################################################################
outs_lst <- lapply(seq(ind_num), function(i){
  set.seed(i)
  out <- ddply(df_scl_all, .(cat), function(x){
    x[sample(nrow(x), 1), ]
  })
})

for (j in seq(ind_num)){
  df_ind <- outs_lst[[j]]
  df_scl <- df_scl_all[!(df_scl_all$plotID %in% df_ind$plotID),]
  
  pred <- df_scl[,c(which(colnames(df_scl) == nm_pred[1]):
                      which(colnames(df_scl) == nm_pred[length(nm_pred)]))]
  resp <- df_scl[,c(which(colnames(df_scl) == nm_resp[1]):
                      which(colnames(df_scl) == nm_resp[length(nm_resp)]))]
  
  for (i in seq(resp)){
    mod <- rfe(pred, resp[,i], method = method,
               rfeControl = rfe_cntrl, tuneLength = tuneLength)
    
    save(mod, file = paste0(modDir, "/indv_model_run", j, "_", type, "_", method, "_", 
                            colnames(resp)[i], ".RData"))
    print(paste0("DONE: ", modDir, "model", type, "_", method, "_", 
                 colnames(resp)[i], "___________run", j))
  }
  
}

save(nm_pred, file = paste0(modDir, "nm_pred.RData"))
save(nm_resp, file = paste0(modDir, "nm_resp.RData"))

