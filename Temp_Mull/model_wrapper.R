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
#setwd for folder with THIS script (only possible within Rstudio)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
sub <- "mar18_50m_resid_nrmlz/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../data/", sub)
###DOCUMENTATION options
#comment for explenatory filename
comm <- "frst"
frst <- T
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
  which(colnames(tbl_rw) %in% "dtm_aspect_mean") : which(colnames(tbl_rw) %in% "dtm_elevation_sd"), 
  which(colnames(tbl_rw) %in% "dtm_slope_max") : which(colnames(tbl_rw) %in% "dtm_surface_ratio"), 
  which(colnames(tbl_rw) %in% "pulse_returns_max") : which(colnames(tbl_rw) %in% "pulse_returns_sd"), 
  which(colnames(tbl_rw) %in% "vegetation_coverage_01m") : which(colnames(tbl_rw) %in% "qntl_rng"), 
  which(colnames(tbl_rw) %in% "gap_frac"))

tbl <- tbl_rw[,tbl_cols]

if (frst == T){
  tbl <- tbl[which(tbl$cat %in% c("flm", "fod", "foc", "fpo", "fpd", "fed", "fer")),]
}

#^ and $ means only to look for this expression and not for resid_SRmammals
nm_resp <- colnames(tbl)[seq(grep("^SRmammals$", names(tbl)), 
                             grep("resid_SRallplants", names(tbl)))]
nm_pred <- colnames(tbl)[seq(grep("BE_ELEV_ASPECT", names(tbl)),
                                grep("gap_frac", names(tbl)))]
###
#MODEL CONTROL 
###


type <- "ffs" #ffs, rfe or train
method <- "pls" # or other caret methods
tuneLength <- 20
fitControl <- trainControl(
  method = "LOOCV",
  returnResamp = "all",
  savePredictions = TRUE)
seeds = 11
# rfe_cntrl <- rfeControl(functions = caretFuncs,
#                         number = 50, method = "LOOCV")


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
df <- cbind(df_pred, df_resp)

#data preparation
df <- Filter(function(x)(length(unique(x))>1), df)
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
df_scl <- cbind(df_scl_pred, df[,c(which(colnames(df) %in% nm_resp))])

pred <- df_scl[,c(which(colnames(df_scl) == nm_pred[1]):
                    which(colnames(df_scl) == nm_pred[length(nm_pred)]))]
resp <- df_scl[,c(which(colnames(df_scl) == nm_resp[1]):
                    which(colnames(df_scl) == nm_resp[length(nm_resp)]))]

for (i in seq(resp)){
  if (type == "ffs"){
    if(length(showConnections()) == 0){
      #cl = parallel::makeCluster(8)
      #doParallel::registerDoParallel(cl)
    }
    
    mod <- ffs(pred, resp[,i], method = method,
               trControl = fitControl, tuneLength = tuneLength)
  }
  if (type == "rfe"){
    mod <- train(pred, resp[,i], method = method,
                 trControl = fitControl, tuneLength = tuneLength)
  }
  save(mod, file = paste0(modDir, "/indv_model_", type, "_", method, "_", 
                          colnames(resp)[i], ".RData"))
  print(paste0("DONE: ", modDir, "model", type, "_", method, "_", 
               colnames(resp)[i]))
}

#parallel::stopCluster(cl)


models <- list.files(path = modDir, pattern = glob2rx("indv_model*"), full.names = TRUE)

mod_all <- list()
for (i in seq(length(models))){
  load(file = models[i])
  nm <- strsplit(x = models[i], split = "_|\\.")[[1]][12]############sollte auf dauer anders (relativ) angegeben werden!
  mod_all[[nm]] <- mod
}
save(mod_all, file = paste0(modDir, "/pls_model_list_all.RData"))

# models = list.files(modDir, pattern = glob2rx("indv_model*"), full.names = TRUE)
# 
# mod <- list()
# for (i in seq(length(models))){
#   load(file = models[i])
#   nm <- strsplit(x = models[i], split = "_|\\.")[[1]][2]
#   mod[[nm]] <- mod_pls
# }
# save(mod, file = paste0(outpath, "pls_model_list_all.RData"))


