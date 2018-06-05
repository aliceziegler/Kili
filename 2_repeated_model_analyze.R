# Description: 
# Author: Alice Ziegler
# Date: 2018-03-07 12:08:34
# to do: 
rm(list=ls())
########################################################################################
###Presettings
########################################################################################
#Packages: 
library(raster)
library(RColorBrewer)
library(rasterVis)
library(grid)
library(caret)
library(gtools)
#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "mai18_50m_resid_nrmlz_newDB_rad/2018-05-30_ffs_pls_ffs_indxINOUT/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../out/", sub)
if (file.exists(outpath)==F){
  dir.create(file.path(outpath), recursive = T)
}
########################################################################################
###Settings
########################################################################################
load(file = paste0(inpath, "nm_pred.RData"))
load(file = paste0(inpath, "nm_resp.RData"))
nm_resp <- gsub("resid_", "resid", nm_resp)
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################

#######################
###merge models
#######################

models <- list.files(path = inpath, pattern = glob2rx("indv_model*"), full.names = TRUE)

mod_all <- list()
run_all <- c()
for (i in seq(length(models))){
  mod <- get(load(file = models[i]))
  nm <- strsplit(x = models[i], split = "_|\\.")
  resp <- nm[[1]][length(nm[[1]])-1]#####################################################sollte auf dauer anders (relativ) angegeben werden!
  run <- nm[[1]][length(nm[[1]])-4]
  mod_all[[paste0(resp, "_", run)]] <- mod
  run_all <- c(run_all, run)
}
run_all <- unique(run_all)
run_all_srt <- mixedsort(run_all)
#save(mod_all, file = paste0(inpath, "/pls_model_list_all.RData"))


mod_lst <- lapply(nm_resp, function(x){
  all_runs <- mod_all[grep(paste0("^", x), names(mod_all))]
  return(all_runs)
})
names(mod_lst) <- nm_resp

save(mod_lst, file = paste0(inpath, "/pls_model_list_all_lst.RData"))

### check testing plots for each run
outs_lst <- get(load(paste0(inpath, "/outs_lst.RData")))
for (i in seq(length(outs_lst))){
  names(outs_lst)[[i]] <- run_all_srt[[i]]
}

###predict in repeated
####rename df_scls per hand resid_ zu resid
dfs <- list.files(path = inpath, pattern = glob2rx("df_scl*"), full.names = F)

prediction_rep <- lapply(names(mod_lst), function(y){
  if (length(mod_lst[[y]]) != 0){
    df_scl <- get(load(paste0(inpath, dfs[grep(paste0("^", "df_scl_", y), dfs)])))
    print(y)
    prediction_run <- lapply(seq(length(mod_lst[[y]])), function(z){
      print(z)
      outs <- outs_lst[[grep(paste0(z, "$"), names(outs_lst))]]$plotID
      new_df <- df_scl[which(df_scl$plotID %in% outs),]
      colnames(new_df)[4] <- y#########################################sollte auf dauer geändert werden
      prediction <- predict(mod_lst[[y]][[paste0(y, "_run", z)]], newdata = new_df)
      stats <- postResample(prediction, new_df[[y]])
      return(stats)
    })
    aprediction_run_mat <- do.call(rbind, prediction_run)
  }
})
names(prediction_rep) <- names(mod_lst)

# df_resp <- as.data.frame(prediction_rep$SRmammals)
# df_resp$resp <- "SRmammals"

df_resp_all <- data.frame(matrix(nrow=0, ncol=4))
for (i in names(mod_lst)){
  if (!is.null(prediction_rep[[i]])){
    print(i)
    df_resp <- as.data.frame(prediction_rep[[i]])
    df_resp$resp <- i
    df_resp_all <- rbind(df_resp_all, df_resp)
  }
}

save(df_resp_all, file = paste0(inpath, "df_resp_all_for_plotting.RData"))
