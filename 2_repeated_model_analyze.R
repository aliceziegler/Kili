# Description: 
# Author: Alice Ziegler
# Date: 2018-03-07 12:08:34
# to do: ###optimieren: mod_all muss vermieden werden. jeder loop muss über die einzelmodelle laufen!
#########werden hier wirklich nur die resp verwendet, die im ffs rauskommen???
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
library(gsubfn)
library(plyr)
library(dplyr)
library(CAST)
#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "jul18_50m/2018-08-02_ffs_pls_cv_onlyForest_noslpasp/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../out/", sub)
if (file.exists(outpath)==F){
  dir.create(file.path(outpath), recursive = T)
}
########################################################################################
###Settings
########################################################################################
nm_pred <- get(load(file = paste0(inpath, "nm_pred.RData")))
nm_resp <- get(load(file = paste0(inpath, "nm_resp.RData")))
outs_lst <- get(load(paste0(inpath, "/outs_lst.RData")))
mrg_tbl <- get(load(paste0(inpath, "../dat_ldr_mrg.RData")))
nm_resp <- gsub("resid_", "resid", nm_resp)
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################
dfs <- list.files(path = inpath, pattern = glob2rx("df_scl*"), full.names = F)
models <- list.files(path = inpath, pattern = glob2rx("indv_model*"), full.names = TRUE)

run_all <- c()
#run_all
model_strsplits <- unique(strsplit(x = paste(models, collapse=""), split = "_")[[1]])
run_all_srt <- mixedsort(model_strsplits[grep("run", model_strsplits)])

for (i in seq(length(outs_lst))){
  names(outs_lst)[[i]] <- run_all_srt[[i]]
}
all_plts <- F
if (all_plts == F){
  if(length(grep("_only", sub))){
    frst <- T
  }else{
    frst <- F
  }
}

if (all_plts == F){
  if (frst == T){
    cat <- c("fer", "flm", "foc", "fod", "fpd", "fpo")
  }else if (frst == F){
    cat <- c("cof", "gra", "hel", "hom", "mai", "sav")
  }
  tbl <- mrg_tbl[which(mrg_tbl$cat %in% cat),]
}
#####check which models were dissmissed due to errorhandling###############
err_hnd_files <- c()
act_files <- list.files(inpath)[grep("indv_model", list.files(inpath))]
act_files <- gsub("jne", "_jne_", act_files) #####################sollte auf dauer in ein gsub statement zsuammengefasst werden
act_files <- gsub("jac", "_jac_", act_files)
act_files <- gsub("jtu", "_jtu_", act_files)
act_files <- gsub("height", "_height_", act_files)
act_files <- gsub("width", "_width_", act_files)
act_files <- gsub("body", "body_", act_files)
act_files <- gsub("kipps", "kipps_", act_files)
act_files <- gsub("tarsus", "_tarsus_", act_files)
act_files <- gsub("wing", "_wing_", act_files)
for (i in nm_resp){
  err_hnd <- act_files[grepl(paste0("_", i), act_files)]
  if(length(err_hnd) != length(outs_lst)){
    file_miss <- c()
    for (j in names(outs_lst)){
      file_ok <- err_hnd[grepl(j, err_hnd)]
      if (length(file_ok) == 0){
        file_miss <- c(file_miss, paste0(i, "_", j))
      }
    }
    err_hnd_files <- c(err_hnd_files, file_miss)
    #err_hnd_files <- c(err_hnd_files, paste0(i, "_", length(err_hnd)))
  }
}
saveRDS(err_hnd_files, file = paste0(inpath, "err_handling_files.rds"))
################################################################################

# ##############Number of plots with this taxa
#i <- models[grep("_SRheteroptera", models)] #######testing
smmry_obs <- lapply(models, function(i){
  #print(i)
  nm <- strsplit(x = i, split = "_|\\.")
  resp <- nm[[1]][length(nm[[1]])-1]####################sollte auf dauer anders (relativ) angegeben werden!
  run <- nm[[1]][length(nm[[1]])-4]
  run_indx <- as.numeric(gsub("[[:alpha:]]", "", run))
  
  resp <- gsub("jne", "_jne_", resp) #####################sollte auf dauer in ein gsub statement zsuammengefasst werden
  resp <- gsub("jac", "_jac_", resp)
  resp <- gsub("jtu", "_jtu_", resp)
  resp <- gsub("height", "_height_", resp)
  resp <- gsub("width", "_width_", resp)
  resp <- gsub("body", "body_", resp)
  resp <- gsub("kipps", "kipps_", resp)
  resp <- gsub("tarsus", "_tarsus_", resp)
  resp <- gsub("wing", "_wing_", resp)
  
  resp_df <- data.frame(Nplots = sum(!is.na(tbl[,resp])))
  row.names(resp_df) <- resp
  meanN_perplot <- mean(tbl[,resp], na.rm = T)
  resp_df$meanN_perplot <- meanN_perplot

  return(resp_df)
})
obs_smmry <- do.call(rbind, smmry_obs)
obs_smmry <- obs_smmry[which(rownames(obs_smmry) %in% nm_resp),]

saveRDS(obs_smmry, file = paste0(inpath, "obs_smmry.rds"))

##############Numer of Species per plot (mean) per taxa





########replace##############################################################################################################jac with _jac_...
# for (i in models){
#pdf(file = paste0(outpath, "plot_ffs.pdf")) ###only uncomment with plot argument and dev.off argument
prediction_rep <- lapply(models, function(i){
  #print(i)
  nm <- strsplit(x = i, split = "_|\\.")
  resp <- nm[[1]][length(nm[[1]])-1]####################sollte auf dauer anders (relativ) angegeben werden!
  run <- nm[[1]][length(nm[[1]])-4]
  run_indx <- as.numeric(gsub("[[:alpha:]]", "", run))
  
  resp <- gsub("jne", "_jne_", resp) #####################sollte auf dauer in ein gsub statement zsuammengefasst werden
  resp <- gsub("jac", "_jac_", resp)
  resp <- gsub("jtu", "_jtu_", resp)
  resp <- gsub("height", "_height_", resp)
  resp <- gsub("width", "_width_", resp)
  resp <- gsub("body", "body_", resp)
  resp <- gsub("kipps", "kipps_", resp)
  resp <- gsub("tarsus", "_tarsus_", resp)
  resp <- gsub("wing", "_wing_", resp)
  
  #load single models
  # mod_nm <- models[grepl(resp, models) & grepl(run, models)]
  #print(mod_nm)
  mod <- get(load(file = i))
  
  ###plot all models in one pdf (lange rechenzeit)
  # p <- (plot_ffs(mod)+ggtitle(nm))
  # ggsave(filename = paste0(outpath, "plot_ffs_", resp, ".pdf"), plot = p, width = 25,
  #        height = 25, units = "cm")
  
  ###hier var imp und eventuell anderes f�r heatmap berechnen!
  varimp <- varImp(mod)$importance
  selvars <- mod$selectedvars
  selvars_perf <- mod$selectedvars_perf
  selvars_perf_SE <- mod$selectedvars_perf_SE
  perf_all <- mod$perf_all
  
  df_scl <- get(load(paste0(inpath, dfs[grep(paste0("_", resp), dfs)])))

  
  outs <- outs_lst[[grep(paste0(run_indx, "$"), names(outs_lst))]]$plotID #1$ means search for 1 at end of line
  new_df <- df_scl[which(df_scl$plotID %in% outs),]
  colnames(new_df)[5] <- resp#########################################sollte auf dauer geändert werden???wofür ist das überhaupt? Nur für einige nätig? bei SRmammals ist es das eh schon
  prediction <- predict(mod, newdata = new_df)
  stats <- postResample(prediction, new_df[[resp]])
  return(list(name = resp, 
              stats = stats, 
              varimp = varimp, 
              selvars = selvars, 
              selvars_perf = selvars_perf, 
              selvars_perf_SE = selvars_perf_SE, 
              perf_all = perf_all
              ))
  #return(stats)

})
stats_lst <- do.call(rbind, prediction_rep)


stats <- data.frame()
varimp_lst <- list()
for (x in (seq(nrow(stats_lst)))){
  resp <- stats_lst[x,]$name
  nmbrs <- data.frame(t(stats_lst[x,]$stats))
  varimp <- stats_lst[x,]$varimp
  tmp_stats <- data.frame(resp = resp, 
                          RMSE = nmbrs$RMSE, 
                          Rsquared = nmbrs$Rsquared, 
                          MAE = nmbrs$MAE)
  #summarys
  stats <- rbind(stats, tmp_stats)

  varimp_lst[[resp]] <- varimp

}

saveRDS(varimp_lst, file = paste0(inpath, "varimp_lst.rds"))
#varimp_lst <- readRDS(file = paste0(inpath, "varimp_lst.rds"))


#####other statistical values (not from within postResample)
stats_smry <- as.data.frame(ddply(stats,~resp,summarise,
                             meanR2 = mean(Rsquared),
                             medianR2 = median(Rsquared),
                             sdR2 = sd(Rsquared), 
                             meanRMSE = mean(RMSE),
                             medianRMSE = median(RMSE),
                             sdRMSE = sd(RMSE)))

stats <- merge(stats, stats_smry)

###merge stats with N plots and mean N of species per plot number
stats <- merge(stats, obs_smmry, by.x = "resp", by.y = 0)

stats$RMSE_norm_by_sd <- NA
for (k in unique(stats$resp)){
  tmp <- stats[which(stats$resp == k),]
  tmp_sd <- sd(mrg_tbl[,which(colnames(mrg_tbl) == k)], na.rm = T)
  stats$RMSE_norm_by_sd[which(stats$resp == k)] <- stats$RMSE[which(stats$resp == k)]/tmp_sd
}

stats$RMSE_norm_by_mean <- NA
for (j in unique(stats$resp)){
  tmp <- stats[which(stats$resp == j),]
  tmp_mean <- mean(mrg_tbl[,which(colnames(mrg_tbl) == j)], na.rm = T)
  stats$RMSE_norm_by_mean[which(stats$resp == j)] <- stats$RMSE[which(stats$resp == j)]/tmp_mean
}

save(stats, file = paste0(inpath, "stats.RData"))
#stats <- get(load(paste0(inpath, "stats.RData")))
