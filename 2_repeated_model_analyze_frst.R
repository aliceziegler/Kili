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
sub <- "jun18_50m/60erFRST/2018-06-12_ffs_pls_cv_onlyForest/"
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
outs_lst <- get(load(paste0(inpath, "/outs_lst.RData")))
#nm_resp <- gsub("resid_", "resid", nm_resp)
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
########replace##############################################################################################################jac with _jac_...
# for (i in models){
#pdf(file = paste0(outpath, "plot_ffs.pdf")) ###only uncomment with plot argument and dev.off argument
prediction_rep <- lapply(models, function(i){
  #print(i)
  nm <- strsplit(x = i, split = "_|\\.")
  resp <- nm[[1]][length(nm[[1]])-1]####################sollte auf dauer anders (relativ) angegeben werden!
  resp <- gsub("jne", "_jne_", resp) #####################sollte auf dauer in ein gsub statement zsuammengefasst werden
  resp <- gsub("jac", "_jac_", resp)
  resp <- gsub("jtu", "_jtu_", resp)
  resp <- gsub("height", "_height_", resp)
  resp <- gsub("width", "_width_", resp)
  resp <- gsub("body", "body_", resp)
  resp <- gsub("kipps", "kipps_", resp)
  resp <- gsub("tarsus", "_tarsus_", resp)
  resp <- gsub("wing", "_wing_", resp)

  run <- nm[[1]][length(nm[[1]])-4]
  run_indx <- as.numeric(gsub("[[:alpha:]]", "", run))

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
#load(file = paste0(inpath, "stats.RData"))

#####other statistical values (not from within postResample)
stats_smry <- as.data.frame(ddply(stats,~resp,summarise,
                             meanR2 = mean(Rsquared),
                             medianR2 = median(Rsquared),
                             sdR2 = sd(Rsquared), 
                             meanRMSE = mean(RMSE),
                             medianRMSE = median(RMSE),
                             sdRMSE = sd(RMSE)))

stats <- merge(stats, stats_smry)

stats$RMSE_norm_by_sd <- NA
for (i in unique(stats$resp)){
  tmp <- stats[which(stats$resp == i),]

  stats$RMSE_norm_by_sd[which(stats$resp == i)] <- stats$RMSE[which(stats$resp == i)]/stats$sdRMSE[which(stats$resp == i)]
}

save(stats, file = paste0(outpath, "stats.RData"))

