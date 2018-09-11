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
library(mgcv)
#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
# sub <- "aug18/2018-09-01_ffs_pls_cv_noForest_alpha_all/"
sub <- "sep18/2018-09-07_ffs_pls_cv_noForest_alpha_all_RMSE/"
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
act_files <- gsub("sum", "sum_", act_files)
act_files <- gsub("N(\\d+)", "_N\\1", act_files)


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
  # print(i)
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
  resp <- gsub("sum", "sum_", resp)
  resp <- gsub("N(\\d+)", "_N\\1", resp)
  
  resp_df <- data.frame(Nplots = sum(!is.na(tbl[,resp])))
  row.names(resp_df) <- resp
  meanN_perplot <- mean(tbl[,resp], na.rm = T)
  sd_per_resp <- sd(tbl[,resp], na.rm = T)
  resp_df$meanN_perplot <- meanN_perplot
  resp_df$sd_per_resp <- sd_per_resp

  return(resp_df)
})
obs_smmry <- do.call(rbind, smmry_obs)
obs_smmry <- obs_smmry[which(rownames(obs_smmry) %in% nm_resp),]

saveRDS(obs_smmry, file = paste0(inpath, "obs_smmry.rds"))



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
  resp <- gsub("sum", "sum_", resp)
  resp <- gsub("N(\\d+)", "_N\\1", resp)
  
  
 

  
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
  
  #####get gam prediction for each SR
  if ((grepl("resid", resp) | grepl("NMDS", resp)) == F){
    dat <- data.frame("elevation"= mrg_tbl$elevation,
                      "response"= mrg_tbl[,grepl(paste0("^", resp, "$"), colnames(mrg_tbl))])
    mod_gam <- gam(response ~ s(elevation),data=dat)
    newdat <- data.frame("elevation"= mrg_tbl$elevation)
    prdct <- predict(object = mod_gam, newdata =  newdat)
    gam_prdct <- data.frame(plotID = mrg_tbl$plotID, plotUnq = mrg_tbl$plotUnq, resp = prdct)
    # gam_prdct <- data.frame(gam_prdct, resp = prdct)
    # colnames(gam_prdct)[colnames(gam_prdct) == "resp"] <- paste0(resp)
  }else{
    gam_prdct <- data.frame(plotID = mrg_tbl$plotID, plotUnq = mrg_tbl$plotUnq, resp = NA)
  }
  
  #####get gam cv predictions for each SR

  # gam_cv_prdct <- lapply(seq(length(outs_lst)), function(k){
  #   out_plt <- outs_lst[[k]]$plotID
  #   mrg_out <- mrg_tbl[-which(mrg_tbl$plotID %in% out_plt),]
  #   dat <- data.frame("elevation"= mrg_out$elevation, 
  #                     "response"= mrg_out[,grepl(paste0("^", resp, "$"), 
  #                                                colnames(mrg_out))])
  #   mod_gam <- gam(response ~ s(elevation),data=dat)
  #   newdat <- data.frame("elevation"= mrg_out$elevation)
  #   if ((grepl("resid", resp) | grepl("NMDS", resp)) == F){
  #     prdct <- predict(object = mod_gam, newdata =  newdat)
  #   }else{
  #     prdct <- NA
  #   }
  #   prdct_df <- cbind(mrg_out$plotID, mrg_out$plotUnq, dat, prdct)
  #   # colnames(prdct_df)[which(colnames(prdct_df) == "prdct")] <- paste0(j, "_", "run_", i)
  #   colnames(prdct_df) <- c("plotID", "plotUnq", "elevation", resp, paste0("prd_", resp, "_", "run_", k))
  #   return(prdct_df = prdct_df)
  # })
    
  gam_cv_prdct <- lapply(seq(length(outs_lst)), function(k){
    out_plt <- outs_lst[[k]]$plotID
    mrg_in <- mrg_tbl[-which(mrg_tbl$plotID %in% out_plt),]
    mrg_out <- mrg_tbl[which(mrg_tbl$plotID %in% out_plt),]
    dat <- data.frame("elevation"= mrg_in$elevation, 
                      "response"= mrg_in[,grepl(paste0("^", resp, "$"), 
                                                 colnames(mrg_in))])
    mod_gam <- gam(response ~ s(elevation),data=dat)
    newdat <- data.frame("elevation" = mrg_tbl[which(mrg_tbl$plotID %in% out_plt),"elevation"])
    #newdat <- data.frame("elevation"= mrg_in$elevation)
    if ((grepl("resid", resp) | grepl("NMDS", resp)) == F){
      prdct <- predict(object = mod_gam, newdata =  newdat)
    }else{
      prdct <- NA
    }
    prdct_df <- data.frame(plotID = mrg_out$plotID, 
                           plotUnq = mrg_out$plotUnq, 
                           elevation = mrg_out$elevation, 
                           resp = mrg_out[,which(colnames(mrg_out) == resp)], 
                           prdct= prdct)
    # colnames(prdct_df)[which(colnames(prdct_df) == "prdct")] <- paste0(j, "_", "run_", i)
    colnames(prdct_df) <- c("plotID", "plotUnq", "elevation", resp, paste0("prd_", resp, "_", "run_", k))
    return(prdct_df = prdct_df)
  })
  
  
  ###prediction of Sr and resid data
  new_df <- df_scl[which(df_scl$plotID %in% outs), ]
  if (nrow(new_df) < length(outs)){ #####wie kann das n�tig werden= wo passeirt das in skript 1? 
    new_df_outs <- data.frame(plotID = outs)
    new_df <- merge(new_df, new_df_outs, by = "plotID", all = T)
  }
  colnames(new_df)[5] <- resp#########################################sollte auf dauer geändert werden???wofür ist das überhaupt? Nur für einige nätig? bei SRmammals ist es das eh schon
 ###predict with pls normal model!
  prediction <- predict(mod, newdata = new_df)
  prdct <- data.frame(plotID = outs, 
                      plotUnq =  outs_lst[[grep(paste0(run_indx, "$"), names(outs_lst))]]$plotUnq, 
                      run = run, 
                      resp = prediction)
  stats <- postResample(prediction, new_df[[resp]])
  
  ###prediction of Sr and resid data on all plots
  new_df_all <- df_scl
  colnames(new_df_all)[5] <- resp
  prediction_all <- predict(mod, newdata = new_df_all)
  prdct_all <- data.frame(plotID = df_scl$plotID, 
                          plotUnq =  df_scl$plotUnq, 
                          run = run, 
                          resp = prediction_all)
  

  return(list(name = resp, 
              nameUnq = paste0(resp, "_", run), 
              stats = stats, 
              varimp = varimp, 
              selvars = selvars, 
              selvars_perf = selvars_perf, 
              selvars_perf_SE = selvars_perf_SE, 
              perf_all = perf_all, 
              gam_prdct = gam_prdct, 
              prdct = prdct, 
              prdct_all = prdct_all, 
              gam_cv_prdct = gam_cv_prdct
              ))

})
stats_lst <- do.call(rbind, prediction_rep)


###########################################
#####variable importance df per response
###########################################
stats <- data.frame()
varimp_lst <- list()
for (x in (seq(nrow(stats_lst)))){
  resp <- stats_lst[x,]$name
  respUnq <- stats_lst[x,]$nameUnq
  nmbrs <- data.frame(t(stats_lst[x,]$stats))
  varimp <- stats_lst[x,]$varimp
  tmp_stats <- data.frame(resp = resp, 
                          respUnq = respUnq, 
                          RMSE = nmbrs$RMSE, 
                          Rsquared = nmbrs$Rsquared, 
                          MAE = nmbrs$MAE)
  #summarys
  stats <- rbind(stats, tmp_stats)

  varimp_lst[[resp]] <- varimp

}

saveRDS(varimp_lst, file = paste0(outpath, "varimp_lst.rds"))
#varimp_lst <- readRDS(file = paste0(inpath, "varimp_lst.rds"))







#########################################
#####other statistical values (not from within postResample) # sollte vll verallgemeinert werden f�r alle statistischen dataframes
########################################
stats_smry <- as.data.frame(ddply(stats,~resp,summarise,
                             meanR2 = mean(Rsquared, na.rm = T),
                             medianR2 = median(Rsquared, na.rm = T),
                             sdR2 = sd(Rsquared, na.rm = T), 
                             meanRMSE = mean(RMSE, na.rm = T),
                             medianRMSE = median(RMSE, na.rm = T),
                             sdRMSE = sd(RMSE, na.rm = T)))

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


# gam pred###

# stats_lst <- do.call(rbind, prediction_rep)

# stats <- data.frame()
# varimp_lst <- list()
gam_prdct_df <- data.frame(plotID = mrg_tbl$plotID, plotUnq = mrg_tbl$plotUnq)
for (x in (seq(nrow(stats_lst)))){
  resp <- stats_lst[x,]$name
  tmp_gam_prdct <- stats_lst[x,]$gam_prdct
  colnames(tmp_gam_prdct)[colnames(tmp_gam_prdct) == "resp"] <- resp
  #summarys
  gam_prdct_df <- merge(gam_prdct_df, tmp_gam_prdct[,c(2:3)], by = "plotUnq")
  gam_prdct_df <- gam_prdct_df[, !grepl("\\.", colnames(gam_prdct_df))]
}

gam_prdct_df <- gam_prdct_df[,!grepl("resid", colnames(gam_prdct_df))]
saveRDS(object = gam_prdct_df, file = paste0(outpath, "gam_prdct_df.rds"))
# gam_prdct_df <- readRDS(paste0(outpath, "gam_prdct_df.rds"))

###gam prdct_cv df

gam_cv_df_mrg_lst <- lapply(seq(nrow(stats_lst)), function(k){
  gam_cv_prdct<- stats_lst[k,]$gam_cv_prdct
  gam_cv_df_mrg <- Reduce(function(x, y) merge(x, y, by="plotUnq", all = T), gam_cv_prdct)
  gam_cv_df_mrg <- gam_cv_df_mrg[, !grepl("\\.", colnames(gam_cv_df_mrg))]
  gam_cv_df_mrg$plotID <- substr(gam_cv_df_mrg$plotUnq, 1, 4)
  return(gam_cv_df_mrg)
})

gam_prdct_cv_df <- Reduce(function(x, y) merge(x, y, by="plotUnq"), gam_cv_df_mrg_lst)
gam_prdct_cv_df <- gam_prdct_cv_df[, !grepl("\\.", colnames(gam_prdct_cv_df))]
gam_prdct_cv_df <- data.frame(plotID = substr(gam_prdct_cv_df$plotUnq, 1, 4), 
                              plotUnq = gam_prdct_cv_df$plotUnq, 
                              #elevation = gam_prdct_cv_df$elevation, 
                              #gam_prdct_cv_df[,grepl("run", colnames(gam_prdct_cv_df))]) 
                              gam_prdct_cv_df[,-(which(colnames(gam_prdct_cv_df) == "plotUnq"))])
gam_prdct_cv_df <- gam_prdct_cv_df[,colSums(is.na(gam_prdct_cv_df)) < nrow(gam_prdct_cv_df)]
gam_prdct_cv_df <- gam_prdct_cv_df[,-which(grepl("resid", colnames(gam_prdct_cv_df)))]
gam_rlvnt <- colnames(gam_prdct_cv_df)[grepl("run", colnames(gam_prdct_cv_df))]
gam_rlvnt_Unq <- unique(substr(gam_rlvnt, 5, nchar(gam_rlvnt)-6))
gam_prdct_cv_df <- gam_prdct_cv_df[,c(which(colnames(gam_prdct_cv_df) =="plotID"), 
                                   which(colnames(gam_prdct_cv_df) =="plotUnq"), 
                                   which(grepl("run", colnames(gam_prdct_cv_df)))#, 
                                   #which(colnames(gam_prdct_cv_df) %in% gam_rlvnt_Unq)
                                   )]
saveRDS(object = gam_prdct_cv_df, file = paste0(outpath, "gam_prdct_cv_df.rds"))

####prediction df
prdct_df <- data.frame(plotID = mrg_tbl$plotID, plotUnq = mrg_tbl$plotUnq)
for (x in (seq(nrow(stats_lst)))){
  resp <- paste0(stats_lst[x,]$name, "_", unique(stats_lst[x,]$prdct$run))
  tmp_prdct <- stats_lst[x,]$prdct
  colnames(tmp_prdct)[colnames(tmp_prdct) == "resp"] <- resp
  tmp_prdct <- tmp_prdct[,-which(colnames(tmp_prdct) == "run")]

  prdct_df <- merge(prdct_df, tmp_prdct[,c(2:3)], by = "plotUnq", all = T)
  #prdct_df <- prdct_df[, !grepl("\\.", colnames(prdct_df))]
  #prdct_df <- prdct_df[, !grepl("run\\.", colnames(prdct_df))]
}
saveRDS(object = prdct_df, file = paste0(outpath, "prdct_df.rds"))


####prediction df_all
prdct_df_all <- data.frame(plotID = mrg_tbl$plotID, plotUnq = mrg_tbl$plotUnq)
for (x in (seq(nrow(stats_lst)))){
  resp <- paste0(stats_lst[x,]$name, "_", unique(stats_lst[x,]$prdct_all$run))
  tmp_prdct_all <- stats_lst[x,]$prdct_all
  colnames(tmp_prdct_all)[colnames(tmp_prdct_all) == "resp"] <- resp
  tmp_prdct_all <- tmp_prdct_all[,-which(colnames(tmp_prdct_all) == "run")]
  
  prdct_df_all <- merge(prdct_df_all, tmp_prdct_all[,c(2:3)], by = "plotUnq", all = T)
  #prdct_df <- prdct_df[, !grepl("\\.", colnames(prdct_df))]
  #prdct_df <- prdct_df[, !grepl("run\\.", colnames(prdct_df))]
}

saveRDS(object = prdct_df_all, file = paste0(outpath, "prdct_df_all.rds"))



####add gam and predicted resid
prdct_res <- prdct_df[,c(which(colnames(prdct_df) == "plotID"), 
                         which(colnames(prdct_df) == "plotUnq"),
                         which(grepl("resid", colnames(prdct_df)) & 
                                 !grepl("NMDS", colnames(prdct_df))))]
gam_resid <- data.frame(plotID = gam_prdct_df$plotID, plotUnq = gam_prdct_df$plotUnq)
##############################################hier muss wg _run zusatz grepl rein
for(i in colnames(prdct_res)[3:ncol(prdct_res)]){
  #match gam (of SR) to predicted resids - and add
  match <- substr(i,6, nchar(i)-5)
  gam_resid$tmp <- gam_prdct_df[,match] + prdct_res[,i]
  colnames(gam_resid)[which(colnames(gam_resid) == "tmp")] <- i
}

saveRDS(object = gam_resid, file = paste0(outpath, "gam_resid.rds"))

