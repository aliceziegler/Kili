# Description: 
# Author: Alice Ziegler
# Date: 2018-03-07 12:08:34
# to do: ###optimieren: mod_all muss vermieden werden. jeder loop muss Ã¼ber die einzelmodelle laufen!
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
sub <- "sep18/2018-09-25_ffs_pls_cv_allplots_allalpha_RMSE_elev_dstrb_elevsq_plsresid_cvindex/"
# sub <- "sep18/2018-09-11_ffs_pls_cv_noForest_alpha_all_RMSE_elev_dstrb/"
all_plts <- T#################################################################dauerhaft mit grepl "all" umstellen
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
tbl_scl <- readRDS(file = paste0(inpath, "tbl_scl.rds"))
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################
#dfs <- list.files(path = inpath, pattern = glob2rx("df_scl*"), full.names = F)
models <- list.files(path = inpath, pattern = glob2rx("indv_model*"), full.names = TRUE)

mrg_tbl$selID <- as.numeric(substr(mrg_tbl$plotID, 4, 4))

run_all <- c()
#run_all
model_strsplits <- unique(strsplit(x = paste(models, collapse=""), split = "_")[[1]])
run_all_srt <- mixedsort(model_strsplits[grep("run", model_strsplits)])

for (i in seq(length(outs_lst))){
  names(outs_lst)[[i]] <- run_all_srt[[i]]
}

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
  tbl_scl <- tbl_scl[which(mrg_tbl$cat %in% cat),]
}else if (all_plts == T){
  tbl <- mrg_tbl
}
cats <- unique(tbl$cat)

cv_in_out_lst <- lapply(seq(length(outs_lst)), function(x){
  out_plt <- outs_lst[[x]]$plotID

  ## m-fache cv von jeder landuseclass eines, aber zuf?llige Wahl der indices und n mal wiederholt
  # cvouts_lst <- lapply(seq(1:50), function(m){
  #   set.seed(m)
  #   out_sel <- ddply(tbl_scl[which(!(tbl_scl$plotID %in% out_plt)),], .(cat), function(x){
  #     x[sample(nrow(x), 1), ]
  #   })
  #   out <- rbind(as.character(out_sel$plotID))
  # })
  tbl_in <- tbl_scl[-which(tbl_scl$plotID %in% out_plt),]
  tbl_out <- tbl_scl[which(tbl_scl$plotID %in% out_plt),]
  
  ###cv index von jeder landuseclass eines, aber zuf?llige Wahl der indices und 20 mal wiederholt
  # cvouts_lst <- lapply(seq(1:20), function(k){
  #   set.seed(k)
  #   out_sel <- ddply(tbl_in, .(cat), function(x){
  #     x[sample(nrow(x), 1), ]
  #   })
  #   out <- rbind(out_sel)
  # })
  # cvIndex <- lapply(cvouts_lst, function(i){
  #   res <- which(!(tbl_in$plotID %in% i$plotID))
  # })
  
  ###cv index gleiches system wie outer loop
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
  cvIndex_out <- lapply(cvouts_lst, function(i){# #######wie übergeben
    res <- which((tbl_in$plotID %in% i$plotID))
  })
return(list(cvIndex = cvIndex, 
            cvIndex_out = cvIndex_out))
})
cv_in_out_lst[[5]]$cvIndex_out
cv_in_out <- do.call(rbind,cv_in_out_lst)
# cvIndex <- cv_in_out[[5,1]]
# cvIndex_out <- cv_in_out[[5,2]]
# # 
# # cvin <- do.call(rbind, cv_in_out["cvIndex",])
# # rownames(cvin) <- paste0("out_run", seq(nrow(cvin)))
# # colnames(cvin) <- paste0("cv_run", seq(ncol(cvin)))
# # 
# # cvout <- do.call(rbind, cv_in_out["cvIndex_out",])
# # rownames(cvout) <- paste0("out_run", seq(nrow(cvout)))
# # colnames(cvout) <- paste0("cv_run", seq(ncol(cvout)))
# 

# ##############Number of plots with this taxa
#i <- models[grep("_SRheteroptera", models)] #######testing
resp_loop <- lapply(nm_resp, function(x){
  # print(x)
 
    ###summary observations
    obs_smmry <- data.frame(Nplots = sum(!is.na(tbl[,x])))
    row.names(obs_smmry) <- x
    meanN_perplot <- mean(tbl[,x], na.rm = T)
    sd_per_resp <- sd(tbl[,x], na.rm = T)
    obs_smmry$meanN_perplot <- meanN_perplot
    obs_smmry$sd_per_resp <- sd_per_resp
    
    
    
    #####pls cv prediction only elev and elevsq
    pls_elevsq_cv_prdct <- lapply(seq(length(outs_lst)), function(k){
      # print(k)
      out_plt <- outs_lst[[k]]$plotID
      
      tbl_in <- tbl_scl[-which(tbl_scl$plotID %in% out_plt),]
      tbl_out <- tbl_scl[which(tbl_scl$plotID %in% out_plt),]
      
      
      cvIndex <- cv_in_out_lst[[k]]$cvIndex
      cvIndex_out <- cv_in_out_lst[[k]]$cvIndex_out
      
      
       mod_pls_elev_cv <- tryCatch(
      train(tbl_in[,c("elevation","elevsq")], tbl_in[,x], method = "pls",
                               tuneGrid = expand.grid(ncomp = c(1,2)),
                               metric = "RMSE",
                               trControl = trainControl(method = "cv", index = cvIndex, 
                                                        indexOut = cvIndex_out)),
      error = function(e)pls_elevsq_cv_prdct <- NA)


      if ((grepl("resid", x) == F || grepl("NMDS", x) == F) && is.na(mod_pls_elev_cv) == F){
        print(mod_pls_elev_cv)
        prdct_pls_elev_cv <- predict(object = mod_pls_elev_cv, newdata = tbl_out)
      }else{
        prdct_pls_elev_cv <- NA
      }
return(list(plotID = tbl_out$plotID, 
            plotUnq = tbl_out$plotUnq, 
            mod_pls_elev_cv = mod_pls_elev_cv,
            prdct_pls_elev_cv = prdct_pls_elev_cv, 
            obs_smmry = obs_smmry))
    })

    return(list(resp = x, 
                resp_pls_elevsq_cv_prdct = pls_elevsq_cv_prdct
                ))
    })

# nm <- resp_loop[[1]]$resp
# mod_run1 <- resp_loop[[1]]$resp_pls_elevsq_cv_prdct[[1]]$mod_pls_elev_cv
# prdct_run1 <- resp_loop[[1]]$resp_pls_elevsq_cv_prdct[[1]]$prdct_pls_elev_cv

# 
#     ################################################################################################hier werden 2 ncomps in datenframe geschrieben
#     prdct_df <- data.frame(plotID = tbl_out$plotID,
#                            plotUnq = tbl_out$plotUnq,
#                            elevation = tbl_out$elevation,
#                            resp = tbl_out[,which(colnames(tbl_out) == x)],
#                            prdct = prdct)
#     # colnames(prdct_df)[which(colnames(prdct_df) == "prdct")] <- paste0(j, "_", "run_", i)
#     colnames(prdct_df) <- c("plotID", "plotUnq", "elevation", x, paste0("prd_", x, "_", "run_", k))
#     return(prdct_df = prdct_df)
#   })

#   return(list(obs_smmry = obs_smmry))
# })
resp_loop_bind <- do.call(rbind, resp_loop)
# # resp_loop <- resp_loop[which(rownames(resp_loop) %in% nm_resp),]
# moths5runs <- resp_loop_bind[which(resp_loop_bind[,1] == "SRmoths"),]
# mod <- moths5runs$resp_pls_elevsq_cv_prdct[[5]]$mod_pls_elev_cv
# prdct <- moths5runs$resp_pls_elevsq_cv_prdct[[5]]$prdct_pls_elev_cv
# # xyz <- do.call(rbind, resp_loop[1,2])
# ##for SRmoths


saveRDS(resp_loop_bind, file = paste0(inpath, "resp_loop_bind.rds"))

###pls_elevsq_cv_prdct dataframe
pls_elevsq_cv_df_mrg_lst <- lapply(seq(nrow(resp_loop_bind)), function(k){
  prdct_all_runs <- lapply(seq(outs_lst), function(m){
    prdct <- resp_loop_bind[k,]$resp_pls_elevsq_cv_prdct[[m]]$prdct_pls_elev_cv
    plotID <- resp_loop_bind[k,]$resp_pls_elevsq_cv_prdct[[m]]$plotID
    plotUnq <-   resp_loop_bind[k,]$resp_pls_elevsq_cv_prdct[[m]]$plotUnq
    prdct_df <- data.frame(plotID, plotUnq, prdct)
    colnames(prdct_df) <- c("plotID", "plotUnq", paste0("prd_", resp_loop_bind[k,]$resp, "_run_", m))
    return(prdct_df)
  })
  pls_elevsq_cv_df_mrg <- Reduce(function(x, y) merge(x, y, by="plotUnq", all = T), prdct_all_runs)
  pls_elevsq_cv_df_mrg <- pls_elevsq_cv_df_mrg[, !grepl("\\.", colnames(pls_elevsq_cv_df_mrg))]
  return(pls_elevsq_cv_df_mrg)
})

pls_elevsq_prdct_cv_df <- Reduce(function(x, y) merge(x, y, by="plotUnq"), pls_elevsq_cv_df_mrg_lst)
pls_elevsq_prdct_cv_df <- pls_elevsq_prdct_cv_df[, !grepl("\\.", colnames(pls_elevsq_prdct_cv_df))]
pls_elevsq_prdct_cv_df <- data.frame(plotID = substr(pls_elevsq_prdct_cv_df$plotUnq, 1, 4), 
                                     plotUnq = pls_elevsq_prdct_cv_df$plotUnq, 
                                     #elevation = pls_elevsq_prdct_cv_df$elevation, 
                                     #pls_elevsq_prdct_cv_df[,grepl("run", colnames(pls_elevsq_prdct_cv_df))]) 
                                     pls_elevsq_prdct_cv_df[,-(which(colnames(pls_elevsq_prdct_cv_df) == "plotUnq"))])
pls_elevsq_prdct_cv_df <- pls_elevsq_prdct_cv_df[,colSums(is.na(pls_elevsq_prdct_cv_df)) < nrow(pls_elevsq_prdct_cv_df)]
pls_elevsq_prdct_cv_df <- pls_elevsq_prdct_cv_df[,-which(grepl("resid", colnames(pls_elevsq_prdct_cv_df)))]
pls_elevsq_rlvnt <- colnames(pls_elevsq_prdct_cv_df)[grepl("run", colnames(pls_elevsq_prdct_cv_df))]
pls_elevsq_rlvnt_Unq <- unique(substr(pls_elevsq_rlvnt, 5, nchar(pls_elevsq_rlvnt)-6))
pls_elevsq_prdct_cv_df <- pls_elevsq_prdct_cv_df[,c(which(colnames(pls_elevsq_prdct_cv_df) =="plotID"), 
                                                    which(colnames(pls_elevsq_prdct_cv_df) =="plotUnq"), 
                                                    which(grepl("run", colnames(pls_elevsq_prdct_cv_df)))#, 
                                                    #which(colnames(pls_elevsq_prdct_cv_df) %in% pls_elevsq_rlvnt_Unq)
)]
saveRDS(object = pls_elevsq_prdct_cv_df, file = paste0(outpath, "pls_elevsq_prdct_cv_df.rds"))


obs_smmry_loop <- lapply(seq(nrow(resp_loop_bind)), function(k){
  obs_all_runs <- lapply(seq(outs_lst), function(m){
    obs_smmry <- resp_loop_bind[k,]$resp_pls_elevsq_cv_prdct[[m]]$obs_smmry

  })})
obs_smmry <- do.call(rbind,obs_smmry_loop)
obs_smmry <- do.call(rbind, obs_smmry)
obs_smmry <- obs_smmry[c(1:length(resp_loop)),]
########replace##############################################################################################################jac with _jac_...
# for (i in models){
#pdf(file = paste0(outpath, "plot_ffs.pdf")) ###only uncomment with plot argument and dev.off argument
prediction_rep <- lapply(models, function(i){
  print(i)
  nm <- strsplit(x = i, split = "_|\\.")
  resp_as_nm <- nm[[1]][length(nm[[1]])-1]####################sollte auf dauer anders (relativ) angegeben werden!
  run <- nm[[1]][length(nm[[1]])-4]
  run_indx <- as.numeric(gsub("[[:alpha:]]", "", run))
  
  resp <- gsub("jne", "_jne_", resp_as_nm) #####################sollte auf dauer in ein gsub statement zsuammengefasst werden
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
  
  err_hnd_files <- c()
    err_hnd <- models[grepl(paste0("_", resp_as_nm), models)]
    if(length(err_hnd) != length(outs_lst)){
      file_miss <- c()
      for (j in names(outs_lst)){
        file_ok <- err_hnd[grepl(j, err_hnd)]
        if (length(file_ok) == 0){
          file_miss <- c(file_miss, paste0(i, "_", j))
        }
      }
      err_hnd_files <- c(err_hnd_files, file_miss)
    }


  ################################################################################
  #load single models
  mod <- get(load(file = i))
  
  ###hier var imp und eventuell anderes für heatmap berechnen!
  varimp <- varImp(mod)$importance
  selvars <- mod$selectedvars
  nmbr_selvars <- length(selvars)
  selvars_perf <- mod$selectedvars_perf
  selvars_perf_SE <- mod$selectedvars_perf_SE
  perf_all <- mod$perf_all
  
  out_plt <- outs_lst[[run_indx]]$plotID 
  
  tbl_in <- tbl_scl[-which(tbl_scl$plotID %in% out_plt),]
  tbl_out <- tbl_scl[which(tbl_scl$plotID %in% out_plt),]
  
  ###prediction of SR and resid data
  # new_df <- tbl_out[, c(which(colnames(tbl_scl) == "plotID"),
  #                     which(colnames(tbl_scl) == resp),
  #                     which(colnames(tbl_scl) %in% colnames(mod$trainingData)))]
  # if (nrow(new_df) < length(outs)){ #####wie kann das nötig werden= wo passeirt das in skript 1?
  #   new_df_outs <- data.frame(plotID = outs)
  #   new_df <- merge(new_df, new_df_outs, by = "plotID", all = T)
  # }
  ###colnames(new_df)[] <- resp#########################################sollte auf dauer geÃ¤ndert werden???wofÃ¼r ist das Ã¼berhaupt? Nur fÃ¼r einige nÃ¤tig? bei SRmammals ist es das eh schon
  ##predict with pls normal model!
  prediction <- predict(mod, newdata = tbl_out)
  prdct <- data.frame(plotID = out_plt, 
                      plotUnq = outs_lst[[run_indx]]$plotUnq, 
                      run = run, 
                      resp = prediction)
  stats <- postResample(prediction, tbl_out[[resp]])
  
  # ###prediction of Sr and resid data on all plots
  # new_df_all <- df_scl
  # colnames(new_df_all)[5] <- resp
  # prediction_all <- predict(mod, newdata = new_df_all)
  # prdct_all <- data.frame(plotID = df_scl$plotID,
  #                         plotUnq =  df_scl$plotUnq,
  #                         run = run,
  #                         resp = prediction_all)


  return(list(name = resp, 
              nameUnq = paste0(resp, "_", run), 
              stats_pls_cv = stats, 
              varimp = varimp, 
              selvars = selvars, 
              nmbr_selvars = nmbr_selvars, 
              selvars_perf = selvars_perf, 
              selvars_perf_SE = selvars_perf_SE, 
              perf_all = perf_all, 
              # gam_prdct = gam_prdct, 
              prdct = prdct, 
              # prdct_all = prdct_all, 
              # gam_cv_prdct = gam_cv_prdct,
              # pls_elevsq_cv_prdct = pls_elevsq_cv_prdct, 
              err_hnd_files = err_hnd_files
              ))

})
stats_lst <- do.call(rbind, prediction_rep)

###########################################
#####variable importance df per response
###########################################
stats <- data.frame()
varimp_lst <- list()
nmbrs_selvars_lst <- list()
err_hand_lst <- list()
for (x in (seq(nrow(stats_lst)))){
  resp <- stats_lst[x,]$name
  respUnq <- stats_lst[x,]$nameUnq
  nmbrs <- data.frame(t(stats_lst[x,]$stats))
  varimp <- stats_lst[x,]$varimp
  err_hand <- stats_lst[x,]$err_hnd_files
  tmp_stats <- data.frame(resp = resp, 
                          respUnq = respUnq, 
                          RMSE = nmbrs$RMSE, 
                          Rsquared = nmbrs$Rsquared, 
                          MAE = nmbrs$MAE)
  #summarys
  stats <- rbind(stats, tmp_stats)

  varimp_lst[[resp]] <- varimp
  nmbrs_selvars_lst[[resp]] <- stats_lst[x,]$nmbr_selvars
  err_hand_lst[[resp]] <- err_hand
}

nmbrs_selvar <- as.data.frame(do.call(rbind,nmbrs_selvars_lst))

saveRDS(varimp_lst, file = paste0(outpath, "varimp_lst.rds"))
#varimp_lst <- readRDS(file = paste0(inpath, "varimp_lst.rds"))

saveRDS(nmbrs_selvar, file = paste0(outpath, "nmbrs_selvar.rds"))

###summarise error handling files
saveRDS(err_hand_lst, file = paste0(inpath, "err_handling_files.rds"))

#########################################
#####other statistical values (not from within postResample) # sollte vll verallgemeinert werden für alle statistischen dataframes
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

####prediction df
prdct_df <- data.frame(plotID = mrg_tbl$plotID, plotUnq = mrg_tbl$plotUnq)
for (x in (seq(nrow(stats_lst)))){
  resp <- paste0(stats_lst[x,]$name, "_", unique(stats_lst[x,]$prdct$run))
  tmp_prdct <- stats_lst[x,]$prdct
  colnames(tmp_prdct)[colnames(tmp_prdct) == "resp"] <- resp
  tmp_prdct <- tmp_prdct[,-which(colnames(tmp_prdct) == "run")]

  prdct_df <- merge(prdct_df, tmp_prdct[,c(2:3)], by = "plotUnq", all = T)
}
saveRDS(object = prdct_df, file = paste0(outpath, "prdct_df.rds"))


# ####prediction df_all
# prdct_df_all <- data.frame(plotID = mrg_tbl$plotID, plotUnq = mrg_tbl$plotUnq)
# for (x in (seq(nrow(stats_lst)))){
#   resp <- paste0(stats_lst[x,]$name, "_", unique(stats_lst[x,]$prdct_all$run))
#   tmp_prdct_all <- stats_lst[x,]$prdct_all
#   colnames(tmp_prdct_all)[colnames(tmp_prdct_all) == "resp"] <- resp
#   tmp_prdct_all <- tmp_prdct_all[,-which(colnames(tmp_prdct_all) == "run")]
#   
#   prdct_df_all <- merge(prdct_df_all, tmp_prdct_all[,c(2:3)], by = "plotUnq", all = T)
#   #prdct_df <- prdct_df[, !grepl("\\.", colnames(prdct_df))]
#   #prdct_df <- prdct_df[, !grepl("run\\.", colnames(prdct_df))]
# }
# 
# saveRDS(object = prdct_df_all, file = paste0(outpath, "prdct_df_all.rds"))



# ####add gam and predicted resid
# prdct_res <- prdct_df[,c(which(colnames(prdct_df) == "plotID"), 
#                          which(colnames(prdct_df) == "plotUnq"),
#                          which(grepl("resid", colnames(prdct_df)) & 
#                                  !grepl("NMDS", colnames(prdct_df))))]
# gam_resid <- data.frame(plotID = gam_prdct_df$plotID, plotUnq = gam_prdct_df$plotUnq)
# ##############################################hier muss wg _run zusatz grepl rein
# for(i in colnames(prdct_res)[3:ncol(prdct_res)]){
#   #match gam (of SR) to predicted resids - and add
#   match <- substr(i,6, nchar(i)-5)
#   gam_resid$tmp <- gam_prdct_df[,match] + prdct_res[,i]
#   colnames(gam_resid)[which(colnames(gam_resid) == "tmp")] <- i
# }
# 
# saveRDS(object = gam_resid, file = paste0(outpath, "gam_resid.rds"))




