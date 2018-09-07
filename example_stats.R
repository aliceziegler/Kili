rm(list=ls())
#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))

#2018-08-31_ffs_pls_cv_onlyForest_alpha_all
#2018-09-01_ffs_pls_cv_noForest_alpha_all

path_nofrst <- "../out/aug18/2018-09-01_ffs_pls_cv_noForest_alpha_all/"

mrg_tbl <- get(load("../data/aug18/dat_ldr_mrg.RData"))
obs_smmry <- readRDS(file = paste0("../data/aug18/2018-09-01_ffs_pls_cv_noForest_alpha_all/obs_smmry.rds"))
###packages
library(caret)

resp_vec <- c("SRcollembola")


##read data
gam_resid <- readRDS(file = paste0(path_nofrst, "gam_resid.rds"))
gam_prdct_cv_df <- readRDS(file = paste0(path_nofrst, "gam_prdct_cv_df.rds"))
gam_prdct_df <- readRDS(paste0(path_nofrst, "gam_prdct_df.rds"))
prdct_df <- readRDS(file = paste0(path_nofrst, "prdct_df.rds"))
prdct_df_all <- readRDS(file = paste0(path_nofrst, "prdct_df_all.rds"))

for (resp in resp_vec){
  
  ##crop data on resp
  ##obs SR (forest and non-forest)
  obs_SRresp <- mrg_tbl[,c(which(colnames(mrg_tbl) %in% c("plotID", "plotUnq")), 
                            which(grepl(paste0("^", resp), colnames(mrg_tbl))))]
  ##obs resid (forest and non_forest)
  obs_residresp <- mrg_tbl[,c(which(colnames(mrg_tbl) %in% c("plotID", "plotUnq")), 
                               which(grepl(paste0("resid", resp), colnames(mrg_tbl))))]
  
  ##pls SR
  pls_SRresp <- prdct_df[,c(which(colnames(prdct_df) %in% c("plotID", "plotUnq")), 
                             which(grepl(paste0("^", resp), colnames(prdct_df))))]
  
  ##pls resid
  pls_residresp <- prdct_df[,c(which(colnames(prdct_df) %in% c("plotID", "plotUnq")), 
                                which(grepl(paste0("resid", resp), colnames(prdct_df))))]
  
  ## pls SR all (no cv) but only non-forest
  pls_SRresp_all <- prdct_df_all[,c(which(colnames(prdct_df_all) %in% c("plotID", "plotUnq")), 
                                     which(grepl(paste0("^", resp), colnames(prdct_df_all))))]
  
  ## pls resid all (no_cv) - but only non-forest
  pls_residresp_all <- prdct_df_all[,c(which(colnames(prdct_df_all) %in% c("plotID", "plotUnq")), 
                                        which(grepl(paste0("resid", resp), colnames(prdct_df_all))))]                              
  ##gam cv 
  gam_cv_SRresp <- gam_prdct_cv_df[, c(which(colnames(gam_prdct_cv_df) %in% c("plotID", "plotUnq")), 
                                        which(grepl(resp, colnames(gam_prdct_cv_df))))]
  
  ##gam SR
  gam_SRresp <- gam_prdct_df[,c(which(colnames(gam_prdct_df) %in% c("plotID", "plotUnq")), 
                                 which(grepl(resp, colnames(gam_prdct_df))))]
  
  ## gam + resid
  gam_resid_resp <- gam_resid[,c(which(colnames(gam_resid) %in% c("plotID", "plotUnq")), 
                                  which(grepl(resp, colnames(gam_resid))))]
  
  ##mean N SR - only calculated from frst/nnon-frst plots
  mn_N_SR <- obs_smmry[which(rownames(obs_smmry) == resp),"meanN_perplot"] 
  
  ##mean N resid - only calculated from frst/nnon-frst plots
  mn_N_resid <- obs_smmry[which(rownames(obs_smmry) == paste0("resid", resp)),"meanN_perplot"]
  
  ##sd SR
  sd_resp_SR <- obs_smmry[which(rownames(obs_smmry) == resp),"sd_per_resp"]
  
  #sdresid
  sd_resp_resid <- obs_smmry[which(rownames(obs_smmry) == paste0("resid", resp)),"sd_per_resp"]
  
  #######################
  ###stats calculations
  #######################
  
  ##SR_pls ~ obs
  runs <- c(paste0(resp, "_run1"), paste0(resp, "_run2"), paste0(resp, "_run3"), 
            paste0(resp, "_run4"), paste0(resp, "_run5"))
  
  stats_pls <- lapply(runs, function(i){
    stat <- as.data.frame(t(postResample(pls_SRresp[,i], obs_SRresp[,resp])))
    RMSE_sd <- stat[["RMSE"]] / sd_resp_SR
    return(list(RMSE_sd = RMSE_sd, stat = stat))
  })
  stats_pls <- do.call("rbind", stats_pls)
  stats_pls_pst <- do.call("rbind", stats_pls[,"stat"])
  stats_pls_pst$type <- "pls"
  stats_pls_RMSE_sd <- data.frame(RMSE_sd = do.call("rbind", stats_pls[,"RMSE_sd"]))
  stats_pls_RMSE_sd$type <- "pls"
  
  ##resid_pls ~ obs
  runs_res <- c(paste0("resid", resp, "_run1"), 
                paste0("resid", resp, "_run2"), paste0("resid", resp, "_run3"), 
                paste0("resid", resp, "_run4"), paste0("resid", resp, "_run5"))
  stats_res <- lapply(runs_res, function(i){
    stat <- as.data.frame(t(postResample(pls_residresp[,i], obs_residresp[,paste0("resid", resp)])))
    RMSE_sd <- stat[["RMSE"]] / sd_resp_resid
    return(list(RMSE_sd = RMSE_sd, stat = stat))
  })
  stats_res <- do.call("rbind", stats_res)
  stats_res_pst <- do.call("rbind", stats_res[,"stat"])
  stats_res_pst$type <- "res"
  stats_res_RMSE_sd <- data.frame(RMSE_sd = do.call("rbind", stats_res[,"RMSE_sd"]))
  stats_res_RMSE_sd$type <- "res"
  
  ##gam_cv ~ obs
  runs_gam_cv <- c(paste0("prd_", resp, "_run_1"), paste0("prd_", resp, "_run_2"), 
                   paste0("prd_", resp, "_run_3"), paste0("prd_", resp, "_run_4"), 
                   paste0("prd_", resp, "_run_5"))
  stats_gam_cv <- lapply(runs_gam_cv, function(i){
    stat <- as.data.frame(t(postResample(gam_cv_SRresp[,which(colnames(gam_cv_SRresp) == i)], 
                                         obs_SRresp[which(obs_SRresp$plotID %in% gam_cv_SRresp$plotID),
                                                     grepl(substr(i,5, (nchar(i)-6)), colnames(obs_SRresp))])))
    
    df <- data.frame(pred = gam_cv_SRresp[,which(colnames(gam_cv_SRresp) == i)], obs = obs_SRresp[which(obs_SRresp$plotID %in% gam_cv_SRresp$plotID),
                                                                                                  grepl(substr(i,5, (nchar(i)-6)), colnames(obs_SRresp))])
    
    RMSE_sd <- stat[["RMSE"]] / sd_resp_SR
    return(list(RMSE_sd = RMSE_sd, stat = stat))
  })
  stats_gam_cv <- do.call("rbind", stats_gam_cv)
  stats_gam_cv_pst <- do.call("rbind", stats_gam_cv[,"stat"])
  stats_gam_cv_pst$type <- "gam_cv"
  stats_gam_cv_RMSE_sd <- data.frame(RMSE_sd = do.call("rbind", stats_gam_cv[,"RMSE_sd"]))
  stats_gam_cv_RMSE_sd$type <- "gam_cv"
  
  ##gam ~ obs
  stats_gam_pst <- as.data.frame(t(postResample(gam_SRresp[,resp], obs_SRresp[,resp])))
  stats_gam_RMSE_sd <- data.frame(RMSE_sd = stats_gam_pst[["RMSE"]] / sd_resp_SR)
  stats_gam_pst$type <- "gam"
  stats_gam_RMSE_sd$type <- "gam"
  
  ##gam_res
  runs_gam_res <- c(paste0("resid", resp, "_run1"), paste0("resid", resp, "_run2"), 
                    paste0("resid", resp, "_run3"), paste0("resid", resp, "_run4"), 
                    paste0("resid", resp, "_run5"))
  stats_gam_res <- lapply(runs_gam_res, function(i){
    stat <- as.data.frame(t(postResample(gam_resid_resp[,i], obs_SRresp[,resp])))
    RMSE_sd <- stat[["RMSE"]] / sd_resp_SR
    return(list(RMSE_sd = RMSE_sd, stat = stat))
  })
  stats_gam_res <- do.call("rbind", stats_gam_res)
  stats_gam_res_pst <- do.call("rbind", stats_gam_res[,"stat"])
  stats_gam_res_pst$type <- "gam_res"
  stats_gam_res_RMSE_sd <- data.frame(RMSE_sd = do.call("rbind", stats_gam_res[,"RMSE_sd"]))
  stats_gam_res_RMSE_sd$type <- "gam_res"
  
  mrg_pst <- (rbind(stats_res_pst, stats_gam_pst, stats_gam_pst, stats_gam_res_pst, stats_pls_pst, 
                    stats_gam_cv_pst))
  
  mrg_RMSE_sd <- (rbind(stats_res_RMSE_sd, stats_gam_RMSE_sd, stats_gam_RMSE_sd, stats_gam_res_RMSE_sd, stats_pls_RMSE_sd, 
                        stats_gam_cv_RMSE_sd))
  
  #pdf(file = paste0(path_nofrst, "stats_", resp, ".pdf"))
  ##plot RMSE
  print(ggplot(data = mrg_pst, aes(x=type, y=RMSE)) + 
    geom_boxplot() + 
    ggtitle(resp))
  
  #v <- lm(pls_residresp[,paste0("resid", resp, "_run1")] ~ obs_residresp[,paste0("resid", resp)])
  #plot(v)
  
  #plot RMSE normiert auf sd
  print(ggplot(data = mrg_RMSE_sd, aes(x=type, y=RMSE_sd)) + 
    geom_boxplot()+ 
    ggtitle(resp))
  
  #plot RMSE normiert auf sd für alle außer Residuen
  print(ggplot(data = mrg_RMSE_sd[which(mrg_RMSE_sd$type != "res"),], aes(x=type, y=RMSE_sd)) + 
    geom_boxplot()+ 
    ggtitle(resp))
  #dev.off()
  
  ##plot R2 aller vorhersagen aller runs: nur in forest/non-forest
  runs <- c(paste0(resp, "_run1"), paste0(resp, "_run2"), paste0(resp, "_run3"), 
            paste0(resp, "_run4"), paste0(resp, "_run5"))
  pls_all_mrg_df <- pls_SRresp_all[,which(colnames(pls_SRresp_all) == "plotUnq") : 
                                      which(colnames(pls_SRresp_all) == runs[1])]
  colnames(pls_all_mrg_df)[3] <- "predicted"
  
  for (i in seq(2,length(runs))){
    colnames(pls_SRresp_all)[2+i] <- "predicted"
    pls_all_mrg_df <- rbind(pls_all_mrg_df, pls_SRresp_all[,c((which(colnames(pls_SRresp_all) == "plotUnq") : 
                                                                  which(colnames(pls_SRresp_all) == "plotID")), (2+i))])
  }
  pls_obs_df <- merge(pls_all_mrg_df, obs_SRresp, by = "plotUnq") 
  pls_obs_df <- pls_obs_df[which(complete.cases(pls_obs_df$predicted)),]
  
  ###statistische Maße über alle runs innerhalb von forest oder non-forest
  stats_pls_all <- as.data.frame(t(postResample(pls_obs_df$predicted, pls_obs_df[,resp])))
  RMSE_sd <- stats_pls_all[["RMSE"]] / sd_resp_SR
  
  
}


