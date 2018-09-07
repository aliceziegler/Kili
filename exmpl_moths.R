####moths example
rm(list=ls())
#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
mrg_tbl <- get(load("../data/aug18/dat_ldr_mrg.RData"))
obs_smmry <- readRDS(file = paste0("../data/aug18/2018-09-01_ffs_pls_cv_noForest_alpha_all/obs_smmry.rds"))
###packages
library(caret)
###gam_prdct_cv_df nicht verwendet bisher - noch in boxplot einbauen!!
###andere Tiere als variable einbauen!?
##############################
### non forest
##############################
path_nofrst <- "../out/aug18/2018-09-01_ffs_pls_cv_noForest_alpha_all/"

##read data
gam_resid <- readRDS(file = paste0(path_nofrst, "gam_resid.rds"))
gam_prdct_cv_df <- readRDS(file = paste0(path_nofrst, "gam_prdct_cv_df.rds"))
gam_prdct_df <- readRDS(paste0(path_nofrst, "gam_prdct_df.rds"))
prdct_df <- readRDS(file = paste0(path_nofrst, "prdct_df.rds"))
prdct_df_all <- readRDS(file = paste0(path_nofrst, "prdct_df_all.rds"))

##extract for moths
##obs SR (forest and non-forest)
obs_SRmoths <- mrg_tbl[,c(which(colnames(mrg_tbl) %in% c("plotID", "plotUnq")), 
                           which(grepl("^SRmoths", colnames(mrg_tbl))))]
##obs resid (forest and non_forest)
obs_residmoths <- mrg_tbl[,c(which(colnames(mrg_tbl) %in% c("plotID", "plotUnq")), 
                             which(grepl("residSRmoths", colnames(mrg_tbl))))]

##pls SR
pls_SRmoths <- prdct_df[,c(which(colnames(prdct_df) %in% c("plotID", "plotUnq")), 
                         which(grepl("^SRmoths", colnames(prdct_df))))]

##pls resid
pls_residmoths <- prdct_df[,c(which(colnames(prdct_df) %in% c("plotID", "plotUnq")), 
                              which(grepl("residSRmoths", colnames(prdct_df))))]

## pls SR all (no cv) but only non-forest
pls_SRmoths_all <- prdct_df_all[,c(which(colnames(prdct_df_all) %in% c("plotID", "plotUnq")), 
                           which(grepl("^SRmoths", colnames(prdct_df_all))))]

## pls resid all (no_cv) - but only non-forest
pls_residmoths_all <- prdct_df_all[,c(which(colnames(prdct_df_all) %in% c("plotID", "plotUnq")), 
                              which(grepl("residSRmoths", colnames(prdct_df_all))))]                              
##gam cv 
gam_cv_SRmoths <- gam_prdct_cv_df[, c(which(colnames(gam_prdct_cv_df) %in% c("plotID", "plotUnq")), 
                                      which(grepl("SRmoths", colnames(gam_prdct_cv_df))))]

##gam SR
gam_SRmoths <- gam_prdct_df[,c(which(colnames(gam_prdct_df) %in% c("plotID", "plotUnq")), 
                               which(grepl("SRmoths", colnames(gam_prdct_df))))]

## gam + resid
gam_resid_moths <- gam_resid[,c(which(colnames(gam_resid) %in% c("plotID", "plotUnq")), 
                               which(grepl("SRmoths", colnames(gam_resid))))]

##mean N SR - only calculated from frst/nnon-frst plots
mn_N_SR <- obs_smmry[which(rownames(obs_smmry) == "SRmoths"),] 

##mean N resid - only calculated from frst/nnon-frst plots
mn_N_resid <- obs_smmry[which(rownames(obs_smmry) == "residSRmoths"),]


###Hier geht der richtige Spaß los!


##SR_pls ~ obs
runs <- c("SRmoths_run1","SRmoths_run2", "SRmoths_run3", "SRmoths_run4", "SRmoths_run5")

stats_pls <- lapply(runs, function(i){
  stat <- as.data.frame(t(postResample(pls_SRmoths[,i], obs_SRmoths$SRmoths)))
  RMSE_sd <- stat[["RMSE"]] / mn_N_SR$meanN_perplot
  return(list(RMSE_sd = RMSE_sd, stat = stat))
})
stats_pls <- do.call("rbind", stats_pls)
stats_pls_pst <- do.call("rbind", stats_pls[,"stat"])
stats_pls_pst$type <- "pls"
stats_pls_RMSE_sd <- data.frame(RMSE_sd = do.call("rbind", stats_pls[,"RMSE_sd"]))
stats_pls_RMSE_sd$type <- "pls"

##resid_pls ~ obs
runs_res <- c("residSRmoths_run1","residSRmoths_run2", "residSRmoths_run3", 
              "residSRmoths_run4", "residSRmoths_run5")
stats_res <- lapply(runs_res, function(i){
  stat <- as.data.frame(t(postResample(pls_residmoths[,i], obs_residmoths$residSRmoths)))
  RMSE_sd <- stat[["RMSE"]] / mn_N_resid$meanN_perplot
  return(list(RMSE_sd = RMSE_sd, stat = stat))
})
stats_res <- do.call("rbind", stats_res)
stats_res_pst <- do.call("rbind", stats_res[,"stat"])
stats_res_pst$type <- "res"
stats_res_RMSE_sd <- data.frame(RMSE_sd = do.call("rbind", stats_res[,"RMSE_sd"]))
stats_res_RMSE_sd$type <- "res"

##gam_cv ~ obs
runs_gam_cv <- c("prd_SRmoths_run_1","prd_SRmoths_run_2", "prd_SRmoths_run_3", 
                 "prd_SRmoths_run_4", "prd_SRmoths_run_5")
stats_gam_cv <- lapply(runs_gam_cv, function(i){
  stat <- as.data.frame(t(postResample(gam_cv_SRmoths[,which(colnames(gam_cv_SRmoths) == i)], 
                                       obs_SRmoths[which(obs_SRmoths$plotID %in% gam_cv_SRmoths$plotID),
                                                   grepl(substr(i,5, (nchar(i)-6)), colnames(obs_SRmoths))])))
  RMSE_sd <- stat[["RMSE"]] / mn_N_SR$meanN_perplot
  return(list(RMSE_sd = RMSE_sd, stat = stat))
})
stats_gam_cv <- do.call("rbind", stats_gam_cv)
stats_gam_cv_pst <- do.call("rbind", stats_gam_cv[,"stat"])
stats_gam_cv_pst$type <- "gam_cv"
stats_gam_cv_RMSE_sd <- data.frame(RMSE_sd = do.call("rbind", stats_gam_cv[,"RMSE_sd"]))
stats_gam_cv_RMSE_sd$type <- "gam_cv"

##gam ~ obs
stats_gam_pst <- as.data.frame(t(postResample(gam_SRmoths$SRmoths, obs_SRmoths$SRmoths)))
stats_gam_RMSE_sd <- data.frame(RMSE_sd = stats_gam_pst[["RMSE"]] / mn_N_SR$meanN_perplot)
stats_gam_pst$type <- "gam"
stats_gam_RMSE_sd$type <- "gam"

##gam_res
runs_gam_res <- c("residSRmoths_run1","residSRmoths_run2", "residSRmoths_run3", 
          "residSRmoths_run4", "residSRmoths_run5")
stats_gam_res <- lapply(runs_gam_res, function(i){
  stat <- as.data.frame(t(postResample(gam_resid_moths[,i], obs_SRmoths$SRmoths)))
  RMSE_sd <- stat[["RMSE"]] / mn_N_SR$meanN_perplot
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
  
  
##plot RMSE
ggplot(data = mrg_pst, aes(x=type, y=RMSE)) + 
  geom_boxplot() 

v <- lm(pls_residmoths$residSRmoths_run1 ~ obs_residmoths$residSRmoths)
plot(v)


#plot RMSE normiert auf sd
ggplot(data = mrg_RMSE_sd, aes(x=type, y=RMSE_sd)) + 
  geom_boxplot() 

#plot RMSE normiert auf sd für alle außer Residuen
ggplot(data = mrg_RMSE_sd[which(mrg_RMSE_sd$type != "res"),], aes(x=type, y=RMSE_sd)) + 
  geom_boxplot() 



##plot R2 aller vorhersagen aller runs: nur in forest/non-forest
runs <- c("SRmoths_run1","SRmoths_run2", "SRmoths_run3", "SRmoths_run4", "SRmoths_run5")
pls_all_mrg_df <- pls_SRmoths_all[,which(colnames(pls_SRmoths_all) == "plotUnq") : 
                                    which(colnames(pls_SRmoths_all) == runs[1])]
colnames(pls_all_mrg_df)[3] <- "predicted"

for (i in seq(2,length(runs))){
  colnames(pls_SRmoths_all)[2+i] <- "predicted"
  pls_all_mrg_df <- rbind(pls_all_mrg_df, pls_SRmoths_all[,c((which(colnames(pls_SRmoths_all) == "plotUnq") : 
                                                               which(colnames(pls_SRmoths_all) == "plotID")), (2+i))])
}
pls_obs_df <- merge(pls_all_mrg_df, obs_SRmoths, by = "plotUnq") 
pls_obs_df <- pls_obs_df[which(complete.cases(pls_obs_df$predicted)),]

###statistische Maße über alle runs innerhalb von forest oder non-forest
stats_pls_all <- as.data.frame(t(postResample(pls_obs_df$predicted, pls_obs_df$SRmoths)))
RMSE_sd <- stats_pls_all[["RMSE"]] / mn_N_SR$meanN_perplot

##########################
###forest
#########################

path_frst <- "../out/aug18/2018-08-31_ffs_pls_cv_onlyForest_alpha_all/"

##read data
fr_gam_resid <- readRDS(file = paste0(path_frst, "gam_resid.rds"))
fr_gam_prdct_cv_df <- readRDS(file = paste0(path_frst, "gam_prdct_cv_df.rds"))
fr_gam_prdct_df <- readRDS(paste0(path_frst, "gam_prdct_df.rds"))
fr_prdct_df <- readRDS(file = paste0(path_frst, "prdct_df.rds"))
fr_obs_smmry <- readRDS(file = paste0("../data/aug18/2018-08-31_ffs_pls_cv_onlyForest_alpha_all/obs_smmry.rds"))
fr_prdct_df_all <- readRDS(file = paste0(path_frst, "prdct_df_all.rds"))

##extract for moths
# ##obs SR
# fr_obs_SRmoths <- mrg_tbl[,c(which(colnames(mrg_tbl) %in% c("plotID", "plotUnq")), 
#                           which(grepl("^SRmoths", colnames(mrg_tbl))))]
# ##obs resid
# fr_obs_residmoths <- mrg_tbl[,c(which(colnames(mrg_tbl) %in% c("plotID", "plotUnq")), 
#                              which(grepl("residSRmoths", colnames(mrg_tbl))))]

##pls SR
fr_pls_SRmoths <- fr_prdct_df[,c(which(colnames(fr_prdct_df) %in% c("plotID", "plotUnq")), 
                           which(grepl("^SRmoths", colnames(fr_prdct_df))))]

##pls resid
fr_pls_residmoths <- fr_prdct_df[,c(which(colnames(fr_prdct_df) %in% c("plotID", "plotUnq")), 
                              which(grepl("residSRmoths", colnames(fr_prdct_df))))]

## pls SR all (no cv) but only forest
fr_pls_SRmoths_all <- fr_prdct_df_all[,c(which(colnames(fr_prdct_df_all) %in% c("plotID", "plotUnq")), 
                                   which(grepl("^SRmoths", colnames(fr_prdct_df_all))))]

## pls resid all (no cv) but only forest
fr_pls_residmoths_all <- fr_prdct_df_all[,c(which(colnames(fr_prdct_df_all) %in% c("plotID", "plotUnq")), 
                                      which(grepl("residSRmoths", colnames(fr_prdct_df_all))))]                              

fr_gam_cv_SRmoths <- fr_gam_prdct_cv_df[, c(which(colnames(fr_gam_prdct_cv_df) %in% c("plotID", "plotUnq")), 
                                      which(grepl("SRmoths", colnames(fr_gam_prdct_cv_df))))]

##gam SR
fr_gam_SRmoths <- fr_gam_prdct_df[,c(which(colnames(fr_gam_prdct_df) %in% c("plotID", "plotUnq")), 
                               which(grepl("SRmoths", colnames(fr_gam_prdct_df))))]

## gam + resid
fr_gam_resid_moths <- fr_gam_resid[,c(which(colnames(fr_gam_resid) %in% c("plotID", "plotUnq")), 
                                which(grepl("SRmoths", colnames(fr_gam_resid))))]

##mean N SR - only calculated from frst/nnon-frst plots
fr_mn_N_SR <- fr_obs_smmry[which(rownames(fr_obs_smmry) == "SRmoths"),] 

##mean N resid - only calculated from frst/nnon-frst plots
fr_mn_N_resid <- fr_obs_smmry[which(rownames(fr_obs_smmry) == "residSRmoths"),]


###Hier geht der richtige Spaß los!

runs <- c("SRmoths_run1","SRmoths_run2", "SRmoths_run3", "SRmoths_run4", "SRmoths_run5")

fr_stats_pls <- lapply(runs, function(i){
  stat <- as.data.frame(t(postResample(fr_pls_SRmoths[,i], obs_SRmoths$SRmoths)))
  #RMSE_sd <- stat[["RMSE"]] / mn_N_SR$meanN_perplot
  #return(list(RMSE_sd, stat))
})
fr_stats_pls <- do.call("rbind", fr_stats_pls)
fr_stats_pls$type <- "pls"

fr_runs_gam_cv <- c("prd_SRmoths_run_1","prd_SRmoths_run_2", "prd_SRmoths_run_3", 
                 "prd_SRmoths_run_4", "prd_SRmoths_run_5")
fr_stats_gam_cv <- lapply(fr_runs_gam_cv, function(i){
  stat <- as.data.frame(t(postResample(fr_gam_cv_SRmoths[,which(colnames(fr_gam_cv_SRmoths) == i)], 
                                       obs_SRmoths[which(obs_SRmoths$plotID %in% fr_gam_cv_SRmoths$plotID),
                                                   grepl(substr(i,5, (nchar(i)-6)), colnames(obs_SRmoths))])))
  #RMSE_sd <- stat[["RMSE"]] / mn_N_SR$meanN_perplot
  #return(list(RMSE_sd, stat))
})
fr_stats_gam_cv <- do.call("rbind", fr_stats_gam_cv)
fr_stats_gam_cv$type <- "gam_cv"

fr_stats_gam <- as.data.frame(t(postResample(fr_gam_SRmoths$SRmoths, obs_SRmoths$SRmoths)))
fr_stats_gam$type <- "gam"

runs_gam_res <- c("residSRmoths_run1","residSRmoths_run2", "residSRmoths_run3", 
                  "residSRmoths_run4", "residSRmoths_run5")
fr_stats_gam_res <- lapply(runs_gam_res, function(i){
  stat <- as.data.frame(t(postResample(fr_gam_resid_moths[,i], obs_SRmoths$SRmoths)))
  #RMSE_sd <- stat[["RMSE"]] / mn_N_SR$meanN_perplot
  #return(list(RMSE_sd, stat))
})
fr_stats_gam_res <- do.call("rbind", fr_stats_gam_res)
fr_stats_gam_res$type <- "gam_res"

runs_res <- c("residSRmoths_run1","residSRmoths_run2", "residSRmoths_run3", 
              "residSRmoths_run4", "residSRmoths_run5")
fr_stats_res <- lapply(runs_res, function(i){
  stat <- as.data.frame(t(postResample(fr_pls_residmoths[,i], obs_residmoths$residSRmoths)))
  #RMSE_sd <- stat[["RMSE"]] / mn_N_SR$meanN_perplot
  #return(list(RMSE_sd, stat))
})
fr_stats_res <- do.call("rbind", fr_stats_res)
fr_stats_res$type <- "res"

fr_mrg <- (rbind(fr_stats_res, fr_stats_gam, fr_stats_gam, 
                 fr_stats_gam_res, fr_stats_pls, fr_stats_gam_cv))

##plot RMSE
ggplot(data = fr_mrg, aes(x=type, y=RMSE)) + 
  geom_boxplot() 

v <- lm(fr_pls_residmoths$residSRmoths_run1 ~ obs_residmoths$residSRmoths)
plot(v)


##plot R2 aller vorhersagen aller runs: 
runs <- c("SRmoths_run1","SRmoths_run2", "SRmoths_run3", "SRmoths_run4", "SRmoths_run5")
fr_pls_all_mrg_df <- fr_pls_SRmoths_all[,which(colnames(fr_pls_SRmoths_all) == "plotUnq") : 
                                    which(colnames(fr_pls_SRmoths_all) == runs[1])]
colnames(fr_pls_all_mrg_df)[3] <- "predicted"

for (i in seq(2,length(runs))){
  colnames(fr_pls_SRmoths_all)[2+i] <- "predicted"
  fr_pls_all_mrg_df <- rbind(fr_pls_all_mrg_df, fr_pls_SRmoths_all[,c((which(colnames(fr_pls_SRmoths_all) == "plotUnq") : 
                                                                which(colnames(fr_pls_SRmoths_all) == "plotID")), (2+i))])
}
fr_pls_obs_df <- merge(fr_pls_all_mrg_df, obs_SRmoths, by = "plotUnq") 
fr_pls_obs_df <- fr_pls_obs_df[which(complete.cases(fr_pls_obs_df$predicted)),]

fr_stats_pls_all <- as.data.frame(t(postResample(fr_pls_obs_df$predicted, fr_pls_obs_df$SRmoths)))
fr_RMSE_sd <- fr_stats_pls_all[["RMSE"]] / fr_mn_N_SR$meanN_perplot


##########################
###all
#########################
#mean N SR all (frst und non-frst together)
mean_N_SR_all <-sd(mrg_tbl[,which(colnames(mrg_tbl) == "SRmoths")])

#mean N resid all (frst und non-frst together)
mean_N_resid_all <-sd(mrg_tbl[,which(colnames(mrg_tbl) == "residSRmoths")])
