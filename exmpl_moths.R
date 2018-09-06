####moths example
rm(list=ls())
#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
mrg_tbl <- get(load("../data/aug18/dat_ldr_mrg.RData"))

###packages
library(caret)


##############################
### non forest
##############################
path_nofrst <- "../out/aug18/2018-09-01_ffs_pls_cv_noForest_alpha_all/"

##read data
gam_resid <- readRDS(file = paste0(path_nofrst, "gam_resid.rds"))
gam_prdct_cv_df <- readRDS(file = paste0(path_nofrst, "gam_prdct_cv_df.rds"))
gam_prdct_df <- readRDS(paste0(path_nofrst, "gam_prdct_df.rds"))
prdct_df <- readRDS(file = paste0(path_nofrst, "prdct_df.rds"))
obs_smmry <- readRDS(file = paste0("../data/aug18/2018-09-01_ffs_pls_cv_noForest_alpha_all/obs_smmry.rds"))

##extract for moths
##obs SR
obs_SRmoths <- mrg_tbl[,c(which(colnames(mrg_tbl) %in% c("plotID", "plotUnq")), 
                           which(grepl("^SRmoths", colnames(mrg_tbl))))]
##obs resid
obs_residmoths <- mrg_tbl[,c(which(colnames(mrg_tbl) %in% c("plotID", "plotUnq")), 
                             which(grepl("residSRmoths", colnames(mrg_tbl))))]

##pls SR
pls_SRmoths <- prdct_df[,c(which(colnames(prdct_df) %in% c("plotID", "plotUnq")), 
                         which(grepl("^SRmoths", colnames(prdct_df))))]

##pls resid
pls_residmoths <- prdct_df[,c(which(colnames(prdct_df) %in% c("plotID", "plotUnq")), 
                              which(grepl("residSRmoths", colnames(prdct_df))))]

##gam SR
gam_SRmoths <- gam_prdct_df[,c(which(colnames(gam_prdct_df) %in% c("plotID", "plotUnq")), 
                               which(grepl("SRmoths", colnames(gam_prdct_df))))]

## gam + resid
gam_resid_moths <- gam_resid[,c(which(colnames(gam_resid) %in% c("plotID", "plotUnq")), 
                               which(grepl("SRmoths", colnames(gam_resid))))]

##mean N SR
mn_N_SR <- obs_smmry[which(rownames(obs_smmry) == "SRmoths"),]

##mean N resid
mn_N_resid <- obs_smmry[which(rownames(obs_smmry) == "residSRmoths"),]


###Hier geht der richtige Spaß los!
runs <- c("SRmoths_run1","SRmoths_run2", "SRmoths_run3", "SRmoths_run4", "SRmoths_run5")

stats_pls <- lapply(runs, function(i){
  stat <- as.data.frame(t(postResample(pls_SRmoths[,i], obs_SRmoths$SRmoths)))
  #RMSE_sd <- stat[["RMSE"]] / mn_N_SR$meanN_perplot
  #return(list(RMSE_sd, stat))
})
stats_pls <- do.call("rbind", stats_pls)
stats_pls$type <- "pls"

runs_gam_res <- c("residSRmoths_run1","residSRmoths_run2", "residSRmoths_run3", 
          "residSRmoths_run4", "residSRmoths_run5")
stats_gam_res <- lapply(runs_gam_res, function(i){
  stat <- as.data.frame(t(postResample(gam_resid_moths[,i], obs_SRmoths$SRmoths)))
  #RMSE_sd <- stat[["RMSE"]] / mn_N_SR$meanN_perplot
  #return(list(RMSE_sd, stat))
})
stats_gam_res <- do.call("rbind", stats_gam_res)
stats_gam_res$type <- "gam_res"

stats_gam <- as.data.frame(t(postResample(gam_SRmoths$SRmoths, obs_SRmoths$SRmoths)))
stats_gam$type <- "gam"

runs_res <- c("residSRmoths_run1","residSRmoths_run2", "residSRmoths_run3", 
                  "residSRmoths_run4", "residSRmoths_run5")
stats_res <- lapply(runs_res, function(i){
  stat <- as.data.frame(t(postResample(pls_residmoths[,i], obs_residmoths$residSRmoths)))
  #RMSE_sd <- stat[["RMSE"]] / mn_N_SR$meanN_perplot
  #return(list(RMSE_sd, stat))
})
stats_res <- do.call("rbind", stats_res)
stats_res$type <- "res"

mrg <- (rbind(stats_res, stats_gam, stats_gam, stats_gam_res, stats_pls))

ggplot(data = mrg, aes(x=type, y=RMSE)) + 
  geom_boxplot() 
  #theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) + 
  #fillscl+ 
  #ggtitle(title)



 v <- lm(pls_residmoths$residSRmoths_run1 ~ obs_residmoths$residSRmoths)

 plot(v)
 
 