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


#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "sep18/2018-09-08_ffs_pls_cv_noForest_alpha_all_RMSE/"
datpath <- paste0("../data/")
inpath <- paste0("../data/", sub)
outpath <- paste0("../out/", sub)
if (file.exists(outpath)==F){
  dir.create(file.path(outpath), recursive = T)
}

##load files
tbl <- get(load(paste0(inpath, "../dat_ldr_mrg.RData")))
# nm_resp <- get(load(paste0(inpath, "nm_resp.RData")))
obs_smmry <- readRDS(paste0(inpath, "obs_smmry.rds"))
stats <- get(load(paste0(inpath, "stats.RData")))
stats_comb_all <- stats[, which(colnames(stats)%in% c("resp", "meanR2", "medianR2", "sdR2", 
                                             "meanRMSE", "medianRMSE", "sdRMSE"))]
stats_comb <- unique(stats_comb_all)

####subset of relevant plots
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
    cat <- c("fer", "flm", "foc", "fod", "fpd", "fpo", "hel")
  }else if (frst == F){
    cat <- c("cof", "gra", "hom", "mai", "sav")
  }
  tbl <- tbl[which(tbl$cat %in% cat),]
}


df_resp <- tbl[,which(colnames(tbl) %in% stats_comb$resp)] 
n_max <- as.numeric(nrow(df_resp))

obs_nmbr <- data.frame(nrow(df_resp) - colSums(is.na(df_resp)))
colnames(obs_nmbr) <- paste0("nmbr_obs_of_", n_max)
obs_nmbr$resp <- rownames(obs_nmbr)

stats_Na <- merge(stats_comb, obs_nmbr, by = "resp")
summary(lm(stats_Na$meanR2 ~ stats_Na$nmbr_obs_of_28))

pdf(file = paste0(outpath, "R2_gg_nmb_obs.pdf"), width = 10, height = 7)
plot(stats_Na$meanR2, stats_Na[,ncol(stats_Na)])
dev.off()
