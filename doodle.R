# library(mgcv)
# 
# 
# i <- "../data/aug18/2018-08-28_ffs_pls_cv_onlyForest_alpha_all/indv_model_run1_ffs_pls_SRbees.RData"
# resp <- "SRbees"
# 
# colnames(mrg_tbl)
# 
# gam_prdct <- data.frame(plotID = mrg_tbl$plotID)
#  if ((grepl("resid", resp) | grepl("sum", resp) | grepl("NMDS", resp)) == F){
#    dat <- data.frame("elevation"= mrg_tbl$elevation,
#                      "response"=mrg_tbl[,grepl(paste0("^", resp, "$"), colnames(mrg_tbl))])
#    mod_gam <- gam(response ~ s(elevation),data=dat)
#    newdat <- data.frame("elevation"= mrg_tbl$elevation)
#    prdct <- predict(object = mod_gam, newdata =  newdat)
#    gam_prdct <- data.frame(gam_prdct, resp = prdct)
#    colnames(gam_prdct)[colnames(gam_prdct) == "resp"] <- paste0(resp)}
#    

   
   
#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "aug18/2018-08-31_ffs_pls_cv_onlyForest_alpha_all/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../out/", sub)
if (file.exists(outpath)==F){
  dir.create(file.path(outpath), recursive = T)
}

library(caret)

gam_prdct_df <- readRDS(paste0(inpath, "gam_prdct_df.rds"))
mrg_tbl <- get(load(paste0(inpath, "../dat_ldr_mrg.RData")))   
prdct_df <- readRDS(file = paste0(inpath, "prdct_df.rds"))


# res_tbl <- mrg_tbl[,c(which(colnames(mrg_tbl) == "plotID"), 
#                       which(colnames(mrg_tbl) == "plotUnq"), 
#                       which(grepl("resid", colnames(mrg_tbl)) & !grepl("NMDS", colnames(mrg_tbl))))]
# colnames(res_tbl) <- gsub("resid*", "", colnames(res_tbl))
 
prdct_res <- prdct_df[,c(which(colnames(prdct_df) == "plotID"), 
                 which(colnames(prdct_df) == "plotUnq"),
                 which(grepl("resid", colnames(prdct_df)) & 
                         !grepl("NMDS", colnames(prdct_df))))]


###add gamm to predicted resids
gam_resid <- data.frame(plotID = gam_prdct_df$plotID, plotUnq = gam_prdct_df$plotUnq)
###############################################################################hier muss wg _run zusatz grepl rein
for(i in colnames(prdct_res)[3:ncol(prdct_res)]){
  #match gam (of SR) to predicted resids - and add
  match <- substr(i,6, nchar(i)-5)
  gam_resid$tmp <- gam_prdct_df[,match] + prdct_res[,i]
  colnames(gam_resid)[which(colnames(gam_resid) == "tmp")] <- i
}
  
#   if(i %in% colnames(gam_prdct_df) & i %in% colnames(res_tbl)){
#     gam_resid$tmp <- gam_prdct_df[,i]+res_tbl[,i]
#     colnames(gam_resid)[which(colnames(gam_resid) == "tmp")] <- i
#   }
# }
# # i <- 3


# gam_resid <- data.frame(plotID = gam_prdct_df$plotID, plotUnq = gam_prdct_df$plotUnq)
# for(i in colnames(res_tbl)[3:ncol(res_tbl)]){
#   if(i %in% colnames(gam_prdct_df) & i %in% colnames(res_tbl)){
#   gam_resid$tmp <- gam_prdct_df[,i]+res_tbl[,i]
#   colnames(gam_resid)[which(colnames(gam_resid) == "tmp")] <- i
#   }
# }

SR_tbl <- mrg_tbl[, which(colnames(mrg_tbl) %in% colnames(gam_prdct_df))]

###R2 gamm to original Data SR
stats_gam <- data.frame(resp = colnames(gam_prdct_df)[3:ncol(gam_prdct_df)], R2 = NA, RMSE = NA)
for (i in colnames(gam_prdct_df)[3:ncol(gam_prdct_df)]){
  stat <- postResample(gam_prdct_df[,which(colnames(gam_prdct_df) == i)], 
                       SR_tbl[,which(colnames(SR_tbl) == i)])
  stats_gam[which(stats_gam$resp == i), which(colnames(stats_gam) == "R2")] <- stat[2]
  stats_gam[which(stats_gam$resp == i), which(colnames(stats_gam) == "RMSE")] <- stat[1]  
  }
saveRDS(object = stats_gam, file = paste0(inpath, "stats_gam.rds"))


####R2 gam + predicted resid to original Data SR
stats_gam_resid <- data.frame(resp_resid = colnames(gam_resid)[3:ncol(gam_resid)], R2 = NA, RMSE = NA)
for (i in colnames(gam_resid)[3:ncol(gam_resid)]){
  match <- substr(i,6, nchar(i)-5)
  stat <- postResample(gam_resid[,which(colnames(gam_resid) == i)], 
                       SR_tbl[,which(colnames(SR_tbl) == match)])
  stats_gam_resid[which(stats_gam_resid$resp_resid == i), which(colnames(stats_gam_resid) == "R2")] <- stat[2]
  stats_gam_resid[which(stats_gam_resid$resp_resid == i), which(colnames(stats_gam_resid) == "RMSE")] <- stat[1]
  stats_gam_resid$resp[which(stats_gam_resid$resp_resid == i)] <- 
    substr(stats_gam_resid$resp_resid[which(stats_gam_resid$resp_resid == i)], 
           6, 
           nchar(as.character(stats_gam_resid$resp_resid[which(stats_gam_resid$resp_resid == i)]))-5)
}

saveRDS(object = stats_gam_resid, file = paste0(inpath, "stats_gam_resid.rds"))



#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#plotten: 
stats_gam <- readRDS(file = paste0(inpath, "stats_gam.rds"))
stats_gam_resid <- readRDS(file = paste0(inpath, "stats_gam_resid.rds"))
stats <- get(load(paste0(inpath, "stats.RData")))
stats_SR <- stats[which(stats$resp %in% stats_gam$resp), c(which(colnames(stats) %in% c("resp", "respUnq", "Rsquared", "RMSE")))]
colnames(stats_SR)[which(colnames(stats_SR) == "Rsquared")]<- "R2"
stats_SR <- data.frame(stats_SR[which(colnames(stats_SR) %in% c("resp", "respUnq"))],stats_SR[which(colnames(stats_SR) =="R2")], stats_SR[which(colnames(stats_SR) == "RMSE")])
stats_gam_resid$respUnq <- stats_gam_resid$resp
stats_gam_resid$respUnq <- gsub("resid", "", stats_gam_resid$respUnq)
stats_gam_resid <- data.frame(stats_gam_resid[which(colnames(stats_gam_resid) == "resp")], 
                              stats_gam_resid[which(colnames(stats_gam_resid) == "respUnq")], 
                              stats_gam_resid[which(colnames(stats_gam_resid) == "R2")], 
                              stats_gam_resid[which(colnames(stats_gam_resid) == "RMSE")])

###merge stats table by unique resp (with run): 

respUnq_mrg <- rbind(stats_SR[,c(2:4)], stats_gam_resid, by = "respUnq")