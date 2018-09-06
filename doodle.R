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
# sub <- "aug18/2018-08-31_ffs_pls_cv_onlyForest_alpha_all/"
sub <- "aug18/2018-08-31_ffs_pls_cv_onlyForest_alpha_all/"

inpath <- paste0("../data/", sub)
outpath <- paste0("../out/", sub)
if (file.exists(outpath)==F){
  dir.create(file.path(outpath), recursive = T)
}

library(caret)
library(ggplot2)


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
}

gam_prdct_cv_df <- readRDS(file = paste0(outpath, "gam_prdct_cv_df.rds"))
gam_prdct_df <- readRDS(paste0(outpath, "gam_prdct_df.rds"))
mrg_tbl <- get(load(paste0(inpath, "../dat_ldr_mrg.RData")))   
prdct_df <- readRDS(file = paste0(outpath, "prdct_df.rds"))
trophic_tbl <- get(load(paste0(inpath, "../../trophic_tbl.RData")))


# res_tbl <- mrg_tbl[,c(which(colnames(mrg_tbl) == "plotID"), 
#                       which(colnames(mrg_tbl) == "plotUnq"), 
#                       which(grepl("resid", colnames(mrg_tbl)) & !grepl("NMDS", colnames(mrg_tbl))))]
# colnames(res_tbl) <- gsub("resid*", "", colnames(res_tbl))
 
prdct_res <- prdct_df[,c(which(colnames(prdct_df) == "plotID"), 
                 which(colnames(prdct_df) == "plotUnq"),
                 which(grepl("resid", colnames(prdct_df)) & 
                         !grepl("NMDS", colnames(prdct_df))))]


###add gam to predicted resids
gam_resid <- data.frame(plotID = gam_prdct_df$plotID, plotUnq = gam_prdct_df$plotUnq)
##############################################hier muss wg _run zusatz grepl rein
for(i in colnames(prdct_res)[3:ncol(prdct_res)]){
  #match gam (of SR) to predicted resids - and add
  match <- substr(i,6, nchar(i)-5)
  gam_resid$tmp <- gam_prdct_df[,match] + prdct_res[,i]
  colnames(gam_resid)[which(colnames(gam_resid) == "tmp")] <- i
}
  
saveRDS(object = gam_resid, file = paste0(outpath, "gam_resid.rds"))
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

###R2 cv gam to original Data SR
stats_gam_cv <- data.frame(respUnq = colnames(gam_prdct_cv_df)[4:ncol(gam_prdct_cv_df)], R2 = NA, RMSE = NA)
for (i in colnames(gam_prdct_cv_df)[3:ncol(gam_prdct_cv_df)]){
  if (substr(i,5, (nchar(i)-6)) %in% colnames(SR_tbl)){
    stat <- postResample(gam_prdct_cv_df[which(substr(gam_prdct_cv_df$plotID,1, 3) %in% cat),
                                         which(colnames(gam_prdct_cv_df) == i)], 
                         #SR_tbl[,which(colnames(SR_tbl) == i)])
                         SR_tbl[which(substr(SR_tbl$plotID,1, 3) %in% cat), 
                                grepl(substr(i,5, (nchar(i)-6)), colnames(SR_tbl))])
    stats_gam_cv[which(stats_gam_cv$respUnq == i), which(colnames(stats_gam_cv) == "R2")] <- stat[2]
    stats_gam_cv[which(stats_gam_cv$respUnq == i), which(colnames(stats_gam_cv) == "RMSE")] <- stat[1]  
  }else {
    stats_gam_cv[which(stats_gam_cv$respUnq == i), which(colnames(stats_gam_cv) == "R2")] <- NA
    stats_gam_cv[which(stats_gam_cv$respUnq == i), which(colnames(stats_gam_cv) == "RMSE")] <- NA  
  }
  
}
stats_gam_cv$resp <- substr(as.character(stats_gam_cv$respUnq), 
                               5, 
                               (nchar(as.character(stats_gam_cv$respUnq))-6))
stats_gam_cv <- data.frame(resp = stats_gam_cv$resp, 
                           respUnq = stats_gam_cv$respUnq, 
                           R2 = stats_gam_cv$R2, 
                           RMSE = stats_gam_cv$RMSE, 
                           type = "gam_cv")

saveRDS(object = stats_gam_cv, file = paste0(outpath, "stats_gam_cv.rds"))



###R2 gam to original Data SR
stats_gam <- data.frame(resp = colnames(gam_prdct_df)[3:ncol(gam_prdct_df)], R2 = NA, RMSE = NA)
for (i in colnames(gam_prdct_df)[3:ncol(gam_prdct_df)]){
  stat <- postResample(gam_prdct_df[,which(colnames(gam_prdct_df) == i)],
                       SR_tbl[,which(colnames(SR_tbl) == i)])
  stats_gam[which(stats_gam$resp == i), which(colnames(stats_gam) == "R2")] <- stat[2]
  stats_gam[which(stats_gam$resp == i), which(colnames(stats_gam) == "RMSE")] <- stat[1]
  }
saveRDS(object = stats_gam, file = paste0(outpath, "stats_gam.rds"))


####R2 gam + predicted resid to original Data SR
stats_gam_resid <- data.frame(resp_resid = colnames(gam_resid)[3:ncol(gam_resid)], R2 = NA, RMSE = NA)
for (i in colnames(gam_resid)[3:ncol(gam_resid)]){
  match <- substr(i,6, nchar(i)-5)
  stat <- postResample(gam_resid[,which(colnames(gam_resid) == i)], 
                       SR_tbl[,which(colnames(SR_tbl) == match)])
  stats_gam_resid[which(stats_gam_resid$resp_resid == i), 
                  which(colnames(stats_gam_resid) == "R2")] <- stat[2]
  stats_gam_resid[which(stats_gam_resid$resp_resid == i), 
                  which(colnames(stats_gam_resid) == "RMSE")] <- stat[1]
  #stats_gam_resid$resp[which(stats_gam_resid$resp_resid == i)] <- 
    # substr(stats_gam_resid$resp_resid[which(stats_gam_resid$resp_resid == i)], 
    #        6, 
    #        nchar(as.character(stats_gam_resid$resp_resid[which(stats_gam_resid$resp_resid == i)]))-5)

  #stats_gam_resid$resp = paste0(stats_gam_resid$resp_resid, "_test")
  stats_gam_resid$resp <- substr(as.character(stats_gam_resid$resp_resid), 
                                6, 
                                (nchar(as.character(stats_gam_resid$resp_resid))-5))
  }

saveRDS(object = stats_gam_resid, file = paste0(outpath, "stats_gam_resid.rds"))
# stats_gam_resid <- readRDS(file = paste0(inpath, "stats_gam_resid.rds"))


#####################################################################################
#####################################################################################
#####################################################################################

stats_gam <- readRDS(file = paste0(outpath, "stats_gam.rds"))
stats_gam_resid <- readRDS(file = paste0(outpath, "stats_gam_resid.rds"))
stats <- get(load(paste0(inpath, "stats.RData")))
stats_gam_cv <- readRDS(file = paste0(outpath, "stats_gam_cv.rds"))
stats_SR <- stats[which(stats$resp %in% stats_gam$resp), c(which(colnames(stats) %in% c("resp", "respUnq", "Rsquared", "RMSE")))]
colnames(stats_SR)[which(colnames(stats_SR) == "Rsquared")]<- "R2"
stats_SR <- data.frame(stats_SR[which(colnames(stats_SR) %in% c("resp", "respUnq"))],stats_SR[which(colnames(stats_SR) =="R2")], stats_SR[which(colnames(stats_SR) == "RMSE")])
stats_SR$type <- "pls"
stats_gam_resid$respUnq <- stats_gam_resid$resp_resid
# stats_gam_resid$respUnq <- gsub("resid", "", stats_gam_resid$respUnq)
stats_gam_resid <- data.frame(stats_gam_resid[which(colnames(stats_gam_resid) == "resp")], 
                              stats_gam_resid[which(colnames(stats_gam_resid) == "respUnq")], 
                              stats_gam_resid[which(colnames(stats_gam_resid) == "R2")], 
                              stats_gam_resid[which(colnames(stats_gam_resid) == "RMSE")], 
                              type = "gam_resid")

stats_gam$respUnq <- stats_gam$resp
stats_gam <- data.frame(stats_gam[which(colnames(stats_gam) == "resp")], 
                        stats_gam[which(colnames(stats_gam) == "respUnq")], 
                        stats_gam[which(colnames(stats_gam) == "R2")], 
                        stats_gam[which(colnames(stats_gam) == "RMSE")], 
                        type = "gam")

###merge stats table by unique resp (with run): 
stats_mrg <- rbind(stats_SR, stats_gam_cv, stats_gam, stats_gam_resid)


for (x in seq(nrow(stats_mrg))){
  trop <- NA
  for (i in trophic_tbl$Taxon){
    match <- grep(i, stats_mrg[x,"resp"], value=TRUE)
    if (length(match) != 0){
      trop <- trophic_tbl$diet[trophic_tbl$Taxon == i]
    }
    #print(trop)
  }
  stats_mrg$troph[x] <- as.character(trop)
}
stats_mrg$troph <- factor(stats_mrg$troph, levels = c("generalist", 
                                                      "predator", 
                                                      "herbivore", 
                                                      "decomposer", 
                                                      "plant", 
                                                      "birds", 
                                                      "bats", 
                                                      "summary", 
                                                      "trait"))
stats_mrg <- stats_mrg[with(stats_mrg, order(troph, resp)),] ####sortierung nach alphabet resp ist nicht sooo optimal, weil resids lfter zusammenstehen und nicht abwechselnd reisid und das entsprechende SR- eventuell "resid" hinten an namen dranschreiben
stats_mrg$resp <- factor(stats_mrg$resp, levels = unique(stats_mrg$resp))




saveRDS(object = stats_mrg, file = paste0(outpath, "stats_mrg.rds"))
#stats_mrg <- readRDS(file = paste0(inpath, "stats_mrg.rds"))



#plotten: 
myColors <- c("red2", "blue2", "aquamarine3", "chocolate1")
fillscl <- scale_fill_manual(name = "col",values = myColors)
if (frst == T){
  title <- "forest"
}else{
  title <- "non-forest"
}

pdf(file = paste0(outpath, "all_stats.pdf"), height= 10, width = 20)#, paper = "a4r")
ggplot(data = stats_mrg, aes(x=resp, y=R2)) + 
  geom_boxplot(aes(fill=type), width = 1) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) + 
  fillscl+ 
  ggtitle(title)
dev.off()



