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

library(mgcv)
#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
# sub <- "aug18/2018-08-31_ffs_pls_cv_onlyForest_alpha_all/"
sub <- "aug18/2018-09-01_ffs_pls_cv_noForest_alpha_all/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../out/", sub)
if (file.exists(outpath)==F){
  dir.create(file.path(outpath), recursive = T)
}

##read data
outs_lst <- get(load(paste0(inpath, "/outs_lst.RData"))) ###outs lst ist bei forest und non forest gleich
mrg_tbl <- get(load(paste0(inpath, "../dat_ldr_mrg.RData")))


###data preparation
all_plts <- F
if (all_plts == F){
  if(length(grep("_only", sub))){
    frst <- T
  }else{
    frst <- F
  }
}


###hieraus lapply machen
gam_cv_pred <- lapply(colnames(mrg_tbl)[c((which(colnames(mrg_tbl) == "SRmammals") : 
                                             which(colnames(mrg_tbl) == "SRmagnoliids")), 
                                          (which(colnames(mrg_tbl) == "sum_generalist_N4") :
                                             which(colnames(mrg_tbl) == "sum_bats_N1")))], 
                      function(j){
                        
                        outs_prdct <- lapply(seq(length(outs_lst)), function(i){
                          out_plt <- outs_lst[[i]]$plotID
                          mrg_out <- mrg_tbl[-which(mrg_tbl$plotID %in% out_plt),]
                          dat <- data.frame("elevation"= mrg_out$elevation,
                                            "response"= mrg_out[,grepl(paste0("^", j, "$"), 
                                                                       colnames(mrg_out))])
                          mod_gam <- gam(response ~ s(elevation),data=dat)
                          newdat <- data.frame("elevation"= mrg_out$elevation)
                          prdct <- predict(object = mod_gam, newdata =  newdat)
                          prdct_df <- cbind(mrg_out$plotID, mrg_out$plotUnq, dat, prdct)
                          # colnames(prdct_df)[which(colnames(prdct_df) == "prdct")] <- paste0(j, "_", "run_", i)
                          colnames(prdct_df) <- c("plotID", "plotUnq", "elevation", j, paste0(j, "_", "run_", i))
                          return(prdct_df = prdct_df)
                        })
                        return(outs_prdct)
                        })


# for (k in seq(length(gam_cv_pred)){
out_df_mrg_lst <- lapply(seq(length(gam_cv_pred)), function(k){
  out_df_mrg <- Reduce(function(x, y) merge(x, y, by="plotUnq", all = T), gam_cv_pred[[k]])
  out_df_mrg <- out_df_mrg[, !grepl("\\.", colnames(out_df_mrg))]
  return(out_df_mrg)
})

gam_prdct_cv_df <- Reduce(function(x, y) merge(x, y, by="plotUnq"), out_df_mrg_lst)
gam_prdct_cv_df <- gam_prdct_cv_df[, !grepl("\\.", colnames(gam_prdct_cv_df))]
gam_prdct_cv_df <- data.frame(plotID = substr(gam_prdct_cv_df$plotUnq, 1, 4), 
                   plotUnq = gam_prdct_cv_df$plotUnq, 
                   elevation = gam_prdct_cv_df$elevation, 
                   gam_prdct_cv_df[,grepl("run", colnames(gam_prdct_cv_df))])
if (frst == T){
  saveRDS(object = gam_prdct_cv_df, file = paste0(inpath, "../gam_prdct_cv_df_frst.rds"))
}else if (frst == F){
  saveRDS(object = gam_prdct_cv_df, file = paste0(inpath, "../gam_prdct_cv_df_nofrst.rds"))
}

