# Description: 
# Author: Alice Ziegler
# Date: 2018-03-07 12:08:34
# to do: ###optimieren: mod_all muss vermieden werden. jeder loop muss ?ber die einzelmodelle laufen!
#########werden hier wirklich nur die resp verwendet, die im ffs rauskommen???
rm(list=ls())
########################################################################################
###Presettings
########################################################################################
#Packages: 
library(caret)
library(pls)
library(plyr)
library(CAST)
#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "sep18/"
all_plts <- T
inpath <- paste0("../data/", sub)
outpath <- paste0("../data/", sub, "example_moths/example_moths_skript/")
if (file.exists(outpath)==F){
  dir.create(file.path(outpath), recursive = T)
}


dat_ldr_mrg <- get(load(paste0(inpath, "dat_ldr_mrg.RData")))

tbl <- dat_ldr_mrg[,c(which(colnames(dat_ldr_mrg) == "plotID") : which(colnames(dat_ldr_mrg) == "lat"), 
                      which(colnames(dat_ldr_mrg) == "dstrb"), 
                      which(colnames(dat_ldr_mrg) == "SRmoths"), 
                      which(colnames(dat_ldr_mrg) == "plotUnq"), 
                      which(colnames(dat_ldr_mrg) == "AGB"), 
                      which(colnames(dat_ldr_mrg) == "BE_FHD") : which(colnames(dat_ldr_mrg) == "BE_PR_55"), 
                      which(colnames(dat_ldr_mrg) == "BE_PR_REG") : which(colnames(dat_ldr_mrg) == "LAI"), 
                      which(colnames(dat_ldr_mrg) == "chm_surface_ratio"), 
                      which(colnames(dat_ldr_mrg) == "pulse_returns_max") : which(colnames(dat_ldr_mrg) == "pulse_returns_mean"), 
                      which(colnames(dat_ldr_mrg) == "pulse_returns_sd"), 
                      which(colnames(dat_ldr_mrg) == "vegetation_coverage_01m") : which(colnames(dat_ldr_mrg) == "vegetation_coverage_10m"), 
                      which(colnames(dat_ldr_mrg) == "mdn_rtrn"), 
                      which(colnames(dat_ldr_mrg) == "sd_rtrn_1"),
                      which(colnames(dat_ldr_mrg) == "gap_frac"))]

tbl$selID <- as.numeric(substr(as.character(tbl$plotID), 4, 4))
tbl$cats <- substr(as.character(tbl$plotID), 1, 3)
cats <- unique(tbl$cats)

outs_lst <- get(load(paste0(inpath, "bsp_outs_lst_allplots.RData")))

rmse_comp <- data.frame(rmsesd_elev = seq(length(outs_lst)), 
                        rmsesd_ffs = seq(length(outs_lst)))
########################################################################################
###predicting, etc
########################################################################################
#runs <- seq(length(outs_lst))

runs <- 5
###loop over 5 runs
for (x in runs){
  out_plt <- outs_lst[[x]]$plotID
  tbl$elevation_scale <- scale(tbl$elevation, center = T, scale = T)
  tbl$elevsq_scale <- scale(tbl$elevsq, center = T, scale = T)
  tbl_in <- tbl[-which(tbl$plotID %in% out_plt),]
  tbl_out <- tbl[which(tbl$plotID %in% out_plt),]
  
###cv index von jeder landuseclass eines, aber zufällige Wahl der indices und 20 mal wiederholt
  cvouts_lst <- lapply(seq(1:20), function(k){
    set.seed(k)
    out_sel <- ddply(tbl_in, .(cat), function(x){
      x[sample(nrow(x), 1), ]
    })
    out <- rbind(out_sel)
  })
  cvIndex <- lapply(cvouts_lst, function(i){
    res <- which(!(tbl_in$plotID %in% i$plotID))
  })
  
  
# ###cv index gleiches system wie outer loop
#   cvind_num <- unique(sort(tbl_in$selID))
#   cvind_num <- cvind_num[which(cvind_num >0)]
#   cvouts_lst <- lapply(cvind_num, function(k){
#     out_sel <- tbl_in[which(tbl_in$selID == k),]
#     miss <- cats[!(cats %in% out_sel$cat)]
#     df_miss <- tbl_in[tbl_in$cat %in% as.vector(miss),]
#     set.seed(k)
#     out_miss <- ddply(df_miss, .(cat), function(x){
#       x[sample(nrow(x), 1), ]
#     })
#     out <- rbind(out_sel, out_miss)
#   })
#   cvIndex <- lapply(cvouts_lst, function(i){
#     res <- which(!(tbl_in$plotID %in% i$plotID))
#   })
  
  dat <- data.frame("elevation_scale"= tbl_in$elevation_scale,
                    "elevsq_scale" = tbl_in$elevsq_scale,
                    "response"= tbl_in[,grepl(paste0("SRmoths"),
                                              colnames(tbl_in))])
######
###pls: only elev + elevsq
###### 
  pred_elev <- data.frame(elevation_scale = dat$elevation_scale, elevsq_scale = dat$elevsq_scale)#, AGB = dat$AGB)
  
  resp_elev <- dat$response
  
  # mod_elev <- train(x = pred_elev, y = resp_elev, method = "pls",
  #                   tuneGrid = expand.grid(ncomp = c(1,2)),
  #                   metric = "RMSE",
  #                   trControl = trainControl(method = "cv", index = cvIndex))
  # save(mod_elev, file = paste0(outpath, "modelrun", x, "only_moths_only_elev_elevsq.RData"))
  mod_elev <- get(load(paste0(outpath, "modelrun", x, "only_moths_only_elev_elevsq.RData")))
  
  newdat_elev <- data.frame("elevation_scale" = tbl_out$elevation_scale,
                            "elevsq_scale" = tbl_out$elevsq_scale)
  prdct_elev <- predict(object = mod_elev, newdata =  newdat_elev)
  #summary(mod_elev)
  r2_elev <- caret::R2(prdct_elev, tbl_out$SRmoths)
  rmse_elev <- caret::RMSE(prdct_elev, tbl_out$SRmoths)
  
  ###write to external table for comparison
  rmse_comp$rmsesd_elev[x] <- rmse_elev
  
######
### pls: with ffs all lidars
######
  resp_ffs <- tbl_in[,which(colnames(tbl_in) == "SRmoths")]
  nm_pred_ffs <- colnames(tbl)[c(seq(grep("AGB", names(tbl)),
                                     grep("gap_frac", names(tbl))), 
                                 grep("elevation", names(tbl)), 
                                 grep("dstrb", names(tbl)), 
                                 grep("elevsq", names(tbl)))]
  
  pred_all_ffs <- tbl_in[,c(which(colnames(tbl_in) %in% nm_pred_ffs))]
  pred_ffs_raw <- pred_all_ffs
  for (i in colnames(pred_all_ffs)){
    frq <- table(pred_all_ffs[i])
    if (max(frq) > floor(nrow(pred_all_ffs) * 0.5)){
      pred_ffs_raw <- pred_ffs_raw[, !(colnames(pred_ffs_raw) %in% i)]
    }
  }
  
  #scaling data to mean 0 and sd 1
  scl_lst <- lapply(pred_ffs_raw, function(m){
    if (class(m) == "numeric"){
      scale(m, center = T, scale = T)
    }else if (class(m) == "integer"){
      scale(as.numeric(m), center = T, scale = T)
    }else{
      m <- m #non numeric or integer columns stay as they are
    }
  })
  pred_ffs <- do.call(data.frame, scl_lst)
  
  
  # cl <- 18
  # registerDoParallel(cl)
  mod_ffs <- ffs(pred_ffs, resp_ffs, method = "pls",
                 tuneGrid = expand.grid(ncomp = c(1:5, 10, 15, 20, 25, 30, 34)),
                 metric = "RMSE", trControl = trainControl(index = cvIndex))
  save(mod_ffs, file = paste0(outpath, "modelrun", x, "_moths_incl_elev_elevsq_in_ffs.RData"))
  # mod_ffs <- get(load(paste0(outpath, "modelrun", x, "_moths_incl_elev_elevsq_in_ffs.RData")))
  prdct_ffs <- predict(object = mod_ffs, newdata =  tbl_out)
  #r2_ffs <- caret::R2(prdct_ffs, tbl_out$SRmoths, na.rm = T)
  rmse_ffs <- caret::RMSE(prdct_ffs, tbl_out$SRmoths)
  rmse_comp$rmsesd_ffs[x] <- rmse_ffs
}

########################################################################################
###performance
########################################################################################
saveRDS(rmse_comp, file = paste0(outpath, "rmse_comp.rds"))
#rmse_comp <- readRDS(file = paste0(outpath, "rmse_comp.rds"))


mod_ffs <- get(load(paste0(outpath, "modelrun", 5, "_moths_incl_elev_elevsq_in_ffs.RData")))

mod_elev

tbl$SRmoths

cbind(tbl$SRmoths, as.character(tbl$selID))

boxplot(tbl$SRmoths ~ tbl$selID)
plot(tbl$SRmoths ~ tbl$elevation)
