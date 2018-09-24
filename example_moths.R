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
outpath <- paste0("../data/", sub, "example_moths/example_moths_skript209_innercv50/")
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
  # tbl$elevation_scale <- scale(tbl$elevation, center = T, scale = T)
  # tbl$elevsq_scale <- scale(tbl$elevsq, center = T, scale = T)
  
  #scaling data to mean 0 and sd 1
  scl_lst <- lapply(colnames(tbl), function(m){
    if(m %in% colnames(tbl)[c(which(colnames(tbl) =="elevation"), 
                              which(colnames(tbl) =="elevsq"), 
                              which(colnames(tbl) =="AGB") : which(colnames(tbl) =="gap_frac"))]){
      if (class(tbl[,m]) == "numeric"){
        scale(tbl[,m], center = T, scale = T)
      }else if (class(tbl[,m]) == "integer"){
        scale(as.numeric(tbl[,m]), center = T, scale = T)
      }else{
        tbl[,m] <- tbl[,m] #non numeric or integer columns stay as they are
      }
    }else{
      tbl[,m] <- tbl[,m] #non numeric or integer columns stay as they are}
    }})
  tbl_scl <- do.call(data.frame, scl_lst)
  colnames(tbl_scl) <- colnames(tbl)
  
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

  # dat <- data.frame("elevation"= tbl_in$elevation,
  #                   "elevsq" = tbl_in$elevsq,
  #                   "response"= tbl_in[,grepl(paste0("SRmoths"),
  #                                             colnames(tbl_in))])
  ######
  ###pls: only elev + elevsq
  ###### 
  pred_elev <- data.frame(elevation = tbl_in$elevation, elevsq = tbl_in$elevsq)
  
  resp_elev <- tbl_in$SRmoths####################################################hardcoding
  
  # mod_elev <- train(x = pred_elev, y = resp_elev, method = "pls",
  #                   tuneGrid = expand.grid(ncomp = c(1,2)),
  #                   metric = "RMSE",
  #                   trControl = trainControl(method = "cv", index = cvIndex, indexOut = cvIndex_out))
  save(mod_elev, file = paste0(outpath, "modelrun", x, "only_moths_only_elev_elevsq.RData"))
  #mod_elev <- get(load(paste0(outpath, "modelrun", x, "only_moths_only_elev_elevsq.RData")))
  
  # newdat_elev <- data.frame("elevation" = tbl_out$elevation,
  #                           "elevsq" = tbl_out$elevsq, 
  #                           )
  #prdct_elev <- predict(object = mod_elev, newdata =  tbl_out)
  #summary(mod_elev)
  #r2_elev <- caret::R2(prdct_elev, tbl_out$SRmoths)
  #rmse_elev <- caret::RMSE(prdct_elev, tbl_out$SRmoths)
  
  ###write to external table for comparison
  #rmse_comp$rmsesd_elev[x] <- rmse_elev
  
  ######
  ### pls: with ffs all lidars
  ######
  resp_ffs <- tbl_in[,which(colnames(tbl_in) == "SRmoths")]
  nm_pred_ffs <- colnames(tbl_scl)[c(seq(grep("AGB", names(tbl_scl)),
                                         grep("gap_frac", names(tbl_scl))), 
                                     grep("elevation", names(tbl_scl)), 
                                     grep("dstrb", names(tbl_scl)), 
                                     grep("elevsq", names(tbl_scl)))]
  
# pred_all_ffs <- tbl_in[,c(which(colnames(tbl_in) %in% nm_pred_ffs))]
  #pred_ffs <- pred_all_ffs
  for (i in nm_pred_ffs){
    frq <- table(tbl_in[,i])
    if (max(frq) > floor(nrow(tbl_in) * 0.5) | is.nan(sum(tbl_in[,i]))){
      nm_pred_ffs <- nm_pred_ffs[!(nm_pred_ffs %in% i)]
    }
  }

  
  # cl <- 18
  # registerDoParallel(cl)
  
  # mod_elev <- train(tbl_in[,c("elevation","elevsq")], tbl_in$SRmoths, method = "pls",
  #                   tuneGrid = expand.grid(ncomp = c(1,2)),
  #                   metric = "RMSE",
  #                   trControl = trainControl(method = "cv", index = cvIndex, indexOut = cvIndex_out))
  # 
  # 
  # mod_ffs <- ffs(tbl_in[,nm_pred_ffs], tbl_in$SRmoths, method = "pls",
  #                tuneGrid = expand.grid(ncomp = c(1:7)), #:5, 10, 15, 20, 25, 30, 34)),
  #                metric = "RMSE", 
  #                trControl = trainControl(method = "cv", index = cvIndex, indexOut = cvIndex_out), 
  #                verbose = F)
  # save(mod_ffs, file = paste0(outpath, "modelrun", x, "_moths_incl_elev_elevsq_in_ffs.RData"))
  mod_ffs <- get(load(paste0(outpath, "modelrun", x, "_moths_incl_elev_elevsq_in_ffs.RData")))
 
  
  prdct_elev <- predict(object = mod_elev, newdata =  tbl_out)
  rmse_elev <- caret::RMSE(prdct_elev, tbl_out$SRmoths)
  rmse_comp$rmsesd_elev[x] <- rmse_elev
  
  prdct_ffs <- predict(object = mod_ffs, newdata = tbl_out)
  rmse_ffs <- caret::RMSE(prdct_ffs, tbl_out$SRmoths)
  rmse_comp$rmsesd_ffs[x] <- rmse_ffs
}

########################################################################################
###performance
########################################################################################
saveRDS(rmse_comp, file = paste0(outpath, "rmse_comp.rds"))
#rmse_comp <- readRDS(file = paste0(outpath, "rmse_comp.rds"))


mod_elev <- get(load(paste0(outpath, "modelrun", x, "only_moths_only_elev_elevsq.RData")))
mod_ffs <- get(load(paste0(outpath, "modelrun", 5, "_moths_incl_elev_elevsq_in_ffs.RData")))

mod_ffs
mod_elev

mod_ffs_index <- get(load("D:/Uni/Projekte/Kili/data/sep18/example_moths/example_moths_skript209_indxcv/modelrun5_moths_incl_elev_elevsq_in_ffs.RData"))

tbl$SRmoths

cbind(tbl$SRmoths, as.character(tbl$selID))

boxplot(tbl$SRmoths ~ tbl$selID)
plot(tbl$SRmoths ~ tbl$elevation)


plot(tbl[which(tbl$plotID %in% tbl_in$plotID), "SRmoths"] ~  tbl[which(tbl$plotID %in% tbl_in$plotID), "elevation"])
points(tbl[which(tbl$plotID %in% tbl_out$plotID), "SRmoths"] ~  tbl[which(tbl$plotID %in% tbl_out$plotID), "elevation"],  col = "red")

