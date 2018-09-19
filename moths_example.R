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
outpath <- paste0("../data/", sub, "example_moths/")
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

out_plt <- outs_lst[[1]]$plotID
tbl$elevation_scale <- scale(tbl$elevation, center = T, scale = T)
tbl$elevsq_scale <- scale(tbl$elevsq, center = T, scale = T)
tbl_in <- tbl[-which(tbl$plotID %in% out_plt),]
tbl_out <- tbl[which(tbl$plotID %in% out_plt),]
# dat <- data.frame("elevation_scale"= tbl_in$elevation_scale,
#                   "elevsq_scale" = tbl_in$elevsq_scale,
#                   "AGB" = tbl_in$AGB,
#                   "response"= tbl_in[,grepl(paste0("SRmoths"),
#                                             colnames(tbl_in))])

cvind_num <- unique(sort(tbl_in$selID))
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

dat <- data.frame("elevation_scale"= tbl_in$elevation_scale,
                  "elevsq_scale" = tbl_in$elevsq_scale,
                  "response"= tbl_in[,grepl(paste0("SRmoths"),
                                            colnames(tbl_in))])

###pls - only elev + elevsq
mod_pls <- plsr(response ~ (elevation_scale + elevsq_scale), data = dat)

# pred_pls <- data.frame(elevation_scale = dat$elevation_scale, elevsq_scale = dat$elevsq_scale, AGB = dat$AGB)
pred_pls <- data.frame(elevation_scale = dat$elevation_scale, elevsq_scale = dat$elevsq_scale)#, AGB = dat$AGB)

resp_pls <- dat$response

mod_pls_trn <- train(x = pred_pls, y = resp_pls, method = "pls", 
                     tuneGrid = expand.grid(ncomp = c(1,2)), 
                     trControl = trainControl(method = "cv", index = cvIndex))
#saveRDS(mod_pls, file = paste0(outpath, "pls_cv_mod_", resp, "_", run, ".rds"))
# newdat_pls <- data.frame("elevation_scale" = tbl_out$elevation_scale,
#                          "elevsq_scale" = tbl_out$elevsq_scale,
#                          "AGB" = tbl_out$AGB)
newdat_pls <- data.frame("elevation_scale" = tbl_out$elevation_scale,
                         "elevsq_scale" = tbl_out$elevsq_scale)
prdct_pls <- predict(object = mod_pls, newdata =  newdat_pls)
prdct_pls_trn <- predict(object = mod_pls_trn, newdata =  newdat_pls)
################achtung, im skript2 line 276 hier werden zwei comps in datenframe geschrieben und im anschluss nicht weiter beachtet! in 283 wird nur 1.umbenannt
r2_pls_1 <- caret::R2(prdct_pls[,,1], tbl_out$SRmoths)
r2_pls_2 <- caret::R2(prdct_pls[,,2], tbl_out$SRmoths)
rmse_pls_1 <- caret::RMSE(prdct_pls[,,1], tbl_out$SRmoths)
rmse_pls_2 <- caret::RMSE(prdct_pls[,,2], tbl_out$SRmoths)
summary(mod_pls_trn)
r2_pls_trn <- caret::R2(prdct_pls_trn, tbl_out$SRmoths)
rmse_pls_trn <- caret::RMSE(prdct_pls_trn, tbl_out$SRmoths)
# plot(RMSEP(mod_pls))
# plot(mod_pls, ncomp = 1)



#ffs all
resp <- tbl_in[,which(colnames(tbl_in) == "SRmoths")]
nm_pred <- colnames(tbl)[c(seq(grep("AGB", names(tbl)),
                               grep("gap_frac", names(tbl))), 
                           grep("elevation", names(tbl)), 
                           grep("dstrb", names(tbl)), 
                           grep("elevsq", names(tbl)))]

pred_all <- tbl_in[,c(which(colnames(tbl_in) %in% nm_pred))]
pred <- pred_all
for (i in colnames(pred_all)){
  frq <- table(pred_all[i])
  if (max(frq) > floor(nrow(pred_all) * 0.5)){
    pred <- pred[, !(colnames(pred) %in% i)]
  }
}

###test
# pred <- pred[,c(1,2)]
####





cl <- 15
registerDoParallel(cl)
# mod <- ffs(pred, resp, method = "pls", tuneGrid = expand.grid(ncomp = c(1:5, 10, 15, 20, 25, 30, 34)),
#            metric = "RMSE", trControl = trainControl(index = cvIndex))
# save(mod, file = paste0(outpath, "modelrun1_moths_only_elev_elevsq_in_ffs.RData"))
mod <- get(load(paste0(outpath, "modelrun1_moths_only_elev_elevsq_in_ffs.RData")))

mod_serv <- get(load(paste0(outpath, "indv_model_run1_ffs_pls_SRmoths.RData")))
#mod$perf_all

prdct <- predict(object = mod, newdata =  tbl_out)
r2 <- caret::R2(prdct, tbl_out$SRmoths, na.rm = T)
rmse <- caret::RMSE(prdct, tbl_out$SRmoths)
#mod <- get(load(file = "D:/Uni/Projekte/Kili/data/sep18/2018-09-14_ffs_pls_cv_allplots_alpha_exmples_RMSE_elev_dstrb_elevsq_plsresid/indv_model_run1_ffs_pls_SRmoths.RData"))
# 
# head(mod$perf_all)
# 
# 
# pls_perf <-readRDS(file = "D:/Uni/Projekte/Kili/out/sep18/2018-09-14_ffs_pls_cv_allplots_alpha_exmples_RMSE_elev_dstrb_elevsq_plsresid/pls_cv_mod_SRmoths_run1.rds")
# summary(pls_perf)
