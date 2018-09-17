# Description: 
# Author: Alice Ziegler
# Date: 2018-02-28 11:52:57
# to do: 
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#Packages: 
library(mgcv)
library(pls)
library(caret)


#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "sep18/"
inpath <- "../data/"
outpath <- paste0("../data/", sub)
outpath_general <- paste0("../data/")

########################################################################################
###Settings
########################################################################################

########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################

dat_SR <- get(load(paste0(inpath, "dat_SR.RData")))
beta <- get(load(paste0(inpath, "beta_anm_plnt.RData")))
elev <- data.frame(elevation = dat_SR$elevation, plotID = dat_SR$plotID)
elevsq <-  data.frame(elevsq = dat_SR$elevsq, plotID = dat_SR$plotID)
troph_sum <- get(load(paste0(inpath, "troph_sum.RData")))
troph_sum <- troph_sum[,-which(colnames(troph_sum) == "sum_trait_N0")]
######Residuals SR data
tbl <- cbind(plotID = dat_SR$plotID, 
             dat_SR[,c(which(colnames(dat_SR) %in% "SRmammals") : 
                         which(colnames(dat_SR) %in% "SRallplants"))])

mrg_obs <- merge(tbl, troph_sum, by = "plotID")
for (j in c(2:ncol(mrg_obs))){
  mrg_obs[,j] <- as.numeric(mrg_obs[,j])
}
elev <- elev[which(elev$plotID %in% mrg_obs$plotID),]
elev$elevation_scale <- scale(elev$elevation, center = T, scale = T)
elevsq <- elevsq[which(elevsq$plotID %in% mrg_obs$plotID),]
elevsq$elevsq_scale <- scale(elevsq$elevsq, center = T, scale = T)


###gam model
tbl_res_gam <- mrg_obs
tbl_res_gam_pred <- mrg_obs
for (i in c(2:ncol(mrg_obs))){
  dat <- data.frame("elevation" = elev$elevation, "response" = mrg_obs[,i])
  mod <- gam(response ~ s(elevation, k = 5), data = dat)
  tbl_res_gam[!is.na(mrg_obs[,i]),i] <- mod$residuals
  colnames(tbl_res_gam)[i] <- paste0("resid", colnames(tbl_res_gam)[i])
  newdat <- data.frame("elevation"= elev$elevation)
  prdct <- predict(object = mod, newdata =  newdat)
  tbl_res_gam_pred[,i] <- as.numeric(prdct)
}

# for (i in seq(2,ncol(tbl_res_gam_pred))){
#   plot(elev$elevation, tbl_res_gam_pred[,i])
# }

# plot(elev$elevation, mrg_obs[,2])
# plot(elev$elevation, tbl_res_gam_pred[,2])

###pls model
tbl_res_pls <- mrg_obs
tbl_res_pls_pred <- mrg_obs
tbl_res_pls_pred_lin <- mrg_obs
for (i in c(2:ncol(mrg_obs))){
  # print(i)
  dat <- data.frame("elevation_scale" = elev$elevation_scale, 
                    "elevsq_scale" = elevsq$elevsq_scale, 
                    "response" = mrg_obs[,i])
  #mod <- plsr(response ~ elevation_scale, data = dat)
  mod <- plsr(response ~ (elevation_scale + elevsq_scale), data = dat)
  tbl_res_pls[!is.na(mrg_obs[,i]),i] <- mod$residuals[,,1]###############kann nur mit 1 komponente ersetzt werden!
  colnames(tbl_res_pls)[i] <- paste0("resid", colnames(tbl_res_pls)[i])
  newdat <- data.frame("elevation_scale"= elev$elevation_scale, "elevsq_scale" = elevsq$elevsq_scale)
  prdct <- predict(object = mod, newdata =  newdat)
  tbl_res_pls_pred_lin[,i] <- as.numeric(prdct[,,1])
  tbl_res_pls_pred[,i] <- as.numeric(prdct[,,2])
}

# for (i in seq(2,ncol(tbl_res_pls_pred))){
#   plot(elev$elevation, tbl_res_gam_pred[,i], main = colnames(tbl_res_pls_pred)[i])
#   plot(elev$elevation, tbl_res_pls_pred[,i], main = colnames(tbl_res_pls_pred)[i])
#   plot(elev$elevation, tbl_res_pls_pred_lin[,i], main = colnames(tbl_res_pls_pred)[i])
# }



#stats gam
stats_gam <- data.frame(resp = colnames(mrg_obs)[2:ncol(mrg_obs)], RMSE = NA, 
                        R2 = NA, sd = NA, RMSE_sd = NA, type = "gam")
for (i in colnames(mrg_obs)[2:ncol(mrg_obs)]) {
  stats_gam[stats_gam$resp == i, "RMSE"] <- caret::RMSE(tbl_res_gam_pred[,i], mrg_obs[,i], na.rm = T)
  stats_gam[stats_gam$resp == i, "R2"] <- caret::R2(tbl_res_gam_pred[,i], mrg_obs[,i], formula = "corr", na.rm = T)
  stats_gam[stats_gam$resp == i, "sd"] <- sd(mrg_obs[,i], na.rm = T)
  stats_gam[stats_gam$resp == i, "RMSE_sd"] <- stats_gam[stats_gam$resp == i, "RMSE"]/sd(mrg_obs[,i], na.rm = T)
  sq_df <- data.frame(plotID = mrg_obs$plotID, sq = NA)
}

#stats_pls
stats_pls <- data.frame(resp = colnames(mrg_obs)[2:ncol(mrg_obs)], RMSE = NA, 
                        R2 = NA, sd = NA, RMSE_sd = NA, type = "pls")
for (i in colnames(mrg_obs)[2:ncol(mrg_obs)]) {
  stats_pls[stats_pls$resp == i, "RMSE"] <- caret::RMSE(tbl_res_pls_pred[,i], mrg_obs[,i], na.rm = T)
  stats_pls[stats_pls$resp == i, "R2"] <- caret::R2(tbl_res_pls_pred[,i], mrg_obs[,i], formula = "corr", na.rm = T)
  stats_pls[stats_pls$resp == i, "sd"] <- sd(mrg_obs[,i], na.rm = T)
  stats_pls[stats_pls$resp == i, "RMSE_sd"] <- stats_pls[stats_pls$resp == i, "RMSE"]/sd(mrg_obs[,i], na.rm = T)
  sq_df <- data.frame(plotID = mrg_obs$plotID, sq = NA)
}

#stats_pls_lin
stats_pls_lin <- data.frame(resp = colnames(mrg_obs)[2:ncol(mrg_obs)], RMSE = NA, 
                            R2 = NA, sd = NA, RMSE_sd = NA, type = "pls_lin")
for (i in colnames(mrg_obs)[2:ncol(mrg_obs)]) {
  stats_pls_lin[stats_pls_lin$resp == i, "RMSE"] <- caret::RMSE(tbl_res_pls_pred_lin[,i], mrg_obs[,i], na.rm = T)
  stats_pls_lin[stats_pls_lin$resp == i, "R2"] <- caret::R2(tbl_res_pls_pred_lin[,i], mrg_obs[,i], formula = "corr", na.rm = T)
  stats_pls_lin[stats_pls_lin$resp == i, "sd"] <- sd(mrg_obs[,i], na.rm = T)
  stats_pls_lin[stats_pls_lin$resp == i, "RMSE_sd"] <- stats_pls_lin[stats_pls_lin$resp == i, "RMSE"]/sd(mrg_obs[,i], na.rm = T)
  sq_df <- data.frame(plotID = mrg_obs$plotID, sq = NA)
}

mod_df <- rbind(stats_gam, stats_pls, stats_pls_lin)

pdf(file = paste0(outpath, "gam_pls_compare.pdf"))
print(ggplot(data = mod_df, aes(x=type, y=RMSE_sd)) + 
        geom_boxplot())

print(ggplot(data = mod_df, aes(x=type, y=R2)) + 
        geom_boxplot())
dev.off()

############################
###former pls - ohne quadratisches Element
###pls model
# tbl_res_pls <- mrg_obs
# tbl_res_pls_pred <- mrg_obs
# for (i in c(2:ncol(mrg_obs))){
#   dat <- data.frame("elevation_scale" = elev$elevation_scale,
#                     "response" = mrg_obs[,i])
#   mod <- plsr(response ~ elevation_scale, data = dat)
#   tbl_res_pls[!is.na(mrg_obs[,i]),i] <- mod$residuals
#   colnames(tbl_res_pls)[i] <- paste0("resid", colnames(tbl_res_pls)[i])
#   newdat <- data.frame("elevation_scale"= elev$elevation_scale)
#   prdct <- predict(object = mod, newdata =  newdat)
#   tbl_res_pls_pred[,i] <- as.numeric(prdct)
# }
# 
# plot(elev$elevation, mrg_obs[,2])
# plot(elev$elevation, tbl_res_pls_pred[,2])


