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

#Sources:
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "sep18/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../data/", sub)
outpath_general <- paste0("../data/")

########################################################################################
###Settings
########################################################################################

########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################

dat_SR <- get(load(paste0(inpath, "dat_SR.RData")))
#beta <- get(load(paste0(inpath, "beta_anm_plnt.RData")))
elev <- data.frame(elevation = dat_SR$elevation, plotID = dat_SR$plotID)
elev$elevation_scale <- scale(elev$elevation, center = T, scale = T)##scaled fpr pls
elevsq <-  data.frame(elevsq = dat_SR$elevsq, plotID = dat_SR$plotID)
elevsq$elevsq_scale <- scale(elevsq$elevsq, center = T, scale = T)##scaled for pls
troph_sum <- get(load(paste0(inpath, "troph_sum.RData")))

######Residuals SR data
tbl <- cbind(plotID = dat_SR$plotID,
             dat_SR[,c(which(colnames(dat_SR) %in% "SRmammals") :
                         which(colnames(dat_SR) %in% "SRallplants"))])



tbl_res <- tbl
for (i in c(2:ncol(tbl))){
  #mod <- gam(tbl[,i] ~ s(elev$elevation, k = 5))
  mod <- plsr(tbl[,i] ~ (elev$elevation_scale + elevsq$elevsq_scale))
  tbl_res[!is.na(tbl[,i]),i] <- mod$residuals[,,1]
  colnames(tbl_res)[i] <- paste0("resid", colnames(tbl_res)[i])
}


save(tbl_res, file = paste0(outpath, "SR_residuals.RData"))

######Residuals beta diversity data
elev_beta <- elev[which(beta$plotID %in% elev$plotID),]

beta_res <- beta
for (i in c(2:ncol(beta))){
  # mod <- gam(beta[,i] ~ s(elev_beta$elevation, k = 5))
  mod <- plsr(beta[,i] ~ (elev$elevation_scale + elevsq$elevsq_scale))
  beta_res[!is.na(beta[,i]),i] <- mod$residuals
  colnames(beta_res)[i] <- paste0("resid", colnames(beta_res)[i])
}

save(beta_res, file = paste0(outpath, "beta_residuals.RData"))


######Residuals trophic sum  data
elev_troph_sum <- elev[which(troph_sum$plotID %in% elev$plotID),]

troph_sum_res <- troph_sum

for (i in c(2:ncol(troph_sum))){
  # mod <- gam(troph_sum[,i] ~ s(elev_troph_sum$elevation, k = 5))
  mod <- plsr(troph_sum[,i] ~ (elev$elevation_scale + elevsq$elevsq_scale))
  troph_sum_res[!is.na(troph_sum[,i]),i] <- mod$residuals
  colnames(troph_sum_res)[i] <- paste0("resid", colnames(troph_sum_res)[i])
}

save(troph_sum_res, file = paste0(outpath, "troph_sum_residuals.RData"))

