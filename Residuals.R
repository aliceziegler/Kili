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

#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "/"
inpath <- "../data/"
outpath <- paste0("../data/", sub)

########################################################################################
###Settings
########################################################################################

########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################

load(paste0(inpath, "dat_SR.RData"))
elev <- cbind(elevation = dat_SR$elevation)
tbl <- cbind(plotID = dat_SR$plotID, 
             dat_SR[,c(which(colnames(dat_SR) %in% "SRmammals") : 
                         which(colnames(dat_SR) %in% "SRallplants"))])

tbl_res <- tbl
for (i in c(2:ncol(tbl))){
  mod <- gam(tbl[,i] ~ s(elev, k = 5))
  tbl_res[!is.na(tbl[,i]),i] <- mod$residuals
  colnames(tbl_res)[i] <- paste0("resid", colnames(tbl_res)[i])
}

save(tbl_res, file = paste0(outpath, "SR_residuals.RData"))



###calculate R2
for (i in c(2:ncol(tbl))){
  mod <- gam(tbl[,i] ~ s(elev, k = 5))
  prd <- predict(mod, tbl[,i])
  tbl_res[!is.na(tbl[,i]),i] <- mod$residuals
  colnames(tbl_res)[i] <- paste0("resid", colnames(tbl_res)[i])
}
