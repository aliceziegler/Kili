# Description: 
# Author: Alice Ziegler
# Date: 2018-06-13 16:58
# to do: 
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#Packages: 
library(ggplot2)

#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "aug18/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../out/", sub)

##load data
# tbl <- readRDS(paste0(inpath, "mrg_tbl_relevant_cols.rds"))
# forest_plotID <- get(load(paste0(inpath, "../forest_plotID.RData")))
mrg_tbl <- get(load(paste0(inpath, "dat_ldr_mrg.RData")))


match_frst <- c("fer", "flm", "foc", "fod", "fpd", "fpo")
# match_nonfrst <- c("cof", "gra", "hel", "hom", "mai", "sav")

matches_frst <- unique(grep(paste(match_frst,collapse="|"), 
                        mrg_tbl$plotID, value=TRUE))
# matches_nonfrst <- unique(grep(paste(match_nonfrst,collapse="|"), 
#                                mrg_tbl$plotID, value=TRUE))

mrg_tbl$frst <- ifelse(mrg_tbl$plotID %in% matches_frst, "forest", "non-forest")
#mrg_tbl$frst <- as.logical(mrg_tbl$frst)




###DON'T change anything past this point, except you know, what you are doing###
vars <- mrg_tbl[, c(which(colnames(mrg_tbl) == "plotID"), 
                    which(colnames(mrg_tbl) == "BE_RD_01"), 
                    which(colnames(mrg_tbl) == "LAI"), 
                    which(colnames(mrg_tbl) == "AGB"), 
                    which(colnames(mrg_tbl) == "chm_height_mean"), 
                    which(colnames(mrg_tbl) == "FHD"), 
                    which(colnames(mrg_tbl) == "gap_frac"),
                    which(colnames(mrg_tbl) == "sd_rtrn_1"),
                    which(colnames(mrg_tbl) == "frst"))]
vars_nm <- colnames(vars)[2:(ncol(vars)-1)]

# #vars <- tbl[,c(1, which(colnames(tbl) == "AGB") : which(colnames(tbl) == "gap_frac"))]

# vars_frst <- vars[which(vars$frst == 1),]
# colnames(vars_frst) <- paste0("frst_", colnames(vars_frst))
# vars_nonfrst <- vars[which(vars$frst == 0),]
# colnames(vars_nonfrst) <- paste0("nonfrst_", colnames(vars_rest))
# 
# anv_df <- data.frame(vars_frst, vars_nonfrst)
# 
# pdf(file = paste0(outpath, "boxpl_frst_gg_rest.pdf"))
# for (i in vars_nm){
# cols <- grep(i, colnames(anv_df),value = T)###!!!!problem bei zb. BE_H_P_10 und BE_H_P_100!!!!
# tmp <- data.frame(anv_df[cols[1]], anv_df[cols[2]])
# boxplot(tmp)
# frmla <- as.formula(paste0(cols[1], " ~ ", cols[2]))
# test <- t.test(tmp[cols[1]], tmp[cols[2]])
# print(test)
# #summary(test)
# }
# dev.off()

pdf(file = paste0(outpath, "boxpl_frst_gg_rest.pdf"), width = 6, height = 6)
for (i in vars_nm){
  plt <- ggplot(vars, aes_string(x = "frst", y = i)) +
    geom_boxplot()+ 
    xlab("")
  print(plt)
}
dev.off()
