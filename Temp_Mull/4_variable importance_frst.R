# Description: 
# Author: Alice Ziegler
# Date: 2018-03-07 12:08:34
# to do: ###optimieren: mod_all muss vermieden werden. jeder loop muss Ã¼ber die einzelmodelle laufen!
#########werden hier wirklich nur die resp verwendet, die im ffs rauskommen???
rm(list=ls())
########################################################################################
###Presettings
########################################################################################
#Packages: 
library(raster)
library(RColorBrewer)
library(reshape2)
library(lattice)
library(rasterVis)
library(grid)
library(compositions)
#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "jun18_50m/60erFRST/2018-06-12_ffs_pls_cv_onlyForest/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../out/", sub)
if (file.exists(outpath)==F){
  dir.create(file.path(outpath), recursive = T)
}

###############################################
varimp_lst <- readRDS(paste0(inpath, "varimp_lst.rds"))
stats <- readRDS(paste0(outpath, "stats_troph.RDS"))
troph_grp <- unique(data.frame(stats$resp, stats$troph))
colnames(troph_grp) <- c("resp", "troph")

#####plotting properties
clr <- colorRampPalette(brewer.pal(9, "YlOrRd"))

rownames(varimp_lst$abundance)

for (i in seq(length(varimp_lst))){
  colnames(varimp_lst[[i]]) <- names(varimp_lst[i])
  varimp_lst[[i]]$pred <- rownames(varimp_lst[[i]])
}
varimp_df <- Reduce(function(x, y) {merge(x, y, by = "pred", all=T)}, varimp_lst)
#saveRDS(varimp_df, file = paste0(outpath, "varimp_df.RDS"))
varimp_df[2:ncol(varimp_df)] <- varimp_df[,2:ncol(varimp_df)]/100

###sort var imp
tmp <- varimp_df[as.character(troph_grp$resp)]
varimp_df <- data.frame(pred = varimp_df$pred, tmp)
#count number of times variable was included in model
var_cnt_c <- (ncol(varimp_df) -1)- rowSums(is.na(varimp_df))
var_cnt <- data.frame(pred = varimp_df$pred, count = var_cnt_c)
var_cnt <- var_cnt[with(var_cnt, order(count)),]
varimp_df <- varimp_df[match(as.character(var_cnt$pred), varimp_df$pred),]


###standard plot
varimp_tmp_lst <- as.list(varimp_df[,-which(colnames(varimp_df) == "pred")])
varimp_mat <- do.call("cbind", varimp_tmp_lst[])
rst <- raster(varimp_mat, xmn = 0.5, xmx = 165.5, 
              ymn = .5, ymx = 113.5)
pdf(file = paste0(outpath, "heat_std.pdf"), width = 10, height = 7)
  levelplot(rst, scales = list(x = list(rot=90, cex = 0.35, at = 1:165, labels = colnames(varimp_mat)), 
                             y = list(at = c(113:1), cex = 0.35, labels = varimp_df$pred)), 
          margin = FALSE, 
          col.regions = clr(101), 
          at = seq(0, 1, 0.1))

dev.off()
###
#
###normalised (R2)
varimp_nrm_df <- varimp_df
for (i in c(2:ncol(varimp_df))){
  varimp_nrm_df[,i] <- varimp_df[,i]*stats[which(stats$resp == names(varimp_df[i]))[1],"meanR2"]
}
varimp_tmp_lst <- as.list(varimp_nrm_df[,-which(colnames(varimp_nrm_df) == "pred")])
varimp_mat <- do.call("cbind", varimp_tmp_lst[])
rst <- raster(varimp_mat, xmn = 0.5, xmx = 165.5, 
              ymn = .5, ymx = 113.5)
pdf(file = paste0(outpath, "heat_nrmR2.pdf"), width = 10, height = 7)
levelplot(rst, scales = list(x = list(rot=90, cex = 0.35, at = 1:165, labels = colnames(varimp_mat)), 
                             y = list(at = c(113:1), cex = 0.35, labels = varimp_nrm_df$pred)), 
          margin = FALSE, 
          col.regions = clr(101), 
          at = seq(0, 0.1, 0.001))
dev.off()
####normalisieren andere möglichkeiten

###gruppieren
# an df anfügen Spalte mit entsprechenden Levels
# oder auch ldr Variablen zusammenfügen (nach ähnlichkeit oder layern)
varimp_df_t <- varimp_df
row.names(varimp_df_t) <- varimp_df_t$pred
varimp_df_t <- varimp_df_t[,-which(colnames(varimp_df_t) == "pred")]
varimp_t <- t(varimp_df_t)
varimp_t <- data.frame(varimp_t)
varimp_t$resp <- row.names(varimp_t)

###group by responses trophic level
varimp_troph <- merge(varimp_t, troph_grp, by = "resp")
agg_troph <- aggregate(varimp_troph, by = list(varimp_troph$troph), FUN = mean, na.rm = T)
agg_troph <- agg_troph[,-c(2, ncol(agg_troph))]
colnames(agg_troph)[1] <- "troph_grp"

agg_troph_lst <- as.list(agg_troph[, -which(colnames(agg_troph) == "troph_grp")])
agg_troph_mat <- do.call("cbind", agg_troph_lst[])
rst <- raster(agg_troph_mat, xmn = 0.5, xmx = 114.5, 
              ymn = .5, ymx = 7.5)

pdf(file = paste0(outpath, "heat_varimp_grp_troph.pdf"), width = 10, height = 7)
levelplot(rst, scales = list(x = list(rot=90, cex = 0.35, at = 1:114, labels = colnames(agg_troph_mat)), 
                             y = list(at = c(7:1), cex = 0.35, labels = agg_troph$troph_grp)), 
          margin = FALSE, 
          col.regions = clr(101), 
          at = seq(0, 1, 0.1))
dev.off()


###group by predictors
pred_grp <- readRDS(file = paste0(inpath, "pred_grp.RDS"))
varimp_pred_grp <- merge(varimp_df, pred_grp, by = "pred")
agg_pred <- aggregate(varimp_pred_grp, by = list(varimp_pred_grp$grp), FUN = mean, na.rm = T)
agg_pred <- agg_pred[,-c(2, ncol(agg_pred))]
colnames(agg_pred)[1] <- "pred_grp"

agg_pred_lst <- as.list(agg_pred[, -which(colnames(agg_pred) == "pred_grp")])
agg_pred_mat <- do.call("cbind", agg_pred_lst[])
rst <- raster(agg_pred_mat, xmn = 0.5, xmx = 166.5, 
              ymn = .5, ymx = 6.5)

pdf(file = paste0(outpath, "heat_varimp_grp_pred.pdf"), width = 10, height = 7)
levelplot(rst, scales = list(x = list(rot=90, cex = 0.35, at = 1:166, labels = colnames(agg_pred_mat)), 
                             y = list(at = c(6:1), cex = 0.35, labels = agg_pred$pred_grp)), 
          margin = FALSE, 
          col.regions = clr(101), 
          at = seq(0, 1, 0.1))
dev.off()

###group by trophic levels and predictors
agg_tbl <- agg_pred
row.names(agg_tbl) <- agg_tbl$pred_grp
agg_all_df_t <- data.frame(t(agg_pred))
colnames(agg_all_df_t) <- rownames(agg_tbl)
agg_all_df_t <- agg_all_df_t[-1,]
agg_all_df_t$resp <- rownames(agg_all_df_t)
agg_all_df_mrg <- merge(agg_all_df_t, troph_grp, by = "resp")
agg_all_df_mrg <- agg_all_df_mrg[,-1]

for (i in seq(ncol(agg_all_df_mrg)-1)){
  agg_all_df_mrg[,i] <- as.numeric(agg_all_df_mrg[,i])
}

agg_all_df <- aggregate(agg_all_df_mrg, by = list(agg_all_df_mrg$troph), FUN = mean, na.rm = T)
agg_all_df <- agg_all_df[,-which(colnames(agg_all_df) == "troph")]
colnames(agg_all_df)[1] <- "troph"


###############################################
#agg_varImp
###############################################
agg_vi_lst <- as.list(agg_all_df[, -which(colnames(agg_all_df) == "troph")])
agg_vi_mat <- do.call("cbind", agg_vi_lst[])
rst <- raster(agg_vi_mat, xmn = 0.5, xmx = 7.5, 
              ymn = .5, ymx = 7.5)

pdf(file = paste0(outpath, "heat_varimp_grp.pdf"), width = 10, height = 7)
levelplot(rst, scales = list(x = list(rot=90, cex = 1, at = 1:7, labels = colnames(agg_vi_mat)), 
                             y = list(at = c(7:1), cex = 1, labels = agg_all_df$troph)), 
          margin = FALSE, 
          col.regions = clr(101), 
          at = seq(0, 100, 10))
dev.off()

