# Description: Function for selecting plots by numerical value of ID
# Author: Alice Ziegler
# Date: 2018-04-24 14:34:49
# to do: 
rm(list=ls())


########################################################################################
###Presettings
########################################################################################
###packages
library(plyr)
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))


########################################################################################
###Settings
########################################################################################
df_meta <- get(load("df_meta.RData"))
min_idx <- 0
max_idx <- 10

########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################
###Functions
sel_by_idx <- function(min_idx, max_idx, df_meta){
  lapply(seq(min_idx, max_idx, 1), function(k){
  out_sel <- df_meta[which(df_meta$selID == k),]
  miss <- unique(df_meta$cat)[!(unique(df_meta$cat) %in% out_sel$cat)]
  df_miss <- df_meta[df_meta$cat %in% as.vector(miss),]
  set.seed(k)
  out_miss <- ddply(df_miss, .(cat), function(x){
    x[sample(nrow(x), 1), ]
  })
  out <- rbind(out_sel, out_miss)
})}

###usecase
outs <- sel_by_idx(min_idx = min_idx, max_idx = max_idx, df_meta = df_meta)
