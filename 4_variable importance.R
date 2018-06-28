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
library(reshape2)
#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "jun18_50m/60erALL/2018-06-12_ffs_pls_cv/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../out/", sub)
if (file.exists(outpath)==F){
  dir.create(file.path(outpath), recursive = T)
}

###############################################
varimp_lst <- readRDS(paste0(inpath, "varimp_lst.rds"))
rownames(varimp_lst$abundance)


for (i in seq(length(varimp_lst))){
  colnames(varimp_lst[[i]]) <- names(varimp_lst[i])
  varimp_lst[[i]]$pred <- rownames(varimp_lst[[i]])
}
varimp_df <- Reduce(function(x, y) {merge(x, y, by = "pred", all=T)}, varimp_lst)


##to do hier etwas mit varimp df machen.