# Description: 
# Author: Alice Ziegler
# Date: 2018-06-13 16:58
# to do: 
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#Packages: 


#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "jun18_50m/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../out/", sub)

##load data
tbl <- readRDS(paste0(inpath, "mrg_tbl_relevant_cols.rds"))
forest_plotID <- get(load(paste0(inpath, "../forest_plotID.RData")))

###DONT change anything past this point, except you know, what you are doing###
vars <- tbl[,c(1, which(colnames(tbl) == "AGB") : which(colnames(tbl) == "gap_frac"))]
vars_nm <- colnames(vars)[2:ncol(vars)]
vars_frst <- vars[which(vars$plotID %in% forest_plotID),]
colnames(vars_frst) <- paste0("frst_", colnames(vars_frst))
vars_rest <- vars[-which(vars$plotID %in% forest_plotID),]
colnames(vars_rest) <- paste0("rest_", colnames(vars_rest))

anv_df <- data.frame(vars_frst, vars_rest)

pdf(file = paste0(outpath, "boxpl_frst_gg_rest.pdf"))
for (i in vars_nm){
cols <- grep(i, colnames(anv_df),value = T)###!!!!problem bei zbBE_H_P_10 und BE_H_P_100!!!!
tmp <- data.frame(anv_df[cols[1]], anv_df[cols[2]])
boxplot(tmp)
frmla <- as.formula(paste0(cols[1], " ~ ", cols[2]))
test <- t.test(tmp[cols[1]], tmp[cols[2]])
print(test)
#summary(test)
}
dev.off()
