##to do: jeweils in eigenen Ordner mit system zeit oder tag und eigenschaft schreiben!

# Description: Calculating model for SRsnails and SRferns 
# Author: Alice Ziegler
# Date: 2017-10-10 17:12:53

rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#Packages: 
library(caret)
library(CAST)
#library(plsdepot)
#Sources: 
inpath <- "/media/sd19006/data/users/aziegler/R-Server/data/"
outpath <- "/media/sd19006/data/users/aziegler/R-Server/out/ffs/"

########################################################################################
###Settings
########################################################################################
load(paste0(inpath, "dat_ldr_mrg.RData")) #mrg_tbl
mrg_tbl <- cbind(mrg_tbl[1:7], 
                 mrg_tbl[14:40], 
                 mrg_tbl[41], 
                 mrg_tbl[43:68], 
                 mrg_tbl[71:74], 
                 mrg_tbl[79:81], 
                 mrg_tbl[86:90], 
                 mrg_tbl[101:104], 
                 mrg_tbl[114:129], 
                 mrg_tbl[134])
nm_bio <- colnames(mrg_tbl[seq(grep("SRmammals", names(mrg_tbl)),
                               grep("SRallplants", names(mrg_tbl)))])
nm_ldr <- colnames(mrg_tbl)[seq(grep("BE_ELEV_ASPECT", names(mrg_tbl)),
                                grep("gap_frac", names(mrg_tbl)))]

###for testing purposes
# nm_ldr <- colnames(mrg_tbl)[c(2, 41:68, 71:90, 94:97, 101:104, 110:129, 131:134)]
# nm_bio <- colnames(mrg_tbl)[c(38)]
# SRsnails <- 30
# SRferns <- 38
# 
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################
# choose which columns are beeing used for training, testing, val
df_ldr <- mrg_tbl[, c(which(colnames(mrg_tbl) %in% nm_ldr))]
df_bio <- mrg_tbl[, c(which(colnames(mrg_tbl) %in% nm_bio))]

df <- cbind(df_ldr, df_bio)

#data preparation
df <- Filter(function(x)(length(unique(x))>1), df)
df <- df[complete.cases(df),]
#scaling data to mean 0 and sd 1
scl_lst <- lapply(df[,c(which(colnames(df) %in% nm_ldr))], function(i){
  if (class(i) == "numeric"){
    scale(i, center = T, scale = T)
  }else if (class(i) == "integer"){
    scale(as.numeric(i), center = T, scale = T)
  }else{
    i <- i #non numeric or integer columns stay as they are
  }
})
df_scl_ldr <- do.call(data.frame, scl_lst)
df_scl <- cbind(df_scl_ldr, df[,c(which(colnames(df) %in% nm_bio))])

###parameter settings
fitControl <- trainControl(
  method = "LOOCV",
  returnResamp = "all",
  savePredictions = TRUE) 

seeds = 11

# rfe_cntrl <- rfeControl(functions = caretFuncs,
#                         number = 50, method = "LOOCV")

pred <- df_scl[,c(which(colnames(df_scl) == nm_ldr[1]):which(colnames(df_scl) == nm_ldr[length(nm_ldr)]))]
resp <- df_scl[,c(which(colnames(df_scl) == nm_bio[1]):which(colnames(df_scl) == nm_bio[length(nm_bio)]))]


###rfe model
# for (i in seq(resp)){
#   mod_pls <- train(pred, resp[,i], method = "pls",
#                  trControl = fitControl, tuneLength = 20)
#   save(mod_pls, file = paste0(outpath, "pls_model_", colnames(resp)[i], ".RData"))
#   print(paste0("DONE: ", outpath, "pls_model_", colnames(resp)[i]))
# }

###ffs model
for (i in seq(resp)){
  mod_pls <- ffs(pred, resp[,i], method = "pls", 
                 trControl = fitControl, tuneLength = 20)
  save(mod_pls, file = paste0(outpath, "pls_model_", colnames(resp)[i], ".RData"))
  print(paste0("DONE: ", outpath, "pls_model_", colnames(resp)[i]))
}

models = list.files(outpath, pattern = glob2rx("pls_model*"), full.names = TRUE)

mod <- list()
for (i in seq(length(models))){
  load(file = models[i])
  nm <- strsplit(x = models[i], split = "pls_model_|\\.")[[1]][2]
  mod[[nm]] <- mod_pls
}
save(mod, file = paste0(outpath, "pls_model_list_all.RData"))
