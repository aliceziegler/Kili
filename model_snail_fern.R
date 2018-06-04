# Description: Calculating model for SRsnails and SRferns
# Author: Alice Ziegler
# Date: 2017-10-10 17:12:53

rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#Packages: 
library(caret)
#library(plsdepot)
#Sources: 
inpath <- "F:/Projekte/Kili/data/"
outpath <- "F:/Projekte/Kili/out/plot_scatter_mat/"

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
nm_ldr <- colnames(mrg_tbl[seq(grep("SRmammals", names(mrg_tbl)),
                               grep("SRallplants", names(mrg_tbl)))])
nm_bio <- colnames(mrg_tbl)[seq(grep("BE_ELEV_ASPECT", names(mrg_tbl)),
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

df <- cbind(df_ldr, SRsnails = df_bio)

#data preparation
df <- Filter(function(x)(length(unique(x))>1), df)
df <- df[complete.cases(df),]
#scaling data to mean 0 and sd 1
scl_lst <- lapply(df[,c(1:ncol(df)-1)], function(i){
  if (class(i) == "numeric"){
    scale(i, center = T, scale = T)
  }else if (class(i) == "integer"){
    scale(as.numeric(i), center = T, scale = T)
  }else{
    i <- i #non numeric or integer columns stay as they are
  }
})
df_scl_all <- do.call(data.frame, scl_lst) ###response is also scaled
df_scl <- cbind(df_scl_all[,c(1:ncol(df_scl_all)-1)], df[,ncol(df)])
colnames(df_scl)[ncol(df_scl)] <- colnames(df)[ncol(df)]

###parameter settings
fitControl <- trainControl(
  method = "LOOCV",
  returnResamp = "all",
  savePredictions = TRUE) 

seeds = 11

rfe_cntrl <- rfeControl(functions = caretFuncs,
                        number = 50, method = "LOOCV")

###pls model with scaled data
# mod_pls <- train(df_scl[, c(2:(ncol(df_scl)-1))], df_scl[,c(ncol(df_scl))], method = "kernelpls", 
#                  trControl = fitControl, tuneLength = 20)
# plot(mod_pls)
# summary(mod_pls)
# print(mod_pls)


mod_pls <- rfe(df_scl[, c(60:(ncol(df_scl)-1))], df_scl[,c(ncol(df_scl))], 
               method = "kernelpls", 
                 trControl = fitControl, rfeControl = rfe_cntrl, tuneLength = 20)
plot(mod_pls)
summary(mod_pls)
print(mod_pls)

###only forest - läuft
# df_scl_frst <- df_scl[which(df_scl$cat %in% c("flm", "fod", "foc", "fpo", "fpd", "fed", "fer")),]
# mod_pls_frst <- train(df_scl_frst[, c(1:(ncol(df_scl_frst)-1))], df_scl_frst[,c(ncol(df_scl_frst))], method = "kernelpls", 
#                  trControl = fitControl, tuneLength = 20)
# plot(mod_pls_frst)
# summary(mod_pls_frst)
# print(mod_pls_frst)

##preprocess seperately
preProcValues_sc <- preProcess(df, method = c("center", "scale"))
df_sc <- predict(preProcValues_sc, df)

mod_df_sc <- train(df_sc[, c(1:(ncol(df_sc)-1))], df_sc[,c(ncol(df_sc))], method = "kernelpls", 
                   trControl = fitControl, tuneLength = 20)
plot(mod_df_sc)
summary(mod_df_sc)
print(mod_df_sc)


##pls scale in caret
mod_pls_sc <- train(df[, c(1:(ncol(df)-1))], df[,c(ncol(df))], method = "kernelpls", 
                   trControl = fitControl, tuneLength = 20, preProcess = c("scale", "center"))
plot(mod_pls_sc)
summary(mod_pls_sc)
print(mod_pls_sc)

##############scale selbst oder preprocess (scale und center) ergibt den gleichen datensatz. Warum ergibt aber train mit caret und selbst trainiert einen unterschied?

###split data
set.seed(37)
trainIndex <- createDataPartition(mrg_tbl$SRsnails, p = .8, list = FALSE, times = 1)
trn_all <- mrg_tbl[ trainIndex,]
trn <- cbind(trn_all[, c(which(colnames(trn_all) %in% nm_ldr))],
             trn_all$SRsnails, trn_all$cat)
colnames(trn)[ncol(trn)-1] <- "SRsnails"
colnames(trn)[ncol(trn)] <- "cat"
tst_all  <- mrg_tbl[-trainIndex,]
tst <- cbind(tst_all[, c(which(colnames(tst_all) %in% nm_ldr))],
             tst_all$SRsnails, tst_all$cat)
colnames(tst)[ncol(tst)-1] <- "SRsnails"
colnames(tst)[ncol(tst)] <- "cat"

trn_scl_lst <- lapply(trn[,c(1:ncol(trn)-1)], function(i){
  print(str(i))
  scale(as.numeric(i), center = T, scale = T)
  })
trn_scl <- do.call(data.frame, trn_scl_lst)
trn_scl <- trn_scl[, c(1:54, 56:70, 72:ncol(trn_scl))]
trn_scl <- trn_scl[complete.cases(trn_scl),]
trn_scl <- cbind(trn_scl, trn[,ncol(trn)])


tst_scl_lst <- lapply(tst[,c(1:ncol(tst)-1)], function(i){
  print(str(i))
  scale(as.numeric(i), center = T, scale = T)
})
tst_scl <- do.call(data.frame, tst_scl_lst)
tst_scl <- cbind(tst_scl, tst[,ncol(tst)])

mod_pls <- train(trn_scl[, c(1:(ncol(trn_scl)-1))], trn_scl[,c(ncol(trn_scl))], method = "kernelpls")
plot(mod_pls)
prd <- predict(mod_pls, newdata=tst)

plot(prd~tst$SRsnails)
summary(mod_pls)

# ggplot(mrg_tbl, aes_string(x=j, y=i)) +
#   labs(x = j, y = i) +
#   geom_point(shape = 16)



trn <- trn[complete.cases(trn),]
predictors <- trn[, c(1:(ncol(trn)-2))]
response <- trn[,c(ncol(trn)-1)]




indx <- createFolds(trn$cat, k = 10, list = TRUE, returnTrain = TRUE)


###@hanna, warum funktioniert rf nicht
fitControl <- trainControl(
  ##10 fold cv
  method = "LOOCV",
  index = indx,
  ## repeated ten times
  #  repeats = 10,
  returnResamp = "all",
  savePredictions = TRUE)            

mod_rf <- train(predictors, response,  
                method = "rf", 
                trControl = fitControl, 
                verbose = FALSE, 
                tuneLength = 5, importance = T)#5 verschiedene mtrys
plot(mod_rf)

pred <- mod_rf$pred[mod_rf$pred$mtry==mod_rf$bestTune$mtry,]
plot(pred$obs,pred$pred)

#####################

dat <- mrg_tbl[complete.cases(mrg_tbl[,c(which(colnames(mrg_tbl) %in% nm_ldr))]),]
predictors <- dat[,c(which(colnames(dat) %in% nm_ldr))]
response <- dat$SRsnails




#indx <- createFolds(trn$cat, k = 10, list = TRUE, returnTrain = TRUE)


fitControl <- trainControl(
  ##10 fold cv
  method = "LOOCV",
  ## repeated ten times
  #  repeats = 10,
  returnResamp = "all",
  savePredictions = TRUE)            

mod_rf <- train(predictors, response,  
                method = "rf", 
                trControl = fitControl, 
                verbose = FALSE, 
                tuneLength = 5, importance = T)#5 verschiedene mtrys
plot(mod_rf)
mod_rf
pred <- mod_rf$pred[mod_rf$pred$mtry==mod_rf$bestTune$mtry,]
plot(pred$obs,pred$pred))

