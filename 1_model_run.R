# Description:
# Author: Alice Ziegler
# Date: 2018-02-09 15:05:58
###to do: tbl_scal beinhaltet nur skalierte Werte der tats?chichen prediktoren, der Rest wir
### unskaliert weitergezogen! Das ist unsch?n und fehleranf?llig, falls versehentlich mal
### ein nicht geplanter predictor ins Modell kommt. Aber: Ich m?chte nicht oben schon einen
### zweiten aussortierten df erstellen und weil in Spalten wie selID auch Zahlen stehen k?nnen
### nicht einfach alle Zahlen skaliert werden!
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#Packages:
library(caret)
library(CAST)
library(doParallel)
library(foreach)
library(parallel)
library(plyr)
#setwd for folder with THIS script (only possible within Rstudio)
# setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
setwd("/mnt/sd19006/data/users/aziegler/src")
sub <- "nov18/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../data/", sub)

########################################################################################
###Settings
########################################################################################

###DOCUMENTATION options
#comment for explenatory filename
comm <- "allplts_noelevelev2_cvindex"
# comm <- "nofrst_noelevelev2_cvindex"
all_plts <- T
frst <- F # set true if model should only be done for forested plots
cl <- 19
###
#DATAFRAME manipulation
###
###choose dataframe and load dataframe
tbl_nm <- "dat_ldr_mrg.rds"

##read datasets
outs_lst <- readRDS(paste0(inpath, "outs_lst.rds"))
dat_ldr_mrg <- readRDS(paste0(inpath, tbl_nm))

###crop table to clumns that could at all be relavant
tbl <- dat_ldr_mrg[,c(which(colnames(dat_ldr_mrg) == "plotID") :
                        which(colnames(dat_ldr_mrg) == "lat"),
                      which(colnames(dat_ldr_mrg) == "plotUnq"),
                      which(colnames(dat_ldr_mrg) == "lui"),
                      which(colnames(dat_ldr_mrg) == "SRmammals") :
                        which(grepl("^sum_plant_N", colnames(dat_ldr_mrg))),
                      which(colnames(dat_ldr_mrg) == "AGB"),
                      which(colnames(dat_ldr_mrg) == "BE_FHD") :
                        which(colnames(dat_ldr_mrg) == "BE_PR_55"),
                      which(colnames(dat_ldr_mrg) == "BE_PR_REG") :
                        which(colnames(dat_ldr_mrg) == "LAI"),
                      which(colnames(dat_ldr_mrg) == "chm_surface_ratio"),
                      which(colnames(dat_ldr_mrg) == "pulse_returns_max") :
                        which(colnames(dat_ldr_mrg) == "pulse_returns_mean"),
                      which(colnames(dat_ldr_mrg) == "pulse_returns_sd"),
                      which(colnames(dat_ldr_mrg) == "vegetation_coverage_01m") :
                        which(colnames(dat_ldr_mrg) == "vegetation_coverage_10m"),
                      which(colnames(dat_ldr_mrg) == "mdn_rtrn"),
                      which(colnames(dat_ldr_mrg) == "sd_rtrn_1"),
                      which(colnames(dat_ldr_mrg) == "gap_frac"))]

##add some columns that are needed later
cats <- unique(tbl$cat)
###

########################################data.frame tbl wird hier tats?chlich zugeschnitten.
### weil sonst scaling ein Problem ist. Man will ja schlie?lich nur ?ber die Wald/nichtwald
### plots scalen. Und nicht ?ber alle!
###Au?erdem ist das rausschmei?en wenn 50% der Daten gleich sind auch nur auf dem tats?chlich
### ins Modell eingehenden Datensatz sinnvoll

if (all_plts == F){
  if (frst == T){
    cat <- c("fer", "flm", "foc", "fod", "fpd", "fpo", "hom")
  }else if (frst == F){
    cat <- c("cof", "gra", "hel", "mai", "sav")
  }
  tbl <- tbl[which(tbl$cat %in% cat),]
}

###choose predictors, responses and meta data
nm_meta <- c("plotID", "selID", "cat", "plotUnq")

#^ and $ means only to look for this expression and not for resid_SRmammals
nm_resp <- colnames(tbl)[c(seq(grep("^SRmammals$", names(tbl)), grep("^SRsnails$", names(tbl))),
                           seq(grep("^SRrosids$", names(tbl)), grep("^SRmagnoliids$", names(tbl))),
                           seq(grep("residSRmammals", names(tbl)), grep("residSRsnails", names(tbl))),
                           seq(grep("residSRrosids", names(tbl)), grep("residSRmagnoliids", names(tbl))),
                           seq(grep("residsum_predator_N", names(tbl)), grep("residsum_plant_N", names(tbl))), 
                           seq(grep("^sum_predator_N", names(tbl)), grep("^sum_plant_N", names(tbl))))]
# nm_resp <- "SRmammals"
nm_pred_all <- colnames(tbl)[c(which(colnames(tbl) %in% "AGB"),
                               which(colnames(tbl) %in% "BE_FHD") : which(colnames(tbl) %in% "BE_PR_55"),
                               which(colnames(tbl) %in% "BE_PR_REG") : which(colnames(tbl) %in% "LAI"),
                               which(colnames(tbl) %in% "chm_surface_ratio"),
                               which(colnames(tbl) %in% "pulse_returns_max") : which(colnames(tbl) %in% "pulse_returns_mean"),
                               which(colnames(tbl) %in% "pulse_returns_sd"),
                               which(colnames(tbl) %in% "vegetation_coverage_01m") : which(colnames(tbl) %in% "vegetation_coverage_10m"),
                               which(colnames(tbl) %in% "mdn_rtrn"),
                               which(colnames(tbl) %in% "sd_rtrn_1"),
                               which(colnames(tbl) %in% "gap_frac")#,
                               # which(colnames(tbl) %in% "elevation"),
                               # which(colnames(tbl) %in% "elevsq")
                               # which(colnames(tbl) %in% "lui")
)]
nm_pred <- nm_pred_all
for (i in nm_pred_all){
  frq <- table(tbl[i])
  if (max(frq) > floor(nrow(tbl) * 0.5)){
    nm_pred <- nm_pred[!(nm_pred == i)]
  }
}

###
#CONTROL parameters
###
runs <- seq(length(outs_lst))
type <- "ffs"
method <- "pls" # or other caret methods


modDir <- paste0(outpath, Sys.Date(), "_", type, "_", method, comm)
if (file.exists(modDir)==F){
  dir.create(file.path(modDir))
}
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################

###create outs list
# outs_lst <- lapply(ind_nums, function(k){
#   out_sel <- tbl[which(tbl$selID == k),]
#   miss <- cats[!(cats %in% out_sel$cat)]
#   df_miss <- tbl[tbl$cat %in% as.vector(miss),]
#   set.seed(k)
#   out_miss <- ddply(df_miss, .(cat), function(x){
#     x[sample(nrow(x), 1), ]
#   })
#   out <- rbind(out_sel, out_miss)
# })
# save(outs_lst, file = paste0(modDir, "/outs_lst.RData"))

###scaling nm_pred columns and writing whole df
scl_lst <- lapply(colnames(tbl), function(m){
  if(m %in% nm_pred){
    if (class(tbl[,m]) == "numeric"){
      scale(tbl[,m], center = T, scale = T)
    }else if (class(tbl[,m]) == "integer"){
      scale(as.numeric(tbl[,m]), center = T, scale = T)
    }else{
      tbl[,m] <- tbl[,m] #non numeric or integer columns stay as they are
    }
  }else{
    tbl[,m] <- tbl[,m] #non numeric or integer columns stay as they are}
  }})
tbl_scl <- do.call(data.frame, scl_lst)
colnames(tbl_scl) <- colnames(tbl)

saveRDS(tbl_scl, file = paste0(modDir, "/tbl_scl.rds"))



registerDoParallel(cl)
# i <- "SRmoths"
model <- foreach(i = nm_resp, .errorhandling = "remove", .packages=c("caret", "CAST", "plyr"))%dopar%{ ###all
  #x <- 5
  for (x in seq(runs)){
    # print(j)
    out_plt <- outs_lst[[x]]$plotID
    tbl_in <- tbl_scl[-which(tbl_scl$plotID %in% out_plt),]
    tbl_out <- tbl_scl[which(tbl_scl$plotID %in% out_plt),]
    
    # ##cv von jeder landuseclass eines, aber zuf?llige Wahl der indices und n mal wiederholt
    # cvouts_lst <- lapply(seq(1:50), function(k){
    #   set.seed(k)
    #   out_sel <- ddply(tbl_in, .(cat), function(x){
    #     x[sample(nrow(x), 1), ]
    #   })
    #   out <- rbind(out_sel)
    # })
    # cvIndex <- lapply(cvouts_lst, function(i){
    #   res <- which(!(tbl_in$plotID %in% i$plotID))
    # })
    # cvIndex_out <- lapply(cvouts_lst, function(i){# #######wie ?bergeben
    #   res <- which((tbl_in$plotID %in% i$plotID))
    # })
    

    ###cv index gleiches system wie outer loop
    cvind_num <- unique(sort(tbl_in$selID))
    cvind_num <- cvind_num[which(cvind_num >0)]
    cvouts_lst <- lapply(cvind_num, function(k){
      out_sel <- tbl_in[which(tbl_in$selID == k),]
      miss <- cats[!(cats %in% out_sel$cat)]
      df_miss <- tbl_in[tbl_in$cat %in% as.vector(miss),]
      set.seed(k)
      out_miss <- ddply(df_miss, .(cat), function(x){
        x[sample(nrow(x), 1), ]
      })
      out <- rbind(out_sel, out_miss)
    })
    cvIndex <- lapply(cvouts_lst, function(i){
      res <- which(!(tbl_in$plotID %in% i$plotID))
    })
    cvIndex_out <- lapply(cvouts_lst, function(i){# #######wie übergeben
      res <- which((tbl_in$plotID %in% i$plotID))
    })

    
    
    # if (type == "rfe"){
    #   mod <- rfe(pred, resp, method = method,
    #              rfeControl = rfe_cntrl, tuneLength = tuneLength)
    # }else if (type == "ffs"){
    mod <- ffs(tbl_in[,nm_pred], tbl_in[,which(colnames(tbl_in) == i)],
               method = "pls",
               tuneGrid = expand.grid(ncomp = c(1:7)), #:5, 10, 15, 20, 25, 30, 34)),
               metric = "RMSE",
               trControl = trainControl(method = "cv", index = cvIndex, indexOut = cvIndex_out),
               verbose = T)
    # mod <- get(load(file = paste0("../data/sep18/2018-09-24_ffs_pls_cv_allplots_",
    #                               "only_moths_RMSE_elev_dstrb_elevsq_plsresid/",
    #                               "indv_model_run5_ffs_pls_SRmoths.RData")))
    
    # ###checking input
    # out_plt
    # data <- tbl_in[,nm_pred]
    # cvIndex
    # cvIndex_out
    # tbl_in$plotID
    
    nm <- gsub("_", "", i)
    
    save(mod, file = paste0(modDir, "/indv_model_run", x, "_", type, "_", method, "_",
                            nm, ".RData"))
    print(paste0("DONE: ", modDir, "model", type, "_", method, "_",
                 i, "___________run", x))
    
  }
  
}



save(nm_pred, file = paste0(modDir, "/nm_pred.RData"))
save(nm_resp, file = paste0(modDir, "/nm_resp.RData"))

