# Description:
# Author: Alice Ziegler
# Date: 2018-11-22 20:16:26
# to do: ###!!!!! bei prediction: falls bei Wald zwei mal foc0 bei anderem dazugezogen wird, funktioniert conditional zeilen rauswerfen nicht
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#Packages:
library(plyr)
library(caret)

#Sources:
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "nov18_test/"
inpath_general <- "../data/" # only original files
inpath <- paste0("../data/", sub)
outpath <- paste0("../data/", sub)
########################################################################################
###Settings
########################################################################################
# set <- c("frst", "nofrst", "allplts")
set <- c("nofrst")
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
##########################################################################################################################################################

##read datasets
dat_ldr_mrg <- readRDS(paste0(inpath, "dat_ldr_mrg.rds"))

# create outs list (outer loop)
ind_nums <- sort(unique(dat_ldr_mrg$selID))
runs <- ind_nums[ind_nums>0]
cats <- unique(dat_ldr_mrg$cat)

rndm_draw <- c()
outs_lst_rw <- lapply(runs, function(k){
  out_sel <- dat_ldr_mrg[which(dat_ldr_mrg$selID == k),]
  miss <- cats[!(cats %in% out_sel$cat)]
  df_miss <- dat_ldr_mrg[dat_ldr_mrg$cat %in% as.vector(miss),]
  print(rndm_draw)
  if (length(rndm_draw) > 0){
    df_miss <- df_miss[-which(df_miss$plotID %in% rndm_draw),]
  }
  set.seed(k)
  out_miss <- ddply(df_miss, .(cat), function(x){
    x[sample(nrow(x), 1), ]
  })
  rndm_draw <<- c(rndm_draw, as.character(out_miss$plotID)) #<<- leider nötig, weil sonst rndm_draw außerhalb nciht verändert wird
  print(k)
  print(rndm_draw)
  out <- rbind(out_sel, out_miss)
  # outs_lst_rw <- append(outs_lst_rw, out) 
  })


outs_lst <- outs_lst_rw
for (j in runs){
  outs_lst[[j]] <- outs_lst_rw[[j]]$plotID
}

saveRDS(outs_lst, file = paste0(outpath, "outs_lst.rds"))


###cv index gleiches system wie outer loop (innerloop)
cvouts_lst <- lapply(seq(outs_lst), function(i){
  cvind_num <- runs[-which(runs == i)]
  cvouts_lst_rw <- lapply(cvind_num, function(k){
    out_sel <- dat_ldr_mrg[which(dat_ldr_mrg$selID == k),]
    miss <- cats[!(cats %in% out_sel$cat)]
    df_miss <- dat_ldr_mrg[dat_ldr_mrg$cat %in% as.vector(miss),]
    set.seed(k)
    out_miss <- ddply(df_miss, .(cat), function(x){
      x[sample(nrow(x), 1), ]
    })
    out <- rbind(out_sel, out_miss)
  })
  
  cvouts_lst_runs <- cvouts_lst_rw
  for (j in seq(cvouts_lst_rw)){
    cvouts_lst_runs[[j]] <- cvouts_lst_rw[[j]]$plotID
  }
  return(cvouts_lst_runs)
})

saveRDS(cvouts_lst, file = paste0(outpath, "cvouts_lst.rds"))


##112218###write function for residual calculation

for (o in set){
  if (o != "allplts"){
    if (o == "frst"){
      cat <- c("fer", "flm", "foc", "fod", "fpd", "fpo", "hom")
    }else if (o == "nofrst"){
      cat <- c("cof", "gra", "hel", "mai", "sav")
    }}
  
  tbl <- dat_ldr_mrg[which(dat_ldr_mrg$cat %in% cat),]
  
  # readRDS(file = paste0(outpath, "resp_prdct_elev.rds"))
    resp_prdct_elev <- lapply(c(which(colnames(tbl) == "SRmammals") : 
                                which(grepl("^sum_plant_N", colnames(tbl)))), 
                            function(i){
                              print(i)
                              # for (x in seq(runs)){
                              runs_prdct_elev <- lapply(seq(runs), function(x){
                                print(x)
                                out_plt <- outs_lst[[x]]
                                tbl_in <- tbl[-which(tbl$plotID %in% out_plt),]
                                tbl_out <- tbl[which(tbl$plotID %in% out_plt),]
                                
                                
                                cvIndex <- lapply(cvouts_lst[[x]], function(i){
                                  res <- which(!(tbl_in$plotID %in% i))
                                })
                                cvIndex_out <- lapply(cvouts_lst[[x]], function(i){
                                  res <- which((tbl_in$plotID %in% i))
                                })
                                
                                
                                tbl_res <- tbl_in
                                tbl_pred_res <- tbl_in
                                #ifelse abfrage, falls alle einträge 0 sind, wie bei trait summary
                                # print(colnames(tbl_in)[i])
                                if(length(unique(tbl_in[,i])) > 1){
                                  # resp_pls <- tbl_in[!is.na(tbl_in[,i]),i]
                                  # pred_pls <- data.frame(elevation_scale = tbl_in[!is.na(tbl_in[,i]),"elevation_scale"],
                                  #                        elevsq_scale = tbl_in[!is.na(tbl_in[,i]),"elevsq_scale"])
                                  resp_pls <- tbl_in[!is.na(tbl_in[,i]),i]
                                  pred_pls <- data.frame(elevation_scale = tbl_in[!is.na(tbl_in[,i]),"elevation_scale"],
                                                         elevsq_scale = tbl_in[!is.na(tbl_in[,i]),"elevsq_scale"])
                                  new_dat <- data.frame(elevation_scale = tbl_out[(tbl_out$selID == x|tbl_out$selID == 0),"elevation_scale"],
                                                        elevsq_scale = tbl_out[(tbl_out$selID == x|tbl_out$selID == 0),"elevsq_scale"])
                                  
                                  mod_pls_trn <- train(x = pred_pls, 
                                                       y = resp_pls, 
                                                       method = "pls", 
                                                       metric = "RMSE",
                                                       tuneGrid = expand.grid(ncomp = c(1,2)),
                                                       trControl = trainControl(method = "cv", index = cvIndex, indexOut = cvIndex_out))
                                  # newdat_pls <- pred_pls
                                  prdct_pls_trn <- predict(object = mod_pls_trn, newdata = new_dat)
                                  tbl_pred_elev <- data.frame(plotID = tbl_out[(tbl_out$selID == x|tbl_out$selID == 0),"plotID"], 
                                                              plotUnq = tbl_out[(tbl_out$selID == x|tbl_out$selID == 0), "plotUnq"], 
                                                              runs = x, 
                                                              selID = tbl_out[(tbl_out$selID == x|tbl_out$selID == 0), "selID"], 
                                                              prdct_elev = prdct_pls_trn)
                                  colnames(tbl_pred_elev)[which(colnames(tbl_pred_elev) == "prdct_elev")] <- colnames(tbl_in[i])
                                  return(tbl_pred_elev)
                                }else{
                                  tbl_pred_elev <- data.frame(plotID = tbl_out$plotID, 
                                                              plotUnq = tbl_out$plotUnq, 
                                                              runs = x, 
                                                              selID = tbl_out$selID, 
                                                              prdct_elev = NA)
                                  colnames(tbl_pred_elev)[which(colnames(tbl_pred_elev) == "prdct_elev")] <- colnames(tbl_in[i])
                                  return(tbl_pred_elev)
                                }
                              })
                              # df_runs_prdct_elev <- Reduce(function(x, y) merge(x, y, by="plotUnq"), runs_prdct_elev)
                              df_runs_prdct_elev <- do.call("rbind", runs_prdct_elev)
                            }) 

    saveRDS(resp_prdct_elev, file = paste0(outpath, "resp_prdct_elev", o, ".rds"))
    # resp_prdct_elev <- readRDS(file = paste0(outpath, "resp_prdct_elev", o, ".rds"))
    
  df_resp_prdct_elev <- do.call("cbind", resp_prdct_elev)
  # df_resp_prdct_elev[17, grepl("plotID", colnames(df_resp_prdct_elev))] ##check for examples if columns hafe the same order
  df_resp_prdct_elev <- df_resp_prdct_elev[, !duplicated(colnames(df_resp_prdct_elev))]
  df_resp_prdct_elev <- df_resp_prdct_elev[order(df_resp_prdct_elev$plotUnq),]
  
  saveRDS(df_resp_prdct_elev, paste0(outpath, "df_resp_prdct_elev_", o, ".rds"))
  
  tbl_res <- tbl[,c(which(colnames(tbl) == "plotID"), 
                    which(colnames(tbl) == "plotUnq"), 
                    which(colnames(tbl) == "SRmammals") : 
                      which(grepl("^sum_plant_N", colnames(tbl))))]
  for (i in colnames(tbl_res)[c(which(colnames(tbl_res) == "SRmammals") : 
                                which(grepl("^sum_plant_N", colnames(tbl_res))))]){
    tbl_res[,i] <- tbl[,i] - df_resp_prdct_elev[,i]
    colnames(tbl_res)[which(colnames(tbl_res) == i)] <- paste0("resid", i)
    saveRDS(tbl_res, paste0(outpath, "tbl_res_", o, ".rds"))
  }
  tbl_mrg <- merge(tbl, tbl_res, by = c("plotUnq", "plotID"))
  # colnames(tbl_mrg)[grepl("plot", colnames(tbl_mrg))]
  saveRDS(tbl_mrg, file = paste0(outpath, "tbl_mrg_", o, ".rds"))
}
  

#   tbl_res[!is.na(tbl_res[,i]),i] <- tbl_res[!is.na(tbl_res[,i]),i] - prdct_pls_trn ##was vorher NA war, bleibt auch jetz NA
#   colnames(tbl_res)[i] <- paste0("resid", colnames(tbl_res)[i])
#   tbl_pred_res[!is.na(tbl_pred_res[,i]),i] <- prdct_pls_trn ##was vorher NA war, bleibt auch jetzt NA
# }else{ ###noch anpassen
#   tbl_res[!is.na(tbl_res[,i]),i] <- NA ##was vorher NA war, bleibt auch jetz NA
#   colnames(tbl_res)[i] <- paste0("resid", colnames(tbl_res)[i])
#   tbl_pred_elev[!is.na(tbl_pred_elev[,i]),i] <- NA ##was vorher NA war, bleibt auch jetz NA
# }


### cv Index ausschreiben
### SR berechnen
### Datensatz für frst, nofrst, allplts ausschreiben





### ...
### 1_model_run anpassen und so verändern, dass je nach flag forest/allplots anderer Datensatz eingeladen wird. 
### Rest in 1_model anpassen und nofrst laufen lassen 




###################################################################################################

###################################################################################################
####Residuals
# tbl_res <- tbl
# tbl_pred_res <- tbl
# #ifelse abfrage, falls alle einträge 0 sind, wie bei trait summary
# for (i in c(which(colnames(tbl) == "SRmammals") : ncol(tbl))){
#   print(colnames(tbl)[i])
#   if(length(unique(tbl[,i])) > 1){
#     resp_pls <- tbl[!is.na(tbl[,i]),i]
#     pred_pls <- data.frame(elevation_scale = tbl[!is.na(tbl[,i]),"elevation_scale"],
#                            elevsq_scale = tbl[!is.na(tbl[,i]),"elevsq_scale"])
#     mod_pls_trn <- train(x = pred_pls, 
#                          y = resp_pls, 
#                          method = "pls", 
#                          metric = "RMSE",
#                          tuneGrid = expand.grid(ncomp = c(1,2)),
#                          trControl = trainControl(method = "cv", index = Index, indexOut = Index_out))
#     # newdat_pls <- pred_pls
#     prdct_pls_trn <- predict(object = mod_pls_trn, newdata =  pred_pls)
#     
#     tbl_res[!is.na(tbl_res[,i]),i] <- tbl_res[!is.na(tbl_res[,i]),i] - prdct_pls_trn ##was vorher NA war, bleibt auch jetz NA
#     colnames(tbl_res)[i] <- paste0("resid", colnames(tbl_res)[i])
#     tbl_pred_res[!is.na(tbl_pred_res[,i]),i] <- prdct_pls_trn ##was vorher NA war, bleibt auch jetzt NA
#   }else{
# #     tbl_res[!is.na(tbl_res[,i]),i] <- NA ##was vorher NA war, bleibt auch jetz NA
# #     colnames(tbl_res)[i] <- paste0("resid", colnames(tbl_res)[i])
# #     tbl_pred_res[!is.na(tbl_pred_res[,i]),i] <- NA ##was vorher NA war, bleibt auch jetz NA
# #   }
# #   
# # }
# # tbl_res <- tbl_res[,-which(colnames(tbl_res) %in% c("selID", "cat", "elevation", "elevation_scale", "elevsq", "elevsq_scale"))]
# tbl_res <- tbl_res[,-c(which(colnames(tbl_res) == "selID") : 
#                          which(colnames(tbl_res) == "lui"))]
# tbl_pred_res <- tbl_pred_res[,-c(which(colnames(tbl_pred_res) == "selID") : 
#                                    which(colnames(tbl_pred_res) == "lui"))]
# saveRDS(tbl_res, file = paste0(outpath, "tbl_res.rds"))
# saveRDS(tbl_pred_res, file = paste0(outpath, "tbl_pred_res.rds"))
