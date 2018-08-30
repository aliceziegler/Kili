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

#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "aug18/2018-08-24_ffs_pls_cv_noForest_alpha_elev_dstrb/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../out/", sub)
if (file.exists(outpath)==F){
  dir.create(file.path(outpath), recursive = T)
}

###############################################
#varimp_df <- readRDS(paste0(outpath, "varimp_df.rds"))
nm_pred <- get(load(paste0(inpath, "nm_pred.RData")))
#pred_grp <- data.frame(pred = varimp_df$pred)
pred_grp <- data.frame(pred = nm_pred)
pred_grp$grp <- NA

#group by topic
for (i in seq(nrow(pred_grp))){

  if(pred_grp$pred[i] %in% c("BE_ELEV_ASPECT", "BE_ELEV_SLOPE", "dtm_aspect_mean", "dtm_aspect_reg",
                             "dtm_aspect_unweighted_mean", "dtm_elevation_sd", "dtm_slope_max",
                             "dtm_slope_mean", "dtm_slope_min", "dtm_slope_reg", "dtm_slope_sd",
                             "dtm_surface_ratio")){
    pred_grp$grp[i] <- "ELEV"
  }else if(pred_grp$pred[i] %in% c("AGB", "BE_FHD", "gap_frac", "LAI", "LAI.x", "LAI.y", "elevation", "dstrb")){
    pred_grp$grp[i] <- as.character(pred_grp$pred[i])
  }else if(pred_grp$pred[i] %in% c("BE_PR_01", "BE_PR_02", "BE_PR_03", "BE_PR_04", "BE_PR_05", "BE_PR_06",
                                   "BE_PR_07", "BE_PR_08", "BE_PR_09", "BE_PR_10", "BE_PR_11", "BE_PR_12",
                                   "BE_PR_13", "BE_PR_14", "BE_PR_15", "BE_PR_16", "BE_PR_17", "BE_PR_18",
                                   "BE_PR_19", "BE_PR_20", "BE_PR_21", "BE_PR_22", "BE_PR_23", "BE_PR_24", 
                                   "BE_PR_25", "BE_PR_26", "BE_PR_27", "BE_PR_28", "BE_PR_29", "BE_PR_30", 
                                   "BE_PR_31", "BE_PR_32", "BE_PR_33", "BE_PR_34", "BE_PR_35", "BE_PR_36", 
                                   "BE_PR_37", "BE_PR_38", "BE_PR_39", "BE_PR_40", "BE_PR_41", "BE_PR_42", 
                                   "BE_PR_43", "BE_PR_44", "BE_PR_45", "BE_PR_46", "BE_PR_47", "BE_PR_48", 
                                   "BE_PR_49", "BE_PR_50", "BE_PR_51", "BE_PR_52", "BE_PR_53", "BE_PR_54", 
                                   "BE_PR_55", "BE_PR_REG", "BE_PR_UND")){
    pred_grp$grp[i] <- "PR"
  }else if(pred_grp$pred[i] %in% c("BE_RD_01", "BE_RD_02", "BE_RD_03", "BE_RD_04", "BE_RD_05", "BE_RD_06",
                                   "BE_RD_07", "BE_RD_08", "BE_RD_09", "BE_RD_10", "BE_RD_11", "BE_RD_12",
                                   "BE_RD_13", "BE_RD_14", "BE_RD_15", "BE_RD_16", "BE_RD_17", "BE_RD_18",
                                   "BE_RD_19", "BE_RD_20", "BE_RD_21", "BE_RD_22", "BE_RD_23", "BE_RD_24",
                                   "BE_RD_25", "BE_RD_26", "BE_RD_27", "BE_RD_28", "BE_RD_29", "BE_RD_30",
                                   "BE_RD_31", "BE_RD_32", "BE_RD_33", "BE_RD_34", "BE_RD_35", "BE_RD_36",
                                   "BE_RD_37", "BE_RD_37", "BE_RD_38", "BE_RD_39", "BE_RD_40", "BE_RD_41", 
                                   "BE_RD_42", "BE_RD_43", "BE_RD_44", "BE_RD_45", "BE_RD_46", "BE_RD_47", 
                                   "BE_RD_48", "BE_RD_49", "BE_RD_50", "BE_RD_51", "BE_RD_52", "BE_RD_53", 
                                   "BE_RD_54", "BE_RD_55", "BE_RD_REG", "BE_RD_UND", "BE_RD_CAN", "BE_RD_GND")){
    pred_grp$grp[i] <- "RD"
  }else if(pred_grp$pred[i] %in% c("BE_H_KURTOSIS", "BE_H_P10", "BE_H_P20", "BE_H_P30", "BE_H_P40", "BE_H_P50", 
                                   "BE_H_P60", "BE_H_P70", "BE_H_P80", "BE_H_P90", "BE_H_P100", "BE_H_SKEW", 
                                   "BE_H_VAR","BE_H_VAR_COEF", "BE_H_MAX", "BE_H_MEAN", "BE_H_MEDIAN", "BE_H_SD", 
                                   "chm_height_max", "chm_height_mean", "chm_height_sd", "chm_surface_ratio", 
                                   "TCH")){
    pred_grp$grp[i] <- "VEG_H"
  }else if(pred_grp$pred[i] %in% c("max_rtrn", "mdn_rtrn", "pulse_returns_max", "pulse_returns_mean",
                                   "pulse_returns_sd", "sd_nmbr_rtrn", "sd_rtrn_1")){
    pred_grp$grp[i] <- "RTRN"
  }else if(pred_grp$pred[i] %in% c("vegetation_coverage_01m", "vegetation_coverage_02m", "vegetation_coverage_05m",
                                   "vegetation_coverage_10m", "chm_surface_ratio")){
    pred_grp$grp[i] <- "VEG_COV"
  }
}
saveRDS(pred_grp, file = paste0(inpath, "../pred_grp.RDS"))

# ##group by layer
# 
# for (i in seq(nrow(pred_grp))){
#   
#   if(pred_grp$pred[i] %in% c("BE_ELEV_ASPECT", "BE_ELEV_SLOPE", "dtm_aspect_mean", "dtm_aspect_reg", 
#                              "dtm_aspect_unweighted_mean", "dtm_elevation_sd", "dtm_slope_max", 
#                              "dtm_slope_mean", "dtm_slope_min", "dtm_slope_reg", "dtm_slope_sd", 
#                              "dtm_surface_ratio")){
#     pred_grp$grp[i] <- "ELEV"
#   }else if(pred_grp$pred[i] %in% c("AGB", "BE_FHD", "gap_frac", "LAI", "LAI.x", "LAI.y", 
#                                    "vegetation_coverage_01m", "vegetation_coverage_05m", 
#                                    "vegetation_coverage_10m")){
#     pred_grp$grp[i] <- "BIO"
#   }else if(pred_grp$pred[i] %in% c("BE_H_KURTOSIS", "BE_H_P10", "BE_H_P30", "BE_H_P90", "BE_H_SKEW", "BE_H_VAR", 
#                                    "BE_H_VAR_COEF")){
#     pred_grp$grp[i] <- "VEG_H"
#   }else if(pred_grp$pred[i] %in% c("BE_PR_01", "BE_PR_02", "BE_PR_03", "BE_PR_04", "BE_PR_05", "BE_PR_06", 
#                                    "BE_PR_07", "BE_PR_08", "BE_PR_09", "BE_PR_10", "BE_PR_11", "BE_PR_12", 
#                                    "BE_PR_13", "BE_PR_14", "BE_PR_15", "BE_PR_16", "BE_PR_17", "BE_PR_18", 
#                                    "BE_PR_19", "BE_PR_20", "BE_PR_21", "BE_PR_22", "BE_PR_23", "BE_PR_25", 
#                                    "BE_PR_26", "BE_PR_27", "BE_PR_28", "BE_PR_29", "BE_PR_30", "BE_PR_32", 
#                                    "BE_PR_33", "BE_PR_34", "BE_PR_CAN", "BE_PR_REG", "BE_PR_UND", 
#                                    "chm_height_max", "chm_height_mean", "chm_height_sd", "chm_surface_ratio", 
#                                    "TCH")){
#     pred_grp$grp[i] <- "PR"
#   }else if(pred_grp$pred[i] %in% c("BE_RD_01", "BE_RD_02", "BE_RD_03", "BE_RD_04", "BE_RD_05", "BE_RD_06", 
#                                    "BE_RD_07", "BE_RD_08", "BE_RD_09", "BE_RD_10", "BE_RD_11", "BE_RD_12", 
#                                    "BE_RD_13", "BE_RD_14", "BE_RD_15", "BE_RD_16", "BE_RD_17", "BE_RD_18", 
#                                    "BE_RD_19", "BE_RD_20", "BE_RD_21", "BE_RD_22", "BE_RD_23", "BE_RD_24", 
#                                    "BE_RD_25", "BE_RD_26", "BE_RD_27", "BE_RD_28", "BE_RD_29", "BE_RD_30", 
#                                    "BE_RD_31", "BE_RD_32", "BE_RD_33", "BE_RD_34", "BE_RD_35", "BE_RD_36", 
#                                    "BE_RD_37", "BE_RD_REG", "BE_RD_UND")){
#     pred_grp$grp[i] <- "RD"
#   }else if(pred_grp$pred[i] %in% c("BE_H_KURTOSIS", "BE_H_P10", "BE_H_P30", "BE_H_P90", "BE_H_SKEW", "BE_H_VAR", 
#                                    "BE_H_VAR_COEF")){
#     pred_grp$grp[i] <- "VEG_H"
#   }else if(pred_grp$pred[i] %in% c("max_rtrn", "mdn_rtrn", "pulse_returns_max", "pulse_returns_mean", 
#                                    "pulse_returns_sd", "sd_nmbr_rtrn", "sd_rtrn_1")){
#     pred_grp$grp[i] <- "RTRN"
#   }
# }
# saveRDS(pred_grp, file = paste0(inpath, "pred_grp.RDS"))
