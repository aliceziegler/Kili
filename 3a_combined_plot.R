rm(list=ls())
#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))


###packages
library(caret)
library(ggplot2)
library(scales)

#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
# sub <- "sep18/2018-09-11_ffs_pls_cv_noForest_alpha_all_RMSE/"
sub <- "sep18/2018-09-14_ffs_pls_cv_allplots_allalpha_RMSE_elev_dstr_elevsq_plsresid/"
all_plts <- T
inpath_general <- "../data/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../out/", sub)
if (file.exists(outpath)==F){
  dir.create(file.path(outpath), recursive = T)
}

# path_nofrst <- "../out/sep18/2018-09-07_ffs_pls_cv_noForest_alpha_all_RMSE/"

mrg_tbl <- get(load("../data/sep18/dat_ldr_mrg.RData"))
obs_smmry <- readRDS(file = paste0(inpath, "obs_smmry.rds"))


if (all_plts == F){
  if(length(grep("_only", sub))){
    frst <- T
  }else{
    frst <- F
  }
}

# resp_vec <- c("SRheteroptera")
# resp <- resp_vec[1]

resp_vec <- colnames(mrg_tbl)[c(which(colnames(mrg_tbl) == "SRmammals") : 
                                  which(colnames(mrg_tbl) == "SRmagnoliids"), 
                                which(colnames(mrg_tbl) == "sum_generalist_N3") : 
                                  which(colnames(mrg_tbl) == "sum_plant_N9"))]


##read data
gam_resid <- readRDS(file = paste0(outpath, "gam_resid.rds"))
gam_prdct_cv_df <- readRDS(file = paste0(outpath, "gam_prdct_cv_df.rds"))
gam_prdct_df <- readRDS(paste0(outpath, "gam_prdct_df.rds"))
prdct_df <- readRDS(file = paste0(outpath, "prdct_df.rds"))
prdct_df_all <- readRDS(file = paste0(outpath, "prdct_df_all.rds"))
trophic_tbl <- get(load(paste0(inpath_general, "trophic_tbl.RData")))
pls_elevsq_prdct_cv_df <- readRDS(file = paste0(outpath, "pls_elevsq_prdct_cv_df.rds"))


stats_all_lst <- lapply(resp_vec, function(resp){
  if(sum(grepl(paste0("^", resp), colnames(prdct_df))) > 0){
  print(resp)
  ##crop data on resp
  ##obs SR (forest and non-forest)
  obs_SRresp <- mrg_tbl[,c(which(colnames(mrg_tbl) %in% c("plotID", "plotUnq")), 
                            which(grepl(paste0("^", resp), colnames(mrg_tbl))))]
  ##obs resid (forest and non_forest)
  obs_residresp <- mrg_tbl[,c(which(colnames(mrg_tbl) %in% c("plotID", "plotUnq")), 
                               which(grepl(paste0("resid", resp), colnames(mrg_tbl))))]
  
  ##pls SR
  pls_SRresp <- prdct_df[,c(which(colnames(prdct_df) %in% c("plotID", "plotUnq")), 
                             which(grepl(paste0("^", resp), colnames(prdct_df))))]
  
  ##pls resid
  pls_residresp <- prdct_df[,c(which(colnames(prdct_df) %in% c("plotID", "plotUnq")), 
                                which(grepl(paste0("resid", resp), colnames(prdct_df))))]
  
  ## pls SR all (no cv) but only non-forest
  pls_SRresp_all <- prdct_df_all[,c(which(colnames(prdct_df_all) %in% c("plotID", "plotUnq")), 
                                     which(grepl(paste0("^", resp), colnames(prdct_df_all))))]
  
  ## pls resid all (no_cv) - but only non-forest
  pls_residresp_all <- prdct_df_all[,c(which(colnames(prdct_df_all) %in% c("plotID", "plotUnq")), 
                                        which(grepl(paste0("resid", resp), colnames(prdct_df_all))))]                              
  ##gam cv 
  gam_cv_SRresp <- gam_prdct_cv_df[, c(which(colnames(gam_prdct_cv_df) %in% c("plotID", "plotUnq")), 
                                        which(grepl(resp, colnames(gam_prdct_cv_df))))]
  
  ##pls_elevsq cv
  pls_elevsq_cv_SRresp <- pls_elevsq_prdct_cv_df[, c(which(colnames(pls_elevsq_prdct_cv_df) %in% c("plotID", "plotUnq")), 
                                                     which(grepl(resp, colnames(pls_elevsq_prdct_cv_df))))]
  
  ##gam SR
  gam_SRresp <- gam_prdct_df[,c(which(colnames(gam_prdct_df) %in% c("plotID", "plotUnq")), 
                                 which(grepl(resp, colnames(gam_prdct_df))))]
  
  ## gam + resid
  gam_resid_resp <- gam_resid[,c(which(colnames(gam_resid) %in% c("plotID", "plotUnq")), 
                                  which(grepl(resp, colnames(gam_resid))))]
  
  ##mean N SR - only calculated from frst/nnon-frst plots
  mn_N_SR <- obs_smmry[which(rownames(obs_smmry) == resp),"meanN_perplot"] 
  
  ##mean N resid - only calculated from frst/nnon-frst plots
  mn_N_resid <- obs_smmry[which(rownames(obs_smmry) == paste0("resid", resp)),"meanN_perplot"]
  
  ##sd SR
  sd_resp_SR <- obs_smmry[which(rownames(obs_smmry) == resp),"sd_per_resp"]
  
  #sdresid
  sd_resp_resid <- obs_smmry[which(rownames(obs_smmry) == paste0("resid", resp)),"sd_per_resp"]
  
  #######################
  ###stats calculations
  #######################
    ##SR_pls ~ obs
  runs <- c(paste0(resp, "_run1"), paste0(resp, "_run2"), paste0(resp, "_run3"), 
            paste0(resp, "_run4"), paste0(resp, "_run5"))
  
  stats_pls <- lapply(runs, function(i){
    # stat <- as.data.frame(t(postResample(pls_SRresp[,i], obs_SRresp[,resp])))
    R2 <- caret::R2(pls_SRresp[,i], obs_SRresp[,resp], na.rm = T)
    RMSE <- caret::RMSE(pls_SRresp[,i], obs_SRresp[,resp], na.rm = T)
    RMSE_sd <- RMSE / sd_resp_SR
    return(list(RMSE_sd = RMSE_sd, RMSE = RMSE, R2 = R2))
  })
  stats_pls <- data.frame(do.call("rbind", stats_pls))
  # stats_pls_pst <- do.call("rbind", stats_pls[,"stat"])
  stats_pls$type <- "pls"
  # stats_pls_RMSE_sd <- data.frame(RMSE_sd = do.call("rbind", stats_pls[,"RMSE_sd"]))
  # stats_pls_RMSE_sd$type <- "pls"
  
  ##resid_pls ~ obs
  runs_res <- c(paste0("resid", resp, "_run1"), 
                paste0("resid", resp, "_run2"), paste0("resid", resp, "_run3"), 
                paste0("resid", resp, "_run4"), paste0("resid", resp, "_run5"))
  stats_res <- lapply(runs_res, function(i){
    # stat <- as.data.frame(t(postResample(pls_residresp[,i], obs_residresp[,paste0("resid", resp)])))
    # RMSE_sd <- stat[["RMSE"]] / sd_resp_resid
    R2 <- caret::R2(pls_residresp[,i], obs_residresp[,paste0("resid", resp)], na.rm = T)
    RMSE <- caret::RMSE(pls_residresp[,i], obs_residresp[,paste0("resid", resp)], na.rm = T)
    RMSE_sd <- RMSE / sd_resp_SR
    return(list(RMSE_sd = RMSE_sd, RMSE = RMSE, R2 = R2))
  })
  stats_res <- data.frame(do.call("rbind", stats_res))
  # stats_res_pst <- do.call("rbind", stats_res[,"stat"])
  stats_res$type <- "res"
  # stats_res_RMSE_sd <- data.frame(RMSE_sd = do.call("rbind", stats_res[,"RMSE_sd"]))
  # stats_res_RMSE_sd$type <- "res"
  
  ##gam_cv ~ obs
  runs_gam_cv <- c(paste0("prd_", resp, "_run_1"), paste0("prd_", resp, "_run_2"), 
                   paste0("prd_", resp, "_run_3"), paste0("prd_", resp, "_run_4"), 
                   paste0("prd_", resp, "_run_5"))
  stats_gam_cv <- lapply(runs_gam_cv, function(i){
    # stat <- as.data.frame(t(postResample(gam_cv_SRresp[,which(colnames(gam_cv_SRresp) == i)], 
    #                                      obs_SRresp[which(obs_SRresp$plotID %in% gam_cv_SRresp$plotID),
    #                                                  grepl(substr(i,5, (nchar(i)-6)), colnames(obs_SRresp))])))
    
    R2 <- caret::R2(gam_cv_SRresp[,which(colnames(gam_cv_SRresp) == i)], 
                    obs_SRresp[which(obs_SRresp$plotID %in% gam_cv_SRresp$plotID),
                               grepl(substr(i,5, (nchar(i)-6)), colnames(obs_SRresp))], na.rm = T)
    RMSE <- caret::RMSE(gam_cv_SRresp[,which(colnames(gam_cv_SRresp) == i)], 
                        obs_SRresp[which(obs_SRresp$plotID %in% gam_cv_SRresp$plotID),
                                   grepl(substr(i,5, (nchar(i)-6)), colnames(obs_SRresp))], na.rm = T)
    RMSE_sd <- RMSE / sd_resp_SR
    # df <- data.frame(pred = gam_cv_SRresp[,which(colnames(gam_cv_SRresp) == i)], 
    #                  obs = obs_SRresp[which(obs_SRresp$plotID %in% gam_cv_SRresp$plotID), 
    #                                   grepl(substr(i,5, (nchar(i)-6)), colnames(obs_SRresp))])
    # 
    # RMSE_sd <- stat[["RMSE"]] / sd_resp_SR
    return(list(RMSE_sd = RMSE_sd, RMSE = RMSE, R2 = R2))
  })
  stats_gam_cv <- data.frame(do.call("rbind", stats_gam_cv))
  # stats_gam_cv_pst <- do.call("rbind", stats_gam_cv[,"stat"])
  stats_gam_cv$type <- "gam_cv"
  # stats_gam_cv_RMSE_sd <- data.frame(RMSE_sd = do.call("rbind", stats_gam_cv[,"RMSE_sd"]))
  # stats_gam_cv_RMSE_sd$type <- "gam_cv"
  
  
  #pls_elevsq ~ obs
 # pls_elevsq_cv_SRresp
  
  runs_pls_elevsq_cv <- c(paste0("prd_", resp, "_run_1"), paste0("prd_", resp, "_run_2"), 
                   paste0("prd_", resp, "_run_3"), paste0("prd_", resp, "_run_4"), 
                   paste0("prd_", resp, "_run_5"))
  stats_pls_elevsq_cv <- lapply(runs_pls_elevsq_cv, function(i){
    # stat <- as.data.frame(t(postResample(gam_cv_SRresp[,which(colnames(gam_cv_SRresp) == i)], 
    #                                      obs_SRresp[which(obs_SRresp$plotID %in% gam_cv_SRresp$plotID),
    #                                                  grepl(substr(i,5, (nchar(i)-6)), colnames(obs_SRresp))])))
    
    R2 <- caret::R2(pls_elevsq_cv_SRresp[,which(colnames(pls_elevsq_cv_SRresp) == i)], 
                    obs_SRresp[which(obs_SRresp$plotID %in% pls_elevsq_cv_SRresp$plotID),
                               grepl(substr(i,5, (nchar(i)-6)), colnames(obs_SRresp))], na.rm = T)
    RMSE <- caret::RMSE(pls_elevsq_cv_SRresp[,which(colnames(pls_elevsq_cv_SRresp) == i)], 
                        obs_SRresp[which(obs_SRresp$plotID %in% pls_elevsq_cv_SRresp$plotID),
                                   grepl(substr(i,5, (nchar(i)-6)), colnames(obs_SRresp))], na.rm = T)
    RMSE_sd <- RMSE / sd_resp_SR
    # df <- data.frame(pred = pls_elevsq_cv_SRresp[,which(colnames(pls_elevsq_cv_SRresp) == i)], 
    #                  obs = obs_SRresp[which(obs_SRresp$plotID %in% pls_elevsq_cv_SRresp$plotID), 
    #                                   grepl(substr(i,5, (nchar(i)-6)), colnames(obs_SRresp))])
    # 
    # RMSE_sd <- stat[["RMSE"]] / sd_resp_SR
    return(list(RMSE_sd = RMSE_sd, RMSE = RMSE, R2 = R2))
  })
  stats_pls_elevsq_cv <- data.frame(do.call("rbind", stats_pls_elevsq_cv))
  # stats_pls_elevsq_cv_pst <- do.call("rbind", stats_pls_elevsq_cv[,"stat"])
  stats_pls_elevsq_cv$type <- "pls_elevsq_cv"
  # stats_pls_elevsq_cv_RMSE_sd <- data.frame(RMSE_sd = do.call("rbind", stats_pls_elevsq_cv[,"RMSE_sd"]))
  # stats_pls_elevsq_cv_RMSE_sd$type <- "pls_elevsq_cv"  

  #
  #
  #
  #
  #
  #
  
  
  
  ##gam ~ obs
  # stats_gam_pst <- as.data.frame(t(postResample(gam_SRresp[,resp], obs_SRresp[,resp])))
  # stats_gam_RMSE_sd <- data.frame(RMSE_sd = stats_gam_pst[["RMSE"]] / sd_resp_SR)
  
  R2 <- caret::R2(gam_SRresp[,resp], obs_SRresp[,resp], na.rm = T)
  RMSE <- caret::RMSE(gam_SRresp[,resp], obs_SRresp[,resp], na.rm = T)
  RMSE_sd <- RMSE / sd_resp_SR
  
  stats_gam <- data.frame(RMSE_sd = RMSE_sd, RMSE = RMSE, R2 = R2 )
  
  stats_gam$type <- "gam"
  # stats_gam_RMSE_sd$type <- "gam"
  
  ##gam_res
  runs_gam_res <- c(paste0("resid", resp, "_run1"), paste0("resid", resp, "_run2"), 
                    paste0("resid", resp, "_run3"), paste0("resid", resp, "_run4"), 
                    paste0("resid", resp, "_run5"))
  stats_gam_res <- lapply(runs_gam_res, function(i){
    # stat <- as.data.frame(t(postResample(gam_resid_resp[,i], obs_SRresp[,resp])))
    # RMSE_sd <- stat[["RMSE"]] / sd_resp_SR
    R2 <- caret::R2(gam_resid_resp[,i], obs_SRresp[,resp], na.rm = T)
    RMSE <- caret::RMSE(gam_resid_resp[,i], obs_SRresp[,resp], na.rm = T)
    RMSE_sd <- RMSE / sd_resp_SR
    return(list(RMSE_sd = RMSE_sd, RMSE = RMSE, R2 = R2))
  })
  stats_gam_res <- data.frame(do.call("rbind", stats_gam_res))
  #stats_gam_res_pst <- do.call("rbind", stats_gam_res[,"stat"])
  stats_gam_res$type <- "gam_res"
  # stats_gam_res_RMSE_sd <- data.frame(RMSE_sd = do.call("rbind", stats_gam_res[,"RMSE_sd"]))
  # stats_gam_res_RMSE_sd$type <- "gam_res"
  
  
  ###mrg stats
  mrg <- (rbind(stats_res, stats_gam, stats_gam, stats_gam_res, 
                stats_pls, stats_gam_cv, stats_pls_elevsq_cv))
  mrg$RMSE_sd <- as.numeric(mrg$RMSE_sd)
  mrg$RMSE <- as.numeric(mrg$RMSE)
  mrg$R2 <- as.numeric(mrg$R2)
  mrg$resp <- resp
  
  return(mrg)
  }
})
  
stats_all <- do.call("rbind", stats_all_lst)

for (x in seq(nrow(stats_all))){
  trop <- NA
  for (i in trophic_tbl$Taxon){
    match <- grep(i, stats_all[x,"resp"], value=TRUE)
    if (length(match) != 0){
      trop <- trophic_tbl$diet[trophic_tbl$Taxon == i]
    }
    #print(trop)
  }
  stats_all$troph[x] <- as.character(trop)
}


for (i in seq(nrow(stats_all))){
  if (grepl("sum", stats_all$resp[i]) == T){
    stats_all$troph_sep[i] <- paste0(stats_all$troph[i], "_sum")
  }else{
    stats_all$troph_sep[i] <- stats_all$troph[i]
  }
}

stats_all$troph <- factor(stats_all$troph, levels = c("generalist", 
                                                      "predator", 
                                                      "herbivore", 
                                                      "decomposer", 
                                                      "plant", 
                                                      "birds", 
                                                      "bats", 
                                                      "summary", 
                                                      "trait"))

stats_all$troph_sep <- factor(stats_all$troph_sep, levels = c("generalist", "generalist_sum", 
                                              "predator", "predator_sum", 
                                              "herbivore", "herbivore_sum", 
                                              "decomposer", "decomposer_sum", 
                                              "plant", "plant_sum", 
                                              "birds", "birds_sum", 
                                              "bats", "bats_sum", 
                                              "summary", "summary_sum", 
                                              "trait", "trait_sum"))
stats_all <- stats_all[with(stats_all, order(troph_sep, resp)),] ####sortierung nach alphabet resp ist nicht sooo optimal, weil resids lfter zusammenstehen und nicht abwechselnd reisid und das entsprechende SR- eventuell "resid" hinten an namen dranschreiben
stats_all$resp <- factor(stats_all$resp, levels = unique(stats_all$resp))




myColors <- c("mediumslateblue", "blue2", "aquamarine3", "chocolate1", 
              "firebrick1", "darkmagenta")
fillscl <- scale_fill_manual(name = "col",values = myColors)
if (all_plts == F){
  if (frst == T){
    title <- "forest"
  }else {
    title <- "non-forest"
  }
}else{ 
  title  <- "all_plots"}


#pdf(file = paste0(outpath, "all_stats.pdf"), height= 10, width = 20)#, paper = "a4r")
print(ggplot(data = stats_all, aes(x=resp, y=RMSE_sd)) + 
        geom_boxplot(aes(fill=type), width = 1) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) + 
  fillscl + 
    ggtitle(title))
#dev.off()



pdf(file = paste0(outpath, "all_stats.pdf"), height= 10, width = 20)#, paper = "a4r")
print(ggplot(data = stats_all, aes(x=resp, y=RMSE_sd)) + 
        #geom_rect(fill=grey_pal()(length(levels(stats_all$troph_sep))+5)[as.numeric(stats_all$troph_sep)+5], 
                  #xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf) +
        geom_boxplot(aes(fill=type), width = 1) + 
        facet_grid(~troph_sep, scales = "free_x", space="free_x", switch = "x") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) + 
        fillscl + 
        ggtitle(title))
dev.off()
  
      
       
#       
#   #pdf(file = paste0(path_nofrst, "stats_", resp, ".pdf"))
#   ##plot RMSE
#   print(ggplot(data = stats_all, aes(x=type, y=RMSE_sd)) + 
#     geom_boxplot() + 
#     ggtitle(resp))
#   
#   #v <- lm(pls_residresp[,paste0("resid", resp, "_run1")] ~ obs_residresp[,paste0("resid", resp)])
#   #plot(v)
# 
#   
#   #plot RMSE normiert auf sd für alle außer Residuen
#   print(ggplot(data = mrg_RMSE_sd[which(mrg_RMSE_sd$type != "res"),], aes(x=type, y=RMSE_sd)) + 
#     geom_boxplot()+ 
#     ggtitle(resp))
#   #dev.off()
#   
#   ##plot R2 aller vorhersagen aller runs: nur in forest/non-forest
#   runs <- c(paste0(resp, "_run1"), paste0(resp, "_run2"), paste0(resp, "_run3"), 
#             paste0(resp, "_run4"), paste0(resp, "_run5"))
#   pls_all_mrg_df <- pls_SRresp_all[,which(colnames(pls_SRresp_all) == "plotUnq") : 
#                                       which(colnames(pls_SRresp_all) == runs[1])]
#   colnames(pls_all_mrg_df)[3] <- "predicted"
#   
#   for (i in seq(2,length(runs))){
#     colnames(pls_SRresp_all)[2+i] <- "predicted"
#     pls_all_mrg_df <- rbind(pls_all_mrg_df, pls_SRresp_all[,c((which(colnames(pls_SRresp_all) == "plotUnq") : 
#                                                                   which(colnames(pls_SRresp_all) == "plotID")), (2+i))])
#   }
#   pls_obs_df <- merge(pls_all_mrg_df, obs_SRresp, by = "plotUnq") 
#   pls_obs_df <- pls_obs_df[which(complete.cases(pls_obs_df$predicted)),]
#   
#   ###statistische Maße über alle runs innerhalb von forest oder non-forest
#   stats_pls_all <- as.data.frame(t(postResample(pls_obs_df$predicted, pls_obs_df[,resp])))
#   RMSE_sd <- stats_pls_all[["RMSE"]] / sd_resp_SR
#   
#   
# }

