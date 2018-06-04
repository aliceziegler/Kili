# Description: 
# Author: Alice Ziegler
# Date: 2018-03-07 12:08:34
# to do: 
rm(list=ls())
##############ACHTUNG: bei plotten hochgestellte zwei in Spaltenübershrift von Dataframe...unschön!
########################################################################################
###Presettings
########################################################################################
#Packages: 
library(raster)
library(RColorBrewer)
library(rasterVis)
library(grid)
library(caret)
#plotting
library(ggplot2)
library(dplyr)
#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "mar18_50m_resid_nrmlz/2018-03-09_rfe_pls_unabh_repeat/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../out/", sub)
if (file.exists(outpath)==F){
  dir.create(file.path(outpath), recursive = T)
}
########################################################################################
###Settings
########################################################################################
load(file = paste0(inpath, "nm_pred.RData"))
load(file = paste0(inpath, "nm_resp.RData"))
nm_resp <- gsub("resid_", "resid", nm_resp)
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################

#######################
###merge models
#######################

models <- list.files(path = inpath, pattern = glob2rx("indv_model*"), full.names = TRUE)

mod_all <- list()
run_all <- c()
for (i in seq(length(models))){
  load(file = models[i])
  nm <- strsplit(x = models[i], split = "_|\\.")
  resp <- nm[[1]][15]############sollte auf dauer anders (relativ) angegeben werden!
  run <- nm[[1]][12]
  mod_all[[paste0(resp, "_", run)]] <- mod
  run_all <- c(run_all, run)
}
run_all <- unique(run_all)
run_all_srt <- c(run_all[1], run_all[c(3:length(run_all))], run_all[2])
save(mod_all, file = paste0(inpath, "/pls_model_list_all.RData"))


mod_lst <- lapply(nm_resp, function(x){
  all_runs <- mod_all[grep(paste0("^", x), names(mod_all))]
  return(all_runs)
})
names(mod_lst) <- nm_resp

save(mod_lst, file = paste0(inpath, "/pls_model_list_all_lst.RData"))

### check testing plots for each run
load(paste0(inpath, "/outs_lst.RData"))
for (i in seq(length(outs_lst))){
  names(outs_lst)[[i]] <- run_all_srt[[i]]
}

###predict in repeated
####rename df_scls per hand resid_ zu resid
dfs <- list.files(path = inpath, pattern = glob2rx("df_scl*"), full.names = F)

prediction_rep <- lapply(names(mod_lst), function(y){
  if (length(mod_lst[[y]]) != 0){
    df_scl <- get(load(paste0(inpath, dfs[grep(paste0("^", "df_scl_", y), dfs)])))
    print(y)
    prediction_run <- lapply(seq(length(mod_lst[[y]])), function(z){
      print(z)
      outs <- outs_lst[[grep(paste0(z, "$"), names(outs_lst))]]$plotID
      new_df <- df_scl[which(df_scl$plotID %in% outs),]
      colnames(new_df)[4] <- y#########################################sollte auf dauer geändert werden
      prediction <- predict(mod_lst[[y]][[paste0(y, "_run", z)]], newdata = new_df)
      stats <- postResample(prediction, new_df[[y]])
      return(stats)
    })
    aprediction_run_mat <- do.call(rbind, prediction_run)
  }
})
names(prediction_rep) <- names(mod_lst)

# df_resp <- as.data.frame(prediction_rep$SRmammals)
# df_resp$resp <- "SRmammals"

df_resp_all <- data.frame(matrix(nrow=0, ncol=4))
for (i in names(mod_lst)){
  if (!is.null(prediction_rep[[i]])){
    df_resp <- as.data.frame(prediction_rep[[i]])
    df_resp$resp <- i
    df_resp_all <- rbind(df_resp_all, df_resp)
  }
}

#save(df_resp_all, file = paste0(inpath, "df_resp_al_for_plotting.RData"))

########################################################################################
###actual plotting
########################################################################################

load(paste0(inpath, "df_resp_al_for_plotting.RData"))
colnames(df_resp_all)[which(colnames(df_resp_all) == "Rsquared")] <- "R²"

levels(df_resp_all$resp) <- c( "all plants (resid)", "all animals (resid)", "ants (resid)", 
                               "asterids (resid)", "bats (resid)", "bees (resid)", 
                               "birds (resid)", "collembola (resid)", "conifers (resid)", 
                               "dungbeetles (resid)", "eudicots (resid)", "ferns (resid)", 
                               "lycopodiopsida (resid)", "magnoliids (resid)", 
                               "mammals (resid)", "millipedes (resid)", "monocots (resid)", 
                               "moths (resid)", "orthoptera (resid)", 
                               "other aculeata (resid)", "other coleoptera (resid)", 
                               "parasitoids (resid)", "rosids (resid)", "snails (resid)", 
                               "spiders (resid)", "syrphids (resid)", "all plants", 
                               "all animals", "ants", "asterids", "bats", "bees", 
                               "birds", "collembola", "conifers", "dungbeetles", 
                               "eudicots", "ferns", "lycopodiopsida", 
                               "magnoliids", "mammals", "millipedes", "monocots", 
                               "moths", "orthoptera", "other aculeata", 
                               "other coleoptera", "parasitoids", "rosids", 
                               "snails", "spiders", "syrphids")




##only resid: 
df_plt_resid <- df_resp_all[grep("resid", df_resp_all$resp),]
df_plt_SR <- df_resp_all[-grep("resid", df_resp_all$resp),]

###for example see: diamonds und https://oer.uni-marburg.de/data/mriliasmooc/lm_data/lm_2050/visualizing-data-using-ggplot2.html
## basic frame
plot_resp <- function(df, var, names, resp_title, path = outpath, comm){
  df$resp = reorder(df$resp, df[[var]], median)
  p <- ggplot(aes_string(x = "resp", y = var), data = df) + 
    geom_boxplot() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    xlab(resp_title) + 
    ylab(var)
  print(p)
  ggsave(filename = paste0(path, "plot_", var, "_", comm, ".pdf"), plot = p, width = 25,
         height = 25, units = "cm")

  dev.off()
}
#ggplot(d, aes(x=reorder(fac, y, mean), y=y))

plot_resp(df = df_plt_resid, var = "RMSE", resp_title = "Taxa (Residuals)", comm = "res")
plot_resp(df = df_plt_resid, var = "R²", resp_title = "Taxa (Residuals)", comm = "res")
plot_resp(df = df_plt_SR, var = "RMSE", resp_title = "Taxa", comm = "")
plot_resp(df = df_plt_SR, var = "R²", resp_title = "Taxa", comm = "")



stats <- as.data.frame(ddply(df_resp_all,~resp,summarise,
                             meanR2 = mean(R²),
                             medianR2 = median(R²),
                             sdR2 = sd(R²), 
                             meanRMSE = mean(RMSE),
                             medianRMSE = median(RMSE),
                             sdRMSE = sd(RMSE)))


save(stats, paste0(inpath, "stats.RData"))
















###predict
#load(file = paste0(inpath, "2018-03-08_rfe_pls_unabh/indv_model_rfe_pls_resid_SRotheraculeata.RData"))
stats_lst <- lapply(seq(length(models)), function(i){
  prediction <- predict(mod_all[[i]], newdata = df_scl_ind)
  stats <- as.data.frame(postResample(prediction, df_scl_ind$resid_SRotheraculeata))
  colnames(stats) <- names(mod_all)[[i]]
  return(stats)
})
stats <- as.data.frame(t(do.call(cbind,stats_lst)))





###zurückskalieren auf ursprüngliche Werte

###############################end new additional independent analysis






#######################
### analyse
#######################

for (i in seq(mod_all)){
  print(names(mod_all[i]))
  #plot(mod_all[[i]])
  print(mod_all[[i]])
}

#max R² per Response
r2_lst <- lapply(mod_all, function(i){
  R2 <- max(i$results$Rsquared)
})
r2 <- as.data.frame(do.call(rbind, r2_lst))
colnames(r2) <- "rsquared"
r2_srt <- r2[order(r2, decreasing = T), , drop = F]

###opt vars
opt_vars <- c()
for (i in seq(mod_all)){
  print(length(mod_all[[i]]$optVariables))
  if (max(mod_all[[i]]$variables$Variables) != length(mod_all[[i]]$optVariables)){
    opt_vars <- c(opt_vars, mod_all[[i]]$optVariables)
  }
}
opt_freq <- as.data.frame(table(opt_vars))
opt_freq <- opt_freq[order(opt_freq$Freq, decreasing = T), ]


########################################################################################
#varImp
########################################################################################

var_imp_tmpl <- as.data.frame(nm_pred)
colnames(var_imp_tmpl) <- "pred"
for (i in seq(mod_all)){
  print(names(mod_all)[[i]])
  print(varImp(mod_all[[i]]))
  df <- as.data.frame(varImp(mod_all[[i]]))
  colnames(df) <- c(names(mod_all)[[i]])
  df$pred <- rownames(df)
  var_imp_tmpl <- merge(var_imp_tmpl, df, all = T)
}

row.names(var_imp_tmpl) <- var_imp_tmpl$pred
var_imp <- var_imp_tmpl[,2:ncol(var_imp_tmpl)]
rm(var_imp_tmpl)

###calculate mean of pred
pred_mn <- rowMeans(var_imp, na.rm = T)
pred_mn <- as.data.frame(pred_mn, na.rm = T)
pred_mn_srt <- pred_mn[order(pred_mn, decreasing = T), , drop = F]

###sort resp by r2_srt

###sort var imp
var_imp_srt <- var_imp[match(rownames(pred_mn_srt), rownames(var_imp)), 
                       match(rownames(r2_srt), colnames(var_imp))]

#######################
###TEST skalierung der var imp auf r²
#######################
var_imp_scl <- var_imp_srt

#####make compatible for plotting
#raster for plotting
imp_rst <- raster(as.matrix(var_imp_srt), xmn = 0.5, xmx = 26.5, 
                  ymn = 0.5, ymx = 60.5)
pred_rst <- raster(as.matrix(pred_mn_srt), xmn = 0.5, xmx = 1,5, 
                  ymn = 0.5, ymx = 60.5)
resp_rst <- raster(as.matrix(t(r2_srt)), xmn = 0.5, xmx = 26.5, 
                  ymn = 0.5, ymx = 1.5)
##############################################################################
#######################
###plotting
#######################

clr <- colorRampPalette(brewer.pal(9, "YlOrRd"))
plt_imp <- levelplot(imp_rst, scales = list(x = list(rot=45, at = 1:26, labels = rownames(r2_srt)), 
                                            y = list(at = c(60:1), labels = rownames(pred_mn_srt))), 
                     margin = FALSE, colorkey = FALSE,
                     col.regions = clr(101), 
                     at = seq(0, 1, 0.0001))

plt_pred <- spplot(pred_rst, scales = list(draw=F), 
                  margin = FALSE, colorkey = FALSE,
                  col.regions = clr(101), 
                  at = seq(0, 1, 0.01))

plt_resp <- spplot(resp_rst, scales = list(draw = F), ################sorum stimmt Plot, aber warum ist das hier umgedreht? 
                  # levelplot(rst_rsq, scales = list(x = list(at = 1:11),y = list(at = 7:1)),
                  margin = FALSE, colorkey = F,
                  col.regions = clr(101), 
                  at = seq(0, 0.60, 0.01))

custom_theme <- old_theme <- trellis.par.get()

custom_theme <- list(
  layout.widths = list(
    left.padding = 0,
    key.ylab.padding = 0,
    ylab.axis.padding = 0,
    axis.key.padding = 0,
    right.padding = 0), 
  layout.heights = list(
    top.padding = 0, 
    bottom.padding = 0, 
    axis.xlab.padding = 0,
    xlab.key.padding = 0,
    key.axis.padding = 0, 
    main.key.padding = 0, 
    key.sub.padding = 0
  )
) 

trellis.par.set(custom_theme)

####################################################################
#png(paste0("imp_spec_av", mod_date, ".png"), width = 14, height = 14, units = "cm", res = 300)
grid.newpage()

bvp0 <- viewport(x = 0, y = 0, just = c("left", "bottom"), 
                 width = 1, height = 0.95)
pushViewport(bvp0)
print(plt_imp, newpage = FALSE)

downViewport(trellis.vpname(name = "figure"))
####horizontal raster

#####arthropod averages as in heatmap
trellis.par.set(custom_theme)
bvp4 <- viewport(x = 0, y = 0.54, just = c("left", "bottom"), 
                 width = 1, height = 1)#######################################width and height anpassen
pushViewport(bvp4)
print(plt_resp, newpage = FALSE)

## vertical raster
upViewport()
bvp2 <- viewport(x = 0.59, y = 0.0, just = c("left", "bottom"), 
                 width = 1, height = 1)#######################width and height anpassen
pushViewport(bvp2)
print(plt_pred, newpage = FALSE)

## colorkey
upViewport()

bvp3 <- viewport(x = 1.25, y = 0, just = c("left", "bottom"), 
                 width = .2, height = 1)
pushViewport(bvp3)
draw.colorkey(key = list(col = clr(101), width = .75, height = .75,
                         at = seq(0, 1, 0.01),
                         space = "right"), draw = TRUE)


#dev.off()
