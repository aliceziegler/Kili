# Description: 
# Author: Alice Ziegler
# Date: 2018-03-07 12:08:34
# to do: 
rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#Packages: 
library(raster)
library(RColorBrewer)
library(rasterVis)
library(grid)
library(caret)
#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "mar18_50m_resid_nrmlz/2018-03-07_rfe_pls/"
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
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################

#######################
###merge models
#######################

models <- list.files(path = inpath, pattern = glob2rx("indv_model*"), full.names = TRUE)

mod_all <- list()
for (i in seq(length(models))){
  load(file = models[i])
  nm <- strsplit(x = models[i], split = "_|\\.")[[1]][12]############sollte auf dauer anders (relativ) angegeben werden!
  mod_all[[nm]] <- mod
}
####wenn hier nur die hälfte der eigentlichen modelle drin stecken, hängt das an einer alten benennung der Dateien: 
# statt resid_...müssen die Dateien resid heißen, weil die filenames an unterstrichen geteilt werden
save(mod_all, file = paste0(inpath, "/pls_model_list_all.RData"))








###new additional independent analysis
models <- list.files(path = paste0(inpath, "2018-03-08_rfe_pls_unabh/"), pattern = glob2rx("indv_model*"), 
                     full.names = TRUE)

mod_all <- list()
for (i in seq(length(models))){
  load(file = models[i])
  nm <- strsplit(x = models[i], split = "_|\\.")[[1]][13]############sollte auf dauer anders (relativ) angegeben werden!
  mod_all[[nm]] <- mod
}
####wenn hier nur die hälfte der eigentlichen modelle drin stecken, hängt das an einer alten benennung der Dateien: 
# statt resid_...müssen die Dateien resid heißen, weil die filenames an unterstrichen geteilt werden
save(mod_all, file = paste0(inpath, "/pls_model_list_all.RData"))


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
