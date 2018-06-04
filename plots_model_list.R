####to do: in funktion schreiben!

# Author: Alice Ziegler
# Date: 2017-10-10 17:12:53

rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#Packages: 
library(caret)
library(raster)
library(RColorBrewer)
library(lattice)
#library(plsdepot)

#Sources: 
setwd(dirname(rstudioapi::getSourceEditorContext()[[2]]))
sub <- "mar18_50m_resid_nrmlz/2018-02-28_ffs_pls/"
inpath <- paste0("../data/", sub)
outpath <- paste0("../out/", sub)

########################################################################################
###Settings
########################################################################################

########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################
########################################################################################
###Settings
########################################################################################
load(paste0(inpath, "pls_model_list_all.RData"))
#load(paste0(inpath, "pls_model_list_all_forest_only.RData"))
#load(paste0(inpath, "pls_model_list_all_no_rfe.RData"))
load(paste0(inpath, "../dat_ldr_mrg.RData"))

########################################################################################
########################################################################################
########################################################################################
###mit rfe
########################################################################################
########################################################################################
########################################################################################

########################################################################################
#show model performances
########################################################################################
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

########################################################################################
#how many $optVariables for each response (take out the models who took all variables)
########################################################################################
opt_vars <- c()
for (i in seq(mod_all)){
  print(length(mod_all[[i]]$optVariables))
  if (max(mod_all[[i]]$variables$Variables) != length(mod_all[[i]]$optVariables)){
    opt_vars <- c(opt_vars, mod_all[[i]]$optVariables)
  }
}
opt_freq <- as.data.frame(table(opt_vars))
opt_freq <- opt_freq[order(opt_freq$Freq, decreasing = T), ]
###könnte man noch plotten um Steigungen zu sehen

########################################################################################
#varImp
########################################################################################
var_imp_tmpl <-  c("BE_H_MEDIAN", "BE_H_P10", "BE_H_P100", "BE_H_P20", "BE_H_P30", "BE_H_P40", "BE_H_P50", "BE_H_P60", "BE_H_P70", 
              "BE_H_P80", "BE_H_P90", "BE_H_SD", "BE_H_SKEW", "BE_H_VAR", "BE_PR_CAN", "BE_PR_REG", "BE_PR_UND", "BE_RD_CAN", 
              "BE_RD_GND", "BE_RD_REG", "BE_RD_UND", "chm_height_max", "chm_height_mean", "chm_height_min", "chm_height_sd", 
              "dtm_aspect_mean", "dtm_aspect_reg", "dtm_aspect_unweighted_mean", "dtm_slope_max", "dtm_slope_mean", 
              "dtm_slope_min", "dtm_slope_reg", "dtm_slope_sd", "pulse_returns_max", "pulse_returns_mean", "pulse_returns_min", 
              "pulse_returns_sd", "vegetation_coverage_01m", "vegetation_coverage_02m", "vegetation_coverage_05m", 
              "vegetation_coverage_10m", "max_hght", "sd", "mdn_rtrn", "max_rtrn", "sd_rtrn_1", "sd_nmbr_rtrn", "qntl_0", 
              "qntl_25", "qntl_50", "qntl_75", "qntl_100", "qntl_rng", "gap_frac")
#var_imp_tmpl <- unique(mod$SRallplants$variables$var)#############################################################für verallgemeinerung
var_imp_tmpl <- as.data.frame(var_imp_tmpl)
colnames(var_imp_tmpl) <- "pred"
for (i in seq(mod)){
  print(names(mod)[[i]])
  print(varImp(mod[[i]]))
  df <- as.data.frame(varImp(mod[[i]]))
  colnames(df) <- c(names(mod)[[i]])
  df$pred <- rownames(df)
  var_imp_tmpl <- merge(var_imp_tmpl, df, all = T)
}

row.names(var_imp_tmpl) <- var_imp_tmpl$pred
var_imp <- var_imp_tmpl[,2:ncol(var_imp_tmpl)]





###calculate mean of ldr
ldr_mn <- rowMeans(var_imp, na.rm = T)
ldr_mn <- as.data.frame(ldr_mn, na.rm = T)
ldr_mn_srt <- ldr_mn[order(ldr_mn, decreasing = T), , drop = F]

###calculate mean of bio
bio_mn <- as.data.frame(colMeans(var_imp, na.rm = T), na.rm = T)
bio_mn_srt <- bio_mn[order(bio_mn, decreasing = T), , drop = F] 

###sort var imp
var_imp_srt <- var_imp[match(rownames(ldr_mn_srt), rownames(var_imp)), 
                       match(rownames(bio_mn_srt), colnames(var_imp))]




#####make compatible for plotting
#raster for plotting
imp_rst <- raster(as.matrix(var_imp_srt), xmn = 0.5, xmx = 26.5, 
                 ymn = 0.5, ymx = 60.5)
ldr_rst <- raster(as.matrix(ldr_mn_srt), xmn = 0.5, xmx = 1,5, 
                  ymn = 0.5, ymx = 60.5)
bio_rst <- raster(as.matrix(t(bio_mn_srt)), xmn = 0.5, xmx = 26.5, 
                  ymn = 0.5, ymx = 1.5)
##############################################################################


clr <- colorRampPalette(brewer.pal(9, "YlOrRd"))
plt_imp <- levelplot(imp_rst, scales = list(x = list(rot=45, at = 1:26, labels = rownames(bio_mn_srt)), 
                                 y = list(at = c(60:1), labels = rownames(ldr_mn_srt))), 
          margin = FALSE, colorkey = FALSE,
          col.regions = clr(101), 
          at = seq(0, 1, 0.0001))

plt_ldr <- spplot(ldr_rst, scales = list(draw=F), 
                           margin = FALSE, colorkey = FALSE,
                           col.regions = clr(101), 
                           at = seq(0, 1, 0.01))

plt_bio <- spplot(bio_rst, scales = list(draw = F), ################sorum stimmt Plot, aber warum ist das hier umgedreht? 
                   # levelplot(rst_rsq, scales = list(x = list(at = 1:11),y = list(at = 7:1)),
                   margin = FALSE, colorkey = F,
                   col.regions = clr(101), 
                   at = seq(0, 0.60, 0.01))


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
print(plt_bio, newpage = FALSE)

## vertical raster
upViewport()
bvp2 <- viewport(x = 0.59, y = 0.0, just = c("left", "bottom"), 
                 width = 1, height = 1)#######################width and height anpassen
pushViewport(bvp2)
print(plt_ldr, newpage = FALSE)

## colorkey
upViewport()

bvp3 <- viewport(x = 1.25, y = 0, just = c("left", "bottom"), 
                 width = .2, height = 1)
pushViewport(bvp3)
draw.colorkey(key = list(col = clr(101), width = .75, height = .75,
                         at = seq(0, 1, 0.01),
                         space = "right"), draw = TRUE)


#dev.off()

########################################################################################
########################################################################################
########################################################################################
###ohne rfe
########################################################################################
########################################################################################
########################################################################################

########################################################################################
#show model performances
########################################################################################
for (i in seq(mod_no_rfe)){
  print(names(mod_no_rfe[i]))
  plot(mod_no_rfe[[i]])
  print(mod_no_rfe[[i]])
}

########################################################################################
#Variable importance
########################################################################################
imp_top5 <- c()
for (i in seq(mod_no_rfe)){
  print(names(mod_no_rfe[i]))
  imp_obj <- (varImp(mod_no_rfe[[i]]))
  imp <- imp_obj$importance
  imp$pred <- rownames(imp)
  rownames(imp) <- c()
  imp <- imp[order(imp$Overall, decreasing = T),]
  imp_top5 <- c(imp_top5, imp$pred[1:5])
  }
imp_freq <- as.data.frame(table(imp_top5))
imp_freq <- imp_freq[order(imp_freq$Freq, decreasing = T), ]

# plot(mod[i])
# summary(mod[i])
# print(mod[i])

####plot var imp
imp_all <- lapply(seq(mod_no_rfe), function(i){
  imp_obj <- (varImp(mod_no_rfe[[i]]))
  imp <- imp_obj$importance
  colnames(imp) <- names(mod_no_rfe[i])
  return(imp)
})
imp_df <- do.call(cbind, imp_all)
imp_mx <- as.matrix(imp_df)

###calculate mean of bio
bio_mn <- colMeans(imp_df)
bio_mn <- as.data.frame(bio_mn)

###calculate mean of ldr
ldr_mn <- rowMeans(imp_df)
ldr_mn <- as.data.frame(ldr_mn)
######################################################################
###imp_df und imp_mx nach den means sortieren
### skalieren (@ Thomas/Hanna)
####mean balken rastern

rst_imp <-  raster(imp_mx, xmn = 0.5, xmx = 26.5, 
                       ymn = 0.5, ymx = 58.5)


clr <- colorRampPalette(brewer.pal(9, "YlOrRd"))
levelplot(rst_imp, scales = list(x = list(rot=45, at = 1:26, labels = colnames(imp_df)), 
                                     y = list(at = c(58:1), labels = rownames(imp_df))), 
          margin = FALSE, colorkey = FALSE,
          col.regions = clr(101), 
          at = seq(0, 100, 1))













plotVarImp <- function(var_imp){
  lapply(var_imp, function(x){
    if(is.null(x)){
      var_imp_plot <- NULL
    } else {
      plot_var_imp <- data.frame(OVERALL = x$mean)
      rownames(plot_var_imp) <- x$VARIABLE
      
      v_imp_varsel <- list(importance = plot_var_imp,
                           model = "loess r-squared",
                           calledFrom = "varImp")
      class(v_imp_varsel) <- "varImp.train"
      var_imp_plot <- plot(v_imp_varsel, main = as.character(x$RESPONSE[1]))      
    }
    return(var_imp_plot)
  })
} 

