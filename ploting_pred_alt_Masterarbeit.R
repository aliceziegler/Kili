#setwd("/media/aziegler/Volume/data_div/") ###alz: wenn sich diese Zeile nicht ausführen lässt: Volume mounten
setwd("D:/Documents/__Studium/Master_Marburg/Masterarbeit/data_div")
#mod_date <- "16_02_09"
#load(paste0("gpm_models_rf_", mod_date, ".rda"))

load("best_mods_16_03_17.rda")
models <- best_mods

inpath <- "D:/Documents/__Studium/Master_Marburg/Masterarbeit/data_div/"

mod_date <- "best_mods"
library(rasterVis)
library(grid)

tests_agg_sum <- read.csv(paste0(inpath, "tests_agg_sum_", mod_date, ".csv"), header=T, sep=",")
#tests_rdc <- tests_agg_sum[-which(tests_agg_sum$Group.1 %in% c("total_insct", "ttl_insct_rdc", "rich_insct")),]
tests_rdc <- tests_agg_sum
tests_rdc$Group.1 <- as.character(tests_rdc$Group.1)
#tests_rdc$Group.1[tests_rdc$Group.1 == "rich_insct"] <- "assemblage richness"
tests_rdc$Group.1[tests_rdc$Group.1 == "Hym_ants"] <- "Formicidae"
tests_rdc$Group.1[tests_rdc$Group.1 == "Hym_excl_ants"] <- "Hymenoptera"
tests_rdc$Group.2 <- as.character(tests_rdc$Group.2)
tests_rdc$Group.2[tests_rdc$Group.2 == "cof"] <- "coffee"
tests_rdc$Group.2[tests_rdc$Group.2 == "forest_disturbed"] <- "disturbed forest"
tests_rdc$Group.2[tests_rdc$Group.2 == "gra"] <- "grassland"
tests_rdc$Group.2[tests_rdc$Group.2 == "hom"] <- "home garden"
tests_rdc$Group.2[tests_rdc$Group.2 == "mai"] <- "maize"
tests_rdc$Group.2[tests_rdc$Group.2 == "sav"] <- "savannah"

tests_rdc$Group.1 <- as.factor(tests_rdc$Group.1)
tests_rdc$Group.2 <- as.factor(tests_rdc$Group.2)
colnames(tests_rdc)[1] <- "arthro"
colnames(tests_rdc)[2] <- "landuse"

####creating average row and column
agg_list_spec <- aggregate(tests_rdc, by=list(tests_rdc$arthro), FUN = mean) 
agg_list_spec$arthro <- agg_list_spec$Group.1
agg_list_spec$landuse <- "average"
agg_list_spec <- agg_list_spec[,c(2:ncol(agg_list_spec))]
agg_df_spec <- rbind(agg_list_spec, tests_rdc)

agg_list_land <- aggregate(agg_df_spec, by = list(agg_df_spec$landuse), FUN = mean)
agg_list_land$landuse <- agg_list_land$Group.1
agg_list_land$arthro <- "average"
agg_list_land <- agg_list_land[,c(2:ncol(agg_list_land))]
agg_df_spec_land <- rbind(agg_df_spec, agg_list_land)



plot_tbl_srt <- tests_rdc[with(tests_rdc, order(tests_rdc$landuse, tests_rdc$arthro, decreasing = F)),]


######plotting "Levelplot of R2 of Species by landuse"###
species <- plot_tbl_srt$arthro
species <- unique(species)

landuse <- plot_tbl_srt$landuse
landuse <- unique(landuse)
# landuse_tst <- append(landuse[8],landuse[1:7])

###create landuse raster from agg_list_land
lst_land <- cbind(agg_list_land$arthro, agg_list_land$landuse, agg_list_land$r_squared)
lst_land <- lst_land[c(2:8),]
df_land <- as.data.frame(lst_land)
colnames(df_land) <- c("", "land", "average_land")
df_land <- df_land[with(df_land, order(df_land$average_land, decreasing = F)),]
order_land <- df_land$land
lst_rsq_land <- as.list(agg_list_land$r_squared[c(2:8)])

dat_rsq_land <- t(do.call("cbind", lst_rsq_land))

dat_rsq_land <- sort(dat_rsq_land, decreasing = T) 
dat_rsq_land <- as.matrix(dat_rsq_land)
rst_rsq_land <- raster(dat_rsq_land, xmn = 0.5, xmx = 1.5, 
                  ymn = 0.5, ymx = 7.5)

###create arthro raster from agg_list_spec
lst_spec <- cbind(as.character(agg_list_spec$arthro), agg_list_spec$landuse, agg_list_spec$r_squared)
df_spec <- as.data.frame(lst_spec)
colnames(df_spec) <- c("spec", "", "average_spec")
df_spec <- df_spec[with(df_spec, order(df_spec$average_spec, decreasing = T)),]
order_spec <- df_spec$spec

lst_rsq_spec <- as.list(agg_list_spec$r_squared)

dat_rsq_spec <- do.call("cbind", lst_rsq_spec)
dat_rsq_spec <- sort(dat_rsq_spec, decreasing = F)
dat_rsq_spec <- t(as.matrix(dat_rsq_spec))
rst_rsq_spec <- raster(dat_rsq_spec, xmn = 0.5, xmx = 8.5, 
                  ymn = 0.5, ymx = 1.5)

###create big raster 
lst <- lapply(as.character(species), function(i) {
  dat <- plot_tbl_srt[grep(i, plot_tbl_srt[, 1]), ]
  dat[, c("arthro", "landuse", "r_squared")]
})

lst_rsq <- lapply(lst, function(i) {
  i$r_squared
})

dat_rsq <- do.call("cbind", lst_rsq)
colnames(dat_rsq) <- species
rownames(dat_rsq) <- landuse

df_rsq <- as.data.frame(dat_rsq)
df_rsq$land <- rownames(df_rsq)
reorder_land <- order_land[c(7,6,5,4,3,2,1)]
df_rsq_srt_land <- df_rsq[match(reorder_land, df_rsq$land),]
df_rsq_srt_land <- rbind(df_rsq_srt_land, colnames(df_rsq_srt_land))
reorder_spec <- order_spec[c(8,7,6,5,4,3,2,1)]
df_rsq_srt <- df_rsq_srt_land[,match(reorder_spec, df_rsq_srt_land[8,])]
df_rsq_resort <- df_rsq_srt[c(1:7),c(1:8)]
df_rsq_num <- lapply(seq(colnames(df_rsq_resort)), function(x){
  rsq_num <- as.numeric(df_rsq_resort[,x])
})
df_rsq_num <- do.call("cbind", df_rsq_num)
df_rst_rsq <- as.matrix(df_rsq_num)

rst_rsq <- raster(df_rst_rsq, xmn = 0.5, xmx = 8.5, 
                  ymn = .5, ymx = 7.5)


########writing out table###

df_lnd_av <- cbind(df_rsq_resort, dat_rsq_land)
spec_av <- c(dat_rsq_spec, NA)
df_lnd_spc_av <- rbind(spec_av, df_lnd_av)
rownames(df_lnd_spc_av)[1] <- "average"
colnames(df_lnd_spc_av)[9] <- "average"

write.csv(df_lnd_spc_av, file = "heatmap_table.csv")

###create plots from all 3
clr <- colorRampPalette(brewer.pal(9, "YlOrRd"))
plt <- levelplot(rst_rsq, scales = list(x = list(rot=45, at = 1:8, labels = as.character(reorder_spec)), 
                                        y = list(at = c(7:1), labels = as.character(reorder_land))), ################sorum stimmt Plot, aber warum ist das hier umgedreht? 
                 # levelplot(rst_rsq, scales = list(x = list(at = 1:11),y = list(at = 7:1)),
                 margin = FALSE, colorkey = FALSE,
                 col.regions = clr(101), 
                 at = seq(0, 0.60, 0.01))
plt_spec <- spplot(rst_rsq_spec, scales = list(draw = F), ################sorum stimmt Plot, aber warum ist das hier umgedreht? 
                   # levelplot(rst_rsq, scales = list(x = list(at = 1:11),y = list(at = 7:1)),
                   margin = FALSE, colorkey = F,
                   col.regions = clr(101), 
                   at = seq(0, 0.60, 0.01))
plt_land <- spplot(rst_rsq_land, scales = list(draw = F), ################sorum stimmt Plot, aber warum ist das hier umgedreht? 
                   # levelplot(rst_rsq, scales = list(x = list(at = 1:11),y = list(at = 7:1)),
                   margin = FALSE, colorkey = F,
                   col.regions = clr(101), 
                   at = seq(0, 0.60, 0.01))

### apply customized theme and settings
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

####################################################################################

png(paste0("land_spec_av", mod_date, ".png"), width = 14, height = 10, units = "cm", res = 300)
grid.newpage()

vp0 <- viewport(x = 0, y = 0, just = c("left", "bottom"), 
                width = .7, height = .9)
pushViewport(vp0)
print(plt, newpage = FALSE)

downViewport(trellis.vpname(name = "figure"))

## horizontal raster
trellis.par.set(custom_theme)
vp1 <- viewport(x = 0, y = .6, just = c("left", "bottom"), 
                width = 1, height = 1)
pushViewport(vp1)
print(plt_spec, newpage = FALSE)

## vertical raster
upViewport()

vp2 <- viewport(x = .59, y = 0, just = c("left", "bottom"), 
                width = 1, height = 1)
pushViewport(vp2)
print(plt_land, newpage = FALSE)

## colorkey
upViewport()

vp3 <- viewport(x = 1.25, y = 0, just = c("left", "bottom"), 
                width = .2, height = 1)
pushViewport(vp3)
draw.colorkey(key = list(col = clr(101), width = .75, height = .75,
                         at = seq(0, 0.6, 0.01),
                         space = "right"), draw = TRUE)
dev.off()

################################################################################
##heatmap variable importance###
################################################################################
var_imp_org <- read.csv(paste0("var_imp_srt_", mod_date, ".csv"))
row.names(var_imp_org) <- var_imp_org$X
var_imp <- var_imp_org[, 2:(ncol(var_imp_org)-1)]
vars <- rownames(var_imp)
vars[which(vars == "cffnt_intcpt")] <- "dens_cof_off"
vars[which(vars == "cffnt_x")] <- "dens_cof_x"
vars[which(vars == "cffnt_x2")] <- "dens_cof_x2"
vars[which(vars == "cffnt_x3")] <- "dens_cof_x3"
vars[which(vars == "qntl_0")] <- "qrtl_0"
vars[which(vars == "qntl_25")] <- "qrtl_25"
vars[which(vars == "qntl_50")] <- "qrtl_50"
vars[which(vars == "qntl_75")] <- "qrtl_75"
vars[which(vars == "qntl_rng")] <- "qrtl_rng"
vars[which(vars == "qntl_75")] <- "qrtl_75"
vars[which(vars == "max_rtrn")] <- "num_rtrn"
vars[which(vars == "sd_per_rtrn_1")] <- "sd_rtrn_1"
vars[which(vars == "sd_per_rtrn_2")] <- "sd_rtrn_2"
vars[which(vars == "sd_max_rtrn")] <- "sd_num_rtrn"
colnames(var_imp)[which(colnames(var_imp)=="Hym_ants")] <- "Formicidae"
colnames(var_imp)[which(colnames(var_imp)=="Hym_excl_ants")] <- "Hymenoptera"

var_imp_srt <- var_imp[, match(reorder_spec, colnames(var_imp))]

var_imp_lst <- as.list(var_imp_srt)
dat_var_imp <- do.call("cbind", var_imp_lst)

rst_var_imp <-  raster(dat_var_imp, xmn = 0.5, xmx = 8.5, 
                       ymn = 0.5, ymx = 18.5)           

var_imp_mean <- var_imp_org[,ncol(var_imp_org)]
var_imp_mean_lst <- as.list(var_imp_mean)
dat_var_imp_mean <- t(do.call("cbind", var_imp_mean_lst))
rst_var_imp_mean <-  raster(dat_var_imp_mean, xmn = 0.5, xmx = 1.5, 
                       ymn = 0.5, ymx = 18.5)

plt_var_imp <- levelplot(rst_var_imp, scales = list(x = list(rot=45, at = 1:8, labels = as.character(reorder_spec)), 
                                        y = list(at = c(18:1), labels = as.character(vars))), 
                 margin = FALSE, colorkey = FALSE,
                 col.regions = clr(101), 
                 at = seq(0, 1, 0.01))

plt_var_imp_mean <- spplot(rst_var_imp_mean, scales = list(draw=F), 
                              margin = FALSE, colorkey = FALSE,
                              col.regions = clr(101), 
                              at = seq(0, 1, 0.01))

plt_spec <- spplot(rst_rsq_spec, scales = list(draw = F), ################sorum stimmt Plot, aber warum ist das hier umgedreht? 
                   # levelplot(rst_rsq, scales = list(x = list(at = 1:11),y = list(at = 7:1)),
                   margin = FALSE, colorkey = F,
                   col.regions = clr(101), 
                   at = seq(0, 0.60, 0.01))

#####################write out table #######################
df_str_av <- cbind(dat_var_imp, dat_var_imp_mean)
colnames(df_str_av)[ncol(df_str_av)] <- "average"
df_str_spec_av <- rbind(spec_av, df_str_av)
rownames(df_str_spec_av) <- c("average", vars)

write.csv(df_str_spec_av, file = "str_var_values.csv")
#############################################################


#############################################################
###plt_spec
png(paste0("imp_spec_av", mod_date, ".png"), width = 14, height = 14, units = "cm", res = 300)
grid.newpage()

bvp0 <- viewport(x = 0, y = 0, just = c("left", "bottom"), 
                 width = 1, height = 0.95)
pushViewport(bvp0)
print(plt_var_imp, newpage = FALSE)

downViewport(trellis.vpname(name = "figure"))
####horizontal raster

#####arthropod averages as in heatmap
trellis.par.set(custom_theme)
bvp4 <- viewport(x = 0, y = 0.54, just = c("left", "bottom"), 
                 width = 1, height = 1)
pushViewport(bvp4)
print(plt_spec, newpage = FALSE)

## vertical raster
upViewport()
bvp2 <- viewport(x = 0.59, y = 0.0, just = c("left", "bottom"), 
                 width = 1, height = 1)
pushViewport(bvp2)
print(plt_var_imp_mean, newpage = FALSE)

## colorkey
upViewport()

bvp3 <- viewport(x = 1.25, y = 0, just = c("left", "bottom"), 
                 width = .2, height = 1)
pushViewport(bvp3)
draw.colorkey(key = list(col = clr(101), width = .75, height = .75,
                         at = seq(0, 1, 0.01),
                         space = "right"), draw = TRUE)


dev.off()




