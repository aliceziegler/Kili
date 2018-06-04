# Description: Plotting bio against ldr in several variations and combinations
# Author: Alice Ziegler
# Date: 2017-10-10 17:12:53

rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#Packages: 
library(ggplot2)
library(mgcv)
library(gridExtra)
library(gtable)
library(corrplot)
#Sources: 
inpath <- "E:/Projekte/Kili/data/"
outpath <- "E:/Projekte/Kili/out/plot_scatter_mat/"

########################################################################################
###Settings
########################################################################################
load(paste0(inpath, "dat_ldr_mrg.RData"))
nm_ldr <- colnames(mrg_tbl)[c(41:68, 71:90, 94:97, 101:104, 110:129, 131:134)]
nm_bio <- colnames(mrg_tbl)[c(14:40, 135:143)]
########################################################################################
###testing stuff
########################################################################################
for (i in c(1:ncol(mrg_tbl))){
  print(colnames(mrg_tbl[i]))
  print(paste0(">>>>>>>>>>>>>>>>>>>>>", class(mrg_tbl[,i])))
  print("___________________________________________________")
}


x<- mrg_tbl$SRferns
y<- mrg_tbl$elevation
h_mean <- mrg_tbl$BE_H_MEAN
fit<-mgcv::gam(x~y, family = gaussian)
fit_glm <- glm(ferns~elev)
qplot(y, x, geom = c("point", "smooth"))
plot.gam(fit, residuals = T)


########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################

for (i in nm_bio){
  print(i)
  p <- list()
  for (j in nm_ldr){
    p[[j]] <- ggplot(mrg_tbl, aes_string(x=j, y=i)) + 
      labs(x = j, y = i) +
      geom_point(shape = 16) +
      stat_smooth(method = "gam")
  }
  plot <- do.call(grid.arrange, c(p, ncol = 5))
  ggsave(plot = plot, filename = paste0(outpath, i, "_by_ldr_gam.pdf"), width = 100, 
         height = 100, units = "cm")
}


###bio by elevation
p <- list()
for (i in nm_bio){
  print(i)
    p[[i]] <- ggplot(mrg_tbl, aes_string(x="elevation", y=i)) + 
      labs(x = "elevation", y = i) +
      geom_point(shape = 16) +
      stat_smooth(method = "gam",  formula = y ~ s(x) )
}
plot <- do.call(grid.arrange, c(p, ncol = 5))
ggsave(plot = plot, filename = paste0(outpath, "bio_by_elev_gam.pdf"), width = 100, 
       height = 100, units = "cm")

###ldr_by elevation
p <- list()
for (i in nm_ldr){
  print(i)
  p[[i]] <- ggplot(mrg_tbl, aes_string(x="elevation", y=i)) + 
    labs(x = "elevation", y = i) +
    geom_point(shape = 16) +
    stat_smooth(method = "gam",  formula = y ~ s(x))
}
plot <- do.call(grid.arrange, c(p, ncol = 5))
ggsave(plot = plot, filename = paste0(outpath, "ldr_by_elev_gam.pdf"), width = 100, 
       height = 100, units = "cm")

####testing
##snails ~ elev

ggplot(mrg_tbl, aes_string(x="elevation", y="SRsnails")) + 
  labs(x = "elevation", y = "SRsnails") +
  geom_point(shape = 16) +
  stat_smooth(method = "gam", formula = y ~ s(x))

mod <- gam(mrg_tbl$elevation~mrg_tbl$SRsnails)

