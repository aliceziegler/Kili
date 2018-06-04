# Description: asymmetric scatterplot matrix
# Author: Alice Ziegler
# Date: 2017-11-30 14:34:45

rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#Packages: 
library(ggplot2)
library(gridExtra)
library(gtable)
library(corrplot)
#Sources: 
inpath <- "F:/Projekte/Kili/data/"
outpath <- "F:/Projekte/Kili/out/plot_scatter_mat/"

########################################################################################
###Settings
########################################################################################
load(paste0(inpath, "dat_ldr_mrg.RData"))
mrg_tbl$cat <- factor(mrg_tbl$cat, levels = c("mai", "sav", "cof", "hom", "gra", "flm", "fod", 
                                              "foc", "fpo", "fpd", "fed", "fer", "hel"))
nm_ldr <- colnames(mrg_tbl)[c(41:68, 71:90, 94:97, 101:104, 110:129, 131:134)]
nm_bio <- colnames(mrg_tbl)[c(14:40, 135:143)]

tst_ldr <- colnames(mrg_tbl[41:44])
tst_bio <- colnames(mrg_tbl[14:17])
i <- nm_ldr[1]
j <- nm_bio[1]
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################
###corr_matrix for all against all
nm_corr <- c(nm_ldr, nm_bio)
corr_tbl <- mrg_tbl [, nm_corr]
corr_mat <- cor(corr_tbl)


cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(corr_tbl)


pdf(file = paste0(outpath, "corr_matrix_all_ellipse25.pdf"), width = 25, height = 25)
corrplot(corr_mat, method = "ellipse", tl.cex=0.5)
dev.off()

pdf(file = paste0(outpath, "corr_matrix_all_sig_cross25.pdf"), width = 25, height = 25)
corrplot(corr_mat, method = "ellipse", p.mat = p.mat, sig.level = 0.05)
dev.off()


###einzelne plots speichern
for (i in seq(nm_bio)){
  print(i)
  for (j in seq(nm_ldr)){
    plot <- ggplot(mrg_tbl, aes(x=mrg_tbl[nm_ldr[j]], y=mrg_tbl[nm_bio[i]], color = mrg_tbl$cat)) + 
      labs(x = nm_ldr[j], y = nm_bio[i], color = "landuse") +
      geom_point(shape = 16)+ 
      theme_light() + 
      theme(axis.text=element_text(size=6), axis.title=element_text(size=8))
    ggsave(plot= plot, filename = paste0(outpath, nm_bio[i], "_by_", nm_ldr[j], ".pdf"))
  }
}











###corr plots in one document
#o <- list()
for (i in nm_bio){
  print(i)
  p <- list()
  for (j in nm_ldr){
    p[[j]] <- ggplot(mrg_tbl, aes_string(x=j, y=i)) + 
      labs(x = j, y = i) +
      geom_point(shape = 16)#+
      theme_light() +
      theme(legend.position = "none", axis.text=element_text(size=6),
            axis.title=element_text(size=8))
  }
  plot <- do.call(grid.arrange, c(p, ncol = 5))
  ggsave(plot = plot, filename = paste0(outpath, i, "_by_ldr.pdf"), width = 100, 
         height = 100, units = "cm")
}




####try scatterplot matrix: 
o <- list()
for (i in seq(nm_bio)){
  print(i)
  p <- list()
  for (j in seq(nm_ldr)){
    p[[j]] <- ggplot(mrg_tbl, aes_string(x=j, y=i)) + 
      labs(x = nm_ldr[j], y = nm_bio[i], color = "landuse") +
      geom_point(shape = 16)+ 
      theme_light() + 
      theme(legend.position = "none", axis.text=element_text(size=6), axis.title=element_text(size=8))
  }
  o[[i]] <- do.call(grid.arrange, c(p, ncol= length(nm_bio)))
}
plot <- do.call(grid.arrange, c(o, nrow = length(nm_ldr)))
ggsave(plot= plot, filename = paste0(outpath, "plot6.pdf"), width = 300, height = 100, units = "cm", limitsize = F)





####color unterschiedung aber alle gleicher plot
# https://stackoverflow.com/questions/28299746/strange-issue-with-ggplot-inside-a-for-loop
# https://stackoverflow.com/questions/15678261/r-ggplot-does-not-work-if-it-is-inside-a-for-loop-although-it-works-outside-of
# https://www.data-imaginist.com/2017/beneath-the-canvas/
o <- list()
for (i in seq(nm_bio)){
  print(i)
  p <- list()
  for (j in seq(nm_ldr)){
    p[[j]] <- ggplot(mrg_tbl, aes(x=mrg_tbl[nm_ldr[j]], y=mrg_tbl[nm_bio[i]], color = mrg_tbl$cat)) + 
      labs(x = nm_ldr[j], y = nm_bio[i], color = "landuse") +
      geom_point(shape = 16)+ 
      theme_light() + 
      theme(legend.position = "none", axis.text=element_text(size=6), axis.title=element_text(size=8))
  }
  o[[i]] <- do.call(grid.arrange, c(p, ncol= length(nm_bio)))
}
plot <- do.call(grid.arrange, c(o, nrow = length(nm_ldr)))
ggsave(plot= plot, filename = paste0(outpath, "plot5.pdf"), width = 300, height = 100, units = "cm", limitsize = F)


###############testing#####################


o <- list()
for (i in seq(tst_trt)){
  print(i)
  p <- list()
  for (j in seq(tst_ldr)){
    p[[j]] <- ggplot(mrg_tbl, aes(x=mrg_tbl[tst_ldr[j]], y=mrg_tbl[tst_trt[i]], color = mrg_tbl$cat)) + 
      labs(x = tst_ldr[j], y = tst_trt[i], color = "landuse") +
      geom_point(shape = 16)+ 
      theme_light() + 
      theme(legend.position = "none", axis.text=element_text(size=6), axis.title=element_text(size=8))
  }
  o[[i]] <- do.call(grid.arrange, c(p, ncol= length(tst_trt)))
}
do.call(grid.arrange, c(o, nrow = length(tst_ldr)))


p <- list()
for(j in tst_ldr){
  print(j)
  p[[j]] <- ggplot(mrg_tbl, aes_string(x=j, y=i)) + ###aes_string geht nicht mit color
    geom_point(shape = 16)
}
 do.call(grid.arrange,c(p, list(ncol = 4)))


