library(ggplot2)
library(reshape)

inpath <- "F:/Projekte/Kili/data/"
outpath <- "F:/Projekte/Kili/out/scatterplot_layout_insa/"


########################################################################################
###Settings
########################################################################################
load(paste0(inpath, "dat_ldr_mrg.RData")) #mrg_tbl

best_prd <- c("BE_FHD"
              , "BE_PR_UND"
              , "BE_RD_CAN"
              , "gap_frac"
              )
names_prd <- c("Foliage Height Density"
               , "Penetration Rate (Understory)"
               , "Return Density (Canopy)"
               , "Gap Fraction"
               )

tbl_prd <- mrg_tbl[,c(which(colnames(mrg_tbl) == "elevation"), which(colnames(mrg_tbl) %in% best_prd))]
fac_tbl <- melt(tbl_prd, id = c("elevation"))
levels(fac_tbl$variable) <- names_prd

###facette plot
  #plot
  plt <- 
    ggplot(fac_tbl, aes(elevation, value)) +
    geom_point() +
    geom_smooth(method = "auto") +
    facet_wrap( ~ variable, scales = "free_y", ncol = 2) +
    #scale_color_gradientn(colours = terrain.colors(13)) +
    xlab("Elevation") +
    ylab("") +
    #scale_x_continuous(breaks = c(1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500))+
    scale_x_continuous(breaks = c(1000, 2000, 3000, 4000))+
    #ggtitle(paste0()) +
    theme(
      plot.title = element_text(color = "black", size = 22, face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(color = "grey", fill = NA),
      #strip.text.x = element_text(size = 12, face = "bold"),
      strip.text.x = element_text(size = 9, face = "bold"),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
  
  # print "veg.sr.pl"
  pdf(paste0(outpath, "Elevation_pred_facet.pdf"))
  print(plt)
  dev.off()


##seperate plots
###############
for (i in seq(length(best_prd))){
  var <- which(colnames(mrg_tbl) == best_prd[i])
  df <- mrg_tbl[,c(3, var)]
  
  #plot
  plt <- ggplot(df, aes_string("elevation", best_prd[i])) +
    geom_point() +
    geom_smooth(method = "auto") +
    #facet_wrap( ~ elevation, scales = "free_y", ncol = 6) +
    #scale_color_gradientn(colours = terrain.colors(13)) +
    xlab("Elevation") +
    ylab(names_prd[1]) +
    #scale_x_continuous(breaks = c(1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500))+
    scale_x_continuous(breaks = c(1000, 2000, 3000, 4000))+
    #ggtitle(paste0()) +
    theme(
      plot.title = element_text(color = "black", size = 22, face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(color = "grey", fill = NA),
      #strip.text.x = element_text(size = 12, face = "bold"),
      strip.text.x = element_text(size = 9, face = "bold"),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
  
  # print "veg.sr.pl"
  pdf(paste0(outpath, "Elevation_", best_prd[i], ".pdf"))
  print(plt)
  dev.off()
}
