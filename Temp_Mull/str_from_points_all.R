###derive structural parameters from all LiDAR points

#set parameters
inpath <- "H:/Kili/data/"
outpath <- "H:/Kili/out/"
source("H:/Kili/src/inkili/R/ldr_query.R")
points_all <- read.csv(file = paste0(inpath, "ldr_all_points.csv"))

###
#presettings
#remotesensing <- RemoteSensing$new("http://192.168.191.183:8081", "user:password") 

plotID <- as.character(unique(points_all$plotID))
landuse <- as.character(unique(points_all$land))


#derive from database
#calculate from all_points
#merge to one table

#str <- ldr_query(plotID = points_all$plotID, crdnt_x = points_all$x, crdnt_y = points_all$y, radius = r)
plt_dnst_fun <- setNames(aggregate(z ~ plotID, points_all, density), c("plotID", "h_dens_fun"))
dnst_fun <- lapply(plotID, function(i){
  dnst <- density(points_all$z[min(which(points_all$plotID == i)):max(which(points_all$plotID == i))])
  dnst_df <- data.frame(cbind(x=dnst$x, y=dnst$y, plotID = i))
  pdf(file = paste0(outpath, "dens_plot_", i, ".pdf"), width = 24, height = 14)
  plot(dnst, main = i)
  dev.off()
  return(dnst_df)
})
dnst_df <- do.call(rbind, dnst_fun)

