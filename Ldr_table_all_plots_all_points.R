###Settings

inpath <- "E:/Projekte/Kili/data/"
outpath <- "E:/Projekte/Kili/data/"
radius <- 10
db_layers <- c("kili", "kili2")
###############################################################################
#don't change anything past this point
###############################################################################
###general stuff
library(rPointDB)
library(dplyr)
# open remote sensing database connection
remotesensing <- RemoteSensing$new("http://192.168.191.183:8081", "user:password") 

###read data
tec_crdnt <- read.csv(paste0(inpath,"tec_crdnt.csv"), header=T, sep=",")
###here we go
plt_crdnt <- unique(tec_crdnt[, c("plotID", "x_pnt", "y_pnt")])


points_all_lay <- lapply(db_layers, function(i){
  pointdb <- remotesensing$lidar(i)
  points_lay <- lapply(plt_crdnt$plotID, function(j){
    extent <- extent_radius(x = plt_crdnt$x_pnt[j], y = plt_crdnt$y_pnt[j], r = radius)
    points <- pointdb$query(ext = extent)
    if (nrow(points)!= 0){
      points$plotID <- plt_crdnt$plotID[j]
      points$layer <- i
    }
    return(points)
  })
  points_lay_bnd <- do.call(rbind, points_lay)
})
points_all <- do.call(rbind, points_all_lay)
rm(points_all_lay)

plt_min <- setNames(aggregate(z ~ plotID, points_all, min), c("plotID", "h_min"))
pnts <- left_join(points_all, plt_min, by = "plotID")
rm(points_all)
pnts$h_rel <- pnts$z-pnts$h_min
pnts$landuse <- substr(pnts$plotID, 1, 3)
pnts <- pnts[,c(which(colnames(pnts) == "plotID"), which(colnames(pnts) == "landuse"), 
                which(colnames(pnts) == "x") : which(colnames(pnts) == "classificationFlags"), 
                which(colnames(pnts) == "layer") : which(colnames(pnts) == "h_rel"))]
  

write.csv(x = pnts, file = paste0(outpath, "ldr_all_points_", radius, "m.csv"), row.names = F)
save(pnts, file = paste0(outpath, "ldr_all_points_", radius, "m.RData"))
