# Description: 
# Author: Alice Ziegler
# Date: 2017-10-12 11:55:18

rm(list=ls())

########################################################################################
###Presettings
########################################################################################
#Packages: 
library(rPointDB)
#Sources: 
inpath <- "E:/Projekte/Kili/data/"
outpath <- "E:/Projekte/Kili/data/"

########################################################################################
###Settings
########################################################################################
db_layers <- c("kili", "kili2")
dmtr <- 50 # (im meter)
gap_hght <- 10 ###############################################################################gap hght als liste it 1.5, 5, 10m 
gap_sze <- 9 # (in number of pixel)
# open remote sensing database connection
remotesensing <- RemoteSensing$new("http://192.168.191.183:8081", "user:password") 
###read data
tec_crdnt <- read.csv(paste0(inpath,"tec_crdnt.csv"), header=T, sep=",")
load(paste0(outpath, "ldr_SR.RData"))
########################################################################################
###Do it (Don't change anything past this point except you know what you are doing!)
########################################################################################
plt_crdnt <- unique(tec_crdnt[, c("plotID", "x_pnt", "y_pnt")])

gap_frac_all_lay <- lapply(db_layers, function(i){
  pointdb <- remotesensing$lidar(i)
  #print(paste0("layer", i))
  gap_frac_lay <- lapply(plt_crdnt$plotID, function(j){
    poi <- remotesensing$poi(group_name = "kili", poi_name = j)
    ext <- ext <- extent_diameter(x = poi$x, y = poi$y, d = dmtr)
    chm <- pointdb$query_raster(ext = ext, type= 'chm')
    #########################################################################################remove outliers
    if (all(is.na(chm@data@values)) == FALSE){
      chm@data@values[chm@data@values < 10] <- NA
      chm@data@values[is.finite(chm@data@values)] <- 0
      chm@data@values[is.na(chm@data@values)] <- 1
      clmp <- clump(chm, directions = 8, gaps = F)
      frq<-as.data.frame(freq(clmp))
      excludeID <- frq$value[which(frq$count <= gap_sze)]
      sieve <- clmp
      sieve[clmp %in% excludeID] <- NA
      total_count <- length(sieve@data@values)
      clear_count <- sum(is.finite(sieve@data@values))
      cover_count <- sum(is.na(sieve@data@values))
      #print(paste0(i, " - write", j))
      gap_frac_plt <- clear_count / total_count
      return(list(gap_frac_plt, i, as.character(j)))
    }
  })
  
  #gap_frac_lay <- complete.cases(gap_frac_lay)
  gap_frac_lay_bnd <- do.call(rbind, gap_frac_lay)
  #print(class(gap_frac_lay))
  #lapply(gap_frac_lay, function(x) x[!is.na(x)])
})
ldr_str_chm <- as.data.frame(do.call(rbind, gap_frac_all_lay))
# rm(points_all_lay)
colnames(ldr_str_chm) <- c("gap_frac", "lay", "plotID")
for (i in 1:ncol(ldr_str_chm)) {
  ldr_str_chm[, i] <- unlist(ldr_str_chm[, i])
}

#save(ldr_str_chm, file = paste0(outpath, "ldr_str_chm.RData"))


