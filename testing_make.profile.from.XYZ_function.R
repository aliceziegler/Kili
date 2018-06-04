#' Derive a vertical profile of a point cloud or a canopy height model
#'
#' Function that creates profiles from XYZ-tables with a custom height
#' resolution (binwidth)
#' @param XYZ.table 3-column data frame with 3rd column containing heights
#' @param binwidth number specifying the vertical width of height classes
#' @return vector of point or pixel counts per height class with height values as names
#' @keywords profile lidar point cloud CHM xyz
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de
library(plyr)
library(data.table)


XYZ.table <- get(load("F:/Projekte/Kili/data/mar18_50m/points_25m.RData"))
XYZ.table <- XYZ.table[which(XYZ.table$plotID == "flm1"), c(3:5)]
binwidth <- 1

make.profile.from.XYZ <- function(XYZ.table, binwidth=1){
  require(plyr)
  XYZ.table <- data.frame(XYZ.table)
  names(XYZ.table) <- c("X", "Y", "Z")
  max.Z <- max(XYZ.table$Z, na.rm=T)
  min.Z <- min(XYZ.table$Z, na.rm=T)
  rdXYZ.table <- XYZ.table
  rdXYZ.table$Z <- round_any(XYZ.table$Z, binwidth, floor)
  rdXYZ.table <- data.table(rdXYZ.table) # was data.table
  Z.summary <- rdXYZ.table[,.N, by='Z']
  profile.bins <- seq(round_any(min.Z, binwidth, floor), round_any(max.Z, binwidth, ceiling), binwidth)
  profile.vals <- rep(0, times=length(profile.bins))
  profile.vals[match(Z.summary$Z, profile.bins)] <- Z.summary$N
  names(profile.vals) <- profile.bins
  return(profile.vals)
}
  
  
  
  
  #######################
  ###orig
  #######################
    rdXYZ.table <- data.table(rdXYZ.table) 
  Z.summary <- rdXYZ.table[,.N, by='Z'] 
  profile.bins <- seq(round_any(min.Z, binwidth, floor), round_any(max.Z, binwidth, ceiling), binwidth)
  profile.vals <- rep(0, times=length(profile.bins))
  profile.vals[match(Z.summary$Z, profile.bins)] <- Z.summary$N
  names(profile.vals) <- profile.bins
  return(profile.vals)
#}




