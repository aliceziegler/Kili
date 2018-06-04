# pnts_tmp sind immer alle punkte die für einen Plot aufgerufen wurden.

###LAI calculation
XYZ.table <- pnts_tmp[,c(which(colnames(pnts_tmp) == "x") : which(colnames(pnts_tmp) == "z"))]
profile <- make.profile.from.XYZ(XYZ.table = XYZ.table)
vert_fol_profile <- vertical.foliage.profile(profile = profile, k = 0.3)
LAI <- sum(vert_fol_profile)