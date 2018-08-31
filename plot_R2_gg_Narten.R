library(MASS)

###skript 3 muss vorher ausgeführt sein


tmp <- stats[which(stats$resp %in% alpha_nm_SR),]
plot(tmp$meanR2 ~ tmp$meanN_perplot)
mod <- lm(tmp$meanR2 ~ tmp$meanN_perplot)
abline(mod)
summary(mod)


# tmp2 <- tmp[which(tmp$resp != "SRanimals"),]
# tmp2 <- tmp2[which(tmp$resp != "SRallplants"),]
# plot(tmp2$meanR2 ~ tmp2$meanN_perplot)
# mod2 <- lm(tmp2$meanR2 ~ tmp2$meanN_perplot)
# abline(mod2)
# summary(mod2)
# 
# 
# #noforest
# 
# tmp <- stats_only[which(stats$resp %in% alpha_nm_SR),]
# plot(tmp$meanR2 ~ tmp$meanN_perplot)
# mod <- lm(tmp$meanR2 ~ tmp$meanN_perplot)
# abline(mod)
# summary(mod)
# 
# 
# tmp2 <- tmp[which(tmp$resp != "SRanimals"),]
# tmp2 <- tmp2[which(tmp$resp != "SRallplants"),]
# plot(tmp2$meanR2 ~ tmp2$meanN_perplot)
# mod2 <- lm(tmp2$meanR2 ~ tmp2$meanN_perplot)
# abline(mod2)
# summary(mod2)

