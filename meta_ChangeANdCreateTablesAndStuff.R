trophic_tbl <- get(load(paste0(inpath_general, "trophic_tbl.RData")))

sum_troph <- data.frame(Taxon = c("generalist", "predator", "herbivore", "decomposer", "plant"), 
                        diet = c("generalist", "predator", "herbivore", "decomposer", "plant"))

trophic_tbl <- rbind(trophic_tbl, sum_troph)

trophic_tbl$diet <- factor(trophic_tbl$diet, levels = c("generalist", 
                                              "predator", 
                                              "herbivore", 
                                              "decomposer", 
                                              "plant", 
                                              "birds", 
                                              "bats", 
                                              "summary", 
                                              "trait"))

save(trophic_tbl, file = paste0(inpath_general, "trophic_tbl.RData"))

