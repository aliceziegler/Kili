lst <- c("a", "b", "c", "d")
pst <- lapply(lst, function(i){
  x <- paste0(i, "happy")
  return(x)
})
pst_unbind <- unlist(pst, recursive = F)


testlist <- list(list("a","b"),list("c","d"))
lapply(testlist,unlist)
unlist(testlist, recursive = F)
unlist(testlist, recursive = T)
