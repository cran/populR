D <- function(target, sourcecode, sourcepop, area) {
  cd <- unique(sourcecode[!is.na(sourcecode)])
  target$D <- 0
  for (i in 1:length(cd)) {
    s <- as.numeric(sum(area[sourcecode == cd[i]], na.rm = T))
    p <- as.numeric(unique(sourcepop[sourcecode == cd[i]]))
    if (s > 0) {
      D <- p/s
      target$D[sourcecode == cd[i]] <- D
    }
  }
  return(target)
}
