D <- function(buildings, code, pop, area) {
  cd <- unique(code[!is.na(code)])
  buildings$D <- 0
  for (i in 1:length(cd)) {
    s <- as.numeric(sum(area[code == cd[i]], na.rm = T))
    p <- as.numeric(unique(pop[code == cd[i]]))
    if (s > 0) {
      D <- p/s
      buildings$D[code == cd[i]] <- D
    }
  }
  return(buildings)
}
