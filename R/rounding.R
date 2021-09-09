#' Rounding Function
#'
#' @description This function converts decimal population estimates produced either by the \link[populR]{areametric} or the \link[populR]{volumetric} approach into integer numbers that sum up to the source zone population counts
#'
#' @param buildings object of class \code{sf}
#' @param buildpop string of building pop column
#' @param blockpop string of block pop column
#' @param blockcode string of block id column
#'
#' @return an object of class \code{sf} including rounded population counts
#' @export
#'
#'
#' @examples
#'     library(populR)
#'     data("buildings")
#'     data("blocks")
#'
#'     # areametric
#'     population_a = areametric(buildings = buildings, blocks = blocks,
#'         code = 'code', pop = 'pop', area = 'area')
#'     population_ar = rounding(buildings = population_a, buildpop = 'popEst',
#'         blockpop = 'pop', blockcode = 'code')
#'
#'     # volumetric
#'     population_v = volumetric(buildings = buildings, blocks = blocks,
#'         code = 'code', pop = 'pop', area = 'area', floors = 'floors')
#'     population_vr = rounding(buildings = population_v, buildpop = 'popEst',
#'         blockpop = 'pop', blockcode = 'code')
#'
rounding <- function (buildings, buildpop, blockpop, blockcode) {
  cd <- unique(buildings[, blockcode, drop = T])
  buildings$newid <- 1:nrow(buildings)
  buildings <- buildings[order(buildings$newid), ]
  buildings$intpop <- as.integer(round(as.numeric(buildings[, buildpop, drop = T]), 0))
  buildings$intpop[is.na(buildings$intpop)] <- 0
  buildings$diaf <- buildings[, buildpop, drop = T] - buildings$intpop
  df <- data.frame()
  for (i in 1:length(cd)) {
    df <- rbind(df, c(cd[i], unique(buildings[, blockpop, drop = T][buildings[, blockcode, drop = T] == cd[i]]), sum(buildings$intpop[buildings[, blockcode, drop = T] == cd[i]])))
  }
  names(df) <- c("code", "blkpp", "bldppi")
  df$diaf <- df$blkpp - df$bldppi
  for (i in 1:nrow(df)) {
    ct <- abs(df$diaf[i])
    if (df$diaf[i] == 0) {
      next
    } else if (df$diaf[i] > 0) {
      bd <- subset(buildings, buildings[, blockcode, drop = T] == df$code[i])
      bd <- bd[order(-bd$diaf), ]
      for (i in 1:ct) {
        bd$intpop[i] <- bd$intpop[i] + 1
      }
    } else {
      bd <- subset(buildings, buildings[, blockcode, drop = T] == df$code[i])
      bd <- bd[order(bd$diaf), ]
      for (i in 1:ct) {
        bd$intpop[i] <- bd$intpop[i] - 1
      }
    }
    bd <- bd[order(bd$newid), ]
    for (i in 1:nrow(bd)) {
      buildings$intpop[buildings$newid == bd$newid[i]] <- bd$intpop[i]
    }
  }
  return(buildings)
}
