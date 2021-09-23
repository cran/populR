#' Rounding Function
#'
#' @description This function converts decimal population estimates produced by
#'     the \link[populR]{pp_estimate} approach into integer numbers that sum up
#'     to the source zone population counts
#'
#' @param target object of class \code{sf}
#' @param targetpop string of target estimated pop column
#' @param sourcepop string of source pop column
#' @param sourcecode string of source id column
#'
#' @return an object of class \code{sf} including rounded population counts
#' @export
#'
#'
#' @examples
#'     library(populR)
#'     data("target")
#'     data("source")
#'
#'     # areametric
#'     pop_aw <- pp_estimate(source = source, target = target,
#'         sourcepop = "pop", sourcecode = "sid")
#'
#'     # areametric round
#'     round_aw <- pp_round(target = pop_aw, targetpop = "pp_est",
#'         sourcepop = "pop", sourcecode = "sid")
#'
#'     # volumetric
#'     pop_vw <- pp_estimate(source = source, target = target,
#'     sourcepop = "pop", sourcecode = "sid", volume = "floors")
#'
#'     # volumetric round
#'     round_vw <- pp_round(target = pop_vw, targetpop = "pp_est",
#'         sourcepop = "pop", sourcecode = "sid")
#'
#'
pp_round <- function (target, targetpop, sourcepop, sourcecode) {
  cd <- unique(target[, sourcecode, drop = T])
  target$newid <- 1:nrow(target)
  target <- target[order(target$newid), ]
  target$pp_int <- as.integer(round(as.numeric(target[, targetpop, drop = T]), 0))
  target$pp_int[is.na(target$pp_int)] <- 0
  target$diaf <- target[, targetpop, drop = T] - target$pp_int
  df <- data.frame()
  for (i in 1:length(cd)) {
    df <- rbind(df, c(cd[i], unique(target[, sourcepop, drop = T][target[, sourcecode, drop = T] == cd[i]]), sum(target$pp_int[target[, sourcecode, drop = T] == cd[i]])))
  }
  names(df) <- c("code", "blkpp", "bldppi")
  df$diaf <- df$blkpp - df$bldppi
  for (i in 1:nrow(df)) {
    ct <- abs(df$diaf[i])
    if (df$diaf[i] == 0) {
      next
    } else if (df$diaf[i] > 0) {
      bd <- subset(target, target[, sourcecode, drop = T] == df$code[i])
      bd <- bd[order(-bd$diaf), ]
      for (i in 1:ct) {
        bd$pp_int[i] <- bd$pp_int[i] + 1
      }
    } else {
      bd <- subset(target, target[, sourcecode, drop = T] == df$code[i])
      bd <- bd[order(bd$diaf), ]
      for (i in 1:ct) {
        bd$pp_int[i] <- bd$pp_int[i] - 1
      }
    }
    bd <- bd[order(bd$newid), ]
    for (i in 1:nrow(bd)) {
      target$pp_int[target$newid == bd$newid[i]] <- bd$pp_int[i]
    }
  }
  return(target)
}
