#' RMS Error
#'
#' @description This function calculates the rmse between the source and target counts
#'
#' @param buildings buildings file in \code{sf} format
#' @param blocks census blocks file in \code{sf} format
#' @param buildpop building population column name \code{string}
#' @param blockcode block id column \code{string}
#' @param blockpop block population column name \code{string}
#' @param title title of scatterplot \code{string}
#'
#' @return rms error, correlation and scatterplot
#' @export
#'
#' @importFrom graphics abline
#' @importFrom graphics text
#' @importFrom stats cor
#' @importFrom stats lm
#'
#' @examples
#'     library(populR)
#'     data("buildings)
#'     data("blocks")
#'
#'     # areametric
#'     population_a = areametric(buildings = buildings, blocks = blocks,
#'         code = 'code', pop = 'pop', area = 'area')
#'     population_ar = rounding(buildings = population_a, buildpop = 'popEst',
#'         blockpop = 'pop', blockcode = 'code')
#'     error_ar = rmse(buildings = population_ar, blocks = blocks,
#'         buildpop = 'intpop', blockcode = 'code',
#'         blockpop = 'pop', title = 'Scatterplot\nAreametric Method')
#'
#'     # volumetric
#'     population_v = volumetric(buildings = buildings, blocks = blocks,
#'         code = 'code', pop = 'pop', area = 'area', floors = 'floors')
#'     population_vr = rounding(buildings = population_v, buildpop = 'popEst',
#'         blockpop = 'pop', blockcode = 'code')
#'     error_vr = rmse(buildings = population_vr, blocks = blocks,
#'         buildpop = 'intpop', blockcode = 'code',
#'         blockpop = 'pop', title = 'Scatterplot\nVolumetric Method')
#'
#'
rmse <- function(buildings, blocks, blockcode, blockpop, buildpop, title) {
  df <- blocks[, c(blockcode, blockpop), drop = T]
  df$buildpop <- 0
  for (i in 1:nrow(df)) {
    df$buildpop[i] <- sum(buildings[, buildpop, drop = T][buildings[, blockcode, drop = T] == df[, blockcode][i]])
  }
  a <- sqrt(mean((df$buildpop - df[, blockpop])^2))
  z <- lm(df[, blockpop] ~ df$buildpop)
  plot(df[, blockpop], df$buildpop, col="#634B56", main = title, xlab = "Observed", ylab = "Predicted")
  abline(z, col="#FD8D3C")
  text(x = min(df[, blockpop]) + 40, y = max(df$buildpop) - 20, label = paste0("r^2 = ", round(cor(df[, blockpop], df$buildpop), 5)))
  return(a)
}
