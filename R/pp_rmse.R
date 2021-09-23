#' RMS Error
#'
#' @description This function calculates the rmse between the source and target counts
#'
#' @param target target file in \code{sf} format
#' @param source census source file in \code{sf} format
#' @param targetpop target estimated pop column name \code{string}
#' @param sourcecode source id column \code{string}
#' @param sourcepop source population column name \code{string}
#' @param title title of scatterplot \code{string}
#'
#' @return rms error, correlation, lm line and scatterplot
#' @export
#'
#' @importFrom graphics abline
#' @importFrom graphics text
#' @importFrom stats cor
#' @importFrom stats lm
#'
#' @examples
#'     library(populR)
#'     data("target")
#'     data("source")
#'
#'     # areametric
#'     pop_aw <- pp_estimate(source = source, target = target, sourcepop = 'pop',
#'         sourcecode = 'sid')
#'
#'     # areametric rmse
#'     pp_rmse(target = pop_aw, source = source, sourcecode = 'sid',
#'         sourcepop = 'pop', targetpop = 'pp_est', title = 'Areametric')
#'
#'     # volumetric
#'     pop_vw <- pp_estimate(source = source, target = target, sourcepop = 'pop',
#'         sourcecode = 'sid', volume = 'floors')
#'
#'     # volumetric rmse
#'     pp_rmse(target = pop_vw, source = source, sourcecode = 'sid',
#'         sourcepop = 'pop', targetpop = 'pp_est', title = 'Volumetric')
#'
#'
pp_rmse <- function(target, source, sourcecode, sourcepop, targetpop, title) {
  df <- source[, c(sourcecode, sourcepop), drop = T]
  df[, targetpop] <- 0
  for (i in 1:nrow(df)) {
    df[,targetpop][i] <- sum(target[, targetpop, drop = T][target[, sourcecode, drop = T] == df[, sourcecode][i]])
  }
  a <- sqrt(mean((df[, targetpop] - df[, sourcepop])^2))
  z <- lm(df[, sourcepop] ~ df[, targetpop])
  plot(df[, sourcepop], df[, targetpop], col="#634B56", main = title, xlab = "Observed", ylab = "Predicted")
  abline(z, col="#FD8D3C")
  text(x = min(df[, sourcepop]) + 40, y = max(df[, targetpop]) - 20, label = paste0("r^2 = ", round(cor(df[, sourcepop], df[, targetpop]), 5)))
  return(a)
}
