#' RMS Error
#'
#' @description This function calculates the rmse between the source and target counts
#'
#' @param target object of class \code{sf}
#' @param source object of class \code{sf}
#' @param targetpop target zone population field
#' @param sourcecode source zone id field
#' @param sourcepop source zone population field
#' @param title scatterplot title \code{string}
#'
#' @return a list including rms error, linear model details and correlation coefficient
#' @export
#'
#' @importFrom graphics abline
#' @importFrom graphics text
#' @importFrom stats cor
#' @importFrom stats lm
#' @importFrom rlang quo_name
#' @importFrom rlang enquo
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
pp_rmse <- function(source, target, sourcecode, sourcepop, targetpop, title) {
  # check arguments
  if (missing(source)) {
    stop('source is required')
  }

  if (missing(target)) {
    stop('target is required')
  }

  if (missing(sourcecode)) {
    stop('sourcecode is required')
  }

  if (missing(sourcepop)) {
    stop('sourcepop is required')
  }

  if (missing(targetpop)) {
    stop('targetpop is required')
  }

  if (missing(title)) {
    stop('title is required')
  }

  # check whether column names exist
  sourcepop <- rlang::quo_name(rlang::enquo(sourcepop))
  sourcecode <- rlang::quo_name(rlang::enquo(sourcecode))
  targetpop <- rlang::quo_name(rlang::enquo(targetpop))

  if (!sourcepop %in% colnames(source)) {
    stop(sprintf('%s cannot be found in the given source object', sourcepop))
  }

  if (!sourcecode %in% colnames(source)) {
    stop(sprintf('%s cannot be found in the given source object', sourcecode))
  }

  if (!targetpop %in% colnames(target)) {
    stop(sprintf('%s cannot be found in the given target object', targetpop))
  }

  df <- source[, c(sourcecode, sourcepop), drop = T]
  df[, targetpop] <- 0

  # sumup target pop for each source zone feature
  for (i in 1:nrow(df)) {
    df[,targetpop][i] <- sum(target[, targetpop, drop = T][target[, sourcecode, drop = T] == df[, sourcecode][i]])
  }

  # calculate rmse, calculate correlation coefficient and create linear regression model
  rmse <- sqrt(mean((df[, targetpop] - df[, sourcepop])^2))
  linear_model <- lm(df[, sourcepop] ~ df[, targetpop])
  correlation_coef <- round(cor(df[, sourcepop], df[, targetpop]), 5)
  myList <- list(rmse = rmse, linear_model = linear_model, correlation_coef = correlation_coef)

  # scatterplot with line and correlation coeficient as text
  plot(df[, sourcepop], df[, targetpop], col="#634B56", main = title, xlab = "Observed", ylab = "Predicted")
  abline(linear_model, col="#FD8D3C")
  text(x = min(df[, sourcepop]) + 40, y = max(df[, targetpop]) - 20, label = paste0("r^2 = ", correlation_coef))

  return(myList)
}
