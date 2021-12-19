#' RMSE
#'
#' @param .target an object of class \code{sf}
#' @param source an object of class \code{sf}
#' @param sid source id
#' @param spop source population
#' @param tpop target population
#' @param title scatterplot title \code{string}
#'
#' @return a list including rmse, mae, linear model details and correlation coefficient
#' @export
#'
#' @importFrom graphics abline
#' @importFrom graphics text
#' @importFrom stats cor
#' @importFrom stats lm
#' @importFrom rlang quo_name
#' @importFrom rlang enquo
#' @importFrom Metrics rmse
#' @importFrom Metrics mae
#'
#' @examples
#' # read lib data
#' data('source')
#' data('target')
#'
#' # areal weighted interpolation - awi
#' awi <- pp_estimate(target, source = source, sid = sid, spop = pop,
#'     method = awi)
#'
#' # volume weighted interpolation - vwi
#' vwi <- pp_estimate(target, source = source, sid = sid, spop = pop,
#'     method = vwi, volume = floors)
#'
#' # awi - rmse
#' pp_rmse(awi, source = source, sid = sid, spop = pop, tpop = pp_est,
#'     title ='awi')
#'
#' # vwi - rmse
#' pp_rmse(vwi, source = source, sid = sid, spop = pop, tpop = pp_est,
#'     title ='vwi')
#'
pp_rmse <- function(.target, source, sid, spop, tpop, title) {
  # check arguments
  if (missing(source)) {
    stop('source is required')
  }

  if (missing(.target)) {
    stop('target is required')
  }

  if (missing(sid)) {
    stop('sid is required')
  }

  if (missing(spop)) {
    stop('spop is required')
  }

  if (missing(tpop)) {
    stop('tpop is required')
  }

  if (missing(title)) {
    stop('title is required')
  }

  # check whether column names exist
  spop <- rlang::quo_name(rlang::enquo(spop))
  sid <- rlang::quo_name(rlang::enquo(sid))
  tpop <- rlang::quo_name(rlang::enquo(tpop))
  title <- rlang::quo_name(rlang::enquo(title))

  if (!spop %in% colnames(source)) {
    stop(sprintf('%s cannot be found in the given source object', spop))
  }

  if (!sid %in% colnames(source)) {
    stop(sprintf('%s cannot be found in the given source object', sid))
  }

  if (!tpop %in% colnames(.target)) {
    stop(sprintf('%s cannot be found in the given target object', tpop))
  }

  # check whether spop and tpop are numeric
  if (!is.numeric(source[, spop, drop = TRUE])) {
    stop('spop must be numeric')
  }

  if (!is.numeric(.target[, tpop, drop = TRUE])) {
    stop('tpop must be numeric')
  }

  df <- source[, c(sid, spop), drop = TRUE]
  df[, tpop] <- 0

  # sumup target pop for each source zone feature
  for (i in 1:nrow(df)) {
    df[,tpop][i] <- sum(.target[, tpop, drop = TRUE][.target[, sid, drop = TRUE] == df[, sid][i]])
  }

  # calculate rmse, calculate correlation coefficient and create linear regression model
  rmse <- rmse(df[, tpop], df[, spop])
  mae <- mae(df[, tpop], df[, spop])
  linear_model <- lm(df[, tpop] ~ df[, spop])
  correlation_coef <- round(cor(df[, spop], df[, tpop]), 5)
  myList <- list(rmse = rmse, mae = mae, linear_model = linear_model, correlation_coef = correlation_coef)

  # scatterplot with line and correlation coeficient as text
  plot(df[, spop], df[, tpop], col="#634B56", main = title, xlab = "Observed", ylab = "Estimated")
  abline(linear_model, col="#FD8D3C")
  text(x = min(df[, spop]) + 40, y = max(df[, tpop]) - 20, label = paste0("r^2 = ", correlation_coef))

  return(myList)

}
