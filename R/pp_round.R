#' Rounding Function
#'
#' @param .target object of class \code{sf}
#' @param tpop target population
#' @param spop source population
#' @param sid source id
#'
#' @return an object of class \code{sf} including rounded population counts
#' @export
#'
#' @importFrom rlang quo_name
#' @importFrom rlang enquo
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
#' # awi - round
#' pp_round(awi, tpop = pp_est, spop = pop, sid = sid)
#'
#' # vwi - round
#' pp_round(vwi, tpop = pp_est, spop = pop, sid = sid)
#'
pp_round <- function(.target, tpop, spop, sid) {
  #check arguments
  if (missing(.target)) {
    stop('target is required')
  }

  if (missing(tpop)) {
    stop('tpop is required')
  }

  if (missing(spop)) {
    stop('spop is required')
  }

  if (missing(sid)) {
    stop('sid is required')
  }

  # check whether colnames exist
  spop <- rlang::quo_name(rlang::enquo(spop))
  sid <- rlang::quo_name(rlang::enquo(sid))
  tpop <- rlang::quo_name(rlang::enquo(tpop))

  # check whether parameters exist in the given target object
  if (!spop %in% colnames(.target)) {
    stop(sprintf('%s cannot be found in the given target object', spop))
  }

  if (!sid %in% colnames(.target)) {
    stop(sprintf('%s cannot be found in the given target object', sid))
  }

  if (!tpop %in% colnames(.target)) {
    stop(sprintf('%s cannot be found in the given target object', tpop))
  }

  # check whether spop and tpop are numeric
  if (!is.numeric(.target[, spop, drop = TRUE])) {
    stop('source population must be numeric')
  }

  if (!is.numeric(.target[, tpop, drop = TRUE])) {
    stop('target population must be numeric')
  }

  .target$newid <- 1:nrow(.target)
  .target$pp_int <- round(.target[, tpop, drop = TRUE], 0)
  .target$diff <- .target[, tpop, drop = TRUE] - .target$pp_int

  code <- unique(.target[, sid, drop = TRUE])

  df <- data.frame()
  for (i in 1:length(code)) {
    df <- rbind(df, c(code[i], unique(.target[, spop, drop = TRUE][.target[, sid, drop = TRUE] == code[i]]),
                      sum(.target[, tpop, drop = TRUE][.target[, sid, drop = TRUE] == code[i]]),
                      sum(.target[, 'pp_int', drop = TRUE][.target[, sid, drop = TRUE] == code[i]])))
  }
  names(df) <- c('code', 'spop', 'estpop', 'intpop')

  for (i in 1:nrow(df)) {
    diaf <- df$spop[i] - df$intpop[i]

    if (diaf == 0) {
      next
    } else if (diaf > 0) {
      d <- abs(diaf)
      sub <- .target[.target[, sid, drop = TRUE] == df$code[i], ]
      sub <- sub[order(-sub$diff), ]
      for (j in 1:d) {
        sub$pp_int[j] <- sub$pp_int[j] + 1
      }
    } else if (diaf < 0) {
      d <- abs(diaf)
      sub <- .target[.target[, sid, drop = TRUE] == df$code[i], ]
      sub <- sub[order(sub$diff), ]
      for (j in 1:d) {
        sub$pp_int[j] <- sub$pp_int[j] - 1
      }
    }
    for (j in 1:nrow(sub)) {
      .target$pp_int[.target$newid == sub$newid[j]] <- sub$pp_int[j]
    }
  }

  .target$newid <- NULL

  return(.target)

}
