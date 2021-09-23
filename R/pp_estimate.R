#' Population Down-scaling
#'
#' @param source object of class \code{sf}
#' @param target object of class \code{sf}
#' @param sourcepop string of source pop column
#' @param sourcecode string of source id column
#' @param volume string of target floor/height column
#'
#' @return an object of class \code{sf} including population counts
#' @export
#'
#' @importFrom sf st_join
#' @importFrom sf st_intersects
#' @importFrom sf st_area
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
#'     #volumetric
#'     pop_vw <- pp_estimate(source = source, target = target, sourcepop = 'pop',
#'         sourcecode = 'sid', volume = 'floors')
#'
#' @references Lwin, K. K., & Murayama, Y. (2009)
#'     \emph{A GIS approach to estimation of building population for micro-spatial analysis. Transactions in GIS, 13(4), 401–414.}
#'     \doi{https://doi.org/10.1111/j.1467-9671.2009.01171.x}
#'
#'
pp_estimate <- function(source, target, sourcepop, sourcecode, volume = NULL) {
  target$pptid <- 1:nrow(target)
  target$pparea <- as.vector(st_area(target))
  join <- st_join(target, source, join = st_intersects, left = TRUE, largest = TRUE)
  join$a <- 0
  if (is.null(volume)) {
    join$a <- join$pparea
  } else {
    join$a <- join$pparea * join[, volume, drop = T]
  }
  join <- D(target = join, sourcecode = join[, sourcecode, drop = T], sourcepop = join[, sourcepop, drop = T], area = join[, 'a', drop = T])
  join$pp_est <- join$a * join$D
  return(join)
}
