#' Areametric Down-scaling
#'
#' @description Population down-scaling from a coarse source zone to a finer scale using the areametric approach
#'
#' @param buildings object of class \code{sf}
#' @param blocks object of class \code{sf}
#' @param code string of block id column
#' @param pop string of block pop column
#' @param area string of building area column
#'
#' @return an object of class \code{sf} including population counts
#' @export
#'
#'
#' @seealso \link[populR]{volumetric}
#'
#' @importFrom sf st_join
#' @importFrom sf st_intersects
#'
#' @examples
#'     library(populR)
#'     data("buildings")
#'     data("blocks")
#'     population_a = areametric(buildings = buildings, blocks = blocks,
#'         code = 'code', pop = 'pop', area = 'area')
#'
#' @references Lwin, K. K., & Murayama, Y. (2009)
#'     \emph{A GIS approach to estimation of building population for micro-spatial analysis. Transactions in GIS, 13(4), 401–414.}
#'     \doi{https://doi.org/10.1111/j.1467-9671.2009.01171.x}
#'
areametric <- function(buildings, blocks, code, pop, area) {
  message('performing spatial join...')
  join <- st_join(buildings, blocks, join = st_intersects, left = TRUE, largest=TRUE)
  join[is.na(join)] <- 0
  message('calculate living area for each building...')
  join <- a(buildings = join, area = join[, area, drop = T])
  message('calculate block density...')
  join <- D(buildings = join, code = join[, code, drop = T], area = join$a, pop = join[, pop, drop = T])
  message('down-scaling...')
  join$popEst <- p(a = join$a, D = join$D)
  return(join)
}
