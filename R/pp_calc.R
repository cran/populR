#' Population Calculation
#'
#' @param target object of class \code{sf}
#' @param a area/volume field
#' @param D density field
#'
#' @return an object of class \code{sf} including population values
#' @export
#'
#'
#' @importFrom rlang quo_name
#' @importFrom rlang enquo
#'
#' @examples
#'     library(populR)
#'
#'     data('source')
#'     data('target')
#'
#'     # area calculation
#'     pp_area <- pp_a(target = target)
#'
#'     # density calculation using area
#'     pp_density_a <- pp_D(source = source, target = pp_area, sourcecode = sid,
#'         sourcepop = pop, area = pp_a)
#'
#'     # population calculation using area
#'     pp_pop_area <- pp_calc(target = pp_density_a, a = pp_a, D = pp_D)
#'
#'     # volume calculation
#'     pp_volume <- pp_a(target = target, volume = floors)
#'
#'     # density calculation using volume
#'     pp_density_v <- pp_D(source = source, target = pp_volume, sourcecode = sid,
#'         sourcepop = pop, area = pp_a)
#'
#'     # population calculation using volume
#'     pp_pop_volume <- pp_calc(target = pp_density_v, a = pp_a, D = pp_D)
#'
#'
pp_calc <- function(target, a, D) {

  # check arguments
  if (missing(target)) {
    stop('target is required')
  }

  if (missing(a)) {
    stop('a is required')
  }

  if (missing(D)) {
    stop('D is required')
  }

  a <- rlang::quo_name(rlang::enquo(a))
  D <- rlang::quo_name(rlang::enquo(D))

  # check whether colnames exist
  if (!a %in% colnames(target)) {
    stop(sprintf('%s cannot be found in the given target object'), a)
  }

  if (!D %in% colnames(target)) {
    stop(sprintf('%s cannot be found in the given target object'), D)
  }

  # calculate pop
  target$pp_est <- target[, a, drop = TRUE] * target[, D, drop = TRUE]

  return(target)
}
