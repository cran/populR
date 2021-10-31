#' Calculate Area/Volume for Target Zone Features
#'
#' @param target object of class \code{sf}
#' @param volume (optional) target number of floors or height values of target features
#'
#' @return an object of class \code{sf} including area or volume values
#'
#' @export
#'
#' @examples
#'     library(populR)
#'     data("target")
#'     data("source")
#'
#'     # area calculation
#'     pp_a(target = target)
#'
#'     # volume calculation
#'     pp_a(target = target, volume = floors)
#'
#' @importFrom sf st_area
#' @importFrom rlang quo_name
#' @importFrom rlang enquo
#'
pp_a <- function (target, volume = NULL) {

  # check arguments
  if (missing(target)) {
    stop('target is required')
  }

  target_sf <- 'sf' %in% class(target)

  if (!target_sf) {
    stop('target must be object of class sf')
  }

  # calculate area
  target$pp_a <- as.vector(sf::st_area(target))

  # calculate volume
  volume <- rlang::quo_name(rlang::enquo(volume))

  if (volume != 'NULL') {
    if (!volume %in% colnames(target)) {
      stop('volume cannot be found in the given target object')
    }

    target$pp_a <- target$pp_a * target[, volume, drop = TRUE]
  }

  return(target)
}
