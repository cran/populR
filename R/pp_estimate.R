#' Population Down-Scaling
#'
#' @param source object of class \code{sf}
#' @param target object of class \code{sf}
#' @param sourcepop source zone population field
#' @param sourcecode source zone id field
#' @param volume source zone number of floors or height field
#' @param point logical - whether to use point geometries or not
#'
#' @return an object of class \code{sf} including population values
#' @export
#'
#' @importFrom rlang quo_name
#' @importFrom rlang enquo
#' @importFrom sf st_crs
#'
#' @examples
#'     library(populR)
#'
#'     data('source')
#'     data('target')
#'
#'     # using area
#'     pp_estimate(source = source, target = target, sourcepop = pop,
#'         sourcecode = sid)
#'
#'     # using volume
#'     pp_estimate(source = source, target = target, sourcepop = pop,
#'         sourcecode = sid, volume = floors)
#'
#'     # point geometries and area
#'     pp_estimate(source = source, target = target, sourcepop = pop,
#'         sourcecode = sid, point = TRUE)
#'
#'     # point geometries and volume
#'     pp_estimate(source = source, target = target, sourcepop = pop,
#'         sourcecode = sid, volume = floors, point = TRUE)
#'
pp_estimate <- function(source, target, sourcepop, sourcecode, volume = NULL, point = FALSE) {

  # check arguments
  if (missing(source)) {
    stop('source is reuired')
  }

  if (missing(target)) {
    stop('target is reuired')
  }

  if (missing(sourcepop)) {
    stop('sourcepop is reuired')
  }

  if (missing(sourcecode)) {
    stop('sourcecode is reuired')
  }

  # check whether colnames exist
  sourcepop <- rlang::quo_name(rlang::enquo(sourcepop))
  sourcecode <- rlang::quo_name(rlang::enquo(sourcecode))
  volume <- rlang::quo_name(rlang::enquo(volume))

  if (!sourcepop %in% colnames(source)) {
    stop(sprintf('%s cannot be found in the given source object', sourcepop))
  }

  if (!sourcecode %in% colnames(source)) {
    stop(sprintf('%s cannot be found in the given source object', sourcecode))
  }

  if (volume != 'NULL') {
    if (!volume %in% colnames(target)) {
      stop(sprintf('%s cannot be found in the given target object', volume))
    }
  }
  # check whether source and target are objects of sf class
  source_sf <- 'sf' %in% class(source)
  target_sf <- 'sf' %in% class(target)

  if (source_sf != target_sf) {
    stop('source and target must be objects of class sf')
  }

  # check whether source and targer share the same crs
  source_crs <- sf::st_crs(source)
  target_crs <- sf::st_crs(target)

  if (source_crs != target_crs) {
    stop('source and target dot not share the same crs')
  }

  # calculate livin space for target features using area or volume
  if (volume == 'NULL') {
    target <- pp_a(target = target)
  } else {
    target <- pp_a(target = target, volume = !!volume)
  }

  # density calculation either using points or not
  if (point == FALSE) {
    target <- pp_D(source = source, target = target, sourcecode = !!sourcecode, sourcepop = !!sourcepop, area = pp_a)
  } else if (point == TRUE) {
    target <- pp_D(source = source, target = target, sourcecode = !!sourcecode, sourcepop = !!sourcepop, area = pp_a, point = TRUE)
  }

  # population estimation
  target <- pp_calc(target = target, a = pp_a, D = pp_D)

  return(target)
}
