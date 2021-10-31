#' Calculate Population Density
#'
#' @param source object of class \code{sf}
#' @param target object of class \code{sf}
#' @param sourcecode source zone id field
#' @param sourcepop source zone population field
#' @param area target zone area field
#' @param point logical - whether to use point geometries or not
#'
#' @return an object of class \code{sf} including density values for each source zone id
#' @export
#'
#' @importFrom sf st_crs
#' @importFrom sf st_centroid
#' @importFrom sf st_join
#' @importFrom sf st_intersects
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
#'     # volume calculation
#'     pp_volume <- pp_a(target = target, volume = floors)
#'
#'     # density calculation using volume
#'     pp_density_v <- pp_D(source = source, target = pp_volume, sourcecode = sid,
#'         sourcepop = pop, area = pp_a)
#'
pp_D <- function(source, target, sourcecode, sourcepop, area, point = FALSE) {

  # check arguments
  if (missing(source)) {
    stop('source is required')
  }

  if (missing(target)) {
    stop('target is required')
  }

  if (missing(sourcepop)) {
    stop('sourcepop is required')
  }

  if (missing(sourcecode)) {
    stop('sourcecode is required')
  }

  if (missing(area)) {
    stop('area is required')
  }

  # check whether colnames exist
  sourcepop <- rlang::quo_name(rlang::enquo(sourcepop))
  sourcecode <- rlang::quo_name(rlang::enquo(sourcecode))
  area <- rlang::quo_name(rlang::enquo(area))

  if (area != 'NULL') {
    if (!area %in% colnames(target)) {
      stop(sprintf('%s cannot be found in the given target object', area))
    }
  }

  if (!sourcepop %in% colnames(source)) {
    stop(sprintf('%s cannot be found in the given source object', sourcepop))
  }

  if (!sourcecode %in% colnames(source)) {
    stop(sprintf('%s cannot be found in the given source object', sourcecode))
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

  if (point == TRUE) {
    target <- sf::st_centroid(target)
  }

  # spatial join on intersect
  join <- sf::st_join(target, source, join = st_intersects, left = TRUE, largest = TRUE)

  cd <- unique(join[, sourcecode, drop = TRUE][!is.na(sourcecode)])

  join$pp_D <- 0

  # calculate density for each source zone feature
  for (i in 1:length(cd)) {
    s <- as.numeric(sum(join[, area, drop = TRUE][join[, sourcecode, drop = TRUE] == cd[i]], na.rm = T))
    p <- as.numeric(unique(join[, sourcepop, drop = TRUE][join[, sourcecode, drop = TRUE] == cd[i]]))

    if (s > 0) {
      D <- p/s
      join$pp_D[join[, sourcecode, drop = TRUE] == cd[i]] <- D
    }
  }

  return(join)
}
