a <- function(buildings, area, floors = NULL) {
  buildings$a <- 0
  if (is.null(floors)) {
    buildings$a <- area
  } else {
    buildings$a <- area * floors
  }
  return(buildings)
}
