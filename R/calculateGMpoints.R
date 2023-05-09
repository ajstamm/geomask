#' Calculate geomasked points
#'
#' @param myshps    List of shapefiles
#' @param maskvars  List of settings for calculating masked locations.
#'
#' @examples
#'
#' maskvars <- list(
#'   min = 100,
#'   max = 1000,
#'   unit = "meters",
#'   point_id = "POINTID",
#'   bound_id = "GEOID10"
#' )
#'
#' ot <- tigris::tracts("NY", "Onondaga", year = 2010)
#' ol <- tigris::landmarks("NY", "point", year = 2015)
#' t <- sf::st_contains(ot, ol) |> unlist()
#' ol <- ol[t, ]
#' myshps <- list(
#'   point = ol,
#'   bound = ot
#' )
#'
#' myshps <- calculateGMpoints(myshps = myshps, maskvars = maskvars)
#'
#' @export
#'


calculateGMpoints <- function(myshps, maskvars) {
  # create temp max/min variables
  # create flag variable for number max/min iterations
  # if buffer fails, min = min/2 & max = min, reiterate if needed
  # subset remaining without valid buffer
  # will need to rerun buffer for just those
  # I'd love to drop the loop, but for now it works
  # possibly create buffers for each point, then recreate on the fly if needed?
  myshps$point$point_id <- data.frame(myshps$point)[, maskvars$point_id]
  myshps$bound$bound_id <- data.frame(myshps$bound)[, maskvars$bound_id]

  myshps$intersect <- sf::st_intersection(myshps$point, myshps$bound)
  myshps$intersect$orig_lon <- sf::st_coordinates(myshps$intersect)[, 1]
  myshps$intersect$orig_lat <- sf::st_coordinates(myshps$intersect)[, 2]

  myshps$buffer <- data.frame(ID = numeric())
  myshps$shifted <- data.frame(ID = numeric())

  temp <- list(max = maskvars$max,
               min = maskvars$min,
               int = myshps$intersect,
               iter = 0)

  while (nrow(temp$int) > 0) {
    temp$bufmin <- sf::st_buffer(temp$int, temp$min)
    temp$bufmin <- dplyr::select(temp$bufmin, !!dplyr::sym("point_id"),
                                 !!dplyr::sym("bound_id"))
    temp$bufmax <- sf::st_buffer(temp$int, temp$max)
    temp$bufmax <- dplyr::select(temp$bufmax, !!dplyr::sym("bound_id"))

    for (i in 1:nrow(temp$int)) {
      temp$bufmin1 <- dplyr::slice(temp$bufmin, i)
      temp$bufmax1 <- dplyr::slice(temp$bufmax, i)
      temp$diff1 <- sf::st_difference(temp$bufmax1, temp$bufmin1)
      temp$bound1 <- dplyr::filter(myshps$bound,
                                   !!dplyr::sym("bound_id") ==
                                     temp$bufmin1$bound_id[1])
      temp$int1 <- sf::st_intersection(temp$bound1, temp$diff1)
      if (nrow(temp$int1) > 0) {
        temp$shift1 <- sf::st_as_sf(sf::st_sample(temp$int1, 1),
                                    crs = sf::st_crs(temp$bufmin1))
        temp$shift1$point_id <- temp$bufmin1$point_id
        temp$shift1$bound_id <- temp$bufmin1$bound_id
        temp$shift1$mask_lon <- sf::st_coordinates(temp$shift1)[,1]
        temp$shift1$mask_lat <- sf::st_coordinates(temp$shift1)[,2]
        temp$shift1$flag <- temp$iter
        if (nrow(myshps$buffer) == 0) {
          myshps$buffer <- temp$int1
          myshps$shifted <- temp$shift1
        } else {
          myshps$buffer <- dplyr::bind_rows(myshps$buffer, temp$int1)
          myshps$shifted <- dplyr::bind_rows(myshps$shifted, temp$shift1)
        }
      }
    }

    temp$int <- dplyr::filter(myshps$intersect,
                              !(!!dplyr::sym("point_id") %in%
                                  myshps$shifted$point_id))
    temp$max <- temp$min
    temp$min <- temp$min / 2
    temp$iter <- temp$iter + 1
  }

  vars <- c("point_id", "bound_id", "mask_lon", "mask_lat", "flag")
  temp$shift <- data.frame(myshps$shifted)[, vars]
  myshps$old_full <- dplyr::full_join(myshps$intersect, temp$shift,
                                      by = c("point_id", "bound_id"))
  vars <- names(myshps$intersect)
  vars <- vars[1:(length(vars)-3)]
  temp$inter <- data.frame(myshps$intersect)[, c(vars, "orig_lon", "orig_lat")]
  myshps$new_full <- dplyr::full_join(myshps$shifted, temp$inter,
                                      by = c("point_id", "bound_id"))

  return(myshps)
}
