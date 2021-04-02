#' Plot GM Comparison Map
#'
#' @description
#' This function draws two maps on top of each other with only the polygon
#' boundaries visible, so that polygon sizes and compositions can be compared.
#' It is designed to be saved to a recordPlot object for later rendering in a
#' PDF rather than displayed directly.
#'
#' @details
#' If plotting directly, the display may be distorted. For best results, save
#' to a recordPlot object and write to PDF.
#'
#' @param bound     A spatial polygons data frame.
#' @param point     A second spatial polygons data frame that ahould have the
#'                  same outer boundary as the first one.
#' @param maskvars  A list of settings for geomasking, including unit type and
#'                  minimum, and maximum values.
#' @param closemap  A boolean to denote whether to close the map window after
#'                  the map is drawn and saved.
#'
#' @examples
#' # define masking variables
#' maskvars <- list(
#'   unit = "meters", # distance unit
#'   min = 100,        # minimum distance
#'   max = 1000        # maximum distance
#' )
#'
#' # draw the map
#' plotGATcompare(
#'   bound = atown,
#'   point = apoint,
#'   maskvars = maskvars
#' )
#'
#' @export

# for arrow and scale bar, see:
# https://cran.r-project.org/web/packages/prettymapr/prettymapr.pdf
# NAD83 = epsg:4269; WGS84, GRS80 = epsg:42310; NAD83, GRS80 = epsg:7019,
# but function doesn't recognize them
plotGMcompare <- function(bound, point, maskvars, closemap = FALSE) {
  # function to handle numbers
  numformat <- function(num) {
    format(as.numeric(gsub(",", "", num)), big.mark=",", scientific=FALSE)
  }

  # set map size
  dev.new(noRStudioGD = TRUE, res = 1200, width = 20, height = 14)
  # enable display list
  dev.control('enable')
  # plot shapefiles ####
  graphics::par(mar = c(3.5,0,2,0), mgp = c(0,0,0)) # bottom, left, top, right

  mylegend <- "original areas"
  sp::plot(bound, border = "red", col = "transparent", lty = "solid", lwd = 1)
  if (class(point) == "SpatialPointsDataFrame") {
    sp::plot(point, col = "black", pch = 1, add = TRUE)
    mylegend <- c("Boundaries", "Points")
  } else if (class(point) == "SpatialPolygonsDataFrame") {
    sp::plot(point, border = "black", col = "transparent",
             lty = "solid", lwd = 2, add = TRUE)
    mylegend <- c("Original areas", "Aggregated areas")
  }

  legend("topleft", legend = mylegend,
         fill = "White", border = c("red", "black"), cex = 1,
         bty = "n", inset = 0, y.intersp = 1.25)

  # add labels ####
  mytitle <- "Map showing boundaries and points"
  mysub <- paste("Geomasking distance:", maskvars$min, "to", maskvars$max,
                 maskvars$unit)
  title(mytitle, sub = mysub, cex.main = 2)

  # draw arrow and scale bar ####
  if (requireNamespace("prettymapr", quietly = TRUE)) {
    prettymapr::addnortharrow(pos = "bottomleft", padin = c(0.2, 0.05),
                              scale = .5, lwd = 1, border = "black",
                              cols = c("white", "black"), text.col = "black")
    prettymapr::addscalebar(plotunit = "mi", plotepsg = 4269, widthhint = 0.25,
                            unitcategory = "imperial", htin = 0.1, lwd = 1,
                            padin = c(0.7, 0.05), style = "ticks",
                            linecol = "black", tick.cex = 0.7,
                            labelpadin = 0.08, label.cex = 0.8,
                            label.col = "black", pos = "bottomleft")
  }
  # save map ####
  map <- recordPlot()
 # default bottom, left, top, right
  graphics::par(mar=c(5,4,4,2)+.1, mgp = c(3, 1, 0))
  if (closemap) {
    dev.off()
  }
  return(map)
}
