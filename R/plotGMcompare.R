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
#' @param bound     Boundary layer.
#' @param original  Original points or areas spatial layer.
#' @param shifted   Shifted points or areas spatial layer.
#' @param maskvars  List of settings for geomasking, including unit type and
#'                  minimum, and maximum values.
#' @param closemap  Boolean denoting whether to close the map window after
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
plotGMcompare <- function(bound, original = NULL, shifted = NULL, maskvars,
                          closemap = FALSE) {

  # set map size
  dev.new(noRStudioGD = TRUE, res = 1200, width = 20, height = 14)
  dev.control('enable') # enable display list
  graphics::par(mar = c(3.5,0,2,0), mgp = c(0,0,0)) # bottom, left, top, right

  # plot shapefiles ----
  # always draw boundary layer first
  # then can show original layer, shifted layer, or both
  plot(sf::st_geometry(bound), border = "green", col = "transparent",
       lty = "solid", lwd = 1)
  mylbl <- "Boundaries"
  mycol <- "green"
  mytype <- "polygon"

  if (!is.null(original)) {
    mycol <- c(mycol, "blue")
    if (sum(sf::st_geometry_type(original) == "POINT") == nrow(original)) {
      plot(sf::st_geometry(original), col = "blue", pch = 20, add = TRUE)
      mylbl <- c(mylbl, "Original points")
      mytype <- c(mytype, "point")
    } else {
      plot(sf::st_geometry(original), border = "blue", col = "transparent",
           lty = "solid", lwd = 1, add = TRUE)
      mylbl <- c(mylbl, "Original areas")
      mytype <- c(mytype, "polygon")
    }
  }
  if (!is.null(shifted)) {
    mycol <- c(mycol, "red")
    if (sum(sf::st_geometry_type(shifted) == "POINT") == nrow(shifted)) {
      plot(sf::st_geometry(shifted), col = "red", pch = 20, add = TRUE)
      mylbl <- c(mylbl, "Shifted points")
      mytype <- c(mytype, "point")
    } else {
      plot(sf::st_geometry(shifted), border = "red", col = "transparent",
           lty = "solid", lwd = 1, add = TRUE)
      mylbl <- c(mylbl, "Shifted areas")
      mytype <- c(mytype, "polygon")
    }
  }

  if ("point" %in% mytype) {
    legend("topleft", legend = mylbl, fill = "White", bty = "n", cex = 1,
           border = c("green", rep("white", length(mycol) - 1)), inset = 0,
           y.intersp = 1.25)
    legend("topleft", legend = rep("", length(mylbl)), pch = 20, cex = 1,
           col = c("white", mycol[!mycol == "green"]), inset = 0, bty = "n",
           y.intersp = 1.25)

  } else {
    legend("topleft", legend = mylbl, fill = "White", bty = "n", cex = 1,
           border = mycol, inset = 0, y.intersp = 1.25)

  }

  # add labels ----
  # function to handle numbers
  numformat <- function(num) {
    format(as.numeric(gsub(",", "", num)), big.mark=",", scientific=FALSE)
  }
  mytitle <- "Map showing ... (need to code this)"
  mysub <- paste("Geomasking distance:", numformat(maskvars$min), "to",
                 numformat(maskvars$max), maskvars$unit)
  title(mytitle, sub = mysub, cex.main = 2)

  # draw arrow and scale bar ####
  # if (requireNamespace("prettymapr", quietly = TRUE)) {
  #   prettymapr::addnortharrow(pos = "bottomleft", padin = c(0.2, 0.05),
  #                             scale = .5, lwd = 1, border = "black",
  #                             cols = c("white", "black"), text.col = "black")
  #   prettymapr::addscalebar(plotunit = "mi", plotepsg = 4269, widthhint = 0.25,
  #                           unitcategory = "imperial", htin = 0.1, lwd = 1,
  #                           padin = c(0.7, 0.05), style = "ticks",
  #                           linecol = "black", tick.cex = 0.7,
  #                           labelpadin = 0.08, label.cex = 0.8,
  #                           label.col = "black", pos = "bottomleft")
  # }

  # save map ####
  map <- recordPlot()
 # default bottom, left, top, right
  graphics::par(mar=c(5,4,4,2)+.1, mgp = c(3, 1, 0))
  if (closemap) {
    dev.off()
  }
  return(map)
}
