#' Write the geomasker log
#'
#' This function writes a log of the geomasking process. It reports the
#' input and output datasets, variables and settings used, map projection,
#' and program start and end times.
#'
#' @param area         Spatial layer.
#' @param filevars     List of file names and paths. Of relevance to this
#'                     function are the filename, filein, and the combined save
#'                     path and save name, userout.
#' @param mysettings   List of system settings, including version, pkgdate,
#'                     starttime, and the booleans kml and exists.
#' @param maskvars     List of settings for calculating masked locations.
#' @param settingsfile R data file (*.Rdata) produced as part of the
#'                     geomasker's output. This file saves all settings for
#'                     the geomasker. Other options can be set to NULL only if
#'                     this option is defined.
#'
#' @details
#'
#' Notes on using the settingsfile option:
#'
#' 1. You will get an error if you moved the input shapefile before running
#'    the function with this option, since the function needs to access the
#'    input shapefile to recreate the log.
#' 2. Reading in an *.Rdata file from a previous version of the geomasker may
#'    result in incorrect elapsed time and geomasker version numbers being
#'    written to the log, or in errors that cause the log file to be
#'    incomplete, due to changes in settings saved to the *.Rdata file as the
#'    geomasker has evolved.
#'
#' @examples
#'
#' # if you run this example, it saves "geomasked_points.log" to your working
#' # directory
#'
#' if (interactive()) {
#'
#' maskvars <- list(
#'   min = 100,
#'   max = 1000,
#'   unit = "meters",
#'   projection = TRUE,
#'   point_id = "POINTID",
#'   bound_id = "GEOID10"
#' )
#'
#' filevars <- list(
#'   pointfile = "points",     # original locations file
#'   pointpath = getwd(),         # original locations path
#'   boundfile = "tracts",        # original boundary file
#'   boundpath = getwd(),         # original boundary path
#'   boundin = paste(getwd(), "tracts", sep = "/"),
#'   pathout = getwd(),           # save path
#'   fileout = "geomasked_points" # save file
#' )
#'
#' mysettings <- list(
#'   starttime = Sys.time(),
#'   version = "1.0",
#'   pkgdate = format(Sys.Date(), "%m-%d-%Y"),
#'   endtime = Sys.time() + 250,
#'   exists = TRUE,
#'   kml = FALSE
#' )
#'
#' ot <- tigris::tracts("NY", "Onondaga", year = 2010)
#'
#' writeGMlog(
#'   area = ot,
#'   filevars = filevars,
#'   mysettings = mysettings,
#'   maskvars = maskvars
#' )
#' }
#' @export


writeGMlog <- function(area = NULL, maskvars, filevars, mysettings = NULL,
                       settingsfile = NULL) {
  # rewrite to remove the need for the area file -
  # only need it for projection, which I could save in settings
  # set up ---
  if (!is.null(settingsfile)) {
    load(settingsfile)
    if (is.null(mysettings)) { # rerunning failed log
      mysettings <- list(version = utils::packageDescription("geomask")$Version,
                         pkgdate = utils::packageDescription("geomask")$Date,
                         starttime = Sys.time(),
                         endtime = Sys.time()) # needed for the log
    }
    mysettings$exists <- file.exists(paste(filevars$userout, "shp", sep = "."))
    area <- sf::st_read(dsn = filevars$pathin, layer = filevars$filein)
  }

  # fill in full list of names below; code will error otherwise
  vars <- names(data.frame(area))
  newvars <- c("point_id", "bound_id", "orig_lon", "orig_lat",
               "mask_lon", "mask_lat", "flag", "geometry")
  vars <- vars[!vars %in% newvars]
  oldvars <- ""
  for (i in 1:(length(vars)-3)) {
    oldvars <- paste0(oldvars, vars[i], ", ")
    if (i %% 6 == 0) {
      oldvars <- paste0(oldvars, "\n", paste(rep(" ", 22), collapse = ""))
    }
  }

  # begin log file ----
  logfile <- paste(filevars$fileout, "log", sep = ".")
  logfile <- paste(filevars$pathout, logfile, sep = "/")

  # settings ----
  logtext <- c("NYSDOH Geomasking Tool Log",
               "\n  Version & date:", mysettings$version, mysettings$pkgdate,
               "\n  Date run:", as.character(as.Date(Sys.time())),
               "\n  Time tool took to run:",
               round(difftime(mysettings$endtime, mysettings$starttime, units = "mins"),
                     digits = 2), "minutes")
  write(logtext, file = logfile, ncolumns = length(logtext), append = FALSE)

  # input file ----
  logtext <- c("\nInput file:          ", filevars$pointin,
               "\n  Projection:        ",
               sf::st_crs(area, parameters = TRUE)$proj4string,
               "\n  Field names:       ", oldvars,
               "\n  Identifier:        ", maskvars$point_id,
               "\n  Boundary file:     ", filevars$boundin,
               "\n  Boundary variable: ", maskvars$bound_id,
               "\nOutput file:         ", filevars$userout)
  write(logtext, file = logfile, ncolumns = length(logtext), append = TRUE)

  # masking settings ----
  logtext <- c("\nMasking settings:    ",
               "\n  Minimum distance:  ",
               format(maskvars$min, big.mark=",", scientific=FALSE),
               maskvars$unit,
               "\n  Minimum distance:  ",
               format(maskvars$max, big.mark=",", scientific=FALSE),
               maskvars$unit)
  write(logtext, file = logfile, ncolumns = length(logtext), append = TRUE)

  # files ----
  if (!mysettings$exists) {
    logtext <- "\n  The shapefiles failed to save. "
    write(logtext, file = logfile, ncolumns = length(logtext), append = TRUE)
  } else {
    # data dictionary ----
    logtext <- c("\n  Variables created by the geomasking process:",
                 paste0(filevars$fileout, ".shp"),
                 "\n    Variables created by GAT:",
                 "\n        point_id:", "duplicate of original locations identifier,",
                 maskvars$point_id,
                 "\n        bound_id:", "duplicate of boundaries identifier,",
                 maskvars$bound_id,
                 "\n        orig_lat:", "original location latitude",
                 "\n        orig_lon:", "original location longitude",
                 "\n        mask_lat:", "shifted location latitude",
                 "\n        mask_lon:", "shifted location longitude",
                 "\n        flag:    ", "number corresponds to the number of times buffers had ",
                 "\n                 ", "to be recalculated to create a valid area to select a ",
                 "\n                 ", "masking point using the equation",
                 "\n                 ", "  new maximum value = prior minimum value",
                 "\n                 ", "  new minimum value = half of prior minimum value",
                 "\n                 ", "e.g. flag = 2 means buffers had to be recalculated twice")
    write(logtext, file = logfile, ncolumns = length(logtext), append = TRUE)

    # saved files ----
    oshp <- paste(filevars$pointfile, "shp", sep = ".")
    bshp <- paste(filevars$boundfile, "shp", sep = ".")
    orshp <- paste(paste(filevars$fileout, "old", sep = "_"), "shp", sep = ".")
    nshp <- paste(paste(filevars$fileout, "new", sep = "_"), "shp", sep = ".")
    bfshp <- paste(paste(filevars$fileout, "buffer", sep = "_"), "shp", sep = ".")

    logtext <- c("All files have been saved to", filevars$pathout,
                 "\n  Original shapefile:  ", oshp,
                 "\n  Boundary shapefile:  ", bshp,
                 "\n  Original shapefile with both original and masked",
                 "latitude and longitude:",
                 "\n                       ", orshp,
                 "\n  Masked shapefile:    ", nshp,
                 "\n  Buffer shapefile:    ", bfshp)
    write(logtext, file = logfile, ncolumns = length(logtext), append = TRUE)
  }
  logtext <- c("\n  Maps:                ",
               paste(filevars$fileout, "map.pdf", sep = "_"),
               "\n  Log file:            ",
               paste0(filevars$fileout, ".log"),
               "\n  R settings file:     ",
               paste(filevars$fileout, "settings.Rdata", sep = "_"))
  write(logtext, file = logfile, ncolumns = length(logtext), append = TRUE)
  if (mysettings$kml) {
    logtext <- c("\n  KML file (raw):    ",
                 paste0(filevars$fileout, ".kml"),
                 "\n  KMZ file (zipped): ",
                 paste0(filevars$fileout, ".kmz"))
  } else {
    logtext <- c(logtext, "\n  You chose not to write a KML file.")
  }

  write(logtext, file = logfile, ncolumns = length(logtext), append = TRUE)
  #end code to create log file
}

