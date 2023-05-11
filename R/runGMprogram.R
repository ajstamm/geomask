#' Run the geomasker
#'
#' @param settings   The filepath to an Rdata file previously created by the
#'                   Geomasker.
#' @param bgcol      Text string containing UI background color.
#' @param buttoncol  Text string containing UI button color.
#'
#' @description
#' This function runs the full geomasking tool.
#'
#' @details
#' This function can read in an optional settings file from a prior run if,
#' for example, you would like to run the geomasker with two different
#' distances, but keep all other settings the same.
#'
#'
#'
#' @examples
#'
#' if (interactive()) {
#' # this code will run the full program
#' runGMprogram()
#' }
#'
#' @export
#'

runGMprogram <- function(bgcol = "thistle1", buttoncol = "plum2",
                         settings = NULL) {

  #---- settings ----
  # pre-load lists
  myshps <- list()
  temp <- list(quit = FALSE, backopt = TRUE)
  step <- 1
  quitopt <- "Quit geomasker"

  if (!is.null(settings)) {
    load(settings)
    step <- 6
    mysettings$version = "1.4.0" # packageDescription("geomask")$Version,
    mysettings$pkgdate = "2023-04-17" # packageDescription("geomask")$Date,
    mysettings$starttime = Sys.time()
    filevars$userout <- paste0(filevars$userout, "_2")
    filevars$fileout <- paste0(filevars$fileout, "_2")
    myshps$point <- sf::st_read(dsn = filevars$pointpath,
                                layer = filevars$pointfile)
    myshps$bound <- sf::st_read(dsn = filevars$boundpath,
                                layer = filevars$boundfile)
  } else {
    maskvars <- list(min = 100, max = 1000, unit = "meters")
    filevars <- list(pointin = "", boundin = "")
    # add ways to define and include point ID and boundary ID by name
    mysettings <- list(version = "1.4.0", # packageDescription("geomask")$Version,
                       pkgdate = "2023-04-17", # packageDescription("geomask")$Date,
                       starttime = Sys.time()) # needed for the log
  }

  #---- progress bar ----
  pb <- list(title = paste("NYSDOH Geomasking Tool",
                           mysettings$version, mysettings$date),
             label = paste("The NYSDOH Geomasking Tool is running.\n",
                           "Please wait for dialogs."))
  tpb <- tcltk::tkProgressBar(title = pb$title, label = pb$label, min = 0,
                              max = 10, initial = 0, width = 400)

  #---- user input ----

  while (step < 7) { # gwen: get user input until finalized
    #---- step 1: request point shapefile ----
    # request or create point ID
    while (step == 1 & !temp$quit) {
      pb <- list(title = "NYSDOH Geomask Tool: identify shapefile",
                 label = "Identifying and selecting the location shapefile.")
      tcltk::setTkProgressBar(tpb, value = step, title = pb$title,
                              label = pb$label)

      temp$msg <- "Select the shapefile you wish to mask"
      temppath <- gatpkg::locateGATshapefile(myfile = filevars$pointin,
                                             myprogram = "the geomasker",
                                             step = step, msg = temp$msg,
                                             bgcol = bgcol,
                                             buttoncol = buttoncol)
      filevars$pointin <- temppath$userin
      filevars$pointfile <- temppath$filein
      filevars$pointpath <- temppath$pathin
      rm(temppath)

      if (filevars$pointin == "cancel") {
        step <- 10
        temp$quit <- TRUE
      } else {
        # only care if file is point-level
        myshps$point <- sf::st_read(dsn = filevars$pointpath,
                                    layer = filevars$pointfile)

        # error checking
        if (!sum(sf::st_geometry_type(myshps$point) == "POINT") ==
            nrow(myshps$point)) {
          # update this later if "shift polygons" is added
          # message: wrong kind of shapefile; repeat dialog
          temp$msg <- paste("The shapefile", filevars$filein,
                            "contains non-point geographies.\n",
                            "Please select a new shapefile.")
          tcltk::tkmessageBox(title = "Shapefile invalid", type = "ok",
                              icon = "error", message = temp$msg)
        } else {
          # shift to error checking
          maskvars$projection <- sum(grepl("long", sf::st_crs(myshps$point))) > 0

          if (!maskvars$projection) {
            # if FALSE, convert to lat/long
            # WGS84 is common: EPSG code 4326
            # allow user to select projection from list?
            myshps$point <- sf::st_set_crs(myshps$point, 4326)
            myshps$point <- sf::st_transform(myshps$point, 4326)
          }

          temp$msg <- paste("Please select the variable that",
                            "\nidentifies your locations.")
          temp$hlp <- paste0("Select your location ID variable. \n",
                             "  \u2022  To continue,  click 'Next >'. \n",
                             "  \u2022  To quit the geomasker, click 'Cancel'.")

          temp$items <- c()
          temp$data <- data.frame(myshps$point)
          temp$names <- names(temp$data)

          for (i in 1:(ncol(temp$data) - 1)) {
            t <- table(temp$data[, temp$names[i]])
            if (length(t) == nrow(temp$data)) {
              temp$items <- c(temp$items, temp$names[i])
            }
          }

          maskvars$point_id <-
            gatpkg::inputGATvariable(mylist = temp$items, instruction = temp$msg,
                                     title = "Location ID Variable", checkopt = "",
                                     checkbox = FALSE, help = temp$hlp, step = step,
                                     helppage = "inputGATvariable", myvar = NULL,
                                     check = "", backopt = temp$backopt,
                                     bgcol = bgcol, buttoncol = buttoncol,
                                     quitopt = quitopt)$myvar

          if (maskvars$point_id == "cancel") {
            step <- 10
            temp$quit <- TRUE
          } else if (!temp$backopt) {
            step <- 7
          } else {
            step <- step + 1
          }
        }
      }


    }

    #---- step 2: request boundary shapefile ----
    # request boundary variable/ID or create one
    while (step == 2 & !temp$quit) {
      pb <- list(title = "NYSDOH Geomasking Tool: identify boundary file",
                 label = "Identifying and selecting the boundary shapefile.")
      tcltk::setTkProgressBar(tpb, value = step, title = pb$title,
                              label = pb$label)

      temp$msg <- "Select the boundary shapefile"
      temppath <- gatpkg::locateGATshapefile(myfile = filevars$boundin,
                                             step = step, msg = temp$msg)
      filevars$boundin <- temppath$userin
      filevars$boundfile <- temppath$filein
      filevars$boundpath <- temppath$pathin
      rm(temppath)

      if (filevars$boundin == "cancel") {
        step <- 10
        temp$quit <- TRUE
      } else {
        # only care if file is polygon-level
        myshps$bound <- sf::st_read(dsn = filevars$boundpath,
                                    layer = filevars$boundfile)
        # error checking
        temp$error <- FALSE
        if (!sum(sf::st_geometry_type(myshps$bound) == "POLYGON") ==
            nrow(myshps$bound)) {
          temp$error <- TRUE
        }
        if (temp$error) {
          temp$msg <- paste("The shapefile", filevars$boundin,
                            "contains non-polygon geographies. \n",
                            "Please select a new shapefile.")
          tcltk::tkmessageBox(title = "Shapefile invalid", type = "ok",
                              icon = "error", message = temp$msg)
          temp$error <- FALSE
        } else {
          myshps$bound <- sf::st_set_crs(myshps$bound, sf::st_crs(myshps$point))
          myshps$bound <- sf::st_transform(myshps$bound, sf::st_crs(myshps$point))

          # myshps$bound <- dplyr::mutate(myshps$bound, bound_id = dplyr::row_number())
          # myshps$bound <- dplyr::select(myshps$bound, bound_id)

          msg <- paste("Please select the variable that",
                       "\nidentifies boundaries within",
                       "\nwhich your points should be relocated.")
          hlp <- paste0("Select your boundary variable. \n",
                        "  \u2022  To continue,  click 'Next >'. \n",
                        "  \u2022  To quit the Geomasker, click 'Cancel'.")

          temp$items <- c()
          temp$data <- data.frame(myshps$bound)
          temp$names <- names(temp$data)

          for (i in 1:(ncol(temp$data) - 1)) {
            t <- table(temp$data[, temp$names[i]])
            if (length(t) == nrow(temp$data)) {
              temp$items <- c(temp$items, temp$names[i])
            }
          }

          maskvars$bound_id <-
            gatpkg::inputGATvariable(mylist = temp$items, instruction = temp$msg,
                                     title = "Boundary Variable", checkopt = "",
                                     checkbox = FALSE, help = temp$hlp, step = step,
                                     helppage = "inputGATvariable", myvar = NULL,
                                     check = "", backopt = temp$backopt,
                                     bgcol = bgcol, buttoncol = buttoncol,
                                     quitopt = quitopt)$myvar


          if (maskvars$point_id == "cancel") {
            step <- 10
            temp$quit <- TRUE
          } else if (!temp$backopt) {
            step <- 7
          } else {
            step <- step + 1
          }
        }
      }


    }

    #---- step 3: min/max distance ----
    while (step == 3 & !temp$quit) {
      pb <- list(title = "NYSDOH Geomasking Tool: select distances",
                 label = "Selecting the minimum and maximum distances.")
      tcltk::setTkProgressBar(tpb, value = step, title = pb$title,
                              label = pb$label)

      templist <- selectGMdistances(step = step, min = maskvars$min,
                                    max = maskvars$max, unit = maskvars$unit,
                                    backopt = temp$backopt,
                                    bgcol = bgcol, buttoncol = buttoncol,
                                    quitopt = quitopt)

      maskvars$min <- as.numeric(gsub(",", "", templist$min))
      maskvars$max <- as.numeric(gsub(",", "", templist$max))
      maskvars$unit <- templist$unit

      # error checking
      if (maskvars$unit == "cancel") {
        step <- 10
        temp$quit <- TRUE
      } else if (maskvars$unit == "back") {
        step <- step - 1
      } else {
        # check that minimum < maximum and minimum > 0
        temp$msg <- ""
        if (maskvars$min < 0) {
          temp$msg <- "Minimum distance must be at least 0. \n"
        }
        if (maskvars$max < maskvars$min) {
          temp$msg <- paste0(temp$msg, "Maximum distance must be greater than",
                             "minimum distance. \n")
        }
        if (maskvars$max == maskvars$min) {
          temp$checkmsg <- paste(" The minimum and maximum distances are the",
                                 "same. \n This will result in points being",
                                 "moved a fixed distance. \n Are you sure you",
                                 "want to proceed?")
          temp$check <- gatpkg::inputGATmessage(title = "Distances confirmation",
                                                msg = temp$checkmsg, step = step,
                                                buttonopt = "Fix settings",
                                                backopt = FALSE)
          if (temp$check == "cancel") {
            temp$msg <- paste0(temp$msg, "Minimum and maximum distances are the",
                               "same. \n")
          }
        }
        if (is.na(maskvars$min)) {
          temp$msg <- paste0(temp$msg, "The minimum distance entered is not a",
                             "number. \n")
        }
        if (is.na(maskvars$max)) {
          temp$msg <- paste0(temp$msg, "The maximum distance entered is not a",
                             "number. \n")
        }
        if (temp$msg != "") {
          temp$msg <- paste0(temp$msg, "Please reselect your distances.")
          tcltk::tkmessageBox(title = "Distance selections invalid", type = "ok",
                              icon = "error", message = temp$msg)
        } else {
          if (!temp$backopt) {
            step <- 7
          } else {
            step <- step + 1
          }
        }
      }
      rm(templist)
    }

    #---- step 4: kml ----
    while (step == 4 & !temp$quit) {
      pb <- list(title = "NYSDOH Geomasking Tool: save KML?",
                 label = "Identifying whether to save a KML file.")
      tcltk::setTkProgressBar(tpb, value = step, title = pb$title, label = pb$label)

      mysettings$kml <- gatpkg::saveGATkml(step = step, backopt = TRUE,
                                           bgcol = bgcol, buttoncol = buttoncol,
                                           quitopt = quitopt)

      if (mysettings$kml %in% c("Yes", "No")) {
        if (mysettings$kml == "Yes") {
          mysettings$kml <- TRUE # save the kml
        } else {
          mysettings$kml <- FALSE # don't save the kml
        }
        if (!temp$backopt) {
          step <- 7
        } else {
          step <- step + 1
        }
      } else if (mysettings$kml == "cancel") {
        temp$quit <- TRUE
        step <- 10
      } else {
        step <- step - 1
      }
    }

    #---- step 5: save location ----
    while (step == 5 & !temp$quit) {
      # identify the save files' name and location
      pb <- list(title = "NYSDOH Geomasking Tool: identify save file",
                 label = "Identifying the name and location of your save file.")
      tcltk::setTkProgressBar(tpb, value = step, title = pb$title,
                              label = pb$label)

      saves <- gatpkg::saveGATfiles()
      filevars$userout <- saves$userout
      filevars$fileout <- saves$fileout
      filevars$pathout <- saves$pathout
      rm(saves)

      if (filevars$fileout == "cancel") {
        temp$quit <- TRUE
        step <- 10
      } else if (!temp$backopt) {
        step <- 7
      } else {
        step <- step + 1
      }
    }

    #---- step 6: confirmation ----
    while (step == 6 & !temp$quit) {
      pb <- list(title = "NYSDOH Geomasking Tool: confirm settings",
                 label = "Confirming your geomasking settings.")
      tcltk::setTkProgressBar(tpb, value = step, title = pb$title,
                              label = pb$label)

      temp$cancel <- confirmGMsettings(maskvars = maskvars, filevars = filevars,
                                       savekml = mysettings$kml, step = step,
                                       bgcol = bgcol, buttoncol = buttoncol,
                                       quitopt = quitopt)


      if (temp$cancel %in% c("Yes", "None")) {
        step <- 7 # done with user input
        # add population file
      } else if (temp$cancel == "back") { # now irrelevant
        step <- step - 1 # go back one
      } else if (temp$cancel == "cancel") {
        step <- 7
        temp$quit <- TRUE
      } else if (grepl("[0-9]", temp$cancel)) {
        temp$backopt <- FALSE
        step <- as.numeric(gsub("[^0-9]", "", temp$cancel))
      }

    }
  } # end while step
  mysettings$quit <- temp$quit
  # rm(temp)

  #---- automatic processing ----
  if (!mysettings$quit) {
    # calculate locations ----
    # at this point, step = 7
    pb <- list(title = "NYSDOH Geomasking Tool: shift locations",
               label = "Shifting locations within their boundaries.")
    tcltk::setTkProgressBar(tpb, value = step, title = pb$title,
                            label = pb$label)

    set.seed(mysettings$starttime)
    myshps <- calculateGMpoints(myshps = myshps, maskvars = maskvars)



    # plot locations ----
    step <- step + 1 # 8
    pb <- list(title = "NYSDOH Geomasking Tool: plot locations",
               label = "Plotting the original and shifted locations.")
    tcltk::setTkProgressBar(tpb, value = step, title = pb$title,
                            label = pb$label)

    myplot <- plotGMcompare(bound = myshps$bound, original = myshps$point,
                            shifted = myshps$shifted, maskvars = maskvars)

    # save files ----
    step <- step + 1 # 9
    pb <- list(title = "NYSDOH Geomasking Tool: save files",
               label = "Saving your files.")
    tcltk::setTkProgressBar(tpb, value = step, title = pb$title,
                            label = pb$label)

    # save shapefiles
    temp$newfile = paste(filevars$fileout, "new", sep = "_")
    temp$oldfile = paste(filevars$fileout, "old", sep = "_")
    temp$bufferfile = paste(filevars$fileout, "buffer", sep = "_")
    sf::write_sf(myshps$old_full, dsn = filevars$pathout,
                 layer = temp$oldfile, driver = "ESRI shapefile")
    sf::write_sf(myshps$buffer, dsn = filevars$pathout,
                 layer = temp$bufferfile, driver = "ESRI shapefile")
    sf::write_sf(myshps$new_full, dsn = filevars$pathout,
                 layer = temp$newfile, driver = "ESRI shapefile")

    # save kml if desired
    if (mysettings$kml) { # now includes descriptions
      gatpkg::writeGATkml(myshp = myshps$new_full, filename = filevars$fileout,
                          filepath = filevars$pathout, myidvar = "point_id")
    }

    # save plot(s)
    grDevices::pdf(paste(filevars$userout, "map.pdf", sep = "_"), onefile=TRUE,
                   width = 10, height = 7)
    grDevices::replayPlot(myplot)
    grDevices::dev.off() # need to close pdf file

    # save settings
    save(file = paste(filevars$userout, "settings.Rdata", sep = "_"),
         list = c("filevars", "maskvars", "mysettings"))


    # save log ----
    mysettings$endtime <- Sys.time()
    mysettings$exists <- file.exists(paste0(filevars$userout, ".shp"))

    writeGMlog(area = myshps$point, maskvars = maskvars, filevars = filevars,
               mysettings = mysettings, settingsfile = settings)


    # print message ----
    step <- step + 1 # 10
    pb <- list(title = "NYSDOH Geomasking Tool: finish",
               label = "The program has completed.")
    tcltk::setTkProgressBar(tpb, value = step, title = pb$title,
                            label = pb$label)


    if (mysettings$exists) {
      msg <- paste0("The geomasker is finished. Your files were saved to ",
                    filevars$pathout,
                    ". \nPlease see the log file for more details.")
      tcltk::tkmessageBox(title = "Program finished", type = "ok",
                          icon = "info", message = msg)

      msg <- paste0("\n\nThe following files have been written to the folder \n",
                    filevars$pathout, ": \n  ",
                    "Shapefile of new locations:  \n  ",
                    filevars$fileout, "_new.dbf \n  ",
                    filevars$fileout, "_new.prj \n  ",
                    filevars$fileout, "_new.shp \n  ",
                    filevars$fileout, "_new.shx \n",
                    "Shapefile of old locations:  \n  ",
                    filevars$fileout, "_old.dbf \n  ",
                    filevars$fileout, "_old.prj \n  ",
                    filevars$fileout, "_old.shp \n  ",
                    filevars$fileout, "_old.shx \n",
                    "Shapefile of buffers, for assessment: \n  ",
                    filevars$fileout, "_buffer.dbf \n  ",
                    filevars$fileout, "_buffer.prj \n  ",
                    filevars$fileout, "_buffer.shp \n  ",
                    filevars$fileout, "_buffer.shx \n",
                    "Additional assessment and process files: \n  ",
                    filevars$fileout, "_map.pdf \n  ",
                    filevars$fileout, ".log \n  ",
                    filevars$fileout, "_settings.Rdata \n")

      if (mysettings$kml==TRUE) {
        msg <- paste0(msg,
                      "KML files: \n  ",
                      filevars$fileout, ".kml \n  ",
                      filevars$fileout, ".kmz \n")
      }
      msg <- paste0(msg, "\nSee the log file for more details.")

      message(msg)
    } else {
      # the shapefile failed to write
      msg <- "Something went wrong. Your shapefiles were not saved."
      tcltk::tkmessageBox(title = "Shapefile save failed", type = "ok",
                          icon = "error", message = msg)
    }

  } else {
    # "else" occurs only if geomasking is cancelled in the input phase
    msg <- "You have chosen to cancel the geomasker."
    tcltk::tkmessageBox(title = "Process cancelled",
                        message = msg, type = "ok", icon = "warning")
  }
  close(tpb)

}


