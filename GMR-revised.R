#---- header ------------------------------------------------------------------
# revision author : Abigail Stamm
# original author : Gwen Babcock
# revision date : 4 January 2021
# last prior revision: V1.3 Nov 2013
#
#---- Gwen's notes ------------------------------------------------------------
# purpose : to jitter point coordinates
# 	input : point-level shapefile
#   process : randomly move the points a specified distance
#   output : shapefile, KML file, log, maybe map(s)
#
# platform :
# 	original : Programmed in R2.13.0 for Windows XP
#   latest prior : adapted to work in R3.0.2 for Windows 7
#   development time : June 2011 - November 2013
#   current: R4.0.4 on Windows 10
#
# libraries : sp, rgdal, tcltk, svDialogs (to be updated)
#
#---- future plans ------------------------------------------------------------
#   1. add features to keep the points within an area and to make the
#      distance the point is moved depend on population density
#   2. requires shapefiles to be in lat/long; look at function spTransform()
#      to change projection if needed
#   3. add code to check if input file is points or not; offer to calculate
#      centroids if it's polygons or lines?
#
#---- change log --------------------------------------------------------------
#   v1.1 June 17, 2011, add minimum distance
#   v1.2 July 20, 2011, add log file and adjust maximum distances if needed
#        Aug 9, 2011 improve error catching for opening files
#   v1.3 November, 2013 adapt to work with R 3.0.2
#	       changed guiDlgOpen to dlgOpen, guiDlgMessage to dlgMessage, dlgSave
#	       add $res after dlgOpen and dlgMessage where needed
#   v1.4 (in progress)
#        convert to package
#        change dlg* to tcltk functions based on gatpkg
#
#------------------------------------------------------------------------------


#---- libraries ----
devtools::load_all("P:/Sections/EHS/Staff/ajs11/R/pkg/geomask/R")
# confirmed required
  #library(tcltk)
  #library(rgdal)
  #library(gatpkg)
  #library(sp)
# requires GAT because I don't feel like being redundant

#---- settings ----
# add ways to define and include point ID and boundary ID by name
mysettings <- list(version = "1.4.0",
                   pkgdate = "2023-04-17",
                   # packageDescription("geomask")$Version,
                   # packageDescription("geomask")$Date,
                   starttime = Sys.time()) # needed for the log

# pre-load lists
myshps <- list()
temp <- list(quit = FALSE, backopt = TRUE)
step <- 1

# for testing; read in settings file
# later, set up menu confirmation like in GAT
settings <- paste("P:/Sections/EHS/Staff/ajs11/R/tools/geomaskTest/results",
                  "save_test_settings.Rdata", sep = "/")

if (!is.null(settings)) {
  load(settings)
  step <- 6
  temp$flagconfirm <- TRUE
  filevars$userout <- paste0(filevars$userout, "_2")
  filevars$fileout <- paste0(filevars$fileout, "_2")
  myshps$point <- sf::st_read(dsn = filevars$pointpath,
                              layer = filevars$pointfile)
  myshps$bound <- sf::st_read(dsn = filevars$boundpath,
                              layer = filevars$boundfile)
} else {
  maskvars <- list(min = 100, max = 1000, unit = "meters")
  filevars <- list(pointin = "", boundin = "")
}





#---- progress bar ----
pb <- list(title = paste("NYSDOH Geomask Tool",
                         mysettings$version, mysettings$date),
           label = "NYSDOH Geomask Tool is running. Please wait for dialogs.")
tpb <- tcltk::tkProgressBar(title = pb$title, label = pb$label, min = 0,
                            max = 26, initial = 0, width = 400)

#---- user input ----

while(step < 7) { # gwen: get user input until finalized
  #---- step 1: request point shapefile ----
  # request or create point ID
  while (step == 1 & !temp$quit) {
    pb <- list(title = "NYSDOH Geomask Tool: identify shapefile",
               label = "Identifying and selecting the point-level shapefile.")
    tcltk::setTkProgressBar(tpb, value = step, title = pb$title,
                            label = pb$label)

    temp$msg <- "Select the shapefile to mask"
    temppath <- gatpkg::locateGATshapefile(myfile = filevars$pointin,
                                           step = step, msg = temp$msg)
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

        step <- 2
      }
    }

    temp$msg <- paste("Please select the variable that \nidentifies your points.")
    temp$hlp <- paste0("Select your point ID variable. \n",
                  "  \u2022  To continue,  click 'Next >'. \n",
                  "  \u2022  To quit the Geomasker, click 'Cancel'.")

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
                               title = "Boundary Variable", checkopt = "",
                               checkbox = FALSE, help = temp$hlp, step = step,
                               helppage = "inputGATboundary", myvar = NULL,
                               check = borders, backopt = FALSE)

  }

  #---- step 2: request boundary shapefile ----
  # request boundary variable/ID or create one
  while (step == 2 & !temp$quit) {
    pb <- list(title = "NYSDOH Geomask Tool: identify boundary file",
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
      } else {
        step <- step + 1
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

        myshps$bound <- dplyr::mutate(myshps$bound, bound_id = dplyr::row_number())
        myshps$bound <- dplyr::select(myshps$bound, bound_id)

        step <- 3
      }
    }

    msg <- paste("Please select the variable that \nidentifies boundaries within",
                 "\nwhich your points should be relocated.")
    hlp <- paste0("Select your boundary variable. \n",
                  "  \u2022  To continue,  click 'Next >'. \n",
                  "  \u2022  To quit the Geomasker, click 'Cancel'.")

    maskvars$bound_id <-
      gatpkg::inputGATvariable(mylist = boundaryitems, instruction = msg,
                               title = "Boundary Variable", checkopt = "",
                               checkbox = FALSE, help = hlp, step = step,
                               helppage = "inputGATboundary", myvar = NULL,
                               check = borders, backopt = FALSE)

  }

  #---- step 3: min/max distance ----
  while (step == 3 & !temp$quit) {
    pb <- list(title = "NYSDOH Geomask Tool: select distances",
               label = "Selecting the minimum and maximum distances.")
    tcltk::setTkProgressBar(tpb, value = step, title = pb$title,
                            label = pb$label)

    templist <- selectGMdistances(step = step, min = maskvars$min,
                                  max = maskvars$max, unit = maskvars$unit,
                                  backopt = temp$backopt)

    maskvars$min <- as.numeric(gsub(",", "", templist$min))
    maskvars$max <- as.numeric(gsub(",", "", templist$max))
    maskvars$unit <- templist$unit

    # error checking
    if (maskvars$unit == "cancel") {
      step <- 10
      temp$quit <- TRUE
    } else if (maskvars$unit == "back") {
      step <- 1
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
        step <- 4
      }
    }
    if (step == 4) {
      # convert max and min values to meters
      if (maskvars$unit == "kilometers") {
        maskvars$min <- maskvars$min * 1000
        maskvars$max <- maskvars$max * 1000
      } else if (maskvars$unit == "miles") {
        maskvars$min <- maskvars$min * 1609.344
        maskvars$max <- maskvars$max * 1609.344
      } else if (maskvars$unit == "feet") {
        maskvars$min <- maskvars$min / 3.2808399
        maskvars$max <- maskvars$max / 3.2808399
      }
    }
    rm(templist)
  }

  #---- step 4: kml ----
  while (step == 4 & !temp$quit) {
    pb <- list(title = "NYSDOH GAT: save KML?",
               label = "Identifying whether to save a KML file.")
    tcltk::setTkProgressBar(tpb, value = step, title = pb$title, label = pb$label)

    mysettings$kml <- gatpkg::saveGATkml(step = step, backopt = TRUE)

    if (mysettings$kml %in% c("Yes", "No")) {
      if (mysettings$kml == "Yes") {
        mysettings$kml <- TRUE # save the kml
      } else {
        mysettings$kml <- FALSE # save the kml
      }
      step <- 5
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
    pb <- list(title = "NYSDOH GAT: identify save file",
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
    } else {
      step <- step + 1
    }
  }

  #---- step 6: confirmation ----
  while (step == 6) {
    pb <- list(title = "NYSDOH Geomask Tool: confirm settings",
               label = "Confirming your geomasking settings.")
    tcltk::setTkProgressBar(tpb, value = step, title = pb$title,
                            label = pb$label)

    # rewrite this dialog to mirror GAT
    temp$checkmsg <- paste0("Do you want to move points from ",
                            filevars$pointfile, "\n", "at least ",
                            maskvars$min, " ", maskvars$unit, " and at most ",
                            maskvars$max, " ", maskvars$unit, "\n",
                            "within the boundaries of ",
                            filevars$boundfile, "?")
    temp$checktitle <- "Masking settings confirmation"
    temp$check <- gatpkg::inputGATmessage(title = temp$checktitle,
                                          msg = temp$checkmsg, step = step,
                                          buttonopt = "Fix settings",
                                          backopt = FALSE)
    if (temp$check == "cancel") {
      step <- 1
    } else {
      step <- 7
      mysettings$quit <- temp$quit
    }
  }
} # end while step
rm(temp)

#---- automatic processing ----
# at this point, step = 7
if (!mysettings$quit) {
  # Abby: need to add progress bar steps
  #       do this later after full program rewritten

  set.seed(mysettings$starttime)
  #---- isolate old points ----
  # for point calculations
  # derive lat/long from geometry, thereby ignoring data entirely
  myshps$point$orig_lon <- sf::st_coordinates(myshps$point)[, 1]
  myshps$point$orig_lat <- sf::st_coordinates(myshps$point)[, 2]

  myshps$intersect <- sf::st_intersection(myshps$point, myshps$bound)
  myshps$buffer <- data.frame(ID = numeric())
  myshps$shifted <- data.frame(ID = numeric())

  # ---- calculate new points ----
  # create temp max/min variables
  # create flag variable for number max/min iterations
  # if buffer fails, min = min/2 & max = min, reiterate if needed
  # subset remaining without valid buffer
  # will need to rerun buffer for just those
  # I'd love to drop the loop, but for now it works
  # possibly create buffers for each point, then recreate on the fly if needed?
  temp <- list(max = maskvars$max,
               min = maskvars$min,
               int = myshps$intersect,
               iter = 0)

  while (nrow(temp$int) > 0) {
    temp$bufmin <- sf::st_buffer(temp$int, temp$min)
    temp$bufmin <- dplyr::select(temp$bufmin, ID, bound_id)
    temp$bufmax <- sf::st_buffer(temp$int, temp$max)
    temp$bufmax <- dplyr::select(temp$bufmax, ID)

    for (i in 1:nrow(temp$int)) {

      temp$bufmin1 <- dplyr::slice(temp$bufmin, i)
      temp$bufmax1 <- dplyr::slice(temp$bufmax, i)
      temp$diff1 <- sf::st_difference(temp$bufmin1, temp$bufmax1)
      temp$bound1 <- dplyr::filter(myshps$bound,
                                   bound_id == temp$bufmin1$bound_id[1])
      temp$int1 <- sf::st_intersection(temp$bound1, temp$diff1)
      if (nrow(t5) > 0) {
        temp$shift1 <- sf::st_as_sf(sf::st_sample(temp$int1, 1),
                                    crs = sf::st_crs(temp$bufmin1))
        temp$shift1$ID <- temp$bufmin1$ID
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
                              !ID %in% myshps$shifted$ID)
    temp$max <- temp$min
    temp$min <- temp$min / 2
    temp$iter <- temp$iter + 1
  }

  rm(temp)


  # join intersect and shifted datasets
  temp <- data.frame(myshps$shifted)[, c("ID", "bound_id", "long", "lat")]
  myshps$point <- dplyr::full_join(myshps$point, temp, by = "ID")





  # testing with old object names - obsolete
  # plot(t4$geometry, border = "green")
  # plot(t2$geometry, border = "blue", add = TRUE)
  # plot(t1$geometry, border = "red", add = TRUE)
  # plot(t5$geometry, col = "yellow", border = "transparent", add = TRUE)
  # plot(sf::st_geometry(t6), col = "black", add = TRUE, pch = 20)

  # warn user if minimum or maximum have been changed - add to log
  # created flag variable to try bypassing this
      flag$log <- ""
      if (flag$max | flag$min) {
        flag$log <- paste("The geomasker had difficulty moving point", j,
                          "in area", i, ". For this point,")
      }
      if (flag$max & flag$min) {
        flag$log <- paste(flag$log, "minimum and maximum distances",
                          "were changed.")
      } else if (flag$max) {
        flag$log <- paste(flag$log, "maximum distances were changed.")
      } else if (flag$min) {
        flag$log <- paste(flag$log, "minimum distances were changed.")
      }

      if (!flag$log == "") {
        maskvars$log <- paste0(maskvars$log, flag$log, " \n")
      }
     # cycle through all points

  #---- step ?: plot original points ----
  # Abby: program rewritten to this point
  #       rewrite and/or reorder everything below this point
  # add progress bar - plot original points
# check if plotGMcompare has been updated to sf

  myplots <- list()
  myplots$original <- plotGMcompare(bound = myshps$bound, point = myshps$point,
                                    maskvars = maskvars)


  pts$coords_new <- data.frame(x = pts$x, y = pts$y)

  # add my minimum/maximum distances
  myshps$new <- cbind(data.frame(myshps$point),
                      min_dist = pts$min,
                      max_dist = pts$max)

  sp::coordinates(myshps$new) <- pts$coords_new
  # assign original projection to new points
  sp::proj4string(myshps$new) <- sp::proj4string(myshps$point)

  #---- step ?: plot new points ----
  # need to create new points layer first

  myplots$new <- plotGMcompare(bound = myshps$bound, point = myshps$new,
                               maskvars = maskvars)


  #---- comparison plot showing both points sets at once? ----
  # will need to revise plotGMcompare





#dev.new()
#use points to add new data to same plot
points(mydata, pch=24, col="black",bg="blue")
  #produces green triangles outlined in black
title("Points before and after moving")
legend(x="topleft",legend=c("before","after"),
       horiz=FALSE,pch=c(21,24),col=c("black","black"),pt.bg=c("red","blue"))

endtime<-Sys.time()

#change names of old coordinates
names(mydata)[names(mydata)=="coords.x1"]<-"prev_x" #change the name of the old coordinates
names(mydata)[names(mydata)=="coords.x2"]<-"prev_y" #change the name of the old coordinates

#write  out shapefile using OGR
#output file doesn't seem to have projection
writeOGR(mydata, userpathout, userfileout, driver="ESRI Shapefile",verbose=TRUE,overwrite_layer=TRUE) #seems fast

#---- step ?: plot new points ----
# need to create new points layer first

myplots$new <- plotGMcompare(bound = myshps$bound, point = myshps$point,
                             maskvars = maskvars)

# notes: order of steps
# calculate new points
# create layer of new points - note units in log
# plot original points
# plot new points (overlaid? - 1px)
# save plots
# save shapefiles
# save kml
# save log

maskvars$log



# save kml ----
if (maskvars$kml == TRUE) { # now includes descriptions
  step <- step + 1
  pb$label = "Writing the KML file."
  tcltk::setTkProgressBar(tpb, value = step, title = pb$title, label = pb$label)

  # make sure this works
  if (mysettings$kml) {
    # need to define GM_id at some point as 1:nrow(x) or rownames(x)
    gatpkg::writeGATkml(myshp = myshps$newpoint, filename = filevars$fileout,
                        filepath = filevars$pathout, myidvar = GM_id)
  }
}

# save shapefile (incomplete) ----
# convert below to shapefile output
#might be able to specify NameField, DescriptionField, AltitudeMode
#NameField=mydata@data[1]
writeOGR(mydata, userpathout, userfileout, driver="MapInfo File",verbose=TRUE,overwrite_layer=TRUE) #seems fast


# write log (incomplete) ----
# shift log to function

################################################################################
#create a log file
#should contain input files, output file, min and max distances or sources (and units), date, run time
#(8 items)
################################################################################
#begin log file
logfile<-paste(userpathout,paste(userfileout,"txt",sep="."),sep="/")

setStatusBar(paste("NYSDOH Geomasking Tool: Writing log file ",logfile))

logtext<-"NYSDOH Geomasking Tool log"
write("",file=logfile,ncolumns=length(logtext),append=TRUE)
#logtext<-c("The current date is ", format(Sys.Date()))
#write(logtext,file=logfile,ncolumns=length(logtext),append=TRUE)
logtext<-c("The current date and time are ", format(Sys.time()))
write(logtext,file=logfile,ncolumns=length(logtext),append=TRUE)

logtext<-c("The input point file is: ",userfile)
write(logtext,file=logfile,ncolumns=length(logtext),append=TRUE)
logtext<-c("Number of points moved: ",length(mydata@data[,1]))
write(logtext,file=logfile,ncolumns=length(logtext),append=TRUE)

logtext<-c("The input boundary file is: ",userfileb)
write(logtext,file=logfile,ncolumns=length(logtext),append=TRUE)
write("",file=logfile,ncolumns=length(logtext),append=TRUE)

if(userdistc[2]=="NONE"){
logtext=c("Minimum distance requested was ",userdistc[1])
}else{logtext=c("Minimum distance was obtained from field ",userdistc[2],userdistc[5])}
write(logtext,file=logfile,ncolumns=length(logtext),append=TRUE)

if(userdistc[4]=="NONE"){#be sure to include units=userdistc[5]
logtext=c("Maximum distance requested was ",userdistc[3],userdistc[5])
}else{
logtext=c("Maximum distance was obtained from field ",userdistc[4])}
write(logtext,file=logfile,ncolumns=length(logtext),append=TRUE)

logtext<-c("The time this program took to move the points: ",endtime-starttime)
write(logtext,file=logfile,ncolumns=length(logtext),append=TRUE)

logtext<-c("The output file(s): ",userout)
write(logtext,file=logfile,ncolumns=length(logtext),append=TRUE)

#end code to create log file
##############################################################################

setStatusBar(paste("NYSDOH Geomask Tool: find your results in: ",userpathout))



#---- save user settings ----
# for testing; move to end later
# save relevant objects
save(file = paste0(filevars$userout, "_settings.Rdata"),
     list = c("filevars", "maskvars", "mysettings"))






} else {
  # "else" occurs only if geomasking is cancelled in the input phase
  msg <- "You have chosen to cancel the geomasking tool."
  tcltk::tkmessageBox(title = "Process cancelled",
                      message = msg, type = "ok", icon = "warning")
}




