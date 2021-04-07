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
# until after initial compile
devtools::load_all("P:/Sections/EHS/Abigail/SubcountyData/Rcode/geomask/R")
# confirmed required
  #library(tcltk)
  #library(rgdal)
  #library(gatpkg)
  #library(sp)



# abby: change function calls to include library
#       then delete this whole section
# gwen: load default libraries. Need this to run as batch.
# abby: replace svDialogs and tcltk with tcltk2 (or read from gatpkg)

# requires GAT because I don't feel like being redundant
# rewrite locateGATshapefile to read that directly, too? yes
#library(datasets)
#library(utils)
#library(grDevices)
#library(graphics)
#library(stats)
#library(methods)
#library(gpclib) #in case not automaticaly loaded
#might be disabled by default, this enables it
#if(exists("gpclibPermit")==TRUE){gpclibPermit()}
#library(svDialogs) #for guiDlgOpen, it needs svMisc
#tclRequire("BWidget")



#---- progress bar ----
mysettings <- list(version = "1.4.0",
                   # packageDescription("geomask")$Version,
                   pkgdate = "2021-01-05",
                   # packageDescription("geomask")$Date,
                   starttime = Sys.time()) # needed for the log

pb <- list(title = paste("NYSDOH Geomask Tool",
                         mysettings$version, mysettings$date),
           label = "NYSDOH Geomask Tool is running. Please wait for dialogs.")
tpb <- tcltk::tkProgressBar(title = pb$title, label = pb$label, min = 0,
                            max = 26, initial = 0, width = 400)

#---- settings ----
# for testing; set up settings file as with GAT?
myshps <- list()
maskvars <- list(min = 100, max = 1000, unit = "meters")
filevars <- list(pointin = "", boundin = "")
temp <- list(quit = FALSE, backopt = TRUE)
step <- 1


#---- user input ----

while(step < 7) { # gwen: get user input until finalized
  #---- step 1: request point shapefile ----
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

    if (filevars$pointin == "cancel") {
      step <- 10
      temp$quit <- TRUE
    } else {
      # only care if file is point-level
      myshps$point <- rgdal::readOGR(dsn = filevars$pointpath,
                                     layer = filevars$pointfile,
                                     stringsAsFactors = FALSE)

      # error checking
      if (class(myshps$point) != "SpatialPointsDataFrame") {
        # message: wrong kind of shapefile; repeat dialog
        temp$msg <- paste("The shapefile", filevars$filein,
                          "does not contain points.\n",
                          "Please select a new shapefile.")
        tcltk::tkmessageBox(title = "Shapefile invalid", type = "ok",
                            icon = "error", message = temp$msg)
      } else {
        # shift to error checking
        maskvars$projection <- grepl("longlat", sp::proj4string(myshps$point),
                                     fixed = TRUE)

        if (!maskvars$projection) {
          # if FALSE, convert to lat/long
          # WGS84 is common: EPSG code 4326
          # allow user to select projection from list?
          myshps$point <- sp::spTransform(myshps$point,
                                          sp::CRS("+init=epsg:4326"))
        }
        # derive lat/long from geometry, thereby ignoring data entirely
        myshps$point@data$gm_lon <- myshps$point@coords[, 1] # x
        myshps$point@data$gm_lat <- myshps$point@coords[, 2] # y

        step <- 2
      }
    }
  }

  #---- step 2: request boundary shapefile ----
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

    if (filevars$boundin == "cancel") {
      step <- 10
      temp$quit <- TRUE
    } else {
      # only care if file is polygon-level
      myshps$bound <- rgdal::readOGR(dsn = filevars$boundpath,
                                     layer = filevars$boundfile,
                                     stringsAsFactors = FALSE)
      # error checking
      temp$error <- FALSE
      if (class(myshps$bound) != "SpatialPolygonsDataFrame") {
        # message: wrong kind of shapefile; repeat dialog
        temp$issue <- "polygons."
        temp$error <- TRUE
      } else {
        step <- step + 1
      }
      if (temp$error) {
        temp$msg <- paste("The shapefile", filevars$boundin,
                          "does not contain", temp$issue, "\n",
                          "Please select a new shapefile.")
        tcltk::tkmessageBox(title = "Shapefile invalid", type = "ok",
                            icon = "error", message = temp$msg)
        temp$error <- FALSE
      } else {
        myshps$bound <- sp::spTransform(myshps$bound,
                                        sp::proj4string(myshps$point))
        step <- 3
      }
    }
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
    tcltk::setTkProgressBar(tpb, value = step, title = pb$title, label = pb$label)

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
rm(temp, temppath, templist)

#---- automatic processing ----
# at this point, step = 7
if (!mysettings$quit) {
  #---- isolate old points ----
  # for point calculations
  pts <- list(n = nrow(myshps$point))
  # save coordinates
  pts$coords<-coordinates(myshps$point)
  pts$x <- pts$coords[,1]
  pts$y <- pts$coords[,2]

  #---- calculate new points ----
  # gwen: set seed based on time to avoid duplicate random number sequences
  set.seed(mysettings$starttime)
  pts$dist <- runif(pts$n, min = maskvars$min, max = maskvars$max)
  pts$angle <- runif(pts$n, min = 0, max = 2 * pi)

  # gwen: make sure arguments of sin/cosine are in radians
  #       approx 111 km per degree latitude
  #       formulas to move points:
  # y: original latitude + sin (r1 * 2 * pi()) * r2 * 500 / 111000
  # x: (original longitude + cos(r1 * 2 * pi()) * r2 * 500) /
  #    (cos(RADIANS(original latitude)) * 111321)
  pts$y_new <- pts$y + sin(pts$angle) * pts$dist / 111000
  pts$x_new <- pts$x + cos(pts$angle) * pts$dist /
               (cos(pts$y * pi / 180) * 111321)

  #---- check new points ----

  # abby: need to rewrite and/or reorder everything below this point


  # what are these for?
  flag <- list(max = TRUE, min = TRUE)

  # get coordinates for polygons, and check if points are inside
  # add an element to check if point is in correct polygon?
  for (i in 1:length(myshps$bound@polygons)) {
    temp <- list()
    # get polygon values ----
    temp$shp <- myshps$bound@polygons[[i]]@Polygons[[1]]  # myp2
    temp$coords <- temp$shp@coords # mypc
    temp$x <- temp$coords[, 1] # mypx
    temp$y <- temp$coords[, 2] # mypy

    # check points in polygons ----
    # 0 = exterior, 1 = interior, 2 & 3 are on edge/vertex
    # formerly checkifin, checkifinnew
    temp$in_old <- sp::point.in.polygon(pts$x, pts$y, temp$x, temp$y)
    temp$in_new <- sp::point.in.polygon(pts$x_new, pts$y_new, temp$x, temp$y)

    # find what area the point is in
    # correct maximum distances based on the maximum possible
    # in the area if needed

    for (j in 1:length(temp$in_old)) { # check each area
      # reset these values to defaults
      trycount <- 0

      if(temp$in_old[j] > 0){ # if point is in area or on edge/vertex
        # reset maximum to maximum possible given boundary restriction
        # units in kilometers, so multiply by 1000 to get meters
        temp$dist <- sp::spDistsN1(temp$coords, myshps$point@coords[j, ],
                                   longlat = TRUE) * 1000
        # temp$coords = polygon coordinates
        # myshps$point@coords = original point coordinates
        # maximum distance is outside farthest point of polygon - unnecessary
        if (max(temp$dist) < maskvars$max[j]) {
          maskvars$max[j] <- max(temp$dist)
          flag$max <- TRUE
        }


        # what is this? when is it created?
        # keep track of the maximum maximum tried -
        # either user entry or farthest point in the boundary polygon
        recordmax[j] <- maskvars$max[j]

        if(userdistmin[p]>userdistmax[p]/2){userdistmin[p]<-userdistmax[p]/2
        minchangeflag<-TRUE
        }
        newdis[p]<-runif(1,min=userdistmin[p],max=userdistmax[p])
        newang[p]<-runif(1,min=0,max=2*pi)
        mynewy[p]<-myy[p]+sin(newang[p])*newdis[p]/111000  #approx 111 km per degree latitude
        mynewx[p]<-myx[p]+cos(newang[p])*newdis[p]/(cos(myy[p]*pi/180)*111321)
        checkifnewin[p]<-point.in.polygon(mynewx[p],mynewy[p],mypx,mypy)
        if(checkifnewin[p]>1){checkifnewin[p]<-1}
        trycount<-trycount+1
      }#end if point is in area

      while(checkifin[p]==1&&checkifnewin[p]==0){#print(paste("moving point",as.character(p)," so it is within boundary"))
        #if the old point is in an area, but the new point is not, need to fix it
        trycount<-trycount+1

        if(trycount/100==floor(trycount/100)){#if try at least 100 times and cannot get a point in the polygon
          #change the maximum distance
          userdistmax[p]<-userdistmax[p]-userdistmax[p]*0.1
          if(userdistmin[p]>userdistmax[p]/2){userdistmin[p]<-userdistmax[p]/2
          minchangeflag<-TRUE}
          maxchangeflag<-TRUE} #end changing max distance after 100 tries

        if(trycount/10==floor(trycount/10)){#If try at least 10 times and cannot get a point in the polygon,
          #change minimum distance
          userdistmin[p]<-userdistmin[p]-userdistmin[p]*0.1
          minchangeflag<-TRUE}

        #get new candidate point
        newdis[p]<-runif(1,min=userdistmin[p],max=userdistmax[p])
        newang[p]<-runif(1,min=0,max=2*pi)
        mynewy[p]<-myy[p]+sin(newang[p])*newdis[p]/111000  #approx 111 km per degree latitude
        mynewx[p]<-myx[p]+cos(newang[p])*newdis[p]/(cos(myy[p]*pi/180)*111321)
        checkifnewin[p]<-point.in.polygon(mynewx[p],mynewy[p],mypx,mypy)
        if(checkifnewin[p]>1){checkifnewin[p]<-1}
      }#for discrepant points
    }#after cycling through number of points
  }#cycle through all areas

  ########################################################################
  #end moving points section.  Next distplay and output results
  ########################################################################

  #---- step ?: plot original points ----
# add progress bar - plot original points
myplots <- list()
myplots$original <- plotGMcompare(bound = myshps$bound, point = myshps$point,
                                  maskvars = maskvars)


# notes: order of steps
# calculate new points
# create layer of new points - note units
# plot original points
# plot new points (overlaid? - 1px)
# save plots
# save shapefiles
# save kml
# save log






mynewcoords<-data.frame(mynewx,mynewy)
mydata<-data.frame(mypoints.shp)
#add my minimum/maximum distances
mydata<-cbind(mydata,userdistmin,recordmax)
names(mydata)[names(mydata)=="userdistmin"] = "min_dist" # rename
names(mydata)[names(mydata)=="recordmax"] = "max_dist" # rename
coordinates(mydata)<-mynewcoords
proj4string(mydata)<-proj4string(mypoints.shp) #assign original projection to new points

#dev.new()
#use points to add new data to same plot
points(mydata, pch=24, col="black",bg="blue") #produces green triangles outlined in black
title("Points before and after moving")
legend(x="topleft",legend=c("before","after"),horiz=FALSE,pch=c(21,24),col=c("black","black"),pt.bg=c("red","blue"))

userfileout<-"" #set as default missing
endtime<-Sys.time()

#warn user if minimum or maximum have been changed
if(minchangeflag==TRUE&&maxchangeflag==TRUE){dlgMessage("There was difficulty moving the points within the area specified.  Maximum and minimum distances were changed for at least some points.  Please check output file for details",title="WARNING: Parameters changed",type="ok",default=1,icon="warning")}

if(minchangeflag==TRUE&&maxchangeflag==FALSE){dlgMessage("There was difficulty moving the points within the area specified.  Minimum distances were changed for at least some points.  Please check output file for details",title="WARNING: Parameters changed",type="ok",default=1,icon="warning")}

if(minchangeflag==FALSE&&maxchangeflag==TRUE){dlgMessage("There was difficulty moving the points within the area specified.  Maximum distances were changed for at least some points.  Please check output file for details",title="WARNING: Parameters changed",type="ok",default=1,icon="warning")}

#get the user to enter filename to save file
setStatusBar(paste("NYSDOH Geomask Tool: save file"))

while(userfileout==""){
userout<-dlgSave(title = "Save Moved Points As", defaultFile = "", defaultDir = "",defaultExt = "", filters = c("Shapefiles (*.shp)", "*.shp"))$res
#now remove any file extention
periodloc=regexpr(".",userout,fixed=TRUE) #will be -1 if no match, otherwise location(s) of matches
if(periodloc>0){userout<-substr(userout,1,periodloc[1]-1)}


#find output file name and path
slashloc<-max(unlist(gregexpr("/",userout,fixed=TRUE))) #find location of last slash, divides path and file name
userfileout<-substr(userout,slashloc+1,nchar(userout))
userpathout<-substr(userout,1,slashloc-1)
#filename shouldn't contain ;=+<>|"[]/\'<>:*?

checkfile<-regexpr(";|:|\\+|=|<|>|\\||\\[|\\]|/|\"|'|\\*|\\?\n",userfileout,perl=TRUE)
if(checkfile[1]!="-1"){userfileout<-""}

checkfile<-charmatch("\\",userfileout,nomatch=-1)
if(checkfile[1]!="-1"){userfileout<-""}

if(userfileout==""){
dlgMessage("Your file name may have been invalid.  Please reenter your file name.", title="WARNING", type="ok")}
} #end while no good file name

#change names of old coordinates
names(mydata)[names(mydata)=="coords.x1"]<-"prev_x" #change the name of the old coordinates
names(mydata)[names(mydata)=="coords.x2"]<-"prev_y" #change the name of the old coordinates

#write  out shapefile using OGR
#output file doesn't seem to have projection
writeOGR(mydata, userpathout, userfileout, driver="ESRI Shapefile",verbose=TRUE,overwrite_layer=TRUE) #seems fast



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







} else {
  # "else" occurs only if geomasking is cancelled in the input phase
  msg <- "You have chosen to cancel the geomasking tool."
  tcltk::tkmessageBox(title = "Process cancelled",
                      message = msg, type = "ok", icon = "warning")
}




