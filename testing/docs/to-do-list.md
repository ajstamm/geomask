## Issues to address

* fix error when clicking "cancel" in boundary file dialog
* remove duplicate process files from the log
* test shapefile preparation document

# Gwen's feedback - mix of bugs & feature requests

* Got errors with writeGMLog using the settingsfile option, not sure if this is a bug or if I'm misunderstanding the usage.
    `> writeGMlog(settingsfile="C:/Users/gdb02/OneDrive - New York State Office of Information Technology Services/Documents/Maps/geomaked_points_settings.Rdata")`
    `Error in st_read.default(dsn = filevars$pathin, layer = filevars$filein) : no st_read method available for objects of class NULL`
    `> writeGMlog(settingsfile="C:/Users/gdb02/OneDrive - New York State Office of Information Technology Services/Documents/Maps/geomaked_points_settings.Rdata",area=NULL, mysettings=NULL)`
    `Error in st_read.default(dsn = filevars$pathin, layer = filevars$filein) : no st_read method available for objects of class NULL`
* `Error in if (length(userfile) > 0 & !userfile == "cancel") { : argument is of length zero`
    Got that error with this code: 
    `runGMprogram(bgcol='white',buttoncol='red')`
    and canceling from the open boundary file dialog.
* For confirmGMsettings {geomask} function, it seems like the quitopt option does not work properly.  I tried: 
    `confirmGMsettings(maskvars = maskvars, filevars = filevars,step=7,bgcol="yellow",quitopt="bye-bye",buttoncol="Red")`
* Wish list for future versions; show shifted points as vectors. Could be a separate map, or just replace the existing output.
* In the technical notes, I found this sentence confusing:  *The output includes: a revised version of the original shapefile with masked point coordinates, a new shapefile that maps to the revised coordinates, a shapefile of the buffers used to select points, a PDF with a map comparing points, a comprehensive log, and if requested a KML file.*  
    Suggested Revision:  *The output includes:  a copy of the original shapefile with masked point coordinate values added, a shapefile with points located at the masked coordinate values, a shapefile of the buffers used to select points, a PDF with a map comparing points, a comprehensive log, and, if requested, a KML file.*
* Also, you refer to 'valid areas' (for the buffers) without really defining it.  Does valid area=area greater than zero?  Maybe add an explanation.  
    Valid area = Area where buffer overlaps boundary.
* I also don't understand this text in the tutorial: *Note: R package documentation allows you to view images, but not link to them. For the map, view a larger image by right clicking on the map and selecting “Open image in new tab.” *   
    Also, you mentioned "right clicking".  Since mouse buttons are customizable to accommodate left-handed people, it's better practice to say "click the primary mouse button" or "click the secondary mouse button".
* Why is the background of the distance boxes gray?  By convention, this often means inactive/disabled, which doesn't seem to be the case here.  Also it's not gray in the documentation. - Bug to check.
* Modify the tutorial code to remove the ALAND10 field from the data to not trigger large-number warnings. 
* In tutorial: The “flag” variable in the old and new buffers refers to the number of times boundaries had to be recalculated to create a valid area to select a masking point.  Flag only appears to be in the old and buffer files, not the new file. - Check that the correct layers saved to the correct files.
* I found what is possibly a larger problem with the geomasker tool:  The buffer file is points, not areas.  The buffers should be areas.  Here is the buffer file from the tutorial:
* Geomasker citation: It needs a URL and date field.
* Started looking at the documentation.  The readme appears to be missing two sections: “Why Create the Geomasker” and “How the  geomasker works”; they are titles with no text.
    The e-mail link could be neatened up/fixed (it doesn’t fill in the subject as intended).  The archive doesn’t actually have much by way of history documentation (or the original script); I suggest adding the version found here: "P:\Sections\EHS\Aggregation\GAT\NYS_GeoTools_2017Feb9.zip" or here: "P:\Sections\EHS\Aggregation\GAT\R_distribution2017Feb9\GeoMask".
    Also, you need a statement briefly describing what the geomasking tool is near the start, something like:  “The purpose of this tool is to move points randomly within a circular region around each point to preserve privacy in when displaying point maps.” (this is from the 1.3 documentation; you could pull other text from that to add to the readme as needed, see "P:\Sections\EHS\Aggregation\GAT\R_distribution2017Feb9\GeoMask\Geomask 1.3 user guide.pdf").


## Features to add

* way to bypass dialog entirely?
* add option to calculate distance using dedicated min/max variables?
* revise log to remove need to input area
    * requires adding projection and list of original location layer's variables to "mysettings" list
* make boundary file optional
    * will need to revise buffering step, too

# Done?

- [ ] add check for OS type to bypass Windows requirement in locate and save steps
- [x] add change log
- [x] add citation and acknowledgements document
- [x] create troubleshooting vignette with known warnings
- [x] finish tech notes
- [x] check that all shapefiles are correctly named and labeled in the log (not reading save name correctly)
- [x] add check that colors are valid, else use default (and add to GAT, too - not added to GAT yet)
- [x] check that distances can contain commas and print them with commas

# Food for thought

https://ij-healthgeographics.biomedcentral.com/articles/10.1186/s12942-020-00219-z - street masking?

* read in OSM to force points near roads?
* maybe with set road distance (5m?)

* load layers in advance, rather than requiring shapefiles?

* handle shifting polygons?
    * calculate and shift centroid, then add difference to indices
    * would guarantee only centroid is within buffer, not full area
    * when use case is unknown, forcing full area within buffer seems unnecessarily complicated
    * shifting lines, too? should work like polygons

