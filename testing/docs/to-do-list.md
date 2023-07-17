* issues to address
  * fix error when clicking "cancel" for boundary file
  * remove duplicate process files from the log
  * test shapefile preparation document

* features to add
  * way to bypass dialog entirely?
  * add option to calculate distance using dedicated min/max variables?
  * revise log to remove need to input area
      * requires adding projection and list of original location layer's variables to "mysettings" list
  * make boundary file optional
      * will need to revise buffering step, too

* done?
  - [ ] add check for OS type to bypass Windows requirement in locate and save steps
  - [x] add change log
  - [x] add citation and acknowledgements document
  - [x] create troubleshooting vignette with known warnings
  - [x] finish tech notes
  - [x] check that all shapefiles are correctly named and labeled in the log (not reading save name correctly)
  - [x] add check that colors are valid, else use default (and add to GAT, too - not added to GAT yet)
  - [x] check that distances can contain commas and print them with commas

* food for thought
  * https://ij-healthgeographics.biomedcentral.com/articles/10.1186/s12942-020-00219-z - street masking?
      * read in OSM to force points near roads?
      * maybe with set road distance (5m?)
  * load layers in advance, rather than requiring shapefiles?
  * handle shifting polygons?
      * calculate and shift centroid, then add difference to indices
      * would guarantee only centroid is within buffer, not full area
      * when use case is unknown, forcing full area within buffer seems unnecessarily complicated
      * shifting lines, too? should work like polygons
