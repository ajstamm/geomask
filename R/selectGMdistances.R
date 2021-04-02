#' Select GeoMasking distances
#' 
#' This function is modified from a similar function in gatpkg. It creates 
#' a user dialog to read the minimum and maximum distances, as well as the 
#' distance units, for masking.
#' 
#' @param unit    String denoting unit of distance.
#' @param min     Number denoting minimum masking distance.
#' @param max     Number denoting maximum masking distance.
#' @param step    Step in the program. Dwfault is 3.
#' @param backopt Boolean denoting whether to include the back button.
#' 

selectGMdistances <- function(step = 3, min = "1,000", max = "10,000",
                              unit = "meters", backopt = TRUE) {
  # exposition ----
  helppage <- "selectGMdistances"
  hlp <- paste0("In the text boxes, enter your desired minimum and maximum", 
                " values. \n",
                "  \u2022  To continue,  click 'Next >'. \n",
                "  \u2022  To return to boundary selection,",
                "click '< Back'. \n", "  \u2022  To quit, click 'Cancel'.")
  instruct <- paste(" 1. Select a unit of distance. \n", 
                    "2. Enter a number for the minimum distance. \n", 
                    "3. Enter a number for the maximum distance. \n")
  fonthead <- tcltk2::tk2font.set(font = "fonthead",
                                  settings = list(family = "Segoe UI", 
                                                  size = 10, bold = TRUE, 
                                                  italic = FALSE))

  # draw window ----
  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt, paste0("Step ", step, 
                               ": Distances"))
  tt$insttl <- tcltk2::tk2label(tt, text = "Instructions", font = "fonthead")
  tcltk::tkgrid(tt$insttl, sticky = "w", padx = 5, pady = 5)
  tcltk::tkgrid(tcltk2::tk2label(tt, text = instruct), columnspan = 4,
                sticky = "w")
  tt$tr <- tcltk::tkframe(tt)
  
  # defaults ----
  unitlist <- c("meters", "kilometers", "miles", "feet")
  myunit <- tcltk::tclVar(unit)
  mymin <- tcltk::tclVar(min)
  mymax <- tcltk::tclVar(max)
  
  # define input boxes ----
  tt$tr$unitlabel = tcltk2::tk2label(tt$tr, font = "fonthead", 
                                     text = "Unit of distance: ")
  tt$tr$unitlist <- tcltk::ttkcombobox(tt$tr, values = unitlist,
                                       textvariable = myunit,
                                       state = "readonly")
  tcltk::tkgrid(tt$tr$unitlabel, row = 1, column = 1, sticky = "w", 
                padx = 5, pady = 5)
  tcltk::tkgrid(tt$tr$unitlist, row = 1, column = 2, sticky = "w", 
                padx = 5, pady = 5)
  
  tt$tr$minlabel = tcltk2::tk2label(tt$tr, font = "fonthead", 
                                     text = "Minimum distance: ")
  tt$tr$minvalue <- tcltk::tkentry(tt$tr, width = "20",
                                  textvariable = mymin)
  tcltk::tkgrid(tt$tr$minlabel, row = 2, column = 1, sticky = "w", 
                padx = 5, pady = 5)
  tcltk::tkgrid(tt$tr$minvalue, row = 2, column = 2, sticky = "w", 
                padx = 5, pady = 5)
  
  tt$tr$maxlabel = tcltk2::tk2label(tt$tr, font = "fonthead", 
                                    text = "Maximum distance: ")
  tt$tr$maxvalue <- tcltk::tkentry(tt$tr, width = "20",
                                   textvariable = mymax)
  tcltk::tkgrid(tt$tr$maxlabel, row = 3, column = 1, sticky = "w", 
                padx = 5, pady = 5)
  tcltk::tkgrid(tt$tr$maxvalue, row = 3, column = 2, sticky = "w", 
                padx = 5, pady = 5)
  
  tcltk::tkgrid(tt$tr, columnspan = 2)
  
  # define buttons ----
  myenv <- new.env()

  onOk <- function() {
    unit <- tcltk::tclvalue(myunit)
    min <- tcltk::tclvalue(mymin)
    max <- tcltk::tclvalue(mymax)
    tcltk::tkdestroy(tt)
    assign("mylist", list(unit = unit, min = min, max = max), envir=myenv)
  }
  onCancel <- function() {
    tcltk::tkdestroy(tt)
    assign("mylist", list(unit = "cancel", min = 0, max = 0), envir=myenv)
  }
  onBack <- function() {
    tcltk::tkdestroy(tt)
    assign("mylist", list(unit = "back", min = 0, max = 0), envir=myenv)
  }
  onHelp <- function() {
    showGAThelp(help = hlp, helptitle = "distance settings",
                helppage = helppage, step = step)
  }
  
  # draw buttons ----
  
  tt$tf <- tcltk::tkframe(tt)
  if (backopt) {
    tt$tf$BackBut <- tcltk2::tk2button(tt$tf, text = "< Back",
                                       command = onBack, width = 12)
    tt$tf$OkBut <- tcltk2::tk2button(tt$tf, text = "Next >",
                                     command = onOk, width = 12,
                                     default = "active")
  } else {
    tt$tf$OkBut <- tcltk2::tk2button(tt$tf, text = "Confirm",
                                     command = onOk, width = 12,
                                     default = "active")
  }
  
  tt$tf$HelpBut <- tcltk2::tk2button(tt$tf, text="Help",
                                     width = 12, command = onHelp)
  tt$tf$CancelBut <- tcltk2::tk2button(tt$tf, text = "Cancel GAT",
                                       width = 12, command = onCancel)
  
  if (backopt) {
    tcltk::tkgrid(tt$tf$BackBut, column = 1, row = 1, padx = 10)
  }
  tcltk::tkgrid(tt$tf$OkBut, column = 2, row = 1, padx = 10)
  tcltk::tkgrid(tt$tf$CancelBut, column = 3, row = 1, padx = 10)
  tcltk::tkgrid(tt$tf$HelpBut, column = 4, row = 1, padx = 10)
  tcltk::tkgrid(tt$tf, pady = 5)
  
  # wait ----
  tcltk::tkwait.window(tt)
  return(myenv$mylist)
}
