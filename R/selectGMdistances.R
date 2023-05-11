#' Select geoMasking distances
#'
#' @description
#'
#' This function is modified from a similar function in gatpkg. It creates
#' a user dialog to read the minimum and maximum distances, as well as the
#' distance units, for masking.
#'
#' @param unit       String denoting unit of distance.
#' @param min        Number denoting minimum masking distance.
#' @param max        Number denoting maximum masking distance.
#' @param step       Step in the program.
#' @param backopt    Boolean denoting whether to include the back button.
#' @param quitopt    Text string for the cancel button.
#' @param bgcol      Text string containing UI background color.
#' @param buttoncol  Text string containing UI button color.
#'
#' @examples
#'
#' if (interactive()) {
#' selectGMdistances()
#' }
#'
#' @export
#'
#'

selectGMdistances <- function(step = 3, min = "1,000", max = "10,000",
                              unit = "meters", backopt = TRUE,
                              bgcol = "lightskyblue3", quitopt = "Quit",
                              buttoncol = "cornflowerblue") {
  # exposition ----
  helppage <- "selectGMdistances"
  hlp <- paste0("In the text boxes, enter your desired minimum and maximum",
                " values. \n",
                "  \u2022  To continue,  click 'Next >'. \n",
                "  \u2022  To return to boundary selection,",
                "click '< Back'. \n", "  \u2022  To quit, click '", quitopt, "'.")
  instruct <- paste(" 1. Select a unit of distance. \n",
                    "2. Enter a number for the minimum distance. \n",
                    "3. Enter a number for the maximum distance. \n")


  fonthead <- tcltk::tkfont.create(family = "Segoe UI", size = 10, weight = "bold")


  # draw window ----
  tt <- tcltk::tktoplevel(background = bgcol)
  tcltk::tkwm.title(tt, paste0("Step ", step,
                               ": Distances"))

  tt$insttl <- tcltk::tklabel(tt, text = "Instructions", font = fonthead,
                              background = bgcol)
  tcltk::tkgrid(tt$insttl, sticky = "w", padx = 5, pady = 5)
  tt$ins <- tcltk::tklabel(tt, text = instruct, justify = "left",
                           background = bgcol)
  tcltk::tkgrid(tt$ins, columnspan = 4, sticky = "w")
  tt$tr <- tcltk::tkframe(tt, background = bgcol)

  # defaults ----
  unitlist <- c("meters", "kilometers", "miles", "feet")
  myunit <- tcltk::tclVar(unit)
  mymin <- tcltk::tclVar(min)
  mymax <- tcltk::tclVar(max)

  # define input boxes ----
  tt$tr$unitlabel = tcltk::tklabel(tt$tr, font = fonthead,
                                   text = "Unit of distance: ",
                                   background = bgcol)
  tt$tr$unitlist <- tcltk::ttkcombobox(tt$tr, values = unitlist,
                                       textvariable = myunit,
                                       state = "readonly")
  tcltk::tkgrid(tt$tr$unitlabel, row = 1, column = 1, sticky = "w",
                padx = 5, pady = 5)
  tcltk::tkgrid(tt$tr$unitlist, row = 1, column = 2, sticky = "w",
                padx = 5, pady = 5)

  tt$tr$minlabel = tcltk::tklabel(tt$tr, font = fonthead,
                                  text = "Minimum distance: ",
                                  background = bgcol)
  tt$tr$minvalue <- tcltk::tkentry(tt$tr, width = "20",
                                   textvariable = mymin)
  tcltk::tkgrid(tt$tr$minlabel, row = 2, column = 1, sticky = "w",
                padx = 5, pady = 5)
  tcltk::tkgrid(tt$tr$minvalue, row = 2, column = 2, sticky = "w",
                padx = 5, pady = 5)

  tt$tr$maxlabel = tcltk::tklabel(tt$tr, font = fonthead,
                                  text = "Maximum distance: ",
                                  background = bgcol)
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
    gatpkg::showGAThelp(help = hlp, helptitle = "distance settings",
                helppage = helppage, step = step)
  }

  # draw buttons ----



  tt$tf <- tcltk::tkframe(tt, background = bgcol)
  if (backopt) {
    tt$tf$BackBut <- tcltk::tkbutton(tt$tf, text = "< Back", width = 12,
                                     command = onBack, background = buttoncol)
    tt$tf$OkBut <- tcltk::tkbutton(tt$tf, text = "Next >", width = 12,
                                   command = onOk, default = "active",
                                   background = buttoncol)
  } else {
    tt$tf$OkBut <- tcltk::tkbutton(tt$tf, text = "Confirm", width = 12,
                                   command = onOk, default = "active",
                                   background = buttoncol)
  }


  tt$tf$CancelBut <- tcltk::tkbutton(tt$tf, text = quitopt,
                                     command = onCancel, width = 12,
                                     background = buttoncol)
  tt$tf$HelpBut <- tcltk::tkbutton(tt$tf, text = "Help", width = 12,
                                   command = onHelp, background = buttoncol)

  # bottom button placements ----
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
