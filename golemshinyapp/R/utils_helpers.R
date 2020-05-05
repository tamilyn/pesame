#' makeBox
#'
#' @shinydashboard
makeBox <- function(mfd, s) {
      if(!is.null(mfd$filename)) {
        xicon = icon("thumbs-up", lib = "glyphicon")
        xcolor = "green"
        s1 <- paste0(mfd$filename, "\n", mfd$num_rows, " x ", mfd$num_cols)
      } else {
        xicon = icon("list")
        xcolor = "yellow"
        s1 <- ""
      }
      infoBox(s,  s1, icon = xicon, color = xcolor)
}
