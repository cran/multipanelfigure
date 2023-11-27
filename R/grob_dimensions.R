#' @title Convenient Access to \code{grob} Dimensions
#' @name grob_dimensions
#' @aliases simplegrobheight simplegrobwidth simple_grob_height simple_grob_width figure_width figure_height
#' @usage figure_width(grob, unit_to = "mm")
#' figure_height(grob, unit_to = "mm")
#' @description Convenience functions extracting dimensions from
#' \code{\link{grob}} objects.
#' @param unit_to A single \code{\link{character}} string representing a valid
#' \pkg{grid}-\code{\link[grid]{unit}}.
#' @param grob A \code{\link[grid]{grob}} object for which dimensions are to be
#' retrieved.
#' @return Single \code{\link{numeric}} objects are returned.
#' @author Johannes Graumann
#' @references Graumann, J., and Cotton, R.J. (2018). multipanelfigure: Simple
#' Assembly of Multiple Plots and Images into a Compound Figure. Journal of
#' Statistical Software 84. doi: 10.18637/jss.v084.c03
#' @seealso \code{\link{multi_panel_figure}}, \code{\link{save_multi_panel_figure}}
#' @importFrom grid convertUnit
#' @importFrom grid heightDetails
#' @importFrom grid widthDetails
#' @importFrom magrittr %>%
#' @examples
#' # Get dimensions of a grid grob
#' a_circle <- grid::circleGrob(x = 15, y = 30, r = 15, default.unit = "mm")
#' figure_height(a_circle)
#' figure_width(a_circle)
#'
#' # Use the unit_to arg to convert units
#' figure_height(a_circle, unit_to = "in")
#' figure_width(a_circle, unit_to = "cm")
#'
#' # Get dimensions of a multi-panel figure
#' figure <- multi_panel_figure(width = 55, height = 55, rows = 2, columns = 2)
#' figure_height(figure)
#' figure_width(figure)
#'
#' # ggsave defaults to measuring dimensions in inches
#' width <- figure_width(figure, unit_to = "in")
#' height <- figure_height(figure, unit_to = "in")
#' tmp_file <- tempfile(fileext = ".png")
#' ggplot2::ggsave(
#'   tmp_file, gtable::gtable_show_layout(figure),
#'   width = width, height = height
#' )
#' \donttest{ # Not testing due to use of external software
#' utils::browseURL(tmp_file)
#' }
#' @export
figure_width <- function(grob, unit_to = "mm"){
  # Check prerequisites
  assert_is_inherited_from(grob, classes = "grob")
  assert_is_a_supported_unit_type(unit_to)

  # Process
  convertUnit(
    widthDetails(grob),
    unitTo = unit_to,
    valueOnly = TRUE)
}

#' @export
figure_height <- function(grob, unit_to = "mm"){
  # Check prerequisites
  assert_is_inherited_from(grob, classes = "grob")
  assert_is_a_supported_unit_type(unit_to)

  # Process
  convertUnit(
    heightDetails(grob),
    unitTo = unit_to,
    valueOnly = TRUE)
}

#' @export
simplegrobheight <- function( ... ){
  .Deprecated(
    new = "figure_height",
    package = "multipanelfigure")
  paramList <- list( ... )
  if("unitTo" %in% names(paramList)){
    unit_to <- paramList[["unitTo"]]
  } else {
    unit_to = "mm"
  }
  figure_height(unit_to = unit_to, ... )
}

#' @export
simplegrobwidth <- function( ... ){
  .Deprecated(
    new = "figure_width",
    package = "multipanelfigure")
  paramList <- list( ... )
  if("unitTo" %in% names(paramList)){
    unit_to <- paramList[["unitTo"]]
  } else {
    unit_to = "mm"
  }
  figure_width(unit_to = unit_to, ... )
}

#' @export
simple_grob_height <- function( ... ){
  .Deprecated(
    new = "figure_height",
    package = "multipanelfigure")
  figure_height( ... )
}

#' @export
simple_grob_width <- function( ... ){
  .Deprecated(
    new = "figure_width",
    package = "multipanelfigure")
  figure_width( ... )
}