#' @title multi_panel_figure
#' @aliases multipanelfigure
#' @description A convenience function building \code{\link[gtable]{gtable}}-based
#' infrastructure for the assembly of multipanel figures.
#' @details The \code{\link[gtable]{gtable}} may be constructed in two ways:
#' \enumerate{
#'   \item{Based on explicit width/height definitions for individual panels.}
#'   \item{Based on total figure/\code{\link[gtable]{gtable}} dimensions given by
#'     \code{width} and \code{height} together with the number of \code{columns}
#'     and \code{rows} requested.}}
#' The function automatically inserts whitespace of width
#' \code{column_spacing} before column panels (and of height
#' \code{row_spacing} before row panels), which has to be considered
#' for the total dimensions of the resulting \code{\link[gtable]{gtable}}. Width
#' of the \code{\link[gtable]{gtable}} in the former case, for example may be
#' calculated
#' \deqn{W[total] = sum(width) + length(width) * column_spacing}
#' while width of resulting panels in the latter table construction approach may
#' be calculated
#' \deqn{W[panel] = (width - columns * column_spacing) / columns}
#'
#' \code{width}, \code{height}, \code{column_spacing} and
#' \code{row_spacing} may be defined numerically or as
#' \code{\link[grid]{unit}} objects.
#'
#' Earlier implementations used parameters \code{widhts} and \code{heights} as
#' synonyms for \code{width} and \code{height} with length greater than one.
#' These parameters have been deprecated. They continue to work, but produce
#' a warning.
#'
#' The two approaches to \code{\link[gtable]{gtable}} construction require
#'  interdepending parameter sets:
#' \describe{
#'   \item{Individual definition of panel dimensions:}{Requires \code{width} and
#'     \code{height} of lengths corresponding to the number of columns/rows
#'     requested. Excludes the use of \code{columns} and \code{rows}.}
#'   \item{Definition of global \code{\link[gtable]{gtable}}/figure dimensions:}{
#'     Requires \code{width}, \code{columns}, \code{height} and \code{rows} of
#'     length 1.}}
#' @param width \code{\link{numeric}} or \code{link[grid]{unit}} defining the
#' width(s) of the resulting \code{\link[gtable]{gtable}} if
#' \code{length(width) == 1} or individual column widths. Units depends on
#' \code{unit} if not provided as \code{\link[grid]{unit}} object. The default
#' '\code{auto}' sets the parameter to the width of the currently used device.
#' See 'Details' for dependent and interfering parameters.
#' @param columns Single \code{\link{numeric}} defining the number of columns in
#' the resulting \code{\link[gtable]{gtable}}. See 'Details' for dependent and
#' interfering parameters.
#' @param height \code{\link{numeric}} or \code{link[grid]{unit}} defining the
#' height of the resulting \code{\link[gtable]{gtable}} if
#' \code{length(height) == 1} or individual row heights.nits depends on
#' \code{unit} if not provided as \code{\link[grid]{unit}} object. The default
#' '\code{auto}' sets the parameter to the height of the currently used device.
#' See 'Details' for dependent and interfering parameters.
#' @param rows Single \code{\link{numeric}} defining the number of rows in
#' the resulting \code{\link[gtable]{gtable}}. See 'Details' for dependent and
#' interfering parameters.
#' @param row_spacing \code{\link{numeric}} or #' \code{\link[grid]{unit}}
#' defining the amount of white space automatically inserted between row panels.
#' Defaults to \code{5 mm} unless explicitly given, in which case the value may
#' depend on the \code{unit} parameter. Recycled to the number of rows.
#' @param column_spacing \code{\link{numeric}} or \code{\link[grid]{unit}}
#' defining the amount of white space automatically inserted between column
#' panels. Defaults to \code{5 mm} unless explicitly given, in which case the
#' value may depends on the \code{unit} parameter. Recycled to the number of
#' columns.
#' @param unit Single \code{\link{character}} object defining the unit of all
#' dimensions defined. Must satisfy \code{grid:::valid.units}.
#' @param figure_name Single \code{\link{character}} object defining the name of
#' the resulting \code{\link[gtable]{gtable}}.
#' @param panel_label_type A string specifying the marker style for the panel labels
#' used for automated annotation.  Defaults to uppercase Latin letters.
#' @param ... Argument to accomodate deprecated arguments \code{widths} and
#' \code{heights}.
#' @return Returns an object of class \code{multipanelfigure} as well as
#' \code{\link[gtable]{gtable}} object with the following additional attributes:
#' \describe{
#'   \item{\code{multipanelfigure.panelsFree}:}{A \code{\link{logical}}
#'     \code{\link{matrix}} with the dimensions of the \code{\link[gtable]{gtable}}
#'     indicating occupancy of the panels in the table.}
#'   \item{\code{multipanelfigure.panellabelsfree}:}{A \code{\link{character}}
#'     \code{\link{vector}} indicative of the \code{panel_labels} still available.}
#'   \item{\code{multipanelfigure.unit}:}{A single \code{\link{character}}
#'     object storing the corresponding value given during object creation.}}
#' @author Johannes Graumann
#' @references Graumann, J., and Cotton, R.J. (2018). multipanelfigure: Simple
#' Assembly of Multiple Plots and Images into a Compound Figure. Journal of
#' Statistical Software 84. doi: 10.18637/jss.v084.c03
#' @export
#' @seealso \code{\link{fill_panel}} for more examples of filling panels
#' \code{\link{figure_width}} for inspecting figure dimensions
#' \code{\link{capture_base_plot}} for including plots created using base graphics
#' \code{\link[gtable]{gtable}} for the underlying structure of a figure
#' @keywords hplot utilities
#' @examples
#' \dontrun{
#' # Figure construction based on the dimensions of the current device
#' figure1 <- multi_panel_figure(
#'    columns = 2,
#'    rows = 2,
#'    figure_name = "figure1")
#'
#' # With no panels, printing shows the layout
#' figure1
#'
#' # Figure construction based on overall dimensions
#' figure2 <- multi_panel_figure(
#'    width = 100,
#'    columns = 4,
#'    height = 90,
#'    rows = 6,
#'    figure_name = "figure2")
#'
#' # Still no panels ...
#' figure2
#'
#' # Figure construction based on individual panel dimensions
#' (figure3 <- multi_panel_figure(
#'    width = c(40,30),
#'    height = c(40,60),
#'    row_spacing = c(5, 1),
#'    column_spacing = c(0, 10),
#'    figure_name = "figure3"))
#'
#' # A more involved example including filling and printing to device ...
#' # Make a simple ggplot object to fill panels
#' ggp <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point()
#' # Fill panels
#' # ggplots and lattice plot objects are added directly
#' # The default position is the top-left panel
#' figure3 <- fill_panel(figure3, ggp)
#' # Raster images are added by passing the path to their file
#' jpg <- system.file("extdata/rhino.jpg", package = "multipanelfigure")
#' figure3 <- fill_panel(figure3, jpg, column = 2)
#' # Plots can take up multiple panels
#' figure3 <- fill_panel(figure3, ggp, row = 2, column = 1:2)
#' # Plot to appropriately sized png device
#' tmpFile <- tempfile(fileext = ".png")
#' ggplot2::ggsave(
#'   tmpFile, figure3,
#'   width = figure_width(figure3, "in"),
#'   height = figure_height(figure3, "in"))
#' message(
#'   paste0("Now have a look at '",tmpFile,"' - nicely sized PNG output."))
#' \donttest{ # Not testing due to use of external software
#' utils::browseURL(tmpFile)
#' }
#' }
multi_panel_figure <- function(
  width = "auto",
  columns = NULL,
  height = "auto",
  rows = NULL,
  row_spacing = NaN,
  column_spacing = NaN,
  unit = "mm",
  figure_name = "FigureX",
  panel_label_type = c("upper-alpha", "lower-alpha", "decimal", "upper-roman", "lower-roman", "upper-greek", "lower-greek", "none"),
  ...)
{
  #######################
  # Check Prerequisites #
  #######################

  # Deal with depreciated arguments 'widths' and 'heights'
  dot_list = list( ... )
  if ("heights" %in% names(dot_list)){
    warning("argument 'heights' deprecated. Use 'height' instead.")
    height <- dot_list[['heights']]
    heights <- NA_character_ # Attempt to ensure failing operations for debugging
  }
  if ("widths" %in% names(dot_list)){
    warning("argument 'widths' deprecated. Use 'width' instead.")
    width <- dot_list[['widths']]
    widths <- NA_character_ # Attempt to ensure failing operations for debugging
  }
  # Deal with depreciated arguments 'inter_column_spacing' and 'inter_row_spacing'
  dot_list = list( ... )
  if ("inter_column_spacing" %in% names(dot_list)){
    warning("argument 'inter_column_spacing' deprecated. Use 'column_spacing' instead.")
    column_spacing <- dot_list[['inter_column_spacing']]
    inter_column_spacing <- NA_character_ # Attempt to ensure failing operations for debugging
  }
  if ("inter_row_spacing" %in% names(dot_list)){
    warning("argument 'inter_row_spacing' deprecated. Use 'row_spacing' instead.")
    row_spacing <- dot_list[['inter_row_spacing']]
    inter_row_spacing <- NA_character_ # Attempt to ensure failing operations for debugging
  }

  # Deal with width/height 'auto'
  assert_is_a_supported_unit_type(unit)

  if(identical(width, "auto")){
    width <- grDevices::dev.size(units = "cm")[1] %>%
      grid::unit(units = "cm") %>%
      grid::convertUnit("mm")
  }

  if(identical(height, "auto")){
    height <- grDevices::dev.size(units = "cm")[2] %>%
      grid::unit(units = "cm") %>%
      grid::convertUnit("mm")
  }

  # Check passed arguments
  args_passed <- names(match.call()[-1])
  width_args_ok <- all(
    (length(width) ==  1 && ("columns" %in% args_passed)) ||
      (length(width) >= 1 && !("columns" %in% args_passed)))
  if(!width_args_ok)
  {
    stop('The figure width is not well specified. The call to multi_panel_figure must contain either\n  1. "width" of length 1 and "columns", or\n  2. "width", defining multiple columns.')
  }
  height_args_ok <- all(
      (length(height) ==  1 && ("rows" %in% args_passed)) ||
        (length(height) >= 1 && !("rows" %in% args_passed)))
  if(!height_args_ok)
  {
    stop('The figure height is not well specified. The call to multi_panel_figure must contain either\n  1. "height" of length 1 and "rows", or\n  2. "height", defining multiple rows.')
  }

  assert_is_a_string(figure_name)

  assert_is_numeric(width)
  assert_all_are_positive(width)
  if(!grid::is.unit(width)){
    width <- grid::unit(width, unit)
  }
  if(length(width) == 1){
    assert_is_not_null(columns)
    assert_is_a_number(columns)
    assert_all_are_whole_numbers(columns)
    assert_all_are_in_range(columns, lower = 1, upper = Inf)
    column_spacing <- fix_panel_spacing_arg(column_spacing, columns, unit)
    tmp_widths <- (width - column_spacing * columns) * (1 / columns) # No `/.unit`
  } else {
    assert_is_null(columns)
    columns <- length(width)
    column_spacing <- fix_panel_spacing_arg(column_spacing, columns, unit)
    tmp_widths <- width
  }

  assert_is_numeric(height)
  assert_all_are_positive(height)
  if(!grid::is.unit(height)){
    height <- grid::unit(height, unit)
  }
  if(length(height) == 1){
    assert_is_not_null(rows)
    assert_is_a_number(rows)
    assert_all_are_whole_numbers(rows)
    assert_all_are_in_range(rows, lower = 1, upper = Inf)
    row_spacing <- fix_panel_spacing_arg(row_spacing, rows, unit)
    tmp_heights <- (height - row_spacing * rows) * (1 / rows) # No `/.unit`
  } else {
    assert_is_null(rows)
    rows <- length(height)
    row_spacing <- fix_panel_spacing_arg(row_spacing, rows, unit)
    tmp_heights <- height
  }

  check_units(width, unit)
  check_units(height, unit)

  tmp_widths %<>% grid::convertUnit(unit)
  tmp_heights %<>% grid::convertUnit(unit)

  # TODO: support all CSS ordered list marker styles
  # greek, hebrew, georgian, hiragana, etc. still TODO
  # http://www.w3schools.com/cssref/pr_list-style-type.asp
  panel_label_type <- match.arg(panel_label_type)

  ####################
  # Construct gtable #
  ####################
  # Basic layout
  tmp_gtable <-
    gtable::gtable(
      widths = tmp_widths,
      heights = tmp_heights,
      name = figure_name) %>%
    # add interpanel space
    gtable_add_col_space2(width = column_spacing) %>%
    gtable_add_row_space2(height = row_spacing)
  ##########################
  # Prep and return output #
  ##########################
  multipanelfigure <- list(
    panelsFree = matrix(
      data = TRUE,
      ncol = columns,
      nrow = rows),
    panelLabelType = panel_label_type,
    unit = unit)
  attributes(tmp_gtable) <- c(
    attributes(tmp_gtable),
    multipanelfigure = multipanelfigure)
  class(tmp_gtable) <- c("multipanelfigure", class(tmp_gtable))
  return(tmp_gtable)
}

fix_panel_spacing_arg <- function(x, n, u)
{
  if(length(x) == 1 && is.na(x)){
    x <- 5
    u <- "mm"
  }
  assert_is_numeric(x)
  assert_all_are_non_negative(x, na_ignore = TRUE)
  x <- rep_len(x, n)
  if(!grid::is.unit(x)){
    x <- grid::unit(x, u)
  }
  x
}

# Adapted from gtable::gtable_add_col_space
# Also adds a column before the first existing column
gtable_add_col_space2 <- function (x, width)
{
    stopifnot(gtable::is.gtable(x))
    n <- ncol(x) # this line changed
    if (n == 0)
      return(x)
    stopifnot(length(width) == n)
    for (i in seq.int(n, 1, by = -1)) { # this line changed
      x <- gtable::gtable_add_cols(x, width[i], pos = i - 1)
    }
    x
}

# Adapted from gtable::gtable_add_row_space
# Also adds a row before the first existing row
gtable_add_row_space2 <- function (x, height)
{
    stopifnot(gtable::is.gtable(x))
    n <- nrow(x)
    if (n == 0)
      return(x)
    stopifnot(length(height) == n)
    for (i in seq.int(n, 1, by = -1)) {
      x <- gtable::gtable_add_rows(x, height[i], pos = i - 1)
    }
    x
}

#' @export
multipanelfigure <- function( ... ){
  .Deprecated(
    new = "multi_panel_figure",
    package = "multipanelfigure")
  paramList <- list( ... )
  if("interPanelSpacing" %in% names(paramList)){
    row_spacing <- paramList[["interPanelSpacing"]]
    column_spacing <- paramList[["interPanelSpacing"]]
  } else {
    row_spacing <- 5
    column_spacing <- 5
  }
  if("figureName" %in% names(paramList)){
    figure_name <- paramList[["figureName"]]
  } else {
    figure_name = "FigureX"
  }
  multi_panel_figure(
    row_spacing = row_spacing,
    column_spacing = column_spacing,
    figure_name = figure_name,
    ... )
}

check_units <- function(x, unit){
  if (getRversion() >= "4.0.0") {
    unitType <- get("unitType", envir=asNamespace("grid"))
    ## Use unitType()
    ## Do NOT use grid::unitType() because R < 4.0.0 will
    ## then complain about it not being exported
    tmp_units <- x %>%
      unitType() %>%
      unique()
  } else {
    if(inherits(x, "unit.list")){
      tmp_units <- x %>%
        rapply(attr, classes = "unit", which = "unit") %>%
        unique()
    } else {
      tmp_units <- x %>%
        attr("unit")
    }
  }
  if(length(tmp_units) != 1 || tmp_units != unit){
    warning("Multiple grid::units detected. Casting all to 'unit' argument ('", unit, "').")
  }
}