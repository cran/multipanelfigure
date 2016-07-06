#' @title multi_panel_figure
#' @aliases multipanelfigure
#' @description A convenience function building \code{\link[gtable]{gtable}}-based
#' infrastructure for the assembly of multipanel figures.
#' @details The \code{\link[gtable]{gtable}} may be constructed in two ways:
#' \enumerate{
#'   \item{Based on explicit width/height definitions for individual panels
#'     handed to it via \code{widths} and \code{heights}.}
#'   \item{Based on total figure/\code{\link[gtable]{gtable}} dimensions given by
#'     \code{width} and \code{height} together with the number of \code{columns}
#'     and \code{rows} requested.}}
#' The function automatically inserts whitespace of width
#' \code{inter_column_spacing} before column panels (and of height
#' \code{inter_row_spacing} before row panels), which has to be considered
#' for the total dimensions of the resulting \code{\link[gtable]{gtable}}. Width
#' of the \code{\link[gtable]{gtable}} in the former case, for example may be
#' calculated
#' \deqn{W[total] = sum(widths) + length(widths) * inter_column_spacing}
#' while width of resulting panels in the latter table construction approach may
#' be calculated
#' \deqn{W[panel] = (width - columns * inter_column_spacing) / columns}
#'
#' The two approaches to \code{\link[gtable]{gtable}} construction require mutually
#' exclusive parameter sets:
#' \describe{
#'   \item{Individual definition of panel dimensions:}{Requires \code{widths} and
#'     \code{heights}. Excludes the use of \code{width}, \code{columns},
#'     \code{heights} and \code{rows}.}
#'   \item{Definition of global \code{\link[gtable]{gtable}}/figure dimensions:}{
#'     Requires \code{width}, \code{columns}, \code{heights} and \code{rows}.
#'     Excludes the use of \code{widths} and \code{heights}.}}
#' @param width Single \code{link{numeric}} defining the width of the resulting
#' \code{\link[gtable]{gtable}} (unit depending on \code{unit}). See 'Details' for
#' dependent and interfering parameters.
#' @param widths \code{\link{vector}} of \code{\link{numeric}}s defining the
#' actual widths of panels/columns in the resulting \code{\link[gtable]{gtable}} (unit
#' depending on \code{unit}). See 'Details' for dependent and
#' interfering parameters.
#' @param columns Single \code{\link{numeric}} defining the number of columns in
#' the resulting \code{\link[gtable]{gtable}}. See 'Details' for dependent and
#' interfering parameters.
#' @param height Single \code{link{numeric}} defining the height of the resulting
#' \code{\link[gtable]{gtable}} (unit depending on \code{unit}). See 'Details' for
#' dependent and interfering parameters.
#' @param heights \code{\link{vector}} of \code{\link{numeric}}s defining the
#' actual heights of panels/rows in the resulting \code{\link[gtable]{gtable}} (unit
#' depending on \code{unit}). See 'Details' for dependent and
#' interfering parameters.
#' @param rows Single \code{\link{numeric}} defining the number of rows in
#' the resulting \code{\link[gtable]{gtable}}. See 'Details' for dependent and
#' interfering parameters.
#' @param inter_row_spacing The amount of white space automatically inserted
#' between row panels. Defaults to \code{5 mm} unless explicitly given, in which
#' case the value depends on the \code{unit} parameter. Recycled to the number
#' of rows.
#' @param inter_column_spacing The amount of white space automatically inserted
#' between column panels. Defaults to \code{5 mm} unless explicitly given, in
#' which case the value depends on the \code{unit} parameter. Recycled to the
#' number of columns.
#' @param unit Single \code{\link{character}} object defining the unit of all
#' dimensions defined. Must satisfy \code{grid:::valid.units}.
#' @param figure_name Single \code{\link{character}} object defining the name of
#' the resulting \code{\link[gtable]{gtable}}.
#' @param panel_label_type A string specifying the marker style for the panel labels
#' used for automated annotation.  Defaults to uppercase Latin letters.
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
#' @export
#' @seealso \code{\link{add_panel}} for more examples of adding panels
#' \code{\link{simple_grob_width}} for inspecting figure dimensions
#' \code{\link{capture_base_plot}} for including plots created using base graphics
#' \code{\link[gtable]{gtable}} for the underlying structure of a figure
#' @keywords hplot utilities
#' @importFrom assertive.base assert_all_are_true
#' @importFrom assertive.properties assert_is_null
#' @importFrom assertive.properties assert_is_not_null
#' @importFrom assertive.numbers assert_all_are_non_negative
#' @importFrom assertive.numbers assert_all_are_positive
#' @importFrom assertive.numbers assert_all_are_whole_numbers
#' @importFrom assertive.numbers assert_all_are_in_range
#' @importFrom assertive.types assert_is_a_bool
#' @importFrom assertive.types assert_is_numeric
#' @importFrom assertive.types assert_is_a_string
#' @importFrom assertive.types assert_is_a_number
#' @importFrom assertive.types assert_is_numeric
#' @importFrom assertive.types assert_is_character
#' @importFrom gtable gtable
#' @importFrom gtable gtable_add_col_space
#' @importFrom gtable gtable_add_row_space
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @examples
#' # Figure construction based on overall dimensions
#' figure1 <- multi_panel_figure(
#'    width = 100,
#'    columns = 4,
#'    height = 90,
#'    rows = 6,
#'    figure_name = "figure1")
#' # With no panels, printing shows the layout
#' figure1
#'
#' # Figure construction based on individual panel dimensions
#' (figure2 <- multi_panel_figure(
#'    widths = c(40,30),
#'    heights = c(40,60),
#'    inter_row_spacing = c(5, 1),
#'    inter_column_spacing = c(0, 10),
#'    figure_name = "figure2"))
#'
#' # A more involved example including filling and printing to device ...
#' # Make a simple ggplot object to fill panels
#' ggp <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point()
#' # Fill panels
#' # ggplots and lattice plot objects are added directly
#' # The default position is the top-left panel
#' figure2 <- add_panel(figure2, ggp)
#' # JPEG, PNG, and TIFF images are added by passing the path to their file
#' jpg <- system.file("extdata/rhino.jpg", package = "multipanelfigure")
#' figure2 <- add_panel(figure2, jpg, left_panel = 2)
#' # Plots can take up multiple panels
#' figure2 <- add_panel(figure2, ggp, top_panel = 2, left_panel = 1, right_panel = 2)
#' # Plot to appropriately sized png device
#' tmpFile <- tempfile(fileext = ".png")
#' ggplot2::ggsave(
#'   tmpFile, figure2,
#'   width = simple_grob_width(figure2, "in"),
#'   height = simple_grob_height(figure2, "in"))
#' message(
#'   paste0("Now have a look at '",tmpFile,"' - nicely sized PNG output."))
#' \donttest{ # Not testing due to use of external software
#' utils::browseURL(tmpFile)
#' }
multi_panel_figure <- function(
  width = NULL,
  widths = NULL,
  columns = NULL,
  height = NULL,
  heights = NULL,
  rows = NULL,
  inter_row_spacing = NaN,
  inter_column_spacing = NaN,
  unit = "mm",
  figure_name = "FigureX",
  panel_label_type = c("upper-alpha", "lower-alpha", "decimal", "upper-roman", "lower-roman", "upper-greek", "lower-greek", "none"))
{
  #######################
  # Check Prerequisites #
  #######################
  unit %>%
    force %>%
    assert_is_a_valid_unit_type()

  assert_is_a_string(figure_name)

  if(!is.null(width)){
    assert_is_null(widths)
    width %<>%
      assert_is_a_number() %>%
      assert_all_are_positive() %>%
      unit(unit)
    columns %>%
      assert_is_not_null() %>%
      assert_is_a_number() %>%
      assert_all_are_whole_numbers() %>%
      assert_all_are_in_range(lower = 1, upper = Inf)
    inter_column_spacing <- fix_panel_spacing_arg(inter_column_spacing, columns, unit)
    widths <- (width - inter_column_spacing * columns) * (1 / columns) # No `/.unit`
  } else {
    assert_is_null(columns)
    widths %<>%
      assert_is_not_null() %>%
      assert_is_numeric() %>%
      assert_all_are_positive() %>%
      unit(unit)
    columns <- length(widths)
    inter_column_spacing <- fix_panel_spacing_arg(inter_column_spacing, columns, unit)
    # width <- sum(widths) + inter_column_spacing * columns
  }

  if(!is.null(height)){
    assert_is_null(heights)
    height %<>%
      assert_is_a_number() %>%
      assert_all_are_positive() %>%
      unit(unit)
    rows %>%
      assert_is_not_null() %>%
      assert_is_a_number() %>%
      assert_all_are_whole_numbers() %>%
      assert_all_are_in_range(lower = 1, upper = Inf)
    inter_row_spacing <- fix_panel_spacing_arg(inter_row_spacing, rows, unit)
    heights <- (height - inter_row_spacing * rows) * (1 / rows) # No `/.unit`
  } else {
    assert_is_null(rows)
    heights %<>%
      assert_is_not_null() %>%
      assert_is_numeric() %>%
      assert_all_are_positive() %>%
      unit(unit)
    rows <- length(heights)
    inter_row_spacing <- fix_panel_spacing_arg(inter_row_spacing, rows, unit)
    # height <- sum(heights) + inter_row_spacing * rows
  }

  widths %<>% convertUnit(unit)
  heights %<>% convertUnit(unit)

  # TODO: support all CSS ordered list marker styles
  # greek, hebrew, georgian, hiragana, etc. still TODO
  # http://www.w3schools.com/cssref/pr_list-style-type.asp
  panel_label_type <- match.arg(panel_label_type)

  ####################
  # Construct gtable #
  ####################
  # Basic layout
  tmp_gtable <-
    gtable(
      widths = widths,
      heights = heights,
      name = figure_name) %>%
    # add interpanel space
    gtable_add_col_space2(width = inter_column_spacing) %>%
    gtable_add_row_space2(height = inter_row_spacing)
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
  assert_is_numeric(x)
  assert_all_are_non_negative(x, na_ignore = TRUE)
  x <- rep_len(x, n)
  x_is_na <- is.na(x)
  x <- unit(ifelse(x_is_na, 5, x), ifelse(x_is_na, "mm", u))
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
    inter_row_spacing <- paramList[["interPanelSpacing"]]
    inter_column_spacing <- paramList[["interPanelSpacing"]]
  } else {
    inter_row_spacing <- 5
    inter_column_spacing <- 5
  }
  if("figureName" %in% names(paramList)){
    figure_name <- paramList[["figureName"]]
  } else {
    figure_name = "FigureX"
  }
  multi_panel_figure(
    inter_row_spacing = inter_row_spacing,
    inter_column_spacing = inter_column_spacing,
    figure_name = figure_name,
    ... )
}
