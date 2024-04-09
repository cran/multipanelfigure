#' @title fill_panel
#' @aliases addPanel addpanel add_panel fill_panel
#' @description A convenience function adding graphical objects to a
#' \code{\link[gtable]{gtable}} constructed by \code{\link{multi_panel_figure}}.
#' @details Currently supported as panel-representing objects (\code{panel}) are
#' \enumerate{
#'   \item{ComplexHeatmap \code{\link[ComplexHeatmap]{Heatmap}} or
#'     \code{\link[ComplexHeatmap]{HeatmapList}} objects.}
#'   \item{ggplot2 \code{\link[ggplot2]{ggplot}} objects.}
#'   \item{grid \code{\link[grid]{grob}}, \code{\link[grid]{gList}}, and
#'     \code{\link[grid]{gTree}} objects.}
#'   \item{lattice \code{\link[lattice]{trellis.object}}s.}
#'   \item{Single \code{\link{character}} objects representing URLs or paths to
#'     image formats accessible by \code{ImageMagick} as used through
#'     \code{\link[magick]{magick}}, which will be read and placed into panels
#'     as requested.}}
#'
#' Native resolution is determined from attributes in the file.  If the attributes are
#' not present, then the DPI is determined by the the
#' \code{multipanelfigure.defaultdpi} global option, or 300 if this has not been
#' set.
#'
#' \pkg{lattice}-generated \code{\link[lattice]{trellis.object}}s are converted
#' to \code{grob}s using \code{grid.grabExpr(print(x))}, as are \code{Heatmap}
#' and \code{HeatmapList}s from \pkg{ComplexHeatmap} - the side effects of
#' which with respect to plot formatting are not well studied.
#' @param figure Object of classes \code{multipanelfigure}/
#' \code{\link[gtable]{gtable}} as produced by \code{\link{multi_panel_figure}}
#' and representing the figure the panel is to be placed in.
#' @param panel Single \code{\link{character}} object representing URL or path to a
#' bitmap image accessible by \code{ImageMagick} as used through
#' \code{\link[magick]{magick}}, a \code{\link[ComplexHeatmap]{Heatmap}} or
#' \code{\link[ComplexHeatmap]{HeatmapList}} object, a
#' \code{\link[ggplot2]{ggplot}} object , a
#' \code{\link[lattice]{trellis.object}}, a \code{\link[grid]{gList}} object or
#' a \code{\link[grid]{grob}} object to be placed in a multipanel figure. See
#' 'Details'.
#' @param row \code{\link{numeric}} object of length 1 or a range, indicating the row
#' indeces the panel that is to be placed in the figure, or "auto" to
#' automatically pick the row (see details). May be used to define  panel
#' spanning (if \code{length(row) > 1}; see examples).
#' @param column \code{\link{numeric}} object of length 1 or a range, indicating the
#' column indeces of the panel that is to be placed in the figure, or "auto" to
#' automatically pick the column (see details). May be used to define  panel
#' spanning (if \code{length(column) > 1}; see examples).
#' @param label Single \code{\link{character}} object defining the panel
#' label used for automated annotation.
#' @param label_just Justification for the label within the interpanel spacing
#' grob to the top-left of the panel content grob.  Passed to
#' \code{\link[grid]{textGrob}}.
#' @param panel_clip Should the display of panel contents be clipped at the
#' panel borders?  See \code{\link[grid]{viewport}}.
#' @param scaling Only used when importing image files. Either "none" to
#' preserve the dimensions of an image, "stretch" to make it fit the panels,
#' "fit" to shrink or enlarge it so that it fills one dimension of the panels
#' while preserving the height to width ratio, or "shrink which does the same
#' but won't enlarge it.
#' @param allow_panel_overwriting A logical value. If \code{TRUE}, overwriting
#' panels is allowed, with a warning.  Otherwise (the default) it will cause an
#' error.
#' @param verbose A logical value. Reduces verbosity if \code{FALSE}.
#' @param ... Additional arguments. Used to deal with deprecated arguments
#' \code{top_panel}, \code{bottom_panel}, \code{left_panel} and \code{right_panel}.
#' @return Returns the \code{\link[gtable]{gtable}} object fed to it
#' (\code{figure}) with the addition of the \code{panel}.
#' @details If the \code{row} argument is "auto", then the first row with
#' a free panel is used.
#' If the \code{column} argument is "auto", then the first column in the
#' row with a free panel is used.
#' @author Johannes Graumann, Richard Cotton
#' @references Graumann, J., and Cotton, R.J. (2018). multipanelfigure: Simple
#' Assembly of Multiple Plots and Images into a Compound Figure. Journal of
#' Statistical Software 84. doi: 10.18637/jss.v084.c03
#' @export
#' @seealso \code{\link[gtable]{gtable}}, \code{\link{multi_panel_figure}}
#' @examples
#' \donttest{ # Not testing - slow grid graphics makes CRAN timing excessive
#' # Create the figure layout
#' (figure <- multi_panel_figure(
#'   width = c(30,40,60),
#'   height = c(40,60,60,60),
#'   panel_label_type = "upper-roman"))
#'
#' # Fill the top-left panel using a grob object directly
#' a_grob <- grid::linesGrob(arrow = grid::arrow())
#' figure %<>% fill_panel(a_grob)
#'
#' # Add a ggplot object directly to the top row, second column.
#' # The panels are chosen automatically, but you can achieve the same effect
#' # using column = 2
#' a_ggplot <- ggplot2::ggplot(mtcars, ggplot2::aes(disp, mpg)) +
#'   ggplot2::geom_point()
#' figure %<>% fill_panel(a_ggplot)
#'
#' # Bitmap images are added by passing the path to their file.
#' image_files <- system.file("extdata", package = "multipanelfigure") %>%
#'   dir(full.names = TRUE) %>%
#'   magrittr::set_names(basename(.))
#'
#' # Add the JPEG to the top row, third column
#' figure %<>% fill_panel(image_files["rhino.jpg"], column = 3)
#'
#' # Add the PNG to the second and third row, first and second column
#' figure %<>% fill_panel(
#'   image_files["Rlogo.png"],
#'   row = 2:3, column = 1:2)
#'
#' # Add the TIFF to the second row, third column
#' figure %<>% fill_panel(
#'   image_files["unicorn.svg"],
#'   row = 2, column = 3)
#'
#' # lattice/trellis plot objects are also added directly
#' Depth <- lattice::equal.count(quakes$depth, number=4, overlap=0.1)
#' a_lattice_plot <- lattice::xyplot(lat ~ long | Depth, data = quakes)
#' # Add the lattice plot to the third row, third column
#' figure %<>% fill_panel(
#'   a_lattice_plot,
#'   row = 3, column = 3)
#'
#' # Incorporate a gList object (such as produced by VennDigram)
#' if(requireNamespace("VennDiagram"))
#' {
#'   a_venn_plot <- VennDiagram::draw.pairwise.venn(50, 30, 20, ind = FALSE)
#'   # Add the Venn diagram to the fourth row, firstd column
#'   figure %<>% fill_panel(
#'     a_venn_plot,
#'     row = 4, column = 1)
#' }
#'
#' # Incorporate a base plot figure
#' a_base_plot <- capture_base_plot(
#'  heatmap(
#'    cor(USJudgeRatings), Rowv = FALSE, symm = TRUE, col = topo.colors(16),
#'    distfun = function(c) as.dist(1 - c), keep.dendro = TRUE,
#'    cexRow = 0.5, cexCol = 0.5))
#' # Add the heatmap to the fourth row, second column
#' figure %<>% fill_panel(
#'   a_base_plot,
#'   row = 4, column = 2)
#'
#' # Incorporate a ComplexHeatmap figure
#' require(ComplexHeatmap)
#' mat = matrix(rnorm(80, 2), 8, 10)
#' mat = rbind(mat, matrix(rnorm(40, -2), 4, 10))
#' rownames(mat) = letters[1:12]
#' colnames(mat) = letters[1:10]
#' ht = Heatmap(mat)
#' a_complex_heatmap <- ht + ht + ht
#' # Add the ComplexHeatmap to the fourth row, third column
#' (figure %<>% fill_panel(
#'   a_complex_heatmap,
#'   row = 4, column = 3))
#' }
fill_panel <- function(
  figure,
  panel,
  row = "auto",
  column = "auto",
  label = NULL,
  label_just = c("right", "bottom"),
  panel_clip = c("on", "off", "inherit"),
  scaling = c("none", "stretch", "fit", "shrink"),
  allow_panel_overwriting = FALSE,
  verbose = TRUE,
  ...)
{
  ####################################################
  # Check prerequisites & transform objects to grobs #
  ####################################################

  # Deal with deprecated parameters
  dot_list <- list( ... )
  if("top_panel" %in% names(dot_list)){
    warning("argument 'top_panel' deprecated. Use 'row' instead.")
    row <- dot_list[['top_panel']]
    top_panel <- NA_character_ # Attempt to ensure failing operations for debugging
  }
  if("bottom_panel" %in% names(dot_list)){
    warning("argument 'bottom_panel' deprecated. Use 'row' instead.")
    if(dot_list[['bottom_panel']] == 'top_panel'){
      row[2] <- row[1]
    } else {
      row[2] <- dot_list[['bottom_panel']]
    }
    top_panel <- NA_character_ # Attempt to ensure failing operations for debugging
  }
  if("left_panel" %in% names(dot_list)){
    warning("argument 'left_panel' deprecated. Use 'column' instead.")
    column <- dot_list[['left_panel']]
    left_panel <- NA_character_ # Attempt to ensure failing operations for debugging
  }
  if("right_panel" %in% names(dot_list)){
    warning("argument 'right_panel' deprecated. Use 'column' instead.")
    if(dot_list[['right_panel']] == 'left_panel'){
      column[2] <- column[1]
    } else {
      column[2] <- dot_list[['right_panel']]
    }
    right_panel <- NA_character_ # Attempt to ensure failing operations for debugging
  }

  figure %>%
    assert_is_multipanelfigure

  panel_clip <- match.arg(panel_clip)

  scaling <- if(is.numeric(scaling))
  {
    assert_all_are_positive(scaling)
    scaling <- rep_len(scaling, 2)
  }
  else
  {
    match.arg(scaling)
  }

  overwriting_severity_fn <- if(allow_panel_overwriting)
  {
    warning
  } else
  {
    stop
  }

  panels_free <- attr(figure, which = "multipanelfigure.panelsFree")
  rows <- nrow(panels_free)
  columns <- ncol(panels_free)

  if(identical(row[1], "auto"))
  {
    row_has_free_panel <- panels_free %>%
      apply(1L, any)
    if(!any(row_has_free_panel))
    {
      overwriting_severity_fn("There are no free panels in the figure.")
    }
    row[1] <- which(row_has_free_panel)[1]
    row %<>% as.numeric()
    if(verbose)
    {
      message("Setting row to ", row[1])
    }
  }

  if(length(row) == 1){
    row[2] <- row[1]
  } else if(length(row) > 2){
    row <- c(
      row[1],
      utils::tail(row, n = 1))
  }
  assert_is_numeric(row)
  assert_all_are_whole_numbers(row)
  assert_all_are_in_closed_range(row, lower = 1, upper = rows)
  assert_all_are_less_than_or_equal_to(row[1], row[2])

  if(identical(column[1], "auto"))
  {
    col_has_free_panel <- panels_free[row[1], ]
    if(!any(col_has_free_panel))
    {
      overwriting_severity_fn("There are no free panels in the figure.")
    }
    column[1] <- which(col_has_free_panel)[1]
    column %<>% as.numeric()
    if(verbose)
    {
      message("Setting column to ", column[1])
    }
  }

  if(length(column) == 1){
    column[2] <- column[1]
  } else if(length(column) > 2){
    column <- c(
      column[1],
      utils::tail(column, n = 1))
  }
  assert_is_numeric(column)
  assert_all_are_whole_numbers(column)
  assert_all_are_in_closed_range(column, lower = 1, upper = columns)
  assert_all_are_less_than_or_equal_to(column[1], column[2])

  # Are the targeted panels free?
  panels_to_fill <- matrix(FALSE, nrow = rows, ncol = columns)
  panels_to_fill[
    seq.int(from = row[1], to = row[2]),
    seq.int(from = column[1], to = column[2])] <- TRUE
  clashes <- panels_to_fill & !panels_free
  if(any(clashes))
  {
    clash_indices <- data.frame(which(clashes, arr.ind = TRUE))
    overwriting_severity_fn(
      "Attempt to use these already filled panels.\n",
      print_and_capture(clash_indices)
    )
  }
  attr(figure, which = "multipanelfigure.panelsFree")[
      seq(from = row[1], to = row[2]),
      seq(from = column[1], to = column[2])] <- FALSE

  # Check/fix panel label
  label <- if(is.null(label))
  {
    next_label(figure)
  } else
  {
    use_first(coerce_to(label, "character"))
  }

  ##############
  # Processing #
  ##############
  # Get the "real" spans (including inter-panel spaces)
  panel_placing <-
    2 * c(row[1], row[2], column[1], column[2]) %>%
    magrittr::set_names(c("top", "bottom", "left", "right"))
  label_placing <- panel_placing[c("top", "left")] - 1

  # Get the available space to contain the panel
  figureUnit <- figure %>%
    attr("multipanelfigure.unit")
  panelWidth <- figure$widths[panel_placing["left"]:panel_placing["right"]] %>%
    sum %>%
    grid::convertUnit(unitTo = figureUnit)
  panelHeight <- figure$heights[panel_placing["top"]:panel_placing["bottom"]] %>%
    sum %>%
    grid::convertUnit(unitTo = figureUnit)

  # Make the panel grob
  panel <- make_grob(
    panel,
    unit_to = attr(figure, "multipanelfigure.unit"),
    panelSize = grid::unit.c(panelWidth, panelHeight),
    scaling = scaling)

  # Create panel label grob
  panel_label <- grid::textGrob(
    label = label,
    x = 1, y = 0,
    just = label_just)
  # Add grobs to gtable
  figure <- gtable::gtable_add_grob(
    figure,
    grobs = panel,
    t = panel_placing[["top"]],
    b = panel_placing[["bottom"]],
    l = panel_placing[["left"]],
    r = panel_placing[["right"]],
    clip = panel_clip)

  figure <- gtable::gtable_add_grob(
    figure,
    grobs = panel_label,
    t = label_placing[["top"]],
    b = label_placing[["top"]],  # *not* bottom
    l = label_placing[["left"]],
    r = label_placing[["left"]], # *not* right
    clip = "off")
  # Return
  return(figure)
}

is_url <- function(x)
{
  grepl("^(?:https?|ftp)://", x, ignore.case = TRUE)
}

sanitise_file_name <- function(x)
{
  gsub('[\\/:*?"<>|]+', '_', x)
}

download_file <- function(x, verbose = TRUE, ...)
{
  tmp <- file.path(tempdir(), sanitise_file_name(basename(x)))
  if(verbose)
  {
    message("Downloading to ", tmp)
  }
  utils::download.file(x, tmp, mode = "wb", ...)
  tmp
}

get_raster_grob <- function(x, unit_to, panelSize, scaling)
{
  image <- magick::image_read(
    path    = x,
    density = getOption("multipanelfigure.defaultdpi", default = 300),
    strip   = FALSE)
  imageInfo <- image %>%
    magick::image_info()
  imageDim <- imageInfo %>%
    magrittr::extract(c('width', 'height')) %>%
    unlist()
  imageDpi <- imageInfo %>%
    magrittr::extract2('density') %>%
    stringi::stri_extract_first_regex('\\d+') %>%
    as.numeric()
  if(is.null(imageDpi))
  {
    imageDpi <- getOption("multipanelfigure.defaultdpi", default = 300)
  }
  make_raster_grob_from_image(image, imageDim, imageDpi, unit_to, panelSize, scaling)
}

make_raster_grob_from_image <- function(image, imageDim, imageDpi, unit_to, panelSize, scaling)
{
  imageSize <-
    (imageDim / imageDpi) %>%
    grid::unit(units = "inches") %>%
    grid::convertUnit(unitTo = unit_to)
  newSize <- resizeImage(scaling, imageSize, panelSize)
  grid::rasterGrob(
    image,
    width = newSize[1],
    height = newSize[2])
}

make_grob <- function(x, unit_to, panelSize, scaling, verbose = TRUE, ...){
  if(is.character(x)){ # It's a path (to an image)
    x <- use_first(x)
    if(!is_url(x)){
      assert_all_are_readable_files(x)
    }
    panel <- get_raster_grob(x, unit_to, panelSize, scaling)
  } else if(inherits(x = x, what = "ggplot")){
    panel <- ggplot2::ggplotGrob(x)
  } else if(inherits(x = x, what = "gList")){
    # Convert gList to gTree so the automatic labelling works
    panel <- do.call(grid::grobTree, x)
  } else if(inherits(x = x, what = c("grob", "gTree"))){
    panel <- x
  } else if (inherits(x = x, what = "trellis")){
    # See http://r.789695.n4.nabble.com/lattice-grob-td1599209.html
    panel <- grid::grid.grabExpr(print(x))
  } else if (inherits(x = x, what = c("Heatmap", "HeatmapList"))){
    if(requireNamespace("ComplexHeatmap", quietly = TRUE)){
      panel <- grid::grid.grabExpr(ComplexHeatmap::draw(x), wrap = TRUE, warn = FALSE)
    } else {
      stop("Install \'ComplexHeatmap\' from Bioconductor first.")
    }
  } else {
    stop("Class of \'panel\' is not supported.")
  }
  return(panel)
}

#' @export
addPanel <- function( figure, ... ){
  .Deprecated(
    new = "fill_panel",
    package = "multipanelfigure")
  paramList <- list ( ... )
  if("topPanel" %in% names(paramList)){
    top_panel = paramList[["topPanel"]]
  } else {
    top_panel = 1
  }
  if("bottomPanel" %in% names(paramList)){
    bottom_panel = paramList[["bottomPanel"]]
  } else {
    bottom_panel = top_panel
  }
  if("leftPanel" %in% names(paramList)){
    left_panel = paramList[["leftPanel"]]
  } else {
    left_panel = 1
  }
  if("rightPanel" %in% names(paramList)){
    right_panel = paramList[["rightPanel"]]
  } else {
    right_panel = left_panel
  }
  fill_panel(
    figure = figure,
    top_panel = top_panel,
    bottom_panel = bottom_panel,
    left_panel = left_panel,
    right_panel = right_panel,
    label = NULL, # for ease of maintenance, only support auto-labelling in deprecated case
    ... )
}

#' @export
addpanel <- addPanel

#' @export
add_panel <- function(  ... ){
  .Deprecated(
    new = "fill_panel",
    package = "multipanelfigure")
  fill_panel( ... )
}
