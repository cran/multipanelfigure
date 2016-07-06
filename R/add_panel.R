#' @title add_panel
#' @aliases addPanel addpanel
#' @description A convenience function adding graphical objects to a
#' \code{\link[gtable]{gtable}} constructed by \code{\link{multi_panel_figure}}.
#' @details Currently supported as panel-representing objects (\code{panel}) are
#' \enumerate{
#'   \item{ggplot2 \code{\link[ggplot2]{ggplot}} objects.}
#'   \item{grid \code{\link[grid]{grob}}, \code{\link[grid]{gList}}, and
#'     \code{\link[grid]{gTree}} objects.}
#'   \item{lattice \code{\link[lattice]{trellis.object}}s.}
#'   \item{Single \code{\link{character}} objects representing URLs or paths to
#'     readable portable network graphics (\code{*.png}), tagged image file
#'     format (\code{*.tiff}/\code{*.tif}), joint photographic experts group
#'     (\code{*.jpg}/\code{*.jpeg}) files, or support vector graphics
#'     (\code{*.svg}) which will be read and placed into panels as requested.}}
#' Note that \code{*.jpg}/\code{*.jpeg} files must be produced
#' using the dimensions of the panel(s) they are to be placed in for sensible
#' results. \code{\link[ggplot2]{ggplot}} objects obviously auto-scale and
#' \code{*.tiff}/\code{*.tif}, as well as \code{*.png} files have their native
#' sizes read out of the file (which isn't working for
#' \code{*.jpg}/\code{*.jpeg}).  \code{*.svg} files are converted to a raster
#' object the size of the panel.
#' \pkg{lattice}-generated \code{\link[lattice]{trellis.object}}s are converted
#' tp \code{grob}s using \code{grid.grabExpr(print(x))}, the side effects of
#' which with respect to plot formatting are not well studied.
#' @param figure Object of classes \code{multipanelfigure}/
#' \code{\link[gtable]{gtable}} as produced by \code{\link{multi_panel_figure}}
#' and representing the figure the panel is to be placed in.
#' @param panel Single \code{\link{character}} object representing path to a
#' bitmap image (\code{*.png}, \code{*.tiff}/\code{*.tif},
#' \code{*.jpg}/\code{*.jpeg}), a \code{\link[ggplot2]{ggplot}} object , a
#' \code{\link[lattice]{trellis.object}}, a \code{\link[grid]{gList}} object or
#' a \code{\link[grid]{grob}} object to be placed in a multipanel figure. See
#' 'Details'.
#' @param top_panel Single \code{\link{numeric}} indicating the row index of
#' the panel that is to be placed in the figure.
#' @param bottom_panel Single \code{\link{numeric}} indicating the lower row
#' index of the panel that is to be placed in the figure. Important for
#' definition of panel spanning (see examples).
#' @param left_panel Single \code{\link{numeric}} indicating the column index
#' of the panel that is to be placed in the figure.
#' @param right_panel Single \code{\link{numeric}} indicating the right column
#' index of the panel that is to be placed in the figure. Important for
#' definition of panel spanning (see examples).
#' @param label Single \code{\link{character}} object defining the panel
#' label used for automated annotation.
#' @param label_just Justification for the label within the interpanel spacing
#' grob to the top-left of the panel content grob.  Passed to
#' \code{\link[grid]{textGrob}}.
#' @param panel_clip Should the display of panel contents be clipped at the
#' panel borders?  See \code{\link[grid]{viewport}}.
#' @param ... Additional arguments passed to \code{\link[utils]{download.file}}
#' when adding PNG, TIFF, or JPEG panels from URL.
#' @return Returns the \code{\link[gtable]{gtable}} object fed to it
#' (\code{figure}) with the addition of the \code{panel}.
#' @author Johannes Graumann
#' @export
#' @seealso \code{\link[gtable]{gtable}}, \code{\link{multi_panel_figure}},
#' \code{\link[tiff]{readTIFF}}, \code{\link[png]{readPNG}},
#' \code{\link[jpeg]{readJPEG}}
#' @importFrom assertive.base assert_all_are_true
#' @importFrom assertive.base use_first
#' @importFrom assertive.base coerce_to
#' @importFrom assertive.numbers assert_all_are_whole_numbers
#' @importFrom assertive.numbers assert_all_are_in_closed_range
#' @importFrom assertive.types assert_is_a_number
#' @importFrom grid textGrob
#' @importFrom gtable gtable_add_grob
#' @importFrom magrittr %>%
#' @importFrom stats setNames
#' @examples
#' # Create the figure layout
#' (figure <- multi_panel_figure(
#'   widths = c(30,40,60),
#'   heights = c(40,60,60,60),
#'   panel_label_type = "upper-roman"))
#'
#' # Fill the top-left panel using a grob object directly
#' a_grob <- grid::linesGrob(arrow = grid::arrow())
#' figure %<>% add_panel(a_grob)
#'
#' # Add a ggplot object directly to the top row, second column.
#' a_ggplot <- ggplot2::ggplot(mtcars, ggplot2::aes(disp, mpg)) +
#'   ggplot2::geom_point()
#' figure %<>% add_panel(a_ggplot, left_panel = 2)
#'
#' # JPEG, PNG, TIFF, and SVG images are added by passing the path to their file.
#' image_files <- system.file("extdata", package = "multipanelfigure") %>%
#'   dir(full.names = TRUE) %>%
#'   setNames(basename(.))
#'
#' # Add the JPEG to the top row, third column
#' figure %<>% add_panel(image_files["rhino.jpg"], left_panel = 3)
#'
#' # Add the PNG to the second and third row, first and second column
#' figure %<>% add_panel(
#'   image_files["Rlogo.png"],
#'   top_panel = 2, bottom_panel = 3, left_panel = 1, right_panel = 2)
#'
#' # Add the TIFF to the second row, third column
#' figure %<>% add_panel(
#'   image_files["unicorn.svg"],
#'   top_panel = 2, left_panel = 3)
#'
#' # lattice/trellis plot objects are also added directly
#' Depth <- lattice::equal.count(quakes$depth, number=4, overlap=0.1)
#' a_lattice_plot <- lattice::xyplot(lat ~ long | Depth, data = quakes)
#' # Add the lattice plot to the third row, third column
#' figure %<>% add_panel(
#'   a_lattice_plot,
#'   top_panel = 3, left_panel = 3)
#'
#' # Incorporate a gList object (such as produced by VennDigram)
#' if(requireNamespace("VennDiagram"))
#' {
#'   a_venn_plot <- VennDiagram::draw.pairwise.venn(50, 30, 20, ind = FALSE)
#'   # Add the Venn diagram to the fourth row, first and second columns
#'   (figure %<>% add_panel(
#'     a_venn_plot,
#'     top_panel = 4, left_panel = 1, right_panel = 2))
#' }
#'
#' # Incorporate a base plot figure
#' a_base_plot <- capture_base_plot(
#'  heatmap(
#'    cor(USJudgeRatings), Rowv = FALSE, symm = TRUE, col = topo.colors(16),
#'    distfun = function(c) as.dist(1 - c), keep.dendro = TRUE,
#'    cexRow = 0.5, cexCol = 0.5))
#' # Add the heatmap to the fourth row, third column
#' (figure %<>% add_panel(
#'   a_base_plot,
#'   top_panel = 4, left_panel = 3))
add_panel <- function(
  figure,
  panel,
  top_panel = 1,
  bottom_panel = top_panel,
  left_panel = 1,
  right_panel = left_panel,
  label = NULL,
  label_just = c("right", "bottom"),
  panel_clip = c("on", "off", "inherit"),
  ...)
{
  ####################################################
  # Check prerequisites & transform objects to grobs #
  ####################################################

  figure %>%
    assert_is_multipanelfigure

  panel_clip <- match.arg(panel_clip)

  panel <- make_grob(panel, unit_to = attr(figure, "multipanelfigure.unit"))

  rows <- nrow(attr(figure,which = "multipanelfigure.panelsFree"))
  columns <- ncol(attr(figure,which = "multipanelfigure.panelsFree"))

  top_panel %>%
    assert_is_a_number() %>%
    assert_all_are_whole_numbers() %>%
    assert_all_are_in_closed_range(lower = 1, upper = rows)

  bottom_panel %>%
    assert_is_a_number() %>%
    assert_all_are_whole_numbers() %>%
    assert_all_are_in_closed_range(lower = 1, upper = rows)

  top_panel %>%
    assert_all_are_in_range(lower = 1, upper = bottom_panel)

  bottom_panel %>%
    assert_all_are_in_range(lower = top_panel, upper = rows)

  left_panel %>%
    assert_is_a_number() %>%
    assert_all_are_whole_numbers() %>%
    assert_all_are_in_range(lower = 1, upper = columns)

  right_panel %>%
    assert_is_a_number() %>%
    assert_all_are_whole_numbers() %>%
    assert_all_are_in_range(lower = 1, upper = columns)
    assert_all_are_true(left_panel <= right_panel)

  left_panel %>%
    assert_all_are_in_closed_range(lower = 1, upper = right_panel)

  right_panel %>%
    assert_all_are_in_closed_range(lower = left_panel, upper = columns)

  # Are the targeted panels free?
  tmpMatrix <- matrix(TRUE, nrow = rows, ncol = columns)
  tmpMatrix[
    seq(from=top_panel, to= bottom_panel),
    seq(from=left_panel,to=right_panel)] <- FALSE
  tmpMatrix <- attr(figure,which = "multipanelfigure.panelsFree") + tmpMatrix
  if(all(tmpMatrix[
    seq(from=top_panel, to= bottom_panel),
    seq(from=left_panel,to=right_panel)] == 1))
  {
    attr(figure, which = "multipanelfigure.panelsFree")[
      seq(from=top_panel, to= bottom_panel),
      seq(from=left_panel,to=right_panel)] <- FALSE
  } else {
    stop("Attempt to use already filled panel. Check \'attr(figure,which = \"multipanelfigure.panelsFree\")\'.")
  }

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
    2 * c(top_panel, bottom_panel, left_panel, right_panel) %>%
    setNames(c("top", "bottom", "left", "right"))
  label_placing <- panel_placing[c("top", "left")] - 1

  # Create panel label grob
  panel_label <- textGrob(
    label = label,
    x = 1, y = 0,
    just = label_just)
  # Add grobs to gtable
  figure <- gtable_add_grob(
    figure,
    grobs = panel,
    t = panel_placing[["top"]],
    b = panel_placing[["bottom"]],
    l = panel_placing[["left"]],
    r = panel_placing[["right"]],
    clip = panel_clip)

  figure <- gtable_add_grob(
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

#' @importFrom utils download.file
download_file <- function(x, ...)
{
  tmp <- file.path(tempdir(), sanitise_file_name(basename(x)))
  message("Downloading to ", tmp)
  download.file(x, tmp, mode = "wb", ...)
  tmp
}

#' @importFrom grid unit
#' @importFrom grid convertUnit
#' @importFrom grid rasterGrob
#' @importFrom png readPNG
get_png_raster_grob <- function(x, unit_to)
{
  panel <- readPNG(x, info = TRUE)
  panelDim <- attr(panel, "info")[["dim"]]
  panelDpi <- attr(panel, "info")[["dpi"]]
  if(is.null(panelDpi)) # DPI not always provided in file
  {
    panelDpi <- 300
  }
  panelSize <-
    (panelDim / panelDpi) %>%
    unit(units = "inches") %>%
    convertUnit(unitTo = unit_to)
  rasterGrob(
    panel,
    width = panelSize[1],
    height = panelSize[2])
}

#' @importFrom tiff readTIFF
get_tiff_raster_grob <- function(x, unit_to)
{
  panel <- readTIFF(x, info = TRUE)
  panelDim <- c(ncol(panel), nrow(panel))
  if(!identical(
    attr(panel, "x.resolution"),
    attr(panel, "y.resolution")))
  {
    warning("Non-identical x/y resolutions.")
  }
  panelDpi <- attr(panel, "x.resolution")
  panelSize <-
    (panelDim / panelDpi) %>%
    unit(units = "inches") %>%
    convertUnit(unitTo = unit_to)
  rasterGrob(
    panel,
    width = panelSize[1],
    height = panelSize[2])
}

#' @importFrom jpeg readJPEG
get_jpeg_raster_grob <- function(x)
{
  panel <- readJPEG(x)
  rasterGrob(
    panel,
    width = unit(1,"npc"),
    height = unit(1, "npc"))
}

#' @importFrom rsvg rsvg
get_svg_raster_grob <- function(x)
{
  panel <- rsvg(x)
  rasterGrob(
    panel,
    width = unit(1,"npc"),
    height = unit(1, "npc"))
}

#' @importFrom assertive.files assert_all_are_readable_files
#' @importFrom ggplot2 ggplotGrob
#' @importFrom grid grobTree
#' @importFrom grid grid.grabExpr
make_grob <- function(x, unit_to, ...){
  if(is.character(x)){ # It's a PNG/JPEG/TIFF image
    x <- use_first(x)
    # Could use pathological::get_extension, but the extra package dependencies
    # aren't really worth it for a single use
    file_type <- if(grepl(pattern = "\\.png$", x = x, ignore.case = TRUE)) "png" else
      if(grepl(pattern = "\\.ti[f]{1,2}$", x = x, ignore.case = TRUE)) "tiff" else
      if(grepl(pattern = "\\.jp[e]*g$", x = x, ignore.case = TRUE)) "jpeg" else
      if(grepl(pattern = "\\.svg$", x = x, ignore.case = TRUE)) "svg" else
      stop("unsupported file format.")

    if(is_url(x))
    {
      x <- download_file(x, ...)
    }

    x %>%
      assert_all_are_readable_files(warn_about_windows = FALSE, severity = "warning")

    panel <- switch(
      file_type,
      png = get_png_raster_grob(x, unit_to),
      tiff = get_tiff_raster_grob(x, unit_to),
      jpeg = get_jpeg_raster_grob(x),
      svg = get_svg_raster_grob(x)
    )
  } else if(inherits(x = x, what = "ggplot")){
    panel <- ggplotGrob(x)
  } else if(inherits(x = x, what = "gList")){
    # Convert gList to gTree so the automatic labelling works
    panel <- do.call(grobTree, x)
  } else if(inherits(x = x, what = c("grob", "gTree"))){
    panel <- x
  } else if (inherits(x = x, what = "trellis")){
    # See http://r.789695.n4.nabble.com/lattice-grob-td1599209.html
    panel <- grid.grabExpr(print(x))
  } else {
    stop("Class of \'panel\' is not supported.")
  }
  return(panel)
}

#' @export
addPanel <- function( figure, ... ){
  .Deprecated(
    new = "add_panel",
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
  add_panel(
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
