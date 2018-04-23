#' @title save_multi_panel_figure
#' @description A convenience function wrapping \code{\link[ggplot2]{ggsave}}
#' from \pkg{ggplot2} for easy saving of \code{\link[gtable]{gtable}} objects
#' constructed by \code{\link{multi_panel_figure}} taking into account the
#' table's dimensions.
#' @details Plot dimensions are determined using
#' \code{\link{figure_height}} and \code{\link{figure_width}}.
#'
#' The Device type to use is guessed from the \code{filename} extension.
#' Currently supported are "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff",
#' "png", "bmp", "svg" or "wmf" (windows only).
#' @param figure Object of classes \code{multipanelfigure}/
#' \code{\link[gtable]{gtable}} as produced by \code{\link{multi_panel_figure}}.
#' @param filename Single \code{\link{character}} object representing file
#' name/path to create on disk.
#' @param dpi Single \code{\link{numeric}} indicating the plot resolution.
#' Applies only to raster output types.
#' @param ... Other arguments passed to \code{\link[ggplot2]{ggsave}}.
#' @author Johannes Graumann
#' @references Graumann, J., and Cotton, R.J. (2018). multipanelfigure: Simple
#' Assembly of Multiple Plots and Images into a Compound Figure. Journal of
#' Statistical Software 84. doi: 10.18637/jss.v084.c03
#' @export
#' @seealso \code{\link[ggplot2]{ggsave}}, \code{\link{figure_width}},
#' \code{\link{figure_height}}
#' @importFrom assertive.numbers assert_all_are_greater_than
#' @importFrom assertive.numbers assert_all_are_whole_numbers
#' @importFrom assertive.types assert_is_a_number
#' @importFrom assertive.types assert_is_a_string
#' @importFrom ggplot2 ggsave
#' @importFrom magrittr %>%
#' @examples
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
#' \dontrun{
#' # Save the figure
#' figure %>%
#'   save_multi_panel_figure(
#'     filename = paste0(
#'     tempfile(),
#'     ".png"))
#' }
save_multi_panel_figure <- function(
  figure,
  filename,
  dpi = 300,
  ...)
{
  #######################
  # Check prerequisites #
  #######################

  figure %>%
    assert_is_multipanelfigure

  filename %>%
    assert_is_a_string

  dpi %>%
    assert_is_a_number %>%
    assert_all_are_whole_numbers() %>%
    assert_all_are_greater_than(0)

  ##############
  # Processing #
  ##############

  filename %>%
    ggsave(
      plot = figure,
      height = figure %>%
        figure_height,
      width = figure %>%
        figure_width,
      dpi = dpi,
      units = "mm",
      ...)

  # Return 'invisible' (mimicking 'ggplot2::ggsave')
  invisible()
}