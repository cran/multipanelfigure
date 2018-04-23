#' Capture a base plot
#'
#' @aliases capture_base_plot capturebaseplot
#' @description Capture a plot drawn using base graphics as a grid \code{grob}.
#' @param expr A expression that draws a plot using base graphics.
#' @return An object of class \code{gTree}.
#' @note A side effect of this function is that plots get drawn twice: once as
#' a base plot, and secondly as a grid plot.
#' @references Graumann, J., and Cotton, R.J. (2018). multipanelfigure: Simple
#' Assembly of Multiple Plots and Images into a Compound Figure. Journal of
#' Statistical Software 84. doi: 10.18637/jss.v084.c03
#' @seealso \code{\link[gridGraphics]{grid.echo}}, \code{\link[grid]{grid.grab}}
#' @examples
#' p <- capture_base_plot(hist(rnorm(1000), seq(-4, 4, 0.2)))
#' grid::grid.draw(p)
#' # If the plot takes multiple lines to draw, then wrap the code in braces.
#' p2 <- capture_base_plot({
#'   par(las = 1)
#'   plot(1:5)
#'   title("One to five")
#' })
#' grid::grid.draw(p2)
#' @importFrom gridGraphics grid.echo
#' @importFrom grid grid.grab
#' @export
capture_base_plot <- function(expr)
{
  force(expr)
  gridGraphics::grid.echo()
  grid::grid.grab()
}

#' @export
capturebaseplot <- function( ... ){
  .Deprecated(
    new = "capture_base_plot",
    package = "multipanelfigure")
  capture_base_plot( ... )
}
