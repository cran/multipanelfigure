#' Print a multi-panel figure
#'
#' Prints and object of class \code{multipanelfigure}.
#' @param x An object of class \code{multipanelfigure}.
#' @param newpage Logical. If \code{TRUE}, a new device page is opened before
#' drawing.
#' @param ... Passed from other print methods.
#' @return The input \code{x} is invisibly returned, but the method is mostly
#' invoked for the side effect of printing the plot to the current device.
#' @references Graumann, J., and Cotton, R.J. (2018). multipanelfigure: Simple
#' Assembly of Multiple Plots and Images into a Compound Figure. Journal of
#' Statistical Software 84. doi: 10.18637/jss.v084.c03
#' @examples
#' p <- lattice::xyplot(dist ~ speed, cars)
#' figure <- multi_panel_figure(
#'   width = 100, height = 100,
#'   rows = 1, columns = 1
#' )
#' # With no panels, printing shows the layout
#' print(figure)
#' figure <- fill_panel(figure, p)
#' # After a panel is added, printing shows the plot.
#' print(figure) # shows plot
#' @export
print.multipanelfigure <- function(x, newpage = TRUE, ...)
{
  if(is_empty(x$grobs))
  {
      grid::grid.layout(nrow = nrow(x), heights = x$heights, ncol = ncol(x),
                  widths = x$widths, respect = x$respect) %>%
      custom.grid.show.layout()
  } else
  {
    if(newpage)
    {
      grid::grid.newpage()
    }
    grid::grid.draw(x)
  }
  invisible(x) # For consistency with other print methods
}

# Copied and modified from grid::grid.show.layout
custom.grid.show.layout <- function (l, newpage = TRUE, vp.ex = 0.8, bg = "light grey",
          cell.border = "blue", cell.fill = "light blue", cell.label = TRUE,
          label.col = "blue", unit.col = "red", vp = NULL)
{
  if (newpage)
    grid::grid.newpage()
  if (!is.null(vp))
    grid::pushViewport(vp)
  grid::grid.rect(gp = grid::gpar(col = NULL, fill = bg))
  vp.mid <- grid::viewport(0.5, 0.5, vp.ex, vp.ex, layout = l)
  grid::pushViewport(vp.mid)
  grid::grid.rect(gp = grid::gpar(fill = "white"))
  gp.red <- grid::gpar(col = unit.col)
  for (i in 1L:l$nrow) for (j in 1L:l$ncol) {
    vp.inner <- grid::viewport(layout.pos.row = i, layout.pos.col = j)
    grid::pushViewport(vp.inner)
    grid::grid.rect(gp = grid::gpar(col = cell.border, fill = cell.fill))
    if (cell.label)
      {
        if(
          is_divisible_by(i,2) &&
          is_divisible_by(j,2))
        {
          grid::grid.text(paste0("(", i/2, ", ", j/2, ")"), gp = grid::gpar(col = label.col))
        }
      }
    if (j == 1)
      grid::grid.text(as.character(round(l$heights[i, top = FALSE], digits = 2)),
                gp = gp.red, just = c("right", "centre"), x = grid::unit(-0.05,
                                                                   "inches"), y = grid::unit(0.5, "npc"), rot = 0)
    if (i == l$nrow)
      grid::grid.text(as.character(round(l$widths[j, top = FALSE], digits = 2)),
                gp = gp.red, just = c("centre", "top"), x = grid::unit(0.5,
                                                                 "npc"), y = grid::unit(-0.05, "inches"), rot = 0)
    if (j == l$ncol)
      grid::grid.text(as.character(round(l$heights[i, top = FALSE], digits = 2)),
                gp = gp.red, just = c("left", "centre"), x = grid::unit(1,
                                                                  "npc") + grid::unit(0.05, "inches"), y = grid::unit(0.5,
                                                                                                          "npc"), rot = 0)
    if (i == 1)
      grid::grid.text(as.character(round(l$widths[j, top = FALSE], digits = 2)),
                gp = gp.red, just = c("centre", "bottom"), x = grid::unit(0.5,
                                                                    "npc"), y = grid::unit(1, "npc") + grid::unit(0.05, "inches"),
                rot = 0)
    grid::popViewport()
  }
  grid::popViewport()
  if (!is.null(vp))
    grid::popViewport()
  invisible(vp.mid)
}

round.unit.list <- function(x, digits = 0)
{
  if (getRversion() >= "4.0.0") {
    unitType <- get("unitType", envir=asNamespace("grid"))
    ## Use unitType()
    ## Do NOT use grid::unitType() because R < 4.0.0 will
    ## then complain about it not being exported
    saved_unit <- x %>%
      unitType() %>%
      magrittr::extract2(1)
  } else {
    saved_unit <- x %>%
      rapply(attr, classes = "unit", which = "unit") %>%
      unlist() %>%
      magrittr::extract2(1)
  }
  x %>%
    grid::convertUnit(saved_unit) %>%
    as.numeric() %>%
    round(digits = digits) %>%
    grid::unit(saved_unit) %>%
    return()
}

round.unit <- function(x, digits = 0)
{
  if (getRversion() >= "4.0.0") {
    unitType <- get("unitType", envir=asNamespace("grid"))
    ## Use unitType()
    ## Do NOT use grid::unitType() because R < 4.0.0 will
    ## then complain about it not being exported
    saved_unit <- x %>%
      unitType()
  } else {
    saved_unit <- x %>%
      attr("unit")
  }
  x %>%
    as.numeric() %>%
    round(digits = digits) %>%
    grid::unit(saved_unit) %>%
    return()
}
