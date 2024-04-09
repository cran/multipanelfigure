# The complete, canonical list of grid units is found in UnitTable in
# https://svn.r-project.org/R/trunk/src/library/grid/src/unit.c
# GRID_UNITS contains all those units, except the following, which are not
# supported:
# null
# snpc
# strwidth
# strheight
# strascent
# strdescent
# grobx
# groby
# grobwidth
# grobheight
# grobascent
# grobdescent
# mylines
# mychar
# mystrwidth
# mystrheight
GRID_UNITS <- c("npc", "cm", "inches", "lines", "native", "snpc", "mm", "points", "picas", "bigpts", "dida", "cicero", "scaledpts", "char", "centimetre", "centimetres", "centimeter", "centimeters", "in", "inch", "line", "millimetre", "millimetres", "millimeter", "millimeters", "point", "pt")

#' Is the input a supported unit type?
#'
#' Check if the input is a supported unit for the \code{grid} package.
#' @param x Input to check.
#' @param Not intended to be called directly.
#' @return \code{is_a_supported_unit_type} returns \code{TRUE} if the input is a
#' supported unit type.  \code{assert_is_a_supported_unit_type} throws an error in the
#' event of failure.
#' @references Graumann, J., and Cotton, R.J. (2018). multipanelfigure: Simple
#' Assembly of Multiple Plots and Images into a Compound Figure. Journal of
#' Statistical Software 84. doi: 10.18637/jss.v084.c03
#' @noRd
is_a_supported_unit_type <- function(x, .xname = get_name_in_parent(x))
{
  # For more general use, this should be vectorised, but this is intended for
  # use only with multipanelfigure, which demands single unit, so the scalar
  # form is more appropriate.
  if(!(ok <- is_a_string(x, .xname)))
  {
    return(ok)
  }
  if(!x %in% GRID_UNITS)
  {
    return(false("%s (= '%s') is not a supported unit.", .xname, x))
  }
  TRUE
}

assert_is_a_supported_unit_type <- function(x, severity = getOption("assertive.severity", "stop"))
{
  assert_engine(
    is_a_supported_unit_type,
    x,
    .xname = get_name_in_parent(x),
    severity = severity
  )
}

#' Check that the input is a multipanelfigure
#'
#' Checks that the input is of class \code{multipanelfigure} and has the
#' appropriate attributes.
#' @param x Object to check.
#' @references Graumann, J., and Cotton, R.J. (2018). multipanelfigure: Simple
#' Assembly of Multiple Plots and Images into a Compound Figure. Journal of
#' Statistical Software 84. doi: 10.18637/jss.v084.c03
#' @export
assert_is_multipanelfigure <- function(x)
{
  assert_is_inherited_from(x, "multipanelfigure")
  assert_has_all_attributes(
    x,
    c(
      "multipanelfigure.panelsFree",
      "multipanelfigure.panelLabelType",
      "multipanelfigure.unit"
    )
  )
}
