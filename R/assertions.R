# See UnitTable in https://svn.r-project.org/R/trunk/src/library/grid/src/unit.c
GRID_UNITS <- c("npc", "cm", "inches", "lines", "native", "null", "snpc", "mm", "points", "picas", "bigpts", "dida", "cicero", "scaledpts", "strwidth", "strheight", "strascent", "strdescent", "char", "grobx", "groby", "grobwidth", "grobheight", "grobascent", "grobdescent", "mylines", "mychar", "mystrwidth", "mystrheight", "centimetre", "centimetres", "centimeter", "centimeters", "in", "inch", "line", "millimetre", "millimetres", "millimeter", "millimeters", "point", "pt")

#' Is the input a valid unit type?
#'
#' Check if the input is a valid unit for the \code{grid} package.
#' @param x Input to check.
#' @param Not intended to be called directly.
#' @return \code{is_a_valid_unit_type} returns \code{TRUE} if the input is a
#' valid unit type.  \code{assert_is_a_valid_unit_type} throws an error in the
#' event of failure.
#' @noRd
#' @importFrom assertive.base get_name_in_parent
#' @importFrom assertive.base false
#' @importFrom assertive.types is_a_string
is_a_valid_unit_type <- function(x, .xname = get_name_in_parent(x))
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
    return(false("%s (= '%s') is not a valid unit.", .xname, x))
  }
  TRUE
}

#' @importFrom assertive.base assert_engine
#' @importFrom assertive.base get_name_in_parent
assert_is_a_valid_unit_type <- function(x, severity = getOption("assertive.severity", "stop"))
{
  assert_engine(
    is_a_valid_unit_type,
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
#' @importFrom assertive.properties assert_has_all_attributes
#' @importFrom assertive.types assert_is_inherited_from
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
