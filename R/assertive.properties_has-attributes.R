#' @noRd
#' @author Richard Cotton <richierocks@gmail.com>
#' @importFrom assertive.base get_name_in_parent
#' @importFrom assertive.base false
has_any_attributes <- function(x, .xname = get_name_in_parent(x))
{
  if(is_empty(attributes(x)))
  {
    return(false("%s has no attributes.", .xname))
  }
  TRUE
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @noRd
#' @importFrom assertive.base get_name_in_parent
#' @importFrom assertive.base false
has_no_attributes <- function(x, .xname = get_name_in_parent(x))
{
  attr_names_x <- names(attributes(x))
  if(!is_empty(attr_names_x))
  {
    return(
      false(
        ngettext(
          length(attr_names_x),
          "%s has the attribute %s.",
          "%s has the attributes %s."
        ), 
        .xname, 
        toString(attr_names_x)
      )
    )
  }
  TRUE
}

#' @author Richard Cotton <richierocks@gmail.com>
#' @importFrom assertive.base bapply
#' @importFrom assertive.base set_cause
#' @importFrom assertive.base get_name_in_parent
#' @noRd
has_attributes <- function(x, attrs, .xname = get_name_in_parent(x))
{
  if(is_empty(attrs)) return(logical())
  set_cause(
    bapply(attrs, function(at) is_not_null(attr(x, at))),
    "no attr"
  )
}
